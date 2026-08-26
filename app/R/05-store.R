# 05-store.R ----------------------------------------------------------------
# Storage layer.
#
# DESIGN
# * Append-only. Nothing is ever updated in place; going Back and changing an
#   answer writes a new row with a higher `rev`. Readers collapse to the last
#   revision per key. This keeps the audit trail intact and makes concurrent
#   writes safe without transactions.
# * Write-through. Every page advance flushes to the backend, so partial
#   responses are retained and reportable, as the v2 field controls require.
#   A respondent who abandons at DCE task 7 leaves seven usable tasks.
# * Four tables, tidy long, keyed on `rid`:
#     respondents  one row per revision: routing, arm, config, status, timings
#     items        one row per item answered (screener, core, demographics)
#     dce          one row per ALTERNATIVE per task (i.e. two rows per task)
#     bws          one row per ITEM SHOWN per set, with best/worst indicators
#   The dce and bws shapes are already what mlogit/apollo/support.BWS expect,
#   so the analysis pipeline does not have to reshape anything.
# * Backends share one interface, so the same app runs on a server (csv),
#   in the browser (memory, download-only), or against Supabase (postgrest).
# ---------------------------------------------------------------------------

STORE_TABLES <- c("respondents", "items", "dce", "bws")

# Blank typed frames, so an empty store still has a schema.
store_schema <- function() list(
  respondents = data.frame(
    rid = character(), rev = integer(), ts_utc = character(), status = character(),
    path = character(), stage_key = character(), arm = character(),
    modules_served = character(), age_band = character(), quota_band = character(),
    screen_out_reason = character(), consent = integer(),
    mdas_total = integer(), mdas_flag = character(),
    dominance_failed = integer(), income_mid = numeric(),
    seconds_total = numeric(), page_reached = integer(), n_pages = integer(),
    instrument = character(), app_version = character(), design_version = character(),
    config = character(), session = character(), stringsAsFactors = FALSE),
  items = data.frame(
    rid = character(), rev = integer(), ts_utc = character(), module = character(),
    item_id = character(), value = character(), seconds = numeric(),
    stringsAsFactors = FALSE),
  dce = data.frame(
    rid = character(), rev = integer(), ts_utc = character(), task_order = integer(),
    set_id = character(), block = integer(), dominance = integer(), side_flipped = integer(),
    alt = character(), rate = numeric(), fee = numeric(), monthly = numeric(),
    chosen = integer(), stage2_take = character(), stage2_outcome = character(),
    seconds = numeric(), stringsAsFactors = FALSE),
  bws = data.frame(
    rid = character(), rev = integer(), ts_utc = character(), set_order = integer(),
    set_id = character(), position = integer(), item_id = character(),
    best = integer(), worst = integer(), seconds = numeric(), stringsAsFactors = FALSE)
)

# --- Backend: in-memory (default in the browser / shinylive) ---------------
store_memory <- function() {
  db <- store_schema()
  rev <- new.env(parent = emptyenv())
  list(
    kind = "memory",
    next_rev = function(rid) { k <- paste0("r", rid); v <- (rev[[k]] %||% 0L) + 1L; rev[[k]] <- v; v },
    append = function(table, df) { if (nrow(df)) db[[table]] <<- rbind(db[[table]], df); invisible(TRUE) },
    read = function() db,
    healthy = function() TRUE
  )
}

# --- Backend: CSV on disk (default on a server) ---------------------------
# One file per table, appended under a lock. dir.create is atomic on POSIX and
# on Windows, which is enough for the concurrency a survey generates.
store_csv <- function(path = CFG$store_path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  files <- setNames(file.path(path, paste0(STORE_TABLES, ".csv")), STORE_TABLES)
  sch <- store_schema()
  for (t in STORE_TABLES)
    if (!file.exists(files[[t]]))
      utils::write.csv(sch[[t]], files[[t]], row.names = FALSE, fileEncoding = "UTF-8")

  with_lock <- function(expr) {
    lock <- file.path(path, ".lock")
    for (i in 1:200) {
      if (dir.create(lock, showWarnings = FALSE)) {
        on.exit(unlink(lock, recursive = TRUE), add = TRUE)
        return(force(expr))
      }
      Sys.sleep(0.01)
    }
    warning("store_csv: could not acquire lock; writing without it")
    force(expr)
  }

  # Revision counters are cached per rid. A rid belongs to exactly one session,
  # so the cache is authoritative after the first lookup; without it every page
  # advance would re-read the whole respondents file and fielding would degrade
  # quadratically in the number of responses collected.
  rev <- new.env(parent = emptyenv())

  list(
    kind = "csv",
    next_rev = function(rid) {
      k <- paste0("r", rid)
      if (is.null(rev[[k]])) {
        rev[[k]] <- with_lock({
          d <- utils::read.csv(files[["respondents"]], stringsAsFactors = FALSE, colClasses = "character")
          r <- if (nrow(d)) suppressWarnings(as.integer(d$rev[d$rid == rid])) else integer(0)
          if (!length(r) || all(is.na(r))) 0L else max(r, na.rm = TRUE)
        })
      }
      rev[[k]] <- rev[[k]] + 1L
      rev[[k]]
    },
    append = function(table, df) {
      if (!nrow(df)) return(invisible(TRUE))
      with_lock(utils::write.table(df, files[[table]], sep = ",", row.names = FALSE,
                                   col.names = FALSE, append = TRUE, qmethod = "double",
                                   fileEncoding = "UTF-8"))
      invisible(TRUE)
    },
    read = function() {
      with_lock(setNames(lapply(STORE_TABLES, function(t)
        utils::read.csv(files[[t]], stringsAsFactors = FALSE)), STORE_TABLES))
    },
    healthy = function() all(file.exists(files))
  )
}

# --- Backend: Supabase / PostgREST ----------------------------------------
# Same four tables. DDL and row-level-security policy are in sql/schema.sql.
# The anon key must be INSERT-only: this app never needs to read back, and the
# admin panel should point at a service-role connection, not the survey's.
store_postgrest <- function(url = Sys.getenv("SUPABASE_URL"),
                            key = Sys.getenv("SUPABASE_ANON_KEY"),
                            service_key = Sys.getenv("SUPABASE_SERVICE_KEY"),
                            schema = "survey") {
  if (!nzchar(url) || !nzchar(key))
    stop("store_postgrest needs SUPABASE_URL and SUPABASE_ANON_KEY")
  for (pkg in c("httr", "jsonlite"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("store_postgrest needs the ", pkg, " package: install.packages('", pkg, "')")
  base <- sub("/$", "", url)
  rev <- new.env(parent = emptyenv())

  post <- function(table, df) {
    r <- httr::POST(
      paste0(base, "/rest/v1/", table),
      httr::add_headers(apikey = key, Authorization = paste("Bearer", key),
                        "Content-Type" = "application/json",
                        "Content-Profile" = schema, Prefer = "return=minimal"),
      body = jsonlite::toJSON(df, na = "null", dataframe = "rows"), encode = "raw")
    if (httr::status_code(r) >= 300)
      warning("postgrest ", table, ": ", httr::status_code(r), " ",
              substr(httr::content(r, "text", encoding = "UTF-8"), 1, 300))
    invisible(httr::status_code(r) < 300)
  }

  # Reading requires the SERVICE key. The anon key is insert-only by design, so
  # the admin panel and the export must run on a connection the browser never
  # sees. If no service key is set, reading fails loudly rather than silently
  # returning nothing.
  get <- function(view) {
    if (!nzchar(service_key))
      stop("Reading needs SUPABASE_SERVICE_KEY. The anon key is insert-only.")
    r <- httr::GET(
      paste0(base, "/rest/v1/", view, "?select=*"),
      httr::add_headers(apikey = service_key, Authorization = paste("Bearer", service_key),
                        "Accept-Profile" = schema))
    if (httr::status_code(r) >= 300)
      stop("postgrest read ", view, ": ", httr::status_code(r), " ",
           substr(httr::content(r, "text", encoding = "UTF-8"), 1, 300))
    d <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"),
                            simplifyDataFrame = TRUE)
    if (!length(d) || !NROW(d)) store_schema()[[sub("^v_", "", view)]] else as.data.frame(d)
  }

  list(
    kind = "postgrest",
    next_rev = function(rid) { k <- paste0("r", rid); v <- (rev[[k]] %||% 0L) + 1L; rev[[k]] <- v; v },
    append = function(table, df) if (nrow(df)) post(table, df) else invisible(TRUE),
    # The v_ views already collapse to the latest revision per key, so
    # store_latest() over them is a no-op rather than a second pass.
    read = function() setNames(lapply(paste0("v_", STORE_TABLES), get), STORE_TABLES),
    can_read = function() nzchar(service_key),
    healthy = function() TRUE
  )
}

# --- Backend: any DBI database ---------------------------------------------
# One backend covering SQLite, PostgreSQL, MariaDB and DuckDB, so the choice of
# database is a deployment decision rather than a code change. `connect` is a
# zero-argument function returning a fresh DBI connection; it is called again
# if the connection has dropped, which matters for managed Postgres services
# that close idle connections after a few minutes.
store_dbi <- function(connect, label = "dbi") {
  if (!requireNamespace("DBI", quietly = TRUE))
    stop("store_dbi needs the DBI package: install.packages('DBI')")
  con <- NULL
  live <- function() {
    if (is.null(con) || !DBI::dbIsValid(con)) con <<- connect()
    con
  }
  # Retry once on failure: a dropped connection should cost a reconnect, not a
  # lost response.
  with_con <- function(f) {
    tryCatch(f(live()), error = function(e) { con <<- NULL; f(live()) })
  }

  sch <- store_schema()
  with_con(function(cn) {
    existing <- DBI::dbListTables(cn)
    for (t in STORE_TABLES) {
      if (!(t %in% existing)) {
        DBI::dbCreateTable(cn, t, sch[[t]])
        try(DBI::dbExecute(cn, sprintf("create index %s_rid_idx on %s (rid)", t, t)), silent = TRUE)
      }
    }
  })

  rev <- new.env(parent = emptyenv())

  list(
    kind = label,
    next_rev = function(rid) {
      k <- paste0("r", rid)
      if (is.null(rev[[k]])) {
        rev[[k]] <- with_con(function(cn) {
          r <- DBI::dbGetQuery(cn, "select max(rev) as m from respondents where rid = ?", list(rid))
          if (!nrow(r) || is.na(r$m[1])) 0L else as.integer(r$m[1])
        })
      }
      rev[[k]] <- rev[[k]] + 1L
      rev[[k]]
    },
    append = function(table, df) {
      if (!nrow(df)) return(invisible(TRUE))
      with_con(function(cn) DBI::dbAppendTable(cn, table, as.data.frame(df)))
      invisible(TRUE)
    },
    read = function() with_con(function(cn)
      setNames(lapply(STORE_TABLES, function(t) DBI::dbReadTable(cn, t)), STORE_TABLES)),
    can_read = function() TRUE,
    disconnect = function() if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con),
    healthy = function() tryCatch(with_con(DBI::dbIsValid), error = function(e) FALSE),
    # Hot backup. VACUUM INTO takes a consistent snapshot of a database that is
    # being written to, so this is safe to run on a cron or Task Scheduler
    # during fielding. Copying the file with the OS is NOT safe: it can catch a
    # write mid-transaction and produce a snapshot that will not open.
    snapshot = function(dest) {
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (file.exists(dest)) unlink(dest)
      with_con(function(cn) DBI::dbExecute(cn, sprintf("vacuum into '%s'", dest)))
      invisible(dest)
    },
    # Returns "ok" on a healthy database, or a description of the corruption.
    integrity = function() with_con(function(cn)
      DBI::dbGetQuery(cn, "pragma integrity_check")[[1]][1])
  )
}

# SQLite. No account, no service, no network — one file that is fully ACID and
# handles survey-rate concurrency comfortably. The right choice for a
# self-hosted Shiny Server or Posit Connect deployment where the disk persists.
store_sqlite <- function(path = file.path(CFG$store_path, "responses.sqlite")) {
  if (!requireNamespace("RSQLite", quietly = TRUE))
    stop("store_sqlite needs RSQLite: install.packages('RSQLite')")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  store_dbi(function() {
    cn <- DBI::dbConnect(RSQLite::SQLite(), path)
    # WAL lets readers and a writer coexist; busy_timeout makes a concurrent
    # writer wait rather than fail. Without both, two respondents submitting at
    # the same moment can produce "database is locked".
    DBI::dbExecute(cn, "pragma journal_mode = WAL")
    DBI::dbExecute(cn, "pragma busy_timeout = 10000")
    DBI::dbExecute(cn, "pragma synchronous = NORMAL")
    cn
  }, label = "sqlite")
}

# Any PostgreSQL: Neon, Azure Database for PostgreSQL, RDS, Render, or a
# database on your own server. Reads DATABASE_URL in the standard
# postgres://user:password@host:port/dbname form.
store_postgres <- function(url = Sys.getenv("DATABASE_URL")) {
  if (!nzchar(url)) stop("store_postgres needs DATABASE_URL")
  if (!requireNamespace("RPostgres", quietly = TRUE))
    stop("store_postgres needs RPostgres: install.packages('RPostgres')")
  store_dbi(function() DBI::dbConnect(RPostgres::Postgres(), dbname = url), label = "postgres")
}

# --- Factory ---------------------------------------------------------------
# "auto" picks csv when the filesystem is writable (server) and memory when it
# is not (shinylive in the browser), so one codebase serves both deployments.
# "auto" resolution order matters. Supabase wins whenever it is configured,
# because several hosts (shinyapps.io among them) give you a writable
# filesystem that does not survive a container restart — so probing for
# writability and picking csv would look like it worked and lose the data.
store_init <- function(backend = CFG$store_backend, path = CFG$store_path) {
  if (identical(backend, "auto")) {
    if (nzchar(Sys.getenv("SUPABASE_URL")) && nzchar(Sys.getenv("SUPABASE_ANON_KEY"))) {
      backend <- "postgrest"
    } else if (nzchar(Sys.getenv("DATABASE_URL"))) {
      backend <- "postgres"
    } else if (nzchar(Sys.getenv("SURVEY_SQLITE"))) {
      backend <- "sqlite"
    } else {
      ok <- tryCatch({ dir.create(path, recursive = TRUE, showWarnings = FALSE)
                       f <- file.path(path, ".probe"); file.create(f); unlink(f); TRUE },
                     error = function(e) FALSE, warning = function(w) FALSE)
      backend <- if (isTRUE(ok)) "csv" else "memory"
    }
  }
  switch(backend,
    csv = store_csv(path), memory = store_memory(), postgrest = store_postgrest(),
    postgres = store_postgres(),
    sqlite = store_sqlite(if (nzchar(Sys.getenv("SURVEY_SQLITE")))
                            Sys.getenv("SURVEY_SQLITE")
                          else file.path(path, "responses.sqlite")),
    dbi = stop("Call store_dbi(connect) directly for a custom connection"),
    stop("Unknown store backend: ", backend))
}

# --- Row builders ----------------------------------------------------------
# Kept separate from the server so dev/simulate.R can produce identically
# shaped data without Shiny.
row_respondent <- function(rid, rev, meta) {
  base <- store_schema()$respondents
  x <- as.list(meta)[names(base)]
  names(x) <- names(base)
  x$rid <- rid; x$rev <- rev; x$ts_utc <- now_utc()
  for (nm in names(x)) if (is.null(x[[nm]]) || length(x[[nm]]) == 0) x[[nm]] <- NA
  as.data.frame(x, stringsAsFactors = FALSE)
}

rows_items <- function(rid, rev, module, answers, seconds = NA_real_) {
  answers <- answers[!vapply(answers, unset, logical(1))]
  if (!length(answers)) return(store_schema()$items)
  data.frame(rid = rid, rev = rev, ts_utc = now_utc(), module = module,
             item_id = names(answers),
             value = vapply(answers, function(v) paste(as.character(v), collapse = "|"), character(1)),
             seconds = seconds, row.names = NULL, stringsAsFactors = FALSE)
}

rows_dce <- function(rid, rev, row, choice, take, outcome, income, seconds) {
  data.frame(
    rid = rid, rev = rev, ts_utc = now_utc(), task_order = as.integer(row$task_order),
    set_id = row$set_id, block = as.integer(row$block), dominance = as.integer(row$dominance),
    side_flipped = as.integer(row$side_flipped), alt = c("A", "B"),
    rate = c(row$a_rate, row$b_rate), fee = c(row$a_fee, row$b_fee),
    monthly = c(monthly_repay(row$a_rate, income), monthly_repay(row$b_rate, income)),
    chosen = as.integer(c(choice == "A", choice == "B")),
    stage2_take = take %||% NA_character_, stage2_outcome = outcome %||% NA_character_,
    seconds = seconds, row.names = NULL, stringsAsFactors = FALSE)
}

rows_bws <- function(rid, rev, set, best_item, worst_item, seconds) {
  ids <- BWS_ITEMS$item_id[set$items]
  data.frame(
    rid = rid, rev = rev, ts_utc = now_utc(), set_order = as.integer(set$set_order),
    set_id = set$set_id, position = seq_along(ids), item_id = ids,
    best = as.integer(ids == (best_item %||% "")),
    worst = as.integer(ids == (worst_item %||% "")),
    seconds = seconds, row.names = NULL, stringsAsFactors = FALSE)
}

# --- Reading and export ----------------------------------------------------
# Collapse the append-only log to the latest revision per key.
store_latest <- function(db) {
  last <- function(d, keys) {
    if (!nrow(d)) return(d)
    d <- d[order(d$rid, d$rev), ]
    k <- do.call(paste, c(d[keys], sep = "\r"))
    d[!duplicated(k, fromLast = TRUE), , drop = FALSE]
  }
  list(
    respondents = last(db$respondents, "rid"),
    items = last(db$items, c("rid", "item_id")),
    dce   = last(db$dce,   c("rid", "set_id", "alt")),
    bws   = last(db$bws,   c("rid", "set_id", "item_id"))
  )
}

store_export <- function(store, file, latest = TRUE) {
  db <- store$read()
  if (latest) db <- store_latest(db)
  db$README <- data.frame(
    field = c("instrument", "app_version", "design_version", "exported_utc",
              "respondents", "items", "dce_rows", "bws_rows", "note"),
    value = c(INSTRUMENT_ID, APP_VERSION, DESIGN_VERSION, now_utc(),
              nrow(db$respondents), nrow(db$items), nrow(db$dce), nrow(db$bws),
              "Append-only log collapsed to latest revision per key. dce has two rows per task (one per alternative); bws has one row per item shown."),
    stringsAsFactors = FALSE)
  write_workbook(db[c("README", STORE_TABLES)], file)
}
