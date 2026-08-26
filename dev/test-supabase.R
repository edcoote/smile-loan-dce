# dev/test-supabase.R -------------------------------------------------------
# Smoke test for the Supabase backing store. Run this once after applying
# sql/schema.sql and before pointing any respondent at the app.
#
#   Rscript dev/test-supabase.R
#
# Checks, in order:
#   1. the environment variables are set
#   2. the anon key can INSERT into all four tables
#   3. the postcode-district constraint actually rejects a full postcode
#   4. the anon key CANNOT read (it must not be able to)
#   5. the service key can read, and the latest-revision views collapse correctly
#
# Test rows use rid values prefixed TEST_ and are deleted at the end.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
if (!dir.exists("app/R")) stop("Run from the repository root: Rscript dev/test-supabase.R")
for (f in sort(list.files("app/R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

PASS <- 0L; FAIL <- 0L
ok <- function(label, expr) {
  r <- tryCatch(isTRUE(expr), error = function(e) structure(FALSE, msg = conditionMessage(e)))
  if (isTRUE(r)) { PASS <<- PASS + 1L; cat(sprintf("  ok    %s\n", label)) }
  else { FAIL <<- FAIL + 1L; cat(sprintf("  FAIL  %s\n        %s\n", label, attr(r, "msg") %||% "")) }
}

URL <- Sys.getenv("SUPABASE_URL")
ANON <- Sys.getenv("SUPABASE_ANON_KEY")
SVC  <- Sys.getenv("SUPABASE_SERVICE_KEY")

cat("\n== Environment ==\n")
ok("SUPABASE_URL is set", nzchar(URL))
ok("SUPABASE_ANON_KEY is set", nzchar(ANON))
if (!nzchar(SVC)) cat("  note  SUPABASE_SERVICE_KEY not set - read tests will be skipped\n")
for (pkg in c("httr", "jsonlite"))
  ok(sprintf("%s installed", pkg), requireNamespace(pkg, quietly = TRUE))
if (FAIL > 0) { cat("\nFix the above first.\n"); quit(status = 1) }

st  <- store_postgrest()
rid <- paste0("TEST_", format(Sys.time(), "%Y%m%d%H%M%S"))
cat("\n== Insert (test rid:", rid, ") ==\n")

ok("respondents insert", st$append("respondents", row_respondent(rid, 1L, list(
  status = "partial", path = "A", stage_key = "considering", arm = "bws_first",
  modules_served = "bws+dce", age_band = "55-59", quota_band = "50-65",
  consent = 1L, instrument = INSTRUMENT_ID, app_version = APP_VERSION,
  design_version = DESIGN_VERSION, config = "smoketest", session = "smoketest",
  seconds_total = 1, page_reached = 1L, n_pages = 30L))))

ok("items insert", st$append("items",
  rows_items(rid, 1L, "core", list(core_srh = "Good", dem_postcode = "CW9"), 9)))

set.seed(1)
ok("dce insert (two rows, one task)",
  st$append("dce", rows_dce(rid, 1L, dce_tasks_for(CFG)[1, ], "A", "No", "pay_privately", 25000, 30)))

s1 <- bws_sets_for(CFG)[[1]]
ids <- BWS_ITEMS$item_id[s1$items]
ok("bws insert (one row per item shown)",
  st$append("bws", rows_bws(rid, 1L, s1, ids[1], ids[2], 20)))

cat("\n== Constraints ==\n")
bad <- rows_items(rid, 1L, "demographics", list(dem_postcode = "CW9 5AB"), 5)
ok("full postcode is rejected by the database", {
  got <- suppressWarnings(st$append("items", bad))
  isFALSE(got)
})

cat("\n== Security ==\n")
anon_read <- tryCatch({
  r <- httr::GET(paste0(sub("/$", "", URL), "/rest/v1/v_respondents?select=rid&limit=1"),
                 httr::add_headers(apikey = ANON, Authorization = paste("Bearer", ANON),
                                   "Accept-Profile" = "survey"))
  httr::status_code(r)
}, error = function(e) NA_integer_)
ok("anon key cannot read responses (expects 401/403/404, not 200)",
   !identical(anon_read, 200L))

if (nzchar(SVC)) {
  cat("\n== Read back (service key) ==\n")
  db <- tryCatch(st$read(), error = function(e) NULL)
  ok("all four tables readable", !is.null(db) && length(db) == 4)
  mine <- lapply(db, function(x) if (NROW(x)) x[x$rid == rid, , drop = FALSE] else x)
  ok("respondent row present", nrow(mine$respondents) == 1)
  ok("two dce rows for one task", nrow(mine$dce) == 2)
  ok("exactly one alternative chosen", sum(mine$dce$chosen) == 1)
  ok("non-loan outcome preserved", all(mine$dce$stage2_outcome == "pay_privately"))
  ok("one row per bws item shown", nrow(mine$bws) == length(ids))
  ok("full postcode never landed", !any(grepl(" ", mine$items$value)))

  # Revision collapsing: write rev 2 and confirm the view returns only it.
  st$append("items", rows_items(rid, 2L, "core", list(core_srh = "Fair"), 4))
  Sys.sleep(1)
  db2 <- st$read()
  v <- db2$items$value[db2$items$rid == rid & db2$items$item_id == "core_srh"]
  ok("latest-revision view returns only rev 2", length(v) == 1 && v == "Fair")

  cat("\n== Export ==\n")
  f <- file.path(tempdir(), "supabase_smoketest.xlsx")
  ok("workbook exports from the live store",
     { store_export(st, f); file.exists(f) && file.size(f) > 1000 })
}

cat("\n== Cleanup ==\n")
if (nzchar(SVC)) {
  del <- vapply(STORE_TABLES, function(t) {
    r <- httr::DELETE(paste0(sub("/$", "", URL), "/rest/v1/", t, "?rid=eq.", rid),
                      httr::add_headers(apikey = SVC, Authorization = paste("Bearer", SVC),
                                        "Content-Profile" = "survey"))
    httr::status_code(r) < 300
  }, logical(1))
  ok("test rows removed", all(del))
} else {
  cat("  note  no service key - remove rid", rid, "by hand in the Supabase table editor\n")
}

cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
if (FAIL > 0) quit(status = 1)
cat("Supabase store is live. Set the same two variables on the host and the app will use it.\n")
