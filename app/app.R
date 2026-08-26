# app.R ---------------------------------------------------------------------
# Barriers to Full-Arch Rehabilitation — survey pathway v2, Shiny implementation.
#
# RUN
#   install.packages("shiny")          # only hard dependency
#   shiny::runApp("app")               # from the repository root
#
# URL PARAMETERS (all optional)
#   ?admin=<key>          fielding monitor
#   ?dev=1                developer view on the thank-you page
#   ?bws_items=7          override item count  (7 | 9 | 11 | 13)
#   ?dce_tasks=6          override task count
#   ?dce_block=1          serve one block only
#   ?split=1              split-sample mode
#   ?src=fb               record a recruitment source with the response
#
# The server is a small state machine over the flow list built in 04-flow.R.
# All persistence goes through the store in 05-store.R and happens on every
# page advance, so abandoned responses are retained rather than lost.
# ---------------------------------------------------------------------------

library(shiny)

for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

STORE <- store_init()

ui <- fluidPage(
  tags$head(tags$style(HTML(APP_CSS)),
            tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
  uiOutput("page")
)

server <- function(input, output, session) {

  q <- parseQueryString(isolate(session$clientData$url_search))

  cfg <- CFG
  if (!is.null(q$bws_items)) cfg$bws_items <- as.integer(q$bws_items)
  if (!is.null(q$dce_tasks)) cfg$dce_tasks <- as.integer(q$dce_tasks)
  if (!is.null(q$dce_block)) cfg$dce_block <- as.integer(q$dce_block)
  if (!is.null(q$split))     cfg$split_sample <- identical(q$split, "1")

  is_admin <- !is.null(q$admin) && identical(q$admin, cfg$admin_key)

  rv <- reactiveValues(
    cfg = cfg, rid = new_rid(), rev = 0L, i = 1L,
    flow = flow_preamble(), msg = NULL, income = 20000,
    status = "landed", screen_reason = NULL,
    meta = list(instrument = INSTRUMENT_ID, app_version = APP_VERSION,
                design_version = DESIGN_VERSION,
                config = paste0("bws_items=", cfg$bws_items, ";dce_tasks=", cfg$dce_tasks,
                                ";block=", cfg$dce_block, ";split=", cfg$split_sample,
                                ";src=", q$src %||% "direct"),
                session = substr(session$token, 1, 12)),
    t_page = Sys.time(), t_start = Sys.time(),
    dev = identical(q$dev, "1"))

  # --- persistence -------------------------------------------------------
  flush <- function(cap, status = NULL) {
    rv$rev <- STORE$next_rev(rv$rid)
    if (length(cap$meta)) for (nm in names(cap$meta)) rv$meta[[nm]] <- cap$meta[[nm]]
    if (!is.null(status)) rv$status <- status
    if (!is.null(cap$items) && nrow(cap$items)) { cap$items$rev <- rv$rev; STORE$append("items", cap$items) }
    if (!is.null(cap$dce)   && nrow(cap$dce))   { cap$dce$rev   <- rv$rev; STORE$append("dce",   cap$dce) }
    if (!is.null(cap$bws)   && nrow(cap$bws))   { cap$bws$rev   <- rv$rev; STORE$append("bws",   cap$bws) }
    meta <- rv$meta
    meta$status <- rv$status
    meta$path <- attr(rv$flow, "path"); meta$stage_key <- attr(rv$flow, "stage_key")
    meta$arm <- attr(rv$flow, "arm"); meta$modules_served <- attr(rv$flow, "modules_served")
    meta$screen_out_reason <- rv$screen_reason
    meta$seconds_total <- round(as.numeric(difftime(Sys.time(), rv$t_start, units = "secs")), 1)
    meta$page_reached <- rv$i; meta$n_pages <- length(rv$flow)
    STORE$append("respondents", row_respondent(rv$rid, rv$rev, meta))
  }

  # --- navigation --------------------------------------------------------
  observeEvent(input$btn_next, {
    page <- rv$flow[[rv$i]]
    rv$msg <- validate_page(page, rv$i, input, rv)
    if (!is.null(rv$msg)) return()

    secs <- round(as.numeric(difftime(Sys.time(), rv$t_page, units = "secs")), 1)
    cap <- capture_page(page, rv$i, input, rv, secs)

    # The screener is the only page that changes the shape of what follows.
    if (identical(page$type, "screener")) {
      a <- setNames(lapply(SCREENER, function(it) input[[pid(rv$i, it$id)]]),
                    vapply(SCREENER, `[[`, "", "id"))
      reason <- screen_out_reason(a)
      if (!is.null(reason)) {
        rv$screen_reason <- reason
        rv$flow <- c(rv$flow, list(list(type = "screened_out", module = "screened_out")))
        flush(cap, status = "screened_out")
        rv$i <- length(rv$flow); rv$t_page <- Sys.time()
        return()
      }
      rest <- flow_build(a, rv$cfg)
      new_flow <- c(rv$flow, rest)
      for (at in c("path", "stage_key", "arm", "modules_served"))
        attr(new_flow, at) <- attr(rest, at)
      rv$flow <- new_flow
      rv$status <- "partial"
    }

    if (identical(page$type, "battery_mixed") && identical(page$module, "core_enabling")) {
      inc <- input[[pid(rv$i, "enab_income")]]
      if (!unset(inc)) rv$income <- income_mid(inc)
    }

    last <- rv$i >= length(rv$flow) - 1L &&
            identical(rv$flow[[length(rv$flow)]]$type, "thanks")
    flush(cap, status = if (identical(page$type, "demographics")) "complete" else rv$status)

    rv$i <- min(rv$i + 1L, length(rv$flow))
    rv$t_page <- Sys.time()
    session$sendCustomMessage("scrollTop", list())
  })

  observeEvent(input$btn_back, {
    rv$msg <- NULL
    rv$i <- max(2L, rv$i - 1L)   # never back into the landing page after consent
    rv$t_page <- Sys.time()
  })

  # --- render ------------------------------------------------------------
  output$page <- renderUI({
    if (is_admin) return(admin_ui())
    render_page(rv$flow[[rv$i]], rv$i, rv)
  })

  # --- developer view ----------------------------------------------------
  my_rows <- reactive({
    db <- tryCatch(STORE$read(), error = function(e) NULL)
    if (is.null(db)) return(NULL)
    d <- store_latest(db)
    lapply(d, function(x) x[x$rid == rv$rid, , drop = FALSE])
  })

  output$dev_summary <- renderTable({
    d <- my_rows(); if (is.null(d)) return(NULL)
    data.frame(table = names(d), rows = vapply(d, nrow, integer(1)), row.names = NULL)
  })

  output$dl_me <- downloadHandler(
    filename = function() paste0(rv$rid, ".csv"),
    content = function(file) {
      d <- my_rows()
      utils::write.csv(if (is.null(d)) data.frame() else d$items, file, row.names = FALSE)
    })

  # --- admin -------------------------------------------------------------
  stats <- reactive({
    invalidateLater(15000, session)
    admin_stats(STORE$read(), rv$cfg)
  })

  output$adm_cherries <- renderTable(stats()$cherries)
  output$adm_reasons  <- renderTable(stats()$reasons)
  output$adm_quota    <- renderTable(stats()$quota)
  output$adm_arms     <- renderTable(stats()$arms)
  output$adm_burden   <- renderTable(stats()$burden)
  output$adm_dq       <- renderTable(stats()$dq)
  output$adm_bws_cat  <- renderTable(bws_catalogue_table())
  output$adm_grid     <- renderTable(admin_burden_grid(rv$cfg))
  output$adm_burden_total <- renderText({
    s <- stats()
    sprintf("Assumed total %s \u00B7 observed median %s",
            fmt_mmss(s$assumed_total),
            if (is.na(s$median_total)) "no completions yet" else fmt_mmss(s$median_total))
  })

  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("barriers_v2_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
    content = function(file) store_export(STORE, file))

  output$dl_zip <- downloadHandler(
    filename = function() paste0("barriers_v2_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip"),
    content = function(file) {
      d <- store_latest(STORE$read())
      tmp <- file.path(tempdir(), paste0("csv", as.integer(runif(1, 1e5, 9e5))))
      dir.create(tmp, showWarnings = FALSE)
      for (nm in names(d))
        utils::write.csv(d[[nm]], file.path(tmp, paste0(nm, ".csv")), row.names = FALSE)
      wd <- setwd(tmp); on.exit(setwd(wd), add = TRUE)
      utils::zip(file, files = list.files(tmp), flags = "-r9Xq")
    })
}

shinyApp(ui, server)
