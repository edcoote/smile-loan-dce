# app.R --------------------------------------------------------------------
# Smile Loan DCE — pilot questionnaire (fixed-floor design). Standalone Shiny app.
# RUN: put engine.R and app.R in the same folder, then in R:
#        install.packages("shiny")   # once, if needed
#        shiny::runApp("path/to/this/folder")
# Only dependency: shiny. Presents the complete 4 x 3 factorial (12 tasks) in full
# to every respondent, both frames, screener, a personalised monthly figure keyed to
# the £12,570 floor, a repeated consistency task, and a downloadable response record.
# --------------------------------------------------------------------------

library(shiny)
source("engine.R")

acc <- "#3b2a55"   # 21D plum
card <- function(...) div(style = "background:#fff;border:1px solid #e6ddf0;border-radius:12px;padding:12px 14px;margin-bottom:8px;", ...)
unset <- function(x) is.null(x) || length(x) == 0 || (length(x) == 1 && x == "")

ui <- fluidPage(
  tags$head(tags$style(HTML(paste0(
    "body{max-width:640px;margin:0 auto;} .btn{border-radius:8px;} ",
    "h3{color:", acc, ";} .muted{color:#666;font-size:13px;} ",
    ".prog{color:#888;font-size:12px;} .well{background:#f7f4fb;}")))),
  titlePanel("Paying for full-jaw treatment \u2014 a short questionnaire"),
  div(class = "muted", style = "margin-top:-8px;margin-bottom:12px;",
      "Research pilot. Not a real loan offer; answers change nothing about your care and are anonymous."),
  uiOutput("body")
)

server <- function(input, output, session) {
  rv <- reactiveValues(started = FALSE, flow = NULL, i = 1, resp = list(),
                       income = 20000, frame = "clinic", exited = FALSE, msg = NULL)

  n_tasks <- reactive(sum(vapply(rv$flow, function(x) x$type == "task", logical(1))))
  cur     <- reactive(rv$flow[[rv$i]])
  task_no <- reactive(sum(vapply(rv$flow[seq_len(rv$i)], function(x) x$type == "task", logical(1))))

  observeEvent(input$btn_start, {
    fk <- if (input$frame == "Random") sample(c("clinic", "prospective"), 1) else input$frame
    reviewer <- isTRUE(input$reviewer)
    d <- dce_design()
    tasks <- if (reviewer) d else d[sample(nrow(d)), ]
    fr <- dce_frames[[fk]]
    fl <- list()
    if (fr$screener && !reviewer) fl[[length(fl) + 1]] <- list(type = "screener")
    fl[[length(fl) + 1]] <- list(type = "about")
    for (r in seq_len(nrow(tasks)))
      fl[[length(fl) + 1]] <- list(type = "task", row = tasks[r, ], isrepeat = FALSE)
    if (!reviewer) {
      rr <- d[d$rep == 1, ][1, ]
      fl[[length(fl) + 1]] <- list(type = "task", row = rr, isrepeat = TRUE)
    }
    fl[[length(fl) + 1]] <- list(type = "end")
    rv$frame <- fk; rv$flow <- fl; rv$i <- 1; rv$resp <- list()
    rv$started <- TRUE; rv$exited <- FALSE; rv$msg <- NULL; rv$income <- 20000
  })

  observeEvent(input$btn_next, {
    it <- cur(); rv$msg <- NULL
    if (it$type == "screener") {
      if (unset(input$screen_q)) { rv$msg <- "Please choose an option."; return() }
      if (input$screen_q == "Yes, already treated") { rv$exited <- TRUE; return() }
    }
    if (it$type == "about") {
      if (unset(input$age) || unset(input$income)) { rv$msg <- "Please answer both."; return() }
      rv$income <- income_mid(input$income)
      rv$resp$about <- data.frame(age = input$age, income = input$income, stringsAsFactors = FALSE)
    }
    if (it$type == "task") {
      ch <- input[[paste0("ch_", rv$i)]]; ce <- input[[paste0("ce_", rv$i)]]
      if (unset(ch) || unset(ce)) {
        rv$msg <- "Please pick an option and say how sure you are."; return() }
      row <- it$row
      rv$resp[[paste0("t", sprintf("%02d", rv$i))]] <- data.frame(
        order = task_no(), id = row$id, rate = row$rate, fee = row$fee,
        monthly = round(monthly_repay(row$rate, rv$income), 2),
        rationality = row$rationality, repeated = as.integer(it$isrepeat),
        choice = ch, certainty = ce, stringsAsFactors = FALSE)
    }
    rv$i <- min(rv$i + 1, length(rv$flow))
  })
  observeEvent(input$btn_back, { rv$msg <- NULL; rv$i <- max(1, rv$i - 1) })
  observeEvent(input$btn_restart, { rv$started <- FALSE; rv$exited <- FALSE })

  resp_df <- reactive({
    rows <- rv$resp[grepl("^t", names(rv$resp))]
    if (!length(rows)) return(NULL)
    do.call(rbind, rows)
  })

  output$body <- renderUI({
    if (!rv$started) {
      return(wellPanel(
        h3("Set up the run"),
        div(class = "muted", "For the pilot: choose a frame, or leave on Random to mimic a real respondent."),
        radioButtons("frame", "Frame", c("In clinic" = "clinic", "Patient group" = "prospective", "Random"), inline = TRUE, selected = "clinic"),
        checkboxInput("reviewer", "Reviewer mode \u2014 walk all 12 tasks in order (no repeat)", FALSE),
        actionButton("btn_start", "Start", class = "btn-primary")))
    }
    if (rv$exited) {
      return(card(h3("Thank you"),
                  p("This questionnaire is for people still considering treatment, so it ends here for you. We appreciate your time."),
                  actionButton("btn_restart", "Back to start")))
    }
    it <- cur(); fr <- dce_frames[[rv$frame]]
    msg <- if (!is.null(rv$msg)) div(style = "color:#b00;font-size:13px;margin:6px 0;", rv$msg)
    navrow <- div(style = "display:flex;justify-content:space-between;margin-top:12px;",
                  if (rv$i > 1) actionButton("btn_back", "Back") else span(),
                  actionButton("btn_next", "Next", class = "btn-primary"))

    if (it$type == "screener") {
      return(tagList(card(
        h3("First, one quick check"),
        p("Have you already had full-jaw dental implant treatment with 21D?"),
        radioButtons("screen_q", NULL, c("Yes, already treated", "No, still considering it"), selected = character(0)),
        div(class = "muted", "\u201CYes\u201D ends the survey \u2014 already-treated patients would distort the estimate.")),
        msg, navrow))
    }
    if (it$type == "about") {
      return(tagList(card(
        h3("A little about you"),
        div(class = "muted", "So the monthly figures can be made realistic for you."),
        radioButtons("age", "Your age band",
          c("35\u201339","40\u201344","45\u201349","50\u201354","55\u201359","60\u201364","65\u201369","70\u201374","75\u201379","80+"),
          selected = character(0), inline = TRUE),
        radioButtons("income", "Your earned income (wages only, not pension)",
          c("Under \u00A312,570","\u00A312,570\u2013\u00A320,000","\u00A320,000\u2013\u00A330,000","\u00A330,000\u2013\u00A345,000","\u00A345,000 or more"),
          selected = character(0))),
        msg, navrow))
    }
    if (it$type == "task") {
      row <- it$row; mo <- monthly_repay(row$rate, rv$income); below <- rv$income <= FLOOR
      hdr <- if (isTRUE(it$isrepeat)) span(style = "color:#7d4fa8;", "\u21BB One more, similar to an earlier one")
             else span(class = "prog", paste0("Task ", task_no(), " of ", n_tasks()))
      loan_line <- if (below)
        "At your income you would make no repayments \u2014 repayments apply only to earnings above the \u00A312,570 personal allowance, and any balance is written off at retirement."
      else
        paste0("\u2248 ", fmt_gbp(mo), "/month at your income, repaid from wages until you retire, then written off.")
      return(tagList(
        div(style = "display:flex;justify-content:space-between;", hdr),
        h3("Which would you choose?"),
        card(
          div(style = "font-size:12px;color:#7d4fa8;font-weight:600;", "SMILE LOAN"),
          p(HTML(paste0("Repay <b>", row$rate, "%</b> of your earnings above <b>\u00A312,570</b> (the tax-free personal allowance)."))),
          p(HTML(paste0("One-off upfront fee: <b>", fmt_gbp(row$fee), "</b>."))),
          div(class = "muted", loan_line),
          div(class = "muted", style = "border-top:1px solid #eee;margin-top:8px;padding-top:6px;",
              "No deposit \u00B7 nothing taken from your pension \u00B7 repaid only from wages \u00B7 written off at retirement or on death")),
        card(div(style = "font-size:12px;color:#666;font-weight:600;", toupper(fr$sq_head)), p(fr$sq_body)),
        radioButtons(paste0("ch_", rv$i), "Your choice", c("Smile Loan" = "loan", setNames("sq", fr$sq_head)), selected = character(0)),
        radioButtons(paste0("ce_", rv$i), "How sure are you?", c("Definitely", "Probably", "Might"), selected = character(0), inline = TRUE),
        msg, navrow))
    }
    # end
    tagList(card(
      h3("Last question"),
      p("If you went ahead with treatment, how would you most likely pay for it?"),
      radioButtons("funding", NULL, c("Savings", "Credit card", "Family loan", "Bank loan", "Couldn't afford it"), selected = character(0)),
      hr(),
      h3("Your responses"),
      div(class = "muted", paste0("Frame: ", fr$label, " \u00B7 floor \u00A312,570")),
      tableOutput("summary"),
      downloadButton("dl", "Download responses (CSV)"),
      actionButton("btn_restart", "Start again")))
  })

  output$summary <- renderTable({
    df <- resp_df(); if (is.null(df)) return(NULL)
    data.frame(Task = df$order, Terms = paste0(df$rate, "%, fee \u00A3", df$fee),
               Monthly = fmt_gbp(df$monthly),
               Check = ifelse(df$repeated == 1, "repeat", ifelse(df$rationality == 1, "best-terms", "")),
               Choice = ifelse(df$choice == "loan", "Loan", "Status quo"),
               Sure = df$certainty, check.names = FALSE)
  })

  output$dl <- downloadHandler(
    filename = function() paste0("dce_pilot_", rv$frame, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      df <- resp_df()
      if (!is.null(rv$resp$about)) { df$age <- rv$resp$about$age; df$income_band <- rv$resp$about$income }
      df$funding <- if (!is.null(input$funding)) input$funding else NA
      write.csv(df, file, row.names = FALSE)
    })
}

shinyApp(ui, server)
