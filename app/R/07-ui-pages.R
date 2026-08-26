# 07-ui-pages.R -------------------------------------------------------------
# Every page type has three functions with the same signature triple:
#   render_page(page, i, rv)          -> UI
#   validate_page(page, i, input, rv) -> NULL, or a message to show
#   capture_page(page, i, input, rv)  -> list(items=, dce=, bws=, meta=)
# Adding a module means adding three cases, not editing the server. The
# validate/capture pair is pure, so dev/simulate.R exercises it without Shiny.
# ---------------------------------------------------------------------------

pid <- function(i, id) paste0("p", i, "_", id)

.err <- function(msg) if (!is.null(msg)) div(class = "err", msg)

.radio <- function(i, item, sel = NULL) {
  radioButtons(pid(i, item$id), item$label, choices = item$options,
               selected = sel %||% character(0))
}

progress_bar <- function(frac)
  div(class = "pbar", div(style = sprintf("width:%.1f%%;", 100 * frac)))

# --- render ----------------------------------------------------------------
render_page <- function(page, i, rv) {
  switch(page$type,

    landing = tagList(
      h3("Paying for full-jaw dental treatment"),
      card(
        p("We are trying to understand what stops people going ahead with full-jaw dental implant treatment, and whether a repayment scheme linked to earnings would change that."),
        p(strong("What it involves. "), sprintf("About %s of questions. There are no right answers.",
          fmt_mmss(burden_estimate(rv$cfg)))),
        p(strong("Your answers are anonymous. "), "They are not linked to your dental records and they will not affect your care in any way."),
        p(strong("The loan terms are hypothetical. "), "Nothing here is an offer of finance."),
        div(class = "muted", "You can stop at any point. If you close the page part-way, the answers you have already given may be kept and used.")),
      card(
        h4("Before you start"),
        checkboxGroupInput(pid(i, "consent"), NULL, choices = CONSENT_POINTS)),
      .err(rv$msg),
      actionButton("btn_next", "Start", class = "btn-primary")),

    screener = tagList(
      progress_bar(flow_progress(rv$flow, i)),
      h3("A few questions to start"),
      card(.radio(i, SCREENER$age), .radio(i, SCREENER$uk)),
      card(.radio(i, SCREENER$arches)),
      card(.radio(i, SCREENER$stage)),
      .err(rv$msg), nav_row(i)),

    single = tagList(
      progress_bar(flow_progress(rv$flow, i)),
      card(.radio(i, page$item),
           if (!is.null(page$item$help)) div(class = "muted", page$item$help)),
      .err(rv$msg), nav_row(i)),

    battery = tagList(
      progress_bar(flow_progress(rv$flow, i)),
      h3(page$title),
      if (!is.null(page$help)) div(class = "muted", page$help),
      tagList(lapply(page$items, function(it)
        card(radioButtons(pid(i, it$id), it$label, choices = page$options,
                          selected = character(0), inline = FALSE)))),
      .err(rv$msg), nav_row(i)),

    battery_mixed = tagList(
      progress_bar(flow_progress(rv$flow, i)),
      h3(page$title),
      tagList(lapply(page$items, function(it) card(.radio(i, it)))),
      .err(rv$msg), nav_row(i)),

    bws_intro = tagList(
      progress_bar(flow_progress(rv$flow, i)),
      h3("Weighing things up"),
      card(p(page$stem),
           p(sprintf("We will show you %d short lists. Each time, pick the one that matters most to you and the one that matters least.", page$n)),
           div(class = "muted", "The same things come up more than once in different combinations. That is deliberate \u2014 it is how we work out the order.")),
      nav_row(i, next_label = "Begin")),

    bws = {
      labs <- setNames(BWS_ITEMS$item_id[page$set$items], BWS_ITEMS$label[page$set$items])
      tagList(
        progress_bar(flow_progress(rv$flow, i)),
        span(class = "prog", sprintf("List %d of %d", page$set$set_order, page$n)),
        h3(page$stem),
        card(radioButtons(pid(i, "best"), "Which of these matters MOST?",
                          choices = labs, selected = character(0))),
        card(radioButtons(pid(i, "worst"), "And which matters LEAST?",
                          choices = labs, selected = character(0))),
        .err(rv$msg), nav_row(i))
    },

    dce_intro = tagList(
      progress_bar(flow_progress(rv$flow, i)),
      h3("Two ways of paying"),
      card(
        p("Next we show you pairs of repayment plans for the same treatment. They are hypothetical."),
        p(HTML(paste0("Both plans work the same way: you pay nothing up front except any fee shown, and you then repay a percentage of ",
                      strong("only the part of your earnings above \u00A312,570"), " \u2014 the tax-free personal allowance. Nothing is taken from a pension. Anything left is written off when you retire, or on death."))),
        div(class = "muted", "Each time, pick whichever plan you prefer, then tell us whether you would actually take it.")),
      nav_row(i, next_label = "Begin")),

    dce = {
      r <- page$row
      # Card contents come from DCE_SPEC$render, so adding an attribute adds a
      # line here automatically and no layout code changes.
      alt_card <- function(nm, side) {
        lines <- dce_render_alt(r, side)
        mono <- if ("rate" %in% dce_names())
          div(class = "muted",
              if (rv$income <= FLOOR) "At your income, no repayments would be due."
              else sprintf("\u2248 %s a month at your income",
                           fmt_gbp(monthly_repay(r[[paste0(side, "_rate")]], rv$income))))
        div(class = "alt", h4(nm), lapply(lines, function(x) p(HTML(x))), mono)
      }
      tagList(
        progress_bar(flow_progress(rv$flow, i)),
        span(class = "prog", sprintf("Pair %d of %d", r$task_order, page$n)),
        h3("Which would you prefer?"),
        div(class = "grid2",
            div(alt_card("LOAN A", "a")),
            div(alt_card("LOAN B", "b"))),
        br(),
        radioButtons(pid(i, "choice"), "Between these two, which do you prefer?",
                     choices = c("Loan A" = "A", "Loan B" = "B"), selected = character(0), inline = TRUE),
        radioButtons(pid(i, "take"), "And would you actually take that plan?",
                     choices = c("Yes", "No"), selected = character(0), inline = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'No'", pid(i, "take")),
          radioButtons(pid(i, "outcome"), "What would you do instead?",
                       choices = c("Pay for the treatment myself" = "pay_privately",
                                   "Not have the treatment" = "do_not_proceed"),
                       selected = character(0))),
        .err(rv$msg), nav_row(i))
    },

    demographics = tagList(
      progress_bar(flow_progress(rv$flow, i)),
      h3("Last few questions"),
      tagList(lapply(DEMOGRAPHICS, function(it)
        card(if (identical(it$type, "text"))
               tagList(textInput(pid(i, it$id), it$label, placeholder = "e.g. CW9"),
                       div(class = "muted", it$help))
             else .radio(i, it)))),
      card(textAreaInput(pid(i, FREETEXT$id), FREETEXT$label, rows = 3)),
      .err(rv$msg), nav_row(i, next_label = "Submit")),

    thanks = tagList(
      h3("Thank you"),
      card(
        p("That is everything \u2014 your answers have been recorded."),
        p("If you would like to talk to someone about treatment options, your own dentist is the right first step. They can refer you if implant treatment is appropriate for you."),
        div(class = "muted", "There is no payment or reward for taking part, and nothing you have said will be linked back to you.")),
      if (isTRUE(rv$kiosk)) card(
        div(class = "muted", "Clinic device \u2014 hand back to a member of staff."),
        actionButton("btn_restart", "Start a new response", class = "btn-primary"),
        div(class = "muted", style = "margin-top:8px;",
            "This screen resets automatically so the next person starts fresh.")),
      if (isTRUE(rv$dev)) card(h4("Developer view"), tableOutput("dev_summary"),
                               downloadButton("dl_me", "Download this response (CSV)"))),

    screened_out = tagList(
      h3("Thank you for your interest"),
      if (isTRUE(rv$kiosk)) card(actionButton("btn_restart", "Start a new response", class = "btn-primary")),
      card(p(switch(rv$screen_reason,
        under_18   = "This study is only open to adults, so it ends here.",
        non_uk     = "This study is about people living in the UK, so it ends here.",
        not_target = "This study is about people who have lost, or are losing, a full jaw of teeth, so it ends here.",
        "This study is not a match for your situation, so it ends here.")),
        p("We appreciate you taking the time."))),

    stop("Unknown page type: ", page$type))
}

nav_row <- function(i, next_label = "Next") {
  div(style = "display:flex;justify-content:space-between;margin-top:14px;",
      if (i > 2) actionButton("btn_back", "Back") else span(),
      actionButton("btn_next", next_label, class = "btn-primary"))
}

# --- validate --------------------------------------------------------------
validate_page <- function(page, i, input, rv) {
  g <- function(id) input[[pid(i, id)]]
  switch(page$type,
    landing = if (length(g("consent")) < length(CONSENT_POINTS))
      "Please tick every box to confirm you are happy to take part." else NULL,
    screener = {
      miss <- vapply(SCREENER, function(it) unset(g(it$id)), logical(1))
      if (any(miss)) "Please answer all four questions." else NULL
    },
    single = if (unset(g(page$item$id))) "Please choose an option." else NULL,
    battery = if (any(vapply(page$items, function(it) unset(g(it$id)), logical(1))))
      "Please answer every row." else NULL,
    battery_mixed = if (any(vapply(page$items, function(it) unset(g(it$id)), logical(1))))
      "Please answer every question." else NULL,
    bws = {
      if (unset(g("best")) || unset(g("worst"))) "Please pick one for each question."
      else if (identical(g("best"), g("worst"))) "Please pick two different ones."
      else NULL
    },
    dce = {
      if (unset(g("choice")) || unset(g("take"))) "Please answer both questions."
      else if (identical(g("take"), "No") && unset(g("outcome"))) "Please tell us what you would do instead."
      else NULL
    },
    demographics = {
      miss <- vapply(DEMOGRAPHICS, function(it)
        if (identical(it$type, "text")) FALSE else unset(g(it$id)), logical(1))
      pc <- g("dem_postcode")
      if (any(miss)) "Please answer the questions above."
      else if (!unset(pc) && !valid_postcode_district(pc))
        "Please give only the first part of your postcode \u2014 for example CW9, not CW9 5AB."
      else NULL
    },
    NULL)
}

# --- capture ---------------------------------------------------------------
capture_page <- function(page, i, input, rv, seconds = NA_real_) {
  g <- function(id) input[[pid(i, id)]]
  out <- list(items = NULL, dce = NULL, bws = NULL, meta = list())
  switch(page$type,
    landing = out$meta$consent <- 1L,
    screener = {
      a <- setNames(lapply(SCREENER, function(it) g(it$id)), vapply(SCREENER, `[[`, "", "id"))
      out$items <- rows_items(rv$rid, rv$rev, "screener", a, seconds)
      out$meta <- list(age_band = a$scr_age, quota_band = age_to_band(a$scr_age))
    },
    single = out$items <- rows_items(rv$rid, rv$rev, page$module,
                                     setNames(list(g(page$item$id)), page$item$id), seconds),
    battery = {
      a <- setNames(lapply(page$items, function(it) g(it$id)), vapply(page$items, `[[`, "", "id"))
      out$items <- rows_items(rv$rid, rv$rev, page$module, a, seconds)
      if (identical(page$module, "core_mdas")) {
        s <- mdas_score(a)
        out$meta <- list(mdas_total = s, mdas_flag = as.character(mdas_flag(s)))
      }
    },
    battery_mixed = {
      a <- setNames(lapply(page$items, function(it) g(it$id)), vapply(page$items, `[[`, "", "id"))
      out$items <- rows_items(rv$rid, rv$rev, page$module, a, seconds)
      if (!unset(a$enab_income)) out$meta <- list(income_mid = income_mid(a$enab_income))
    },
    bws = out$bws <- rows_bws(rv$rid, rv$rev, page$set, g("best"), g("worst"), seconds),
    dce = {
      out$dce <- rows_dce(rv$rid, rv$rev, page$row, g("choice"), g("take"),
                          if (identical(g("take"), "Yes")) "take_loan" else g("outcome"),
                          rv$income, seconds)
      f <- dce_dominance_failed(page$row, g("choice"))
      if (!is.na(f)) out$meta <- list(dominance_failed = f)
    },
    demographics = {
      a <- setNames(lapply(DEMOGRAPHICS, function(it) g(it$id)), vapply(DEMOGRAPHICS, `[[`, "", "id"))
      a$dem_postcode <- toupper(trimws(a$dem_postcode %||% ""))
      a[[FREETEXT$id]] <- g(FREETEXT$id)
      out$items <- rows_items(rv$rid, rv$rev, "demographics", a, seconds)
    },
    NULL)
  out
}
