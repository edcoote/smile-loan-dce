# 08-admin.R ----------------------------------------------------------------
# Fielding monitor. Reachable only at ?admin=<key>.
#
# The statistics are pure functions of the stored tables so they can be run
# from a scheduled script as well as from the app — the same numbers land in
# the fielding log and in the CHERRIES reporting table without being computed
# twice in two places.
# ---------------------------------------------------------------------------

pct <- function(a, b) if (b == 0) "\u2014" else sprintf("%.1f%%", 100 * a / b)

admin_stats <- function(db, cfg = CFG) {
  d <- store_latest(db)
  R <- d$respondents
  n_start <- nrow(R)
  n_out   <- sum(R$status == "screened_out", na.rm = TRUE)
  n_comp  <- sum(R$status == "complete", na.rm = TRUE)
  n_part  <- sum(R$status == "partial", na.rm = TRUE)
  elig    <- n_start - n_out

  # CHERRIES-style participation and completion rates.
  cherries <- data.frame(
    measure = c("Entered the landing page", "Screened out",
                "Eligible and started", "Partial (abandoned)", "Completed",
                "Participation rate (started / entered)",
                "Completion rate (completed / eligible)"),
    value = c(n_start, n_out, elig, n_part, n_comp,
              pct(elig, n_start), pct(n_comp, elig)),
    stringsAsFactors = FALSE)

  # Screen-out reasons.
  so <- R$screen_out_reason[!is.na(R$screen_out_reason) & R$screen_out_reason != ""]
  reasons <- if (length(so)) as.data.frame(table(reason = so), stringsAsFactors = FALSE) else
    data.frame(reason = character(), Freq = integer())

  # Quota by age band, with a lag flag against target.
  ok <- R[R$status %in% c("partial", "complete"), , drop = FALSE]
  qb <- setNames(as.integer(table(factor(ok$quota_band, levels = cfg$quota_bands))), cfg$quota_bands)
  quota <- data.frame(band = cfg$quota_bands, recruited = as.integer(qb),
                      target = cfg$quota_target,
                      progress = pct(as.integer(qb), 1) , stringsAsFactors = FALSE)
  quota$progress <- sprintf("%.0f%%", 100 * quota$recruited / pmax(1, quota$target))
  quota$status <- ifelse(quota$recruited / pmax(1, quota$target) <
                           max(quota$recruited / pmax(1, quota$target)) - 0.15,
                         "LAGGING \u2014 target reminders here", "on track")

  # Arm balance and modules served.
  arms <- if (nrow(ok)) as.data.frame(table(arm = ok$arm), stringsAsFactors = FALSE) else
    data.frame(arm = character(), Freq = integer())

  # Observed burden, replacing the a priori constants once data exists.
  med <- function(x) if (!length(x) || all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
  comp <- R[R$status == "complete", , drop = FALSE]
  by_mod <- if (nrow(d$items)) stats::aggregate(seconds ~ module, d$items, med) else
    data.frame(module = character(), seconds = numeric())
  burden <- rbind(
    data.frame(module = "DCE (per task)",
               seconds = med(stats::aggregate(seconds ~ rid + set_id, d$dce, function(z) z[1])$seconds)),
    data.frame(module = "BWS (per set)",
               seconds = med(stats::aggregate(seconds ~ rid + set_id, d$bws, function(z) z[1])$seconds)),
    by_mod)
  burden$assumed <- NA_real_
  burden$assumed[burden$module == "DCE (per task)"] <- BURDEN$dce_per_task
  burden$assumed[burden$module == "BWS (per set)"]  <- BURDEN$bws_per_set
  burden$seconds <- round(burden$seconds, 1)

  # Data quality.
  dq <- data.frame(
    check = c("Dominance test failed",
              "Completed under 40% of estimated time (speeder)",
              "BWS straightlining (same item best in every set)",
              "DCE non-trader (always the same side)"),
    n = c(sum(R$dominance_failed == 1, na.rm = TRUE),
          sum(comp$seconds_total < 0.4 * burden_estimate(cfg), na.rm = TRUE),
          .n_bws_straightline(d$bws),
          .n_dce_nontrader(d$dce)),
    note = "reported, never used to exclude", stringsAsFactors = FALSE)

  list(cherries = cherries, reasons = reasons, quota = quota, arms = arms,
       burden = burden, dq = dq,
       median_total = round(med(comp$seconds_total), 1),
       assumed_total = burden_estimate(cfg))
}

.n_bws_straightline <- function(b) {
  if (!nrow(b)) return(0L)
  pick <- b[b$best == 1, c("rid", "item_id")]
  if (!nrow(pick)) return(0L)
  tab <- table(pick$rid, pick$item_id)
  sum(apply(tab, 1, function(r) sum(r > 0) == 1 & sum(r) >= 3))
}

.n_dce_nontrader <- function(d) {
  if (!nrow(d)) return(0L)
  ch <- d[d$chosen == 1, c("rid", "alt")]
  if (!nrow(ch)) return(0L)
  tab <- table(ch$rid, ch$alt)
  sum(apply(tab, 1, function(r) any(r == 0) & sum(r) >= 5))
}

# Configuration comparison: the Thursday decision, computed rather than guessed.
admin_burden_grid <- function(cfg = CFG) {
  rows <- list()
  for (bi in as.integer(names(BWS_CATALOGUE))) for (dt in c(6, 12)) for (ss in c(FALSE, TRUE)) {
    c2 <- cfg; c2$bws_items <- bi; c2$dce_tasks <- dt; c2$split_sample <- ss
    sec <- burden_estimate(c2)
    rows[[length(rows) + 1]] <- data.frame(
      bws_items = bi, bws_sets = nrow(bws_build(bi)), dce_tasks = dt,
      split_sample = ss, est_time = fmt_mmss(sec),
      under_13min = sec <= 13 * 60, stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

admin_ui <- function() {
  d <- dce_design(12)
  tagList(
    h3("Fielding monitor"),
    div(class = "muted", sprintf("%s \u00B7 app %s \u00B7 %s", INSTRUMENT_ID, APP_VERSION, now_utc())),
    card(h4("Recruitment"), tableOutput("adm_cherries"), tableOutput("adm_reasons")),
    card(h4("Age quota"), tableOutput("adm_quota")),
    card(h4("Randomisation balance"), tableOutput("adm_arms")),
    card(h4("Burden \u2014 assumed vs observed"),
         div(class = "muted", "Assumed values come from the v2 inventory. Once completions accumulate, use the observed medians instead."),
         textOutput("adm_burden_total"), tableOutput("adm_burden")),
    card(h4("Data quality"), tableOutput("adm_dq")),
    card(h4("Design record"),
         div(class = "muted", "Stored with every response so the served design is recoverable at analysis."),
         tags$pre(style = "white-space:pre-wrap;font-size:12px;", dce_design_summary(d)),
         h5("BWS balanced-design catalogue"), tableOutput("adm_bws_cat")),
    card(h4("Configuration options"),
         div(class = "muted", "Estimated burden under every catalogued combination."),
         tableOutput("adm_grid")),
    card(h4("Export"),
         downloadButton("dl_xlsx", "Download workbook (.xlsx)"),
         downloadButton("dl_zip", "Download raw tables (.zip of CSVs)")))
}
