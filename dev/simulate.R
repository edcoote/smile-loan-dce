# dev/simulate.R ------------------------------------------------------------
# Drives synthetic respondents through the REAL flow builder, the REAL
# validate/capture functions and the REAL store, without Shiny. Two purposes:
#
#   1. Architecture test. If routing, capture or storage is broken this fails
#      here rather than during fielding.
#   2. A populated dataset in the exact shape fielding will produce, so the
#      analysis scripts can be written and debugged before the first real
#      respondent arrives.
#
# Choices are generated from a known utility model, so the last section can
# refit that model from the exported data and check the parameters come back.
# That closes the loop: design -> serving -> storage -> export -> estimable.
#
# RUN:  Rscript dev/simulate.R [n] [outdir]
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
N   <- if (length(args) >= 1) as.integer(args[1]) else 300
OUT <- if (length(args) >= 2) args[2] else "dev/sim-data"
BE  <- if (length(args) >= 3) args[3] else "csv"   # csv | sqlite

`%||%` <- function(a, b) if (is.null(a)) b else a
# Run from the repository root.
if (!dir.exists("app/R")) stop("Run from the repository root: Rscript dev/simulate.R")
for (f in sort(list.files("app/R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

unlink(OUT, recursive = TRUE)
CFG$store_path <- OUT
STORE <- if (identical(BE, "sqlite")) store_sqlite(file.path(OUT, "responses.sqlite")) else store_init("csv", OUT)
cat("Store backend:", STORE$kind, "\n")

# --- True parameters -------------------------------------------------------
TRUE_B <- c(rate = -0.28, fee = -0.45)   # fee per £100; both should be negative
TRUE_BWS <- c(2.2, 1.6, 0.9, 1.1, 0.2, -0.3, -0.6, 0.4, -0.2, -1.1, -0.9, 0.6, -1.4)
P_TAKE_INTERCEPT <- 0.9                  # participation margin

set.seed(20260826)

pick <- function(x, p = NULL) if (is.null(p)) sample(x, 1) else sample(x, 1, prob = p)
gumbel <- function(n) -log(-log(stats::runif(n)))

sim_one <- function(k) {
  rid <- new_rid()
  rv <- list(cfg = CFG, rid = rid, rev = 0L, income = 20000, dev = FALSE, screen_reason = NULL)
  meta <- list(instrument = INSTRUMENT_ID, app_version = APP_VERSION,
               design_version = DESIGN_VERSION, config = "sim", session = "sim",
               consent = 1L)
  status <- "landed"
  inp <- list()

  # --- screener ---
  flow <- flow_preamble()
  i <- 2L
  age <- pick(SCREENER$age$options, p = c(.01, .03, .05, .10, .12, .14, .16, .16, .15, .08))
  a <- list(scr_age = age,
            scr_uk = pick(c("Yes", "No"), p = c(.97, .03)),
            scr_arches = pick(SCREENER$arches$options, p = c(.16, .16, .30, .30, .05, .03)),
            scr_stage = pick(SCREENER$stage$options, p = c(.14, .26, .18, .12, .18, .12)))
  for (nm in names(a)) inp[[pid(i, nm)]] <- a[[nm]]

  stopifnot(is.null(validate_page(flow[[i]], i, inp, rv)))
  cap <- capture_page(flow[[i]], i, inp, rv, seconds = stats::rgamma(1, 6, 1 / 6))
  rv$rev <- STORE$next_rev(rid)
  if (nrow(cap$items)) { cap$items$rev <- rv$rev; STORE$append("items", cap$items) }
  meta <- utils::modifyList(meta, cap$meta)

  reason <- screen_out_reason(a)
  if (!is.null(reason)) {
    meta <- utils::modifyList(meta, list(status = "screened_out", screen_out_reason = reason,
                                         seconds_total = 45, page_reached = 2L, n_pages = 3L))
    STORE$append("respondents", row_respondent(rid, rv$rev, meta))
    return(invisible("screened_out"))
  }

  rest <- flow_build(a, CFG)
  flow <- c(flow, rest)
  for (at in c("path", "stage_key", "arm", "modules_served")) attr(flow, at) <- attr(rest, at)
  meta <- utils::modifyList(meta, list(
    path = attr(flow, "path"), stage_key = attr(flow, "stage_key"),
    arm = attr(flow, "arm"), modules_served = attr(flow, "modules_served")))
  status <- "partial"

  # Abandonment: 18% drop out at a uniformly random page. Retained, as the
  # field controls require, so the stored data must handle it.
  quit_at <- if (stats::runif(1) < 0.18) sample(3:length(flow), 1) else Inf
  total_sec <- 0

  for (i in 3:length(flow)) {
    if (i >= quit_at) break
    page <- flow[[i]]
    if (page$type %in% c("bws_intro", "dce_intro", "thanks")) next

    ans <- switch(page$type,
      single = setNames(list(pick(page$item$options)), page$item$id),
      battery = setNames(lapply(page$items, function(it) pick(page$options)),
                         vapply(page$items, `[[`, "", "id")),
      battery_mixed = setNames(lapply(page$items, function(it) pick(it$options)),
                               vapply(page$items, `[[`, "", "id")),
      bws = {
        v <- TRUE_BWS[page$set$items]
        b <- v + gumbel(length(v)); w <- -v + gumbel(length(v))
        ids <- BWS_ITEMS$item_id[page$set$items]
        bi <- which.max(b); wi <- which.max(w)
        if (bi == wi) wi <- order(w, decreasing = TRUE)[2]
        list(best = ids[bi], worst = ids[wi])
      },
      dce = {
        r <- page$row
        u <- c(TRUE_B["rate"] * r$a_rate + TRUE_B["fee"] * r$a_fee / 100,
               TRUE_B["rate"] * r$b_rate + TRUE_B["fee"] * r$b_fee / 100) + gumbel(2)
        ch <- c("A", "B")[which.max(u)]
        chosen_rate <- if (ch == "A") r$a_rate else r$b_rate
        p_take <- stats::plogis(P_TAKE_INTERCEPT + TRUE_B["rate"] * (chosen_rate - 10.5))
        take <- if (stats::runif(1) < p_take) "Yes" else "No"
        list(choice = ch, take = take,
             outcome = if (take == "No") pick(c("pay_privately", "do_not_proceed"), p = c(.35, .65)) else NULL)
      },
      demographics = {
        z <- setNames(lapply(DEMOGRAPHICS, function(it)
          if (identical(it$type, "text")) paste0(sample(LETTERS, 2, TRUE), collapse = "") else pick(it$options)),
          vapply(DEMOGRAPHICS, `[[`, "", "id"))
        z$dem_postcode <- paste0(paste0(sample(LETTERS[1:20], 2, TRUE), collapse = ""), sample(1:20, 1))
        z[[FREETEXT$id]] <- if (stats::runif(1) < 0.25) "It was mainly the money." else ""
        z
      },
      NULL)

    for (nm in names(ans)) inp[[pid(i, nm)]] <- ans[[nm]]
    if (identical(page$type, "dce") && !is.null(ans$outcome)) inp[[pid(i, "outcome")]] <- ans$outcome

    v <- validate_page(page, i, inp, rv)
    if (!is.null(v)) stop("Simulated respondent failed validation on ", page$type, ": ", v)

    secs <- switch(page$type,
      dce = stats::rgamma(1, 5, 5 / BURDEN$dce_per_task),
      bws = stats::rgamma(1, 5, 5 / BURDEN$bws_per_set),
      stats::rgamma(1, 5, 5 / 35))
    total_sec <- total_sec + secs

    cap <- capture_page(page, i, inp, rv, secs)
    rv$rev <- STORE$next_rev(rid)
    for (tb in c("items", "dce", "bws")) {
      x <- cap[[tb]]
      if (!is.null(x) && nrow(x)) { x$rev <- rv$rev; STORE$append(tb, x) }
    }
    meta <- utils::modifyList(meta, cap$meta)
    if (identical(page$module, "core_enabling") && !unset(ans$enab_income))
      rv$income <- income_mid(ans$enab_income)
    if (identical(page$type, "demographics")) status <- "complete"
  }

  meta <- utils::modifyList(meta, list(status = status, seconds_total = round(total_sec, 1),
                                       page_reached = min(i, length(flow)), n_pages = length(flow)))
  STORE$append("respondents", row_respondent(rid, rv$rev, meta))
  invisible(status)
}

cat("Simulating", N, "respondents into", OUT, "\n")
t0 <- Sys.time()
res <- vapply(seq_len(N), sim_one, character(1))
cat("done in", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s\n\n")
print(table(res))

db <- store_latest(STORE$read())
cat("\nStored rows:\n"); print(vapply(db, nrow, integer(1)))

xl <- file.path(OUT, "barriers_v2_simulated.xlsx")
store_export(STORE, xl)
cat("\nWorkbook:", xl, "-", round(file.size(xl) / 1024, 1), "KB\n")

# --- Admin statistics ------------------------------------------------------
s <- admin_stats(STORE$read(), CFG)
cat("\n--- CHERRIES ---\n"); print(s$cherries, row.names = FALSE)
cat("\n--- Quota ---\n");    print(s$quota, row.names = FALSE)
cat("\n--- Arms ---\n");     print(s$arms, row.names = FALSE)
cat("\n--- Data quality ---\n"); print(s$dq, row.names = FALSE)
cat("\nAssumed total", fmt_mmss(s$assumed_total),
    "| observed median", if (is.na(s$median_total)) "-" else fmt_mmss(s$median_total), "\n")

# --- Parameter recovery ----------------------------------------------------
# Refit the generating model from the EXPORTED table, not from memory, so this
# tests the stored shape and not just the simulation.
d <- db$dce
d <- d[!is.na(d$chosen) & d$dominance == 0, ]
w <- reshape(d[, c("rid", "set_id", "alt", "rate", "fee", "chosen")],
             idvar = c("rid", "set_id"), timevar = "alt", direction = "wide")
X <- cbind(rate = w$rate.A - w$rate.B, fee = (w$fee.A - w$fee.B) / 100)
y <- w$chosen.A

nll <- function(b) { xb <- as.vector(X %*% b); -sum(y * xb - log1p(exp(xb))) }
fit <- stats::optim(c(0, 0), nll, method = "BFGS", hessian = TRUE)
se <- sqrt(diag(solve(fit$hessian)))
cat("\n--- Conditional logit refit from the exported DCE table ---\n")
print(data.frame(term = c("rate", "fee_per_100"), true = as.numeric(TRUE_B),
                 est = round(fit$par, 3), se = round(se, 3),
                 covered = abs(fit$par - TRUE_B) < 1.96 * se, row.names = NULL))
cat("Implied WTP: 1 percentage point of repayment rate =",
    fmt_gbp(100 * fit$par[1] / fit$par[2]), "of upfront fee\n")

# --- BWS recovery ----------------------------------------------------------
b <- db$bws
bw <- stats::aggregate(cbind(best, worst) ~ item_id, b, sum)
n <- stats::aggregate(best ~ item_id, b, length); names(n)[2] <- "shown"
bw <- merge(bw, n)
bw$bw_score <- (bw$best - bw$worst) / bw$shown
bw$true <- TRUE_BWS[match(bw$item_id, BWS_ITEMS$item_id)]
bw <- bw[order(-bw$bw_score), ]
cat("\n--- BWS: standardised best-minus-worst vs truth ---\n")
print(bw[, c("item_id", "shown", "best", "worst", "bw_score", "true")], row.names = FALSE)
cat("Spearman correlation with true values:",
    round(stats::cor(bw$bw_score, bw$true, method = "spearman"), 3), "\n")
