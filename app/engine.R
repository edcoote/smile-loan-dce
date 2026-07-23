# engine.R — Smile Loan DCE pilot: design definition + helpers.
# Self-contained, base R only (shinylive-compatible). No external packages.

# Attribute levels (reference)
dce_levels <- list(
  rate      = c(6, 9, 12, 15),   # % of earnings above threshold
  threshold = c(15, 20, 25, 30), # £000 of earned income
  fee       = c(0, 250, 750)     # £ upfront
)

# The 24-task balanced design (2 blocks of 12). rep = repeated consistency task.
dce_design <- function() {
  A <- data.frame(
    id   = sprintf("A%02d", 1:12),
    rate = c(15,15,15,12,12,12, 9, 9, 9, 6, 6, 6),
    thr  = c(15,20,30,15,20,25,15,20,30,15,20,25),
    fee  = c( 0,750,  0,750,250,750,250,250,  0,  0,750,250),
    rep  = c( 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    block = "A", stringsAsFactors = FALSE)
  B <- data.frame(
    id   = sprintf("B%02d", 1:12),
    rate = c(15,15,15,12,12,12, 9, 9, 9, 6, 6, 6),
    thr  = c(15,25,30,20,25,30,20,25,30,15,25,30),
    fee  = c(250,250,750,  0,  0,250,  0,750,750,750,  0,250),
    rep  = c( 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    block = "B", stringsAsFactors = FALSE)
  rbind(A, B)
}

# Frame-specific wording
dce_frames <- list(
  clinic = list(
    label   = "In-clinic (self-payer)",
    sq_head = "Carry on as I am",
    sq_body = "Pay the \u00A316,995 myself now",
    screener = FALSE),
  prospective = list(
    label   = "Prospective (patient group)",
    sq_head = "Do nothing for now",
    sq_body = "Remain untreated for now",
    screener = TRUE)
)

# Income band midpoints for the personalised monthly figure
income_mid <- function(band) {
  switch(band,
    "Under \u00A315,000"      = 12000,
    "\u00A315,000\u2013\u00A325,000" = 20000,
    "\u00A325,000\u2013\u00A335,000" = 30000,
    "\u00A335,000 or more"    = 40000,
    20000)
}

# Illustrative monthly repayment (earnings-contingent)
monthly_repay <- function(rate, thr, income) {
  above <- max(0, income - thr * 1000)
  (rate / 100) * above / 12
}

fmt_gbp <- function(x) paste0("\u00A3", formatC(round(x), format = "f", big.mark = ",", digits = 0))
