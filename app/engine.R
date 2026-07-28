# engine.R — Smile Loan DCE (fixed-floor design). Base R only; shinylive-compatible.
# Threshold fixed at the £12,570 personal allowance; only rate and fee vary.
# Design = complete 4 x 3 factorial = 12 tasks, shown in full to every respondent.

FLOOR <- 12570   # income-tax personal allowance: repayment floor, FIXED (not an attribute)

dce_levels <- list(
  rate = c(6, 9, 12, 15),   # % of earnings above the £12,570 floor
  fee  = c(0, 250, 750)     # £ one-off upfront fee
)

# Full 4 x 3 factorial (12 profiles). rationality = best-terms check; rep = consistency repeat.
dce_design <- function() {
  d <- data.frame(
    id   = sprintf("T%02d", 1:12),
    rate = rep(c(6, 9, 12, 15), each = 3),
    fee  = rep(c(0, 250, 750), times = 4),
    stringsAsFactors = FALSE)
  d$rationality <- as.integer(d$rate == 6  & d$fee == 0)     # cheapest possible loan
  d$rep         <- as.integer(d$rate == 9  & d$fee == 250)   # moderate task, repeated
  d
}

dce_frames <- list(
  clinic = list(label = "In-clinic (self-payer)", sq_head = "Carry on as I am",
                sq_body = "Pay the \u00A316,995 myself now", screener = FALSE),
  prospective = list(label = "Prospective (patient group)", sq_head = "Do nothing for now",
                sq_body = "Remain untreated for now", screener = TRUE)
)

# Income band midpoints for the personalised monthly figure
income_mid <- function(band) {
  switch(band,
    "Under \u00A312,570"             = 8000,
    "\u00A312,570\u2013\u00A320,000" = 16000,
    "\u00A320,000\u2013\u00A330,000" = 25000,
    "\u00A330,000\u2013\u00A345,000" = 37000,
    "\u00A345,000 or more"           = 52000,
    20000)
}

# Personalised monthly repayment: rate% of earnings above the fixed £12,570 floor
monthly_repay <- function(rate, income) {
  above <- max(0, income - FLOOR)
  (rate / 100) * above / 12
}

fmt_gbp <- function(x) paste0("\u00A3", formatC(round(x), format = "f", big.mark = ",", digits = 0))
