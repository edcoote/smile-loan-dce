# 00-config.R ---------------------------------------------------------------
# Barriers to Full-Arch Rehabilitation — survey pathway v2.
# Constants, runtime switches and theme. Base R only (shinylive-compatible).
# ---------------------------------------------------------------------------

APP_VERSION    <- "0.3.0"
DESIGN_VERSION <- "dce-2026-08-exhaustive-D-opt; bws-13-4-1-BIBD"
INSTRUMENT_ID  <- "21D-BARRIERS-v2"

FLOOR <- 12570   # income-tax personal allowance. Repayment floor: FIXED, not an attribute.

# --- Runtime switches ------------------------------------------------------
# These are the three live decisions from the v2 open-items list. Change them
# here (or via URL query string) and the flow, the burden estimate and the
# stored design metadata all follow. Nothing else needs editing.
CFG <- list(
  bws_items         = 13,     # 7 | 9 | 11 | 13 — each is a balanced BIBD, see 03-design-bws.R
  dce_tasks         = 12,     # 12 = full D-optimal set. 6 = one balanced block (see 02-design-dce.R)
  dce_block         = NA,     # NA = use all tasks; 1 or 2 = serve that block only
  split_sample      = FALSE,  # TRUE = each respondent gets BWS *or* DCE, not both (50/50)
  include_dominance = TRUE,   # append the dominance-test task (reported, never used to exclude)
  randomise_modules = TRUE,   # 50/50 module order at entry to section 3 (anti-priming)
  store_backend     = "auto", # "auto" | "csv" | "memory" | "postgrest"
  store_path        = "data",
  # Fails closed. With no SURVEY_ADMIN_KEY set in the server environment the
  # admin route does not exist at all. This repository is public, so a default
  # key committed here would be a published credential for an endpoint that
  # exports the entire response set.
  admin_key         = Sys.getenv("SURVEY_ADMIN_KEY", ""),
  quota_bands       = c("<50", "50-65", "66+"),
  quota_target      = c(120, 120, 120)  # per band; drives the lag flag in the admin panel
)

# --- Burden model ----------------------------------------------------------
# Seconds per unit, calibrated to the v2 instrument inventory. Once real
# completions exist the admin panel replaces these with observed medians.
BURDEN <- list(
  landing        = 30,
  screener       = 30,
  path_extra     = 8,    # per path-specific item
  aohs           = 10,
  mdas_per_item  = 12,
  enabling_per_item = 15,
  bws_per_set    = 24,
  dce_per_task   = 30,   # includes the dual-response follow-up
  demographics   = 60
)

burden_estimate <- function(cfg = CFG, n_mdas = 5, n_enabling = 4, path_items = 1) {
  n_sets <- nrow(bws_design(cfg))
  bws <- if (cfg$split_sample) n_sets * 0.5 else n_sets
  dce <- if (cfg$split_sample) cfg$dce_tasks * 0.5 else cfg$dce_tasks
  if (cfg$include_dominance) dce <- dce + 1
  BURDEN$landing + BURDEN$screener + path_items * BURDEN$path_extra +
    BURDEN$aohs + n_mdas * BURDEN$mdas_per_item + n_enabling * BURDEN$enabling_per_item +
    bws * BURDEN$bws_per_set + dce * BURDEN$dce_per_task + BURDEN$demographics
}

fmt_mmss <- function(sec) sprintf("%d:%02d", sec %/% 60, round(sec %% 60))

# --- Small helpers ---------------------------------------------------------
fmt_gbp <- function(x) paste0("\u00A3", formatC(round(x), format = "f", big.mark = ",", digits = 0))

unset <- function(x) is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || x == ""))

new_rid <- function() {
  paste0("R", format(Sys.time(), "%y%m%d%H%M%S"),
         paste0(sample(c(letters, 0:9), 5, TRUE), collapse = ""))
}

now_utc <- function() format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# Monthly repayment: rate% of earnings above the fixed floor.
monthly_repay <- function(rate, income) (rate / 100) * max(0, income - FLOOR) / 12

income_mid <- function(band) {
  switch(band,
    "Under \u00A312,570"             = 8000,
    "\u00A312,570\u2013\u00A320,000" = 16000,
    "\u00A320,000\u2013\u00A330,000" = 25000,
    "\u00A330,000\u2013\u00A345,000" = 37000,
    "\u00A345,000\u2013\u00A360,000" = 52000,
    "\u00A360,000 or more"           = 75000,
    20000)
}

age_to_band <- function(age_band) {
  if (age_band %in% c("18\u201324", "25\u201334", "35\u201344", "45\u201349")) "<50"
  else if (age_band %in% c("50\u201354", "55\u201359", "60\u201365")) "50-65"
  else "66+"
}

# --- Theme -----------------------------------------------------------------
ACC   <- "#3b2a55"   # 21D plum
ACC_2 <- "#7d4fa8"

APP_CSS <- paste0(
  "body{max-width:680px;margin:0 auto;padding-bottom:48px;font-size:15px;}",
  ".btn{border-radius:8px;} h3{color:", ACC, ";margin-top:4px;}",
  ".muted{color:#666;font-size:13px;} .prog{color:#888;font-size:12px;}",
  ".well{background:#f7f4fb;}",
  ".pbar{height:4px;background:#ece5f5;border-radius:3px;margin:6px 0 14px;}",
  ".pbar > div{height:4px;background:", ACC_2, ";border-radius:3px;}",
  ".card{background:#fff;border:1px solid #e6ddf0;border-radius:12px;padding:12px 14px;margin-bottom:10px;}",
  ".alt{background:#fff;border:1px solid #e6ddf0;border-radius:12px;padding:10px 12px;}",
  ".alt h4{margin:0 0 6px;font-size:13px;letter-spacing:.04em;color:", ACC_2, ";}",
  ".grid2{display:flex;gap:10px;} .grid2 > div{flex:1;}",
  ".bwsrow{display:flex;align-items:center;gap:10px;padding:6px 0;border-bottom:1px solid #f0ecf6;}",
  ".bwsrow .lab{flex:1;} .err{color:#b00;font-size:13px;margin:6px 0;}",
  "@media(max-width:600px){.grid2{flex-direction:column;}}"
)

card <- function(...) shiny::div(class = "card", ...)
