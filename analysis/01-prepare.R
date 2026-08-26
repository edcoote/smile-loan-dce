# analysis/01-prepare.R -----------------------------------------------------
# Turns the stored response tables into the shapes the estimation packages
# want, and writes them to analysis/data/.
#
# WHY A CONVERSION IS NEEDED
# The store holds the DCE long — one row per alternative, two rows per task —
# because that is what mlogit and support.BWS expect and it is the honest
# record of what was shown. Apollo wants it WIDE: one row per choice task, with
# each alternative's attributes as their own columns and the choice as a single
# integer. Neither shape is more correct; they are different conventions, and
# this script is the bridge.
#
#   Rscript analysis/01-prepare.R [outdir]
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
if (!dir.exists("app/R")) stop("Run from the repository root: Rscript analysis/01-prepare.R")
for (f in sort(list.files("app/R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
OUT  <- if (length(args) >= 1) args[1] else "analysis/data"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

st <- if (nzchar(Sys.getenv("SURVEY_STORE_PATH"))) store_init("csv", Sys.getenv("SURVEY_STORE_PATH")) else store_init()
cat("Store backend:", st$kind, "\n")
db <- store_latest(st$read())
if (!is.null(st$disconnect)) on.exit(try(st$disconnect(), silent = TRUE), add = TRUE)

# --- Respondent-level covariates ------------------------------------------
R <- db$respondents
items <- db$items
wide_items <- if (nrow(items)) {
  stats::reshape(items[, c("rid", "item_id", "value")], idvar = "rid",
                 timevar = "item_id", direction = "wide")
} else data.frame(rid = character())
names(wide_items) <- sub("^value[.]", "", names(wide_items))

covars <- merge(
  R[, c("rid", "status", "path", "stage_key", "arm", "modules_served",
        "quota_band", "mdas_total", "income_mid", "seconds_total")],
  wide_items, by = "rid", all.x = TRUE)

# --- DCE: long -> Apollo wide ---------------------------------------------
d <- db$dce
d <- d[!is.na(d$chosen), ]
cat("DCE rows:", nrow(d), "| tasks:", nrow(d) / 2, "| respondents:", length(unique(d$rid)), "\n")

# The dominance-test task is retained in the file with a flag, never silently
# dropped: the v2 field controls report it, they do not exclude on it. Model
# scripts filter it explicitly so that choice is visible in the code.
# Explicit A/B merge rather than stats::reshape. reshape() groups on the idvar
# columns, and `block` is NA for the dominance task — every NA-block row across
# every respondent collapsed into a single group, silently discarding 339 of
# 4,413 tasks and all but one dominance test. Merging on an explicit key cannot
# fail that way, and the row count is asserted below.
keycols <- c("rid", "set_id", "task_order", "block", "dominance", "side_flipped",
             "stage2_take", "stage2_outcome")
A <- d[d$alt == "A", c(keycols, "rate", "fee", "monthly", "chosen")]
B <- d[d$alt == "B", c("rid", "set_id", "rate", "fee", "monthly", "chosen")]
names(A)[names(A) %in% c("rate", "fee", "monthly", "chosen")] <-
  paste0(c("rate", "fee", "monthly", "chosen"), "_A")
names(B)[names(B) %in% c("rate", "fee", "monthly", "chosen")] <-
  paste0(c("rate", "fee", "monthly", "chosen"), "_B")
w <- merge(A, B, by = c("rid", "set_id"), all = FALSE)

stopifnot(
  "A/B merge lost or duplicated tasks" = nrow(w) == nrow(d) / 2,
  "a task has no chosen alternative"   = all(w$chosen_A + w$chosen_B == 1)
)

# Apollo wants the choice as an integer index over alternatives.
w$choice <- ifelse(w$chosen_A == 1, 1L, 2L)
w$chosen_A <- NULL; w$chosen_B <- NULL

# Stage 2. Three mutually exclusive outcomes, never collapsed into one opt-out:
#   1 take the chosen loan   2 pay privately   3 do not proceed
w$choice2 <- ifelse(w$stage2_outcome == "take_loan", 1L,
             ifelse(w$stage2_outcome == "pay_privately", 2L,
             ifelse(w$stage2_outcome == "do_not_proceed", 3L, NA_integer_)))

# Attributes of the alternative actually chosen at stage 1 — this is what the
# participation decision is conditioned on.
w$chosen_rate <- ifelse(w$choice == 1, w$rate_A, w$rate_B)
w$chosen_fee  <- ifelse(w$choice == 1, w$fee_A,  w$fee_B)

# Availability: both loans always shown, so both are always available.
w$av_A <- 1L; w$av_B <- 1L

w <- merge(w, covars, by = "rid", all.x = TRUE)
w <- w[order(w$rid, w$task_order), ]

# Apollo needs a numeric ID; keep the original alongside for traceability.
w$ID <- as.integer(factor(w$rid))
w$task <- w$task_order
rownames(w) <- NULL

# --- BWS: long, already in shape ------------------------------------------
b <- merge(db$bws, covars[, c("rid", "quota_band", "arm", "path")], by = "rid", all.x = TRUE)
b <- b[order(b$rid, b$set_order, b$position), ]
b$ID <- as.integer(factor(b$rid))

# Standardised best-minus-worst score per item per respondent, the descriptive
# summary that should always be reported alongside any model.
bw <- stats::aggregate(cbind(best, worst) ~ rid + item_id, b, sum)
shown <- stats::aggregate(best ~ rid + item_id, b, length); names(shown)[3] <- "shown"
bw <- merge(bw, shown)
bw$bw_std <- (bw$best - bw$worst) / bw$shown
bw <- merge(bw, BWS_ITEMS, by = "item_id", all.x = TRUE)

# --- Write ----------------------------------------------------------------
utils::write.csv(w,       file.path(OUT, "dce_apollo_wide.csv"), row.names = FALSE)
utils::write.csv(d,       file.path(OUT, "dce_long.csv"),        row.names = FALSE)
utils::write.csv(b,       file.path(OUT, "bws_long.csv"),        row.names = FALSE)
utils::write.csv(bw,      file.path(OUT, "bws_scores.csv"),      row.names = FALSE)
utils::write.csv(covars,  file.path(OUT, "respondents.csv"),     row.names = FALSE)

cat("\nWritten to", OUT, ":\n")
cat(sprintf("  dce_apollo_wide.csv  %d tasks x %d cols (Apollo)\n", nrow(w), ncol(w)))
cat(sprintf("  dce_long.csv         %d rows (mlogit / gmnl)\n", nrow(d)))
cat(sprintf("  bws_long.csv         %d rows (support.BWS)\n", nrow(b)))
cat(sprintf("  bws_scores.csv       %d respondent-item scores\n", nrow(bw)))
cat(sprintf("  respondents.csv      %d respondents\n", nrow(covars)))

cat("\nStage 1 choice split:\n"); print(table(w$choice[w$dominance == 0]))
cat("Stage 2 outcome split:\n")
print(table(factor(w$choice2[w$dominance == 0], 1:3,
                   c("take_loan", "pay_privately", "do_not_proceed"))))
if (any(w$dominance == 1)) {
  dm <- w[w$dominance == 1, ]
  worse <- ifelse(dm$rate_A > dm$rate_B, 1L, 2L)
  cat(sprintf("Dominance test: %d of %d chose the strictly worse loan (reported, not excluded)\n",
              sum(dm$choice == worse), nrow(dm)))
}
