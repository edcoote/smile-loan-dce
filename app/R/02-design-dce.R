# 02-design-dce.R -----------------------------------------------------------
# Smile Loan DCE — dual-response, unlabelled paired design.
#
# Attributes (threshold FIXED at the £12,570 personal allowance, not varied):
#   rate : 6, 9, 12, 15  (% of earnings above the floor)
#   fee  : 0, 250, 750   (£ one-off upfront)
#
# Stage 1 : forced choice, Loan A vs Loan B.
# Stage 2 : "would you actually take it?" -> yes / no; if no, the non-loan
#           outcome is recorded as *pay privately* or *do not proceed*,
#           never collapsed into a single opt-out.
#
# Design construction. The 4x3 factorial gives 12 profiles and C(12,2)=66
# unordered pairs. A pair is dominated when one profile is weakly cheaper on
# both attributes; those carry no trade-off information in stage 1. Removing
# them leaves exactly 18 candidate pairs (4C2 rate contrasts x 3C2 fee
# contrasts). Choosing 12 of 18 is C(18,12) = 18,564 designs, so the D-optimal
# design is found by complete enumeration, not by a heuristic search. It is
# therefore exact and reproducible without a seed.
#
# Under the standard null prior (beta = 0) both alternatives in a binary set
# carry p = 0.5, and the MNL information matrix collapses to
#       I(beta=0) = 0.25 * sum_s d_s d_s'      with d_s = x_A - x_B
# so D-optimality is maximisation of det(sum_s d_s d_s'). This is exact for the
# linear-in-attributes specification used for the primary WTP model; the
# effects-coded D-error is reported alongside as a robustness figure.
# ---------------------------------------------------------------------------

DCE_LEVELS <- list(rate = c(6, 9, 12, 15), fee = c(0, 250, 750))

# All 12 profiles.
dce_profiles <- function() {
  d <- expand.grid(fee = DCE_LEVELS$fee, rate = DCE_LEVELS$rate)
  d <- data.frame(rate = d$rate, fee = d$fee)
  d$pid <- sprintf("P%02d", seq_len(nrow(d)))
  d[, c("pid", "rate", "fee")]
}

# Linear coding used for the primary model: rate in points, fee in £100s,
# both centred so the difference vector is scale-comparable.
dce_x_linear <- function(p) cbind(rate = (p$rate - 10.5) / 3, fee = (p$fee - 250) / 100)

# Effects coding for the robustness check: 3 df for rate, 2 df for fee.
dce_x_effects <- function(p) {
  ec <- function(v, lv) {
    m <- matrix(0, length(v), length(lv) - 1)
    for (i in seq_along(v)) {
      k <- match(v[i], lv)
      if (k < length(lv)) m[i, k] <- 1 else m[i, ] <- -1
    }
    m
  }
  cbind(ec(p$rate, DCE_LEVELS$rate), ec(p$fee, DCE_LEVELS$fee))
}

# Candidate pairs that are not dominated: A strictly better on rate,
# strictly worse on fee (or vice versa — order is fixed later at random).
dce_candidates <- function() {
  p <- dce_profiles()
  out <- NULL
  for (i in seq_len(nrow(p))) for (j in seq_len(nrow(p))) {
    if (i >= j) next
    lo_rate <- p$rate[i] < p$rate[j]; hi_fee <- p$fee[i] > p$fee[j]
    if ((lo_rate && hi_fee) || (!lo_rate && p$rate[i] > p$rate[j] && p$fee[i] < p$fee[j]))
      out <- rbind(out, data.frame(a = i, b = j))
  }
  out
}

# det(sum d d') for a subset of candidate pairs, given a coding function.
.d_criterion <- function(idx, cand, Xm) {
  D <- Xm[cand$a[idx], , drop = FALSE] - Xm[cand$b[idx], , drop = FALSE]
  det(crossprod(D))
}

# Attribute-level imbalance: summed variance of level appearance counts.
# Zero would be perfect balance; the non-dominated candidate set makes perfect
# balance unreachable here, so this is used only to break ties.
.level_imbalance <- function(idx, cand, p) {
  d <- cand[idx, ]
  r <- table(factor(c(p$rate[d$a], p$rate[d$b]), levels = DCE_LEVELS$rate))
  f <- table(factor(c(p$fee[d$a],  p$fee[d$b]),  levels = DCE_LEVELS$fee))
  stats::var(as.numeric(r)) + stats::var(as.numeric(f))
}

# Complete enumeration of C(18, n) subsets.
#
# Primary criterion is the EFFECTS-coded D-criterion, not the linear one.
# Rate is carried at four levels precisely so that curvature in the repayment
# rate can be tested; a linear-optimal design loads on the extreme levels and
# leaves the interior levels thin (7/5/5/7 across 6/9/12/15). The effects-
# optimal design gives 7/6/6/5 and a 48% larger effects-coded determinant, at
# the cost of ~16% relative efficiency for the linear WTP model. That is the
# right side of the trade when non-linearity is a stated estimand.
#
# Ties on the effects criterion (there are four) are broken first on level
# balance, then on the linear criterion — so the served design is the most
# balanced, most linear-efficient member of the effects-optimal set, and is
# fully determined without a random seed.
dce_optimal <- function(n = 12, criterion = c("effects", "linear"), verbose = FALSE) {
  criterion <- match.arg(criterion)
  p    <- dce_profiles()
  cand <- dce_candidates()
  XL   <- dce_x_linear(p)
  XE   <- dce_x_effects(p)
  combos <- utils::combn(nrow(cand), n)
  cl <- apply(combos, 2, .d_criterion, cand = cand, Xm = XL)
  ce <- apply(combos, 2, .d_criterion, cand = cand, Xm = XE)
  primary <- if (criterion == "effects") ce else cl
  ties <- which(primary > max(primary) - 1e-9)
  bal  <- vapply(ties, function(k) .level_imbalance(combos[, k], cand, p), numeric(1))
  sec  <- if (criterion == "effects") cl[ties] else ce[ties]
  k    <- ties[order(bal, -sec)][1]
  best <- combos[, k]
  if (verbose)
    cat(sprintf("candidates=%d  subsets=%d  ties=%d  det(effects)=%.1f  det(linear)=%.1f  imbalance=%.3f\n",
                nrow(cand), ncol(combos), length(ties), ce[k], cl[k], bal[order(bal, -sec)][1]))
  structure(cand[best, ], n_candidates = nrow(cand), n_subsets = ncol(combos),
            criterion = criterion, n_ties = length(ties),
            det_linear = cl[k], det_effects = ce[k],
            rel_eff_linear = (cl[k] / max(cl))^(1 / ncol(XL)),
            imbalance = bal[order(bal, -sec)][1])
}

# The enumeration is deterministic, so it is computed once per process and
# cached. Without this every respondent pays ~0.3s at the first DCE page.
.DESIGN_CACHE <- new.env(parent = emptyenv())

# The served design: one row per choice set, both profiles side by side.
dce_design <- function(n = 12) {
  key <- paste0("d", n)
  if (!is.null(.DESIGN_CACHE[[key]])) return(.DESIGN_CACHE[[key]])
  .DESIGN_CACHE[[key]] <- .dce_design_build(n)
  .DESIGN_CACHE[[key]]
}

.dce_design_build <- function(n = 12) {
  p    <- dce_profiles()
  sel  <- dce_optimal(n)
  d <- data.frame(
    set_id  = sprintf("S%02d", seq_len(nrow(sel))),
    a_rate  = p$rate[sel$a], a_fee = p$fee[sel$a],
    b_rate  = p$rate[sel$b], b_fee = p$fee[sel$b],
    dominance = 0L, stringsAsFactors = FALSE)
  d$block <- dce_blocks(d)
  for (a in c("det_linear", "det_effects", "rel_eff_linear", "imbalance",
              "criterion", "n_candidates", "n_subsets", "n_ties"))
    attr(d, a) <- attr(sel, a)
  d
}

# One-line design summary for the audit record and the admin panel.
dce_design_summary <- function(d = dce_design(12)) {
  sprintf("%d sets from %d non-dominated pairs (%d enumerated); criterion=%s; det_eff=%.0f; det_lin=%.0f; rel_eff_linear=%.3f",
          nrow(d), attr(d, "n_candidates"), attr(d, "n_subsets"), attr(d, "criterion"),
          attr(d, "det_effects"), attr(d, "det_linear"), attr(d, "rel_eff_linear"))
}

# The dominance test: a pair where one loan is cheaper on both attributes.
# Reported as a data-quality indicator; never used to exclude a respondent.
dce_dominance_task <- function() {
  data.frame(set_id = "SDOM", a_rate = 6, a_fee = 0, b_rate = 15, b_fee = 750,
             dominance = 1L, block = NA_integer_, stringsAsFactors = FALSE)
}

# Split the design into two balanced blocks of equal size, chosen to maximise
# the *minimum* D-criterion across blocks (maximin, not average) so that a
# blocked fielding does not leave one arm materially weaker than the other.
# C(12,6)/2 = 462 splits: enumerated exactly.
dce_blocks <- function(d, n_blocks = 2) {
  n <- nrow(d)
  if (n_blocks != 2 || n %% 2 != 0) return(rep(1L, n))
  p <- dce_profiles()
  X <- dce_x_linear(p)
  key <- function(rate, fee) match(paste(rate, fee), paste(p$rate, p$fee))
  ia <- key(d$a_rate, d$a_fee); ib <- key(d$b_rate, d$b_fee)
  crit <- function(idx) { D <- X[ia[idx], , drop = FALSE] - X[ib[idx], , drop = FALSE]; det(crossprod(D)) }
  combos <- utils::combn(n, n / 2)
  combos <- combos[, combos[1, ] == 1, drop = FALSE]   # de-duplicate mirror splits
  score <- apply(combos, 2, function(k) min(crit(k), crit(setdiff(seq_len(n), k))))
  best <- combos[, which.max(score)]
  out <- rep(2L, n); out[best] <- 1L
  out
}

# Respondent-level task list: block filter, random task order, random A/B side
# assignment (so position is not confounded with terms), dominance task placed
# at a random interior position.
dce_tasks_for <- function(cfg = CFG) {
  d <- dce_design(12)
  if (!is.na(cfg$dce_block)) d <- d[d$block == cfg$dce_block, ]
  if (cfg$dce_tasks < nrow(d)) d <- d[seq_len(cfg$dce_tasks), ]
  d <- d[sample(nrow(d)), ]
  if (isTRUE(cfg$include_dominance)) {
    pos <- sample(2:(nrow(d)), 1)
    d <- rbind(d[seq_len(pos - 1), ], dce_dominance_task(), d[pos:nrow(d), ])
  }
  flip <- sample(c(TRUE, FALSE), nrow(d), TRUE)
  sw <- d[flip, ]
  d[flip, c("a_rate", "a_fee", "b_rate", "b_fee")] <- sw[, c("b_rate", "b_fee", "a_rate", "a_fee")]
  d$side_flipped <- as.integer(flip)
  d$task_order <- seq_len(nrow(d))
  rownames(d) <- NULL
  d
}

# Did the respondent fail the dominance test? (chose the strictly worse loan)
dce_dominance_failed <- function(row, choice) {
  if (row$dominance != 1L) return(NA_integer_)
  worse <- if (row$a_rate > row$b_rate) "A" else "B"
  as.integer(choice == worse)
}
