# 02-design-dce.R -----------------------------------------------------------
# Smile Loan DCE — dual-response, unlabelled paired design.
#
# SINGLE SOURCE OF TRUTH
# Everything about the experiment is declared in DCE_SPEC below: the attributes,
# their levels, whether they are ordered, and how each level is shown to a
# respondent. Profiles, coding matrices, dominance filtering, the efficient
# design search, the respondent-facing cards and the platform export are all
# DERIVED from it. Adding, removing or re-levelling an attribute means editing
# DCE_SPEC and nothing else — no export code, no UI code, no test code.
#
# The one thing that is not derived is respondent-facing wording, which lives in
# each attribute's `render` function. That is deliberate: auto-generated text
# ("fee: 250") is exactly the kind of thing that quietly damages validity, so
# wording stays explicit and hand-written, just co-located with the attribute it
# describes.
#
# DUAL RESPONSE
#   Stage 1  forced choice, Loan A vs Loan B.
#   Stage 2  "would you actually take it?" -> yes / no; if no, the non-loan
#            outcome is recorded as *pay privately* or *do not proceed*, never
#            collapsed into a single opt-out.
# ---------------------------------------------------------------------------

# --- THE SPEC --------------------------------------------------------------
# name      column name used everywhere downstream (stored, exported, modelled)
# label     short human name for codebooks and exports
# levels    the levels, in their natural order
# monotone  "lower_better" | "higher_better" | NA (unordered)
#           Only monotone attributes participate in dominance filtering.
# unit      free text for the codebook
# render    function(level) -> HTML string shown inside a loan card
DCE_SPEC <- list(
  list(name = "rate", label = "Repayment rate", levels = c(6, 9, 12, 15),
       monotone = "lower_better", unit = "% of earnings above the floor",
       render = function(x) sprintf("Repay <b>%s%%</b> of earnings above \u00A312,570", x)),
  list(name = "fee", label = "Upfront fee", levels = c(0, 250, 750),
       monotone = "lower_better", unit = "GBP, one-off",
       render = function(x) sprintf("Upfront fee: <b>%s</b>", fmt_gbp(x)))
)

# Everything below is generic over DCE_SPEC.
dce_names  <- function(spec = DCE_SPEC) vapply(spec, `[[`, "", "name")
dce_levels <- function(spec = DCE_SPEC) setNames(lapply(spec, `[[`, "levels"), dce_names(spec))

# --- Profiles --------------------------------------------------------------
dce_profiles <- function(spec = DCE_SPEC) {
  g <- expand.grid(rev(dce_levels(spec)), stringsAsFactors = FALSE)
  g <- g[, rev(seq_len(ncol(g))), drop = FALSE]
  names(g) <- dce_names(spec)
  g$pid <- sprintf("P%02d", seq_len(nrow(g)))
  g[, c("pid", dce_names(spec)), drop = FALSE]
}

# --- Coding ----------------------------------------------------------------
# Linear: monotone numeric attributes only, centred and scaled so difference
# vectors are comparable across attributes of different magnitudes.
dce_x_linear <- function(p, spec = DCE_SPEC) {
  keep <- vapply(spec, function(a) !is.na(a$monotone) && is.numeric(a$levels), logical(1))
  if (!any(keep)) stop("No monotone numeric attributes: the linear coding is undefined.")
  m <- vapply(spec[keep], function(a) {
    v <- p[[a$name]]; lv <- a$levels
    (v - mean(range(lv))) / (diff(range(lv)) / 2)
  }, numeric(nrow(p)))
  colnames(m) <- dce_names(spec)[keep]
  m
}

# Effects coding: every attribute, nlev - 1 columns each.
dce_x_effects <- function(p, spec = DCE_SPEC) {
  blocks <- lapply(spec, function(a) {
    lv <- a$levels; v <- p[[a$name]]
    m <- matrix(0, length(v), length(lv) - 1,
                dimnames = list(NULL, paste0(a$name, "_", lv[-length(lv)])))
    for (i in seq_along(v)) {
      k <- match(v[i], lv)
      if (k < length(lv)) m[i, k] <- 1 else m[i, ] <- -1
    }
    m
  })
  do.call(cbind, blocks)
}

# --- Dominance -------------------------------------------------------------
# A pair is dominated when one profile is weakly better on every MONOTONE
# attribute and strictly better on at least one, with all unordered attributes
# equal. Unordered attributes cannot make a profile better or worse, so a pair
# differing on one is never dominated.
.dominates <- function(a, b, spec) {
  mono <- Filter(function(x) !is.na(x$monotone), spec)
  unord <- Filter(function(x) is.na(x$monotone), spec)
  for (x in unord) if (!identical(a[[x$name]], b[[x$name]])) return(FALSE)
  strict <- FALSE
  for (x in mono) {
    av <- a[[x$name]]; bv <- b[[x$name]]
    better <- if (identical(x$monotone, "lower_better")) av <= bv else av >= bv
    if (!better) return(FALSE)
    if (av != bv) strict <- TRUE
  }
  strict
}

dce_candidates <- function(spec = DCE_SPEC) {
  p <- dce_profiles(spec)
  n <- nrow(p)
  out <- vector("list", 0)
  for (i in seq_len(n - 1)) for (j in (i + 1):n) {
    if (.dominates(p[i, ], p[j, ], spec) || .dominates(p[j, ], p[i, ], spec)) next
    out[[length(out) + 1]] <- c(i, j)
  }
  if (!length(out)) stop("No non-dominated pairs: check the attribute monotonicity flags.")
  do.call(rbind, lapply(out, function(z) data.frame(a = z[1], b = z[2])))
}

# --- Efficiency criterion --------------------------------------------------
# At beta = 0 both alternatives carry p = 0.5, so the MNL information matrix
# collapses to 0.25 * sum_s d_s d_s' with d_s = x_A - x_B, and D-optimality is
# maximisation of det(sum_s d_s d_s').
.d_criterion <- function(idx, cand, Xm) {
  D <- Xm[cand$a[idx], , drop = FALSE] - Xm[cand$b[idx], , drop = FALSE]
  det(crossprod(D))
}

.level_imbalance <- function(idx, cand, p, spec) {
  d <- cand[idx, ]
  sum(vapply(spec, function(a) {
    v <- c(p[[a$name]][d$a], p[[a$name]][d$b])
    stats::var(as.numeric(table(factor(v, levels = a$levels))))
  }, numeric(1)))
}

# Random-swap search, used when the candidate space is too large to enumerate.
# Multi-start, deterministic given the seed, so the served design is still
# reproducible from the repository alone.
.dce_swap <- function(n, cand, Xm, starts = 40, seed = 20260826) {
  set.seed(seed)
  N <- nrow(cand); best <- NULL; bestv <- -Inf
  for (s in seq_len(starts)) {
    idx <- sample(N, n)
    cur <- .d_criterion(idx, cand, Xm)
    repeat {
      improved <- FALSE
      for (pos in seq_len(n)) {
        for (repl in setdiff(seq_len(N), idx)) {
          trial <- idx; trial[pos] <- repl
          v <- .d_criterion(trial, cand, Xm)
          if (v > cur + 1e-12) { idx <- trial; cur <- v; improved <- TRUE }
        }
      }
      if (!improved) break
    }
    if (cur > bestv) { bestv <- cur; best <- idx }
  }
  best
}

# Chooses exhaustive enumeration when it is affordable and swapping when it is
# not, and records which was used so the method section can state it.
DCE_ENUM_LIMIT <- 2e5

dce_optimal <- function(n = CFG$dce_tasks, spec = DCE_SPEC,
                        criterion = c("effects", "linear"), verbose = FALSE) {
  criterion <- match.arg(criterion)
  p    <- dce_profiles(spec)
  cand <- dce_candidates(spec)
  XE   <- dce_x_effects(p, spec)
  XL   <- tryCatch(dce_x_linear(p, spec), error = function(e) NULL)
  Xp   <- if (criterion == "effects") XE else XL
  if (is.null(Xp)) stop("Requested criterion is unavailable for this spec.")
  if (n > nrow(cand)) stop("Asked for ", n, " sets but only ", nrow(cand),
                           " non-dominated pairs exist.")

  nsub <- tryCatch(choose(nrow(cand), n), error = function(e) Inf)
  method <- if (is.finite(nsub) && nsub <= DCE_ENUM_LIMIT) "exhaustive" else "swap"

  if (method == "exhaustive") {
    combos <- utils::combn(nrow(cand), n)
    pv <- apply(combos, 2, .d_criterion, cand = cand, Xm = Xp)
    ties <- which(pv > max(pv) - 1e-9)
    bal <- vapply(ties, function(k) .level_imbalance(combos[, k], cand, p, spec), numeric(1))
    sec <- if (!is.null(XL)) vapply(ties, function(k) .d_criterion(combos[, k], cand, XL), numeric(1))
           else rep(0, length(ties))
    k <- ties[order(bal, -sec)][1]
    best <- combos[, k]; nties <- length(ties)
    rel <- if (!is.null(XL)) (.d_criterion(best, cand, XL) / max(sec, .d_criterion(best, cand, XL)))^(1 / ncol(XL)) else NA_real_
    lin_max <- if (!is.null(XL)) max(apply(combos, 2, .d_criterion, cand = cand, Xm = XL)) else NA_real_
  } else {
    best <- .dce_swap(n, cand, Xp); nties <- NA_integer_; lin_max <- NA_real_
  }

  dl <- if (!is.null(XL)) .d_criterion(best, cand, XL) else NA_real_
  de <- .d_criterion(best, cand, XE)
  if (verbose)
    cat(sprintf("method=%s candidates=%d subsets=%s det(effects)=%.1f det(linear)=%.1f\n",
                method, nrow(cand), format(nsub, big.mark = ","), de, dl))

  # Relative efficiency of the served design for the LINEAR model, against the
  # best linear design available. Only computable when the space was enumerated;
  # NA under swapping, where no global maximum is known.
  rel_lin <- if (!is.na(lin_max) && !is.na(dl) && lin_max > 0)
    (dl / lin_max)^(1 / ncol(XL)) else NA_real_
  structure(cand[best, ], method = method, n_candidates = nrow(cand),
            n_subsets = nsub, criterion = criterion, n_ties = nties,
            det_linear = dl, det_effects = de, rel_eff_linear = rel_lin,
            imbalance = .level_imbalance(best, cand, p, spec))
}

# --- Served design ---------------------------------------------------------
.DESIGN_CACHE <- new.env(parent = emptyenv())

dce_design <- function(n = CFG$dce_tasks, spec = DCE_SPEC) {
  key <- paste0("d", n, "_", length(spec), "_", paste(dce_names(spec), collapse = "."))
  if (!is.null(.DESIGN_CACHE[[key]])) return(.DESIGN_CACHE[[key]])
  .DESIGN_CACHE[[key]] <- .dce_design_build(n, spec)
  .DESIGN_CACHE[[key]]
}

.dce_design_build <- function(n = CFG$dce_tasks, spec = DCE_SPEC) {
  p   <- dce_profiles(spec)
  sel <- dce_optimal(n, spec)
  nm  <- dce_names(spec)
  d <- data.frame(set_id = sprintf("S%02d", seq_len(nrow(sel))), stringsAsFactors = FALSE)
  for (a in nm) {
    d[[paste0("a_", a)]] <- p[[a]][sel$a]
    d[[paste0("b_", a)]] <- p[[a]][sel$b]
  }
  d$dominance <- 0L
  d$block <- dce_blocks(d, spec = spec)
  for (at in c("method", "det_linear", "det_effects", "rel_eff_linear", "imbalance",
               "criterion", "n_candidates", "n_subsets", "n_ties"))
    attr(d, at) <- attr(sel, at)
  d
}

dce_design_summary <- function(d = dce_design()) {
  sprintf("%d sets over %d attributes; %d non-dominated pairs; method=%s; criterion=%s; det_eff=%.0f; det_lin=%.0f",
          nrow(d), length(DCE_SPEC), attr(d, "n_candidates"), attr(d, "method"),
          attr(d, "criterion"), attr(d, "det_effects"), attr(d, "det_linear"))
}

# The dominance test: one profile better on every monotone attribute. Reported
# as a data-quality indicator, never used to exclude.
dce_dominance_task <- function(spec = DCE_SPEC) {
  best <- worst <- list()
  for (a in spec) {
    lv <- a$levels
    if (is.na(a$monotone)) { best[[a$name]] <- lv[1]; worst[[a$name]] <- lv[1] }
    else if (identical(a$monotone, "lower_better")) {
      best[[a$name]] <- min(lv); worst[[a$name]] <- max(lv)
    } else { best[[a$name]] <- max(lv); worst[[a$name]] <- min(lv) }
  }
  d <- data.frame(set_id = "SDOM", stringsAsFactors = FALSE)
  for (a in dce_names(spec)) {
    d[[paste0("a_", a)]] <- best[[a]]; d[[paste0("b_", a)]] <- worst[[a]]
  }
  d$dominance <- 1L; d$block <- NA_integer_
  d
}

# Which side is strictly worse in a dominance task?
.dce_worse_side <- function(row, spec = DCE_SPEC) {
  a <- setNames(lapply(dce_names(spec), function(n) row[[paste0("a_", n)]]), dce_names(spec))
  b <- setNames(lapply(dce_names(spec), function(n) row[[paste0("b_", n)]]), dce_names(spec))
  if (.dominates(a, b, spec)) "B" else if (.dominates(b, a, spec)) "A" else NA_character_
}

dce_dominance_failed <- function(row, choice, spec = DCE_SPEC) {
  if (row$dominance != 1L) return(NA_integer_)
  w <- .dce_worse_side(row, spec)
  if (is.na(w)) return(NA_integer_)
  as.integer(choice == w)
}

# Two equal blocks, maximin on the D-criterion so a blocked fielding does not
# leave one arm materially weaker.
dce_blocks <- function(d, n_blocks = 2, spec = DCE_SPEC) {
  n <- nrow(d)
  if (n_blocks != 2 || n %% 2 != 0 || n > 24) return(rep(1L, n))
  p <- dce_profiles(spec); nm <- dce_names(spec)
  X <- tryCatch(dce_x_linear(p, spec), error = function(e) dce_x_effects(p, spec))
  key <- function(pref) {
    vals <- do.call(paste, lapply(nm, function(a) d[[paste0(pref, "_", a)]]))
    match(vals, do.call(paste, lapply(nm, function(a) p[[a]])))
  }
  ia <- key("a"); ib <- key("b")
  crit <- function(idx) { D <- X[ia[idx], , drop = FALSE] - X[ib[idx], , drop = FALSE]; det(crossprod(D)) }
  combos <- utils::combn(n, n / 2)
  combos <- combos[, combos[1, ] == 1, drop = FALSE]
  score <- apply(combos, 2, function(k) min(crit(k), crit(setdiff(seq_len(n), k))))
  out <- rep(2L, n); out[combos[, which.max(score)]] <- 1L
  out
}

# --- Respondent-level task list -------------------------------------------
dce_tasks_for <- function(cfg = CFG, spec = DCE_SPEC) {
  d <- dce_design(cfg$dce_tasks, spec)
  if (!is.na(cfg$dce_block)) d <- d[d$block == cfg$dce_block, ]
  d <- d[sample(nrow(d)), ]
  if (isTRUE(cfg$include_dominance)) {
    pos <- sample(2:nrow(d), 1)
    d <- rbind(d[seq_len(pos - 1), ], dce_dominance_task(spec), d[pos:nrow(d), ])
  }
  # Random side assignment so position is not confounded with terms.
  flip <- sample(c(TRUE, FALSE), nrow(d), TRUE)
  nm <- dce_names(spec)
  for (a in nm) {
    ac <- paste0("a_", a); bc <- paste0("b_", a)
    tmp <- d[flip, ac]; d[flip, ac] <- d[flip, bc]; d[flip, bc] <- tmp
  }
  d$side_flipped <- as.integer(flip)
  d$task_order <- seq_len(nrow(d))
  rownames(d) <- NULL
  d
}

# --- Respondent-facing rendering ------------------------------------------
# Returns the HTML lines for one alternative, in spec order.
dce_render_alt <- function(row, side, spec = DCE_SPEC) {
  vapply(spec, function(a) a$render(row[[paste0(side, "_", a$name)]]), character(1))
}
