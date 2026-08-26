# 03-design-bws.R -----------------------------------------------------------
# Best–Worst Scaling, Case 1 (object case) over barrier items.
#
# WHY THIS FILE LOOKS LIKE THIS
# The v2 open-items list offers "shorten the BWS to ~7 sets" as a way to claw
# back burden. Enumerating every way of dropping blocks from the (13,4,1)
# design shows that option does not exist: across all 1,716 seven-block subsets
# the item-appearance counts take only four distinct multisets, none balanced,
# and one of them drops an item to zero appearances. The same holds at every
# block count from 5 to 12. Balance is a property of the whole cyclic design,
# not something a good choice of blocks can recover.
#
# The lever that does work is the ITEM COUNT. Each catalogue entry below is a
# genuine BIBD in which every item appears equally often and every pair of
# items co-occurs equally often, so all pairwise contrasts are estimated with
# equal precision. Pick the row whose burden you can afford, then cut the item
# list to match in the qualitative phase, rather than fielding an unbalanced
# design and carrying unequal precision into the analysis.
# ---------------------------------------------------------------------------

# --- Constructions ---------------------------------------------------------
# Cyclic development of a difference set D mod v.
.bibd_cyclic <- function(v, D) {
  do.call(rbind, lapply(0:(v - 1), function(s) sort((D + s) %% v) + 1L))
}

# Block complements: turns a (v,k,.) design into a (v, v-k, .) design.
.bibd_complement <- function(B, v) t(apply(B, 1, function(b) sort(setdiff(seq_len(v), b))))

# Affine plane AG(2,3): rows, columns and both diagonal families of a 3x3 grid.
.bibd_ag23 <- function() {
  g <- matrix(1:9, 3, 3, byrow = TRUE)
  bl <- list()
  for (i in 1:3) bl[[length(bl) + 1]] <- g[i, ]
  for (j in 1:3) bl[[length(bl) + 1]] <- g[, j]
  for (s in 0:2) bl[[length(bl) + 1]] <- vapply(0:2, function(i) g[i + 1L, ((i + s) %% 3) + 1L], integer(1))
  for (s in 0:2) bl[[length(bl) + 1]] <- vapply(0:2, function(i) g[i + 1L, ((-i + s) %% 3) + 1L], integer(1))
  do.call(rbind, lapply(bl, sort))
}

# --- Catalogue -------------------------------------------------------------
BWS_CATALOGUE <- list(
  "7"  = function() .bibd_complement(.bibd_cyclic(7, c(1, 2, 4)), 7),   # (7,4,2)
  "9"  = .bibd_ag23,                                                    # (9,3,1)
  "11" = function() .bibd_cyclic(11, c(1, 3, 4, 5, 9)),                 # (11,5,2)
  "13" = function() .bibd_cyclic(13, c(0, 1, 3, 9))                     # (13,4,1)
)

.BWS_CACHE <- new.env(parent = emptyenv())

bws_build <- function(n_items = 13) {
  key <- as.character(n_items)
  if (!is.null(.BWS_CACHE[[key]])) return(.BWS_CACHE[[key]])
  .BWS_CACHE[[key]] <- .bws_build(n_items)
  .BWS_CACHE[[key]]
}

.bws_build <- function(n_items = 13) {
  key <- as.character(n_items)
  if (is.null(BWS_CATALOGUE[[key]]))
    stop("No balanced BWS design catalogued for ", n_items,
         " items. Available: ", paste(names(BWS_CATALOGUE), collapse = ", "))
  B <- BWS_CATALOGUE[[key]]()
  colnames(B) <- paste0("pos", seq_len(ncol(B)))
  attr(B, "v") <- n_items
  B
}

# Verification. Called by dev/check.R and surfaced in the admin panel, so a
# broken design is caught before fielding rather than at analysis.
bws_properties <- function(B) {
  v <- attr(B, "v"); if (is.null(v)) v <- max(B)
  r <- as.integer(table(factor(as.vector(B), levels = seq_len(v))))
  lam <- matrix(0L, v, v)
  for (i in seq_len(nrow(B))) {
    cb <- utils::combn(sort(B[i, ]), 2)
    for (j in seq_len(ncol(cb))) {
      a <- cb[1, j]; b <- cb[2, j]
      lam[a, b] <- lam[a, b] + 1L; lam[b, a] <- lam[b, a] + 1L
    }
  }
  off <- lam[upper.tri(lam)]
  list(v = v, k = ncol(B), b = nrow(B),
       r = sort(unique(r)), lambda = sort(unique(off)),
       balanced = length(unique(r)) == 1L && length(unique(off)) == 1L,
       burden_sec = nrow(B) * BURDEN$bws_per_set)
}

bws_catalogue_table <- function() {
  do.call(rbind, lapply(names(BWS_CATALOGUE), function(k) {
    p <- bws_properties(bws_build(as.integer(k)))
    data.frame(items = p$v, set_size = p$k, sets = p$b, appearances = p$r[1],
               lambda = p$lambda[1], balanced = p$balanced,
               burden = fmt_mmss(p$burden_sec), stringsAsFactors = FALSE)
  }))
}

# --- Respondent-level serving ---------------------------------------------
bws_design <- function(cfg = CFG) bws_build(cfg$bws_items)

# Random set order, random item order within set.
bws_sets_for <- function(cfg = CFG) {
  B <- bws_design(cfg)
  ord <- sample(nrow(B))
  lapply(seq_along(ord), function(j)
    list(set_id = sprintf("B%02d", ord[j]), set_order = j, items = sample(B[ord[j], ])))
}
