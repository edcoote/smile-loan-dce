# dev/export-design.R -------------------------------------------------------
# Exports the served designs as platform-ready CSVs plus a codebook.
#
# THIS FILE CONTAINS NO ATTRIBUTE NAMES. Everything is read from DCE_SPEC and
# the BWS catalogue, so changing the experiment — adding an attribute, adding a
# level, changing the task count, moving to a different BWS item count — changes
# the exported files with no edit here. Re-run and upload.
#
#   Rscript dev/export-design.R [outdir]
#
# Produces, in outdir:
#   dce_wide.csv        one row per choice set, alt1_* / alt2_* columns.
#                       The shape Qualtrics loop-and-merge, Gorilla spreadsheets
#                       and most panel platforms ingest directly.
#   dce_long.csv        one row per alternative per set, with the rendered
#                       respondent-facing text for each attribute.
#   dce_codebook.csv    attribute, level, code, label, unit, direction.
#   bws_sets.csv        one row per item shown per set, with item text.
#   design_properties.txt  the method record for the SAP and the manuscript.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
if (!dir.exists("app/R")) stop("Run from the repository root: Rscript dev/export-design.R")
for (f in sort(list.files("app/R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
OUT  <- if (length(args) >= 1) args[1] else "export"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

nm   <- dce_names()
spec <- DCE_SPEC
d    <- dce_design()
if (isTRUE(CFG$include_dominance)) d <- rbind(d, dce_dominance_task())

# --- Wide: one row per choice set -----------------------------------------
wide <- data.frame(set_id = d$set_id, block = d$block, dominance = d$dominance,
                   stringsAsFactors = FALSE)
for (a in spec) {
  wide[[paste0("alt1_", a$name)]] <- d[[paste0("a_", a$name)]]
  wide[[paste0("alt2_", a$name)]] <- d[[paste0("b_", a$name)]]
}
# Rendered text alongside the raw levels, so a platform that wants display
# strings does not need the levels re-formatted by hand in its own editor.
for (a in spec) {
  wide[[paste0("alt1_", a$name, "_text")]] <- vapply(d[[paste0("a_", a$name)]],
    function(x) gsub("<[^>]+>", "", a$render(x)), character(1))
  wide[[paste0("alt2_", a$name, "_text")]] <- vapply(d[[paste0("b_", a$name)]],
    function(x) gsub("<[^>]+>", "", a$render(x)), character(1))
}

# --- Long: one row per alternative ----------------------------------------
long <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
  do.call(rbind, lapply(c(A = "a", B = "b"), function(side) {
    row <- data.frame(set_id = d$set_id[i], block = d$block[i],
                      dominance = d$dominance[i],
                      alt = if (side == "a") "A" else "B", stringsAsFactors = FALSE)
    for (a in spec) row[[a$name]] <- d[[paste0(side, "_", a$name)]][i]
    row$display_text <- paste(gsub("<[^>]+>", "", dce_render_alt(d[i, ], side)),
                              collapse = " | ")
    row
  }))
}))
rownames(long) <- NULL

# --- Codebook --------------------------------------------------------------
codebook <- do.call(rbind, lapply(spec, function(a)
  data.frame(attribute = a$name, label = a$label,
             level_index = seq_along(a$levels), level_value = a$levels,
             unit = a$unit, direction = a$monotone %||% "unordered",
             display_text = vapply(a$levels, function(x) gsub("<[^>]+>", "", a$render(x)), character(1)),
             stringsAsFactors = FALSE)))
rownames(codebook) <- NULL

# --- BWS -------------------------------------------------------------------
B <- bws_design(CFG)
bws <- do.call(rbind, lapply(seq_len(nrow(B)), function(i)
  data.frame(set_id = sprintf("B%02d", i), position = seq_len(ncol(B)),
             item_id = BWS_ITEMS$item_id[B[i, ]],
             item_text = BWS_ITEMS$label[B[i, ]], stringsAsFactors = FALSE)))
rownames(bws) <- NULL
bp <- bws_properties(B)

# --- Write -----------------------------------------------------------------
utils::write.csv(wide,     file.path(OUT, "dce_wide.csv"),     row.names = FALSE)
utils::write.csv(long,     file.path(OUT, "dce_long.csv"),     row.names = FALSE)
utils::write.csv(codebook, file.path(OUT, "dce_codebook.csv"), row.names = FALSE)
utils::write.csv(bws,      file.path(OUT, "bws_sets.csv"),     row.names = FALSE)

props <- c(
  sprintf("%s  |  app %s  |  exported %s", INSTRUMENT_ID, APP_VERSION, now_utc()),
  "",
  "DCE",
  sprintf("  attributes         %d (%s)", length(spec), paste(nm, collapse = ", ")),
  sprintf("  levels             %s", paste(vapply(spec, function(a)
    sprintf("%s: %s", a$name, paste(a$levels, collapse = "/")), character(1)), collapse = "; ")),
  sprintf("  profiles           %d", nrow(dce_profiles())),
  sprintf("  non-dominated pairs %d of %d", attr(dce_design(), "n_candidates"),
          choose(nrow(dce_profiles()), 2)),
  sprintf("  choice sets        %d (+%d dominance test)", CFG$dce_tasks,
          as.integer(CFG$include_dominance)),
  sprintf("  search method      %s over %s candidate designs", attr(dce_design(), "method"),
          format(attr(dce_design(), "n_subsets"), big.mark = ",")),
  sprintf("  criterion          %s-coded D-optimality", attr(dce_design(), "criterion")),
  sprintf("  det(effects)       %.1f", attr(dce_design(), "det_effects")),
  sprintf("  det(linear)        %.1f", attr(dce_design(), "det_linear")),
  sprintf("  rel. eff. linear   %s", ifelse(is.na(attr(dce_design(), "rel_eff_linear")), "n/a (swap search)",
          sprintf("%.3f", attr(dce_design(), "rel_eff_linear")))),
  sprintf("  blocks             %s", paste(table(dce_design()$block), collapse = " / ")),
  "  dual response      forced choice, then take-it; non-loan outcome recorded",
  "                     separately as pay privately or do not proceed",
  "",
  "BWS Case 1",
  sprintf("  items              %d", bp$v),
  sprintf("  sets               %d of %d shown per set", bp$b, bp$k),
  sprintf("  appearances/item   %d", bp$r[1]),
  sprintf("  pair co-occurrence %d (balanced: %s)", bp$lambda[1], bp$balanced),
  "",
  "Randomisation to apply in the platform",
  "  Module order       50/50 BWS-first vs DCE-first; store the arm as a variable",
  "  DCE task order     randomised within respondent",
  "  DCE side           randomise which profile is shown as A",
  "  BWS set order      randomised within respondent",
  "  BWS item order     randomised within set",
  "",
  sprintf("Estimated burden    %s", fmt_mmss(burden_estimate(CFG))))
writeLines(props, file.path(OUT, "design_properties.txt"))

cat(paste(props, collapse = "\n"), "\n\n")
cat("Written to", OUT, ":\n")
for (f in list.files(OUT)) cat(sprintf("  %-24s %s\n", f,
  format(file.size(file.path(OUT, f)), big.mark = ",")))
