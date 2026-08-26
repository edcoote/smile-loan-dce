# dev/lock-check.R ----------------------------------------------------------
# Readiness gate, run immediately before locking the instrument and registering
# the SAP.
#
# dev/check.R asks "is the code correct". This asks "is the instrument ready to
# put in front of a person" — placeholder wording removed, item bank matching
# the served design, no credentials committed, burden inside target. Every
# failure here is something that would survive a green check.R run and reach a
# respondent.
#
#   Rscript dev/lock-check.R
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
if (!dir.exists("app/R")) stop("Run from the repository root: Rscript dev/lock-check.R")
for (f in sort(list.files("app/R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

PASS <- 0L; FAIL <- 0L; WARN <- 0L
ok <- function(label, expr, fatal = TRUE) {
  r <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (isTRUE(r)) { PASS <<- PASS + 1L; cat(sprintf("  ok    %s\n", label)) }
  else if (fatal) { FAIL <<- FAIL + 1L; cat(sprintf("  BLOCK %s\n", label)) }
  else { WARN <<- WARN + 1L; cat(sprintf("  warn  %s\n", label)) }
}

src <- paste(unlist(lapply(list.files("app/R", pattern = "[.]R$", full.names = TRUE),
                           readLines, warn = FALSE)), collapse = "\n")

cat("\n== Placeholder wording ==\n")
ok("no VERBATIM_REQUIRED markers remain", !grepl("VERBATIM_REQUIRED", src, fixed = TRUE))
ok("AOHS stem is not the placeholder", !grepl("^\\[", AOHS$label))
ok("no MDAS stem is a placeholder",
   !any(vapply(MDAS$items, function(it) grepl("^\\[", it$label), logical(1))))
ok("AOHS has five response options", length(AOHS$options) == 5)
ok("MDAS has five items and five response options",
   length(MDAS$items) == 5 && length(MDAS$options) == 5)

cat("\n== BWS item bank ==\n")
B <- bws_design(CFG)
served <- max(B)
ok(sprintf("item bank (%d) matches the served design (%d items)", nrow(BWS_ITEMS), served),
   nrow(BWS_ITEMS) == served)
if (nrow(BWS_ITEMS) > served)
  cat(sprintf("        %d item(s) below the cut are never shown: %s\n",
              nrow(BWS_ITEMS) - served,
              paste(BWS_ITEMS$item_id[(served + 1):nrow(BWS_ITEMS)], collapse = ", ")))
ok("no placeholder text in the served items",
   !any(grepl("PLACEHOLDER|TODO|TBC|XXX", BWS_ITEMS$label[seq_len(served)], ignore.case = TRUE)))
ok("served design is balanced", bws_properties(B)$balanced)

cat("\n== Credentials ==\n")
ok("no admin key default committed", !grepl('SURVEY_ADMIN_KEY", "[^"]+"', src))
ok("no key-like literals in source",
   !grepl("eyJhbGciOi|sk_live_|service_role", src), fatal = TRUE)
ok("SURVEY_ADMIN_KEY set in this environment (needed for the monitor)",
   nzchar(Sys.getenv("SURVEY_ADMIN_KEY")), fatal = FALSE)

cat("\n== Burden ==\n")
sec <- burden_estimate(CFG)
cat(sprintf("        estimate %s (BWS %d items / %d sets, DCE %d tasks%s)\n",
            fmt_mmss(sec), CFG$bws_items, nrow(B), CFG$dce_tasks,
            if (CFG$include_dominance) " + dominance" else ""))
ok("burden at or under the 13-minute target", sec <= 13 * 60, fatal = FALSE)
ok("burden under 15 minutes", sec <= 15 * 60)

cat("\n== Storage ==\n")
st <- tryCatch(store_init(), error = function(e) NULL)
ok("a store initialises", !is.null(st))
if (!is.null(st)) {
  cat(sprintf("        backend: %s\n", st$kind))
  ok("backend is not memory (memory keeps nothing)", !identical(st$kind, "memory"))
  ok("backend is not csv (use a database for multi-device fielding)",
     !identical(st$kind, "csv"), fatal = FALSE)
  ok("store is healthy", isTRUE(st$healthy()))
  if (!is.null(st$integrity)) ok("database integrity ok", identical(st$integrity(), "ok"))
  if (!is.null(st$disconnect)) try(st$disconnect(), silent = TRUE)
}

cat("\n== Analysis pipeline ==\n")
ok("Apollo installed", requireNamespace("apollo", quietly = TRUE), fatal = FALSE)
ok("prepare script present", file.exists("analysis/01-prepare.R"))
ok("model script present", file.exists("analysis/02-apollo-dce.R"))

cat("\n== Governance (manual — confirm before fielding) ==\n")
for (x in c("HRA decision tool completed and REC route confirmed",
            "SAP registered on OSF",
            "Retention period set and documented with the governance team",
            "Participant information names the data controller and retention period",
            "Backups scheduled and a restore tested at least once",
            "Recruitment materials use the neutral framing, not 'what stopped you'"))
  cat("  [ ] ", x, "\n")

cat(sprintf("\n%d passed, %d warnings, %d blocking\n", PASS, WARN, FAIL))
if (FAIL > 0) { cat("NOT READY TO LOCK.\n"); quit(status = 1) }
cat("No blocking issues. Confirm the manual items above, then lock.\n")
