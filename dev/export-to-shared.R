# dev/export-to-shared.R ----------------------------------------------------
# Writes the current response set to an Excel workbook in a shared folder —
# OneDrive, SharePoint, a network drive — on a schedule.
#
# THE DIRECTION MATTERS. The database is the write path and the only
# authoritative copy. This workbook is a read-only derivative, regenerated from
# scratch each run. Nothing ever writes responses *into* OneDrive.
#
# Putting the live store in OneDrive instead would lose data: file sync has no
# row-level locking and resolves collisions last-writer-wins, so two responses
# submitted seconds apart produce a "conflicted copy" and one of them is gone,
# silently. Regenerating a derived file has no such failure mode — a bad run
# just leaves the previous workbook in place.
#
#   Rscript dev/export-to-shared.R [destination.xlsx]
#
# Windows Task Scheduler, every 15 minutes during fielding:
#   Program:   Rscript.exe
#   Arguments: dev/export-to-shared.R
#   Start in:  C:\path\to\repo
#
# Destination defaults to SURVEY_SHARED_XLSX, e.g.
#   C:\Users\You\OneDrive - 21D Clinical\Barriers survey\responses_live.xlsx
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
if (!dir.exists("app/R")) stop("Run from the repository root: Rscript dev/export-to-shared.R")
for (f in sort(list.files("app/R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
DEST <- if (length(args) >= 1) args[1] else Sys.getenv("SURVEY_SHARED_XLSX")
if (!nzchar(DEST)) stop("Set SURVEY_SHARED_XLSX or pass a destination path")

st <- store_init()
on.exit(try(if (!is.null(st$disconnect)) st$disconnect(), silent = TRUE), add = TRUE)
cat("Store backend:", st$kind, "\n")

if (!is.null(st$can_read) && !isTRUE(st$can_read()))
  stop("This store is insert-only. Set SUPABASE_SERVICE_KEY to export.")

db <- store_latest(st$read())
n  <- vapply(db, nrow, integer(1))
cat("Rows:", paste(names(n), n, sep = "=", collapse = "  "), "\n")

if (n[["respondents"]] == 0) {
  cat("No responses yet; leaving the existing workbook untouched.\n")
  quit(status = 0)
}

# Write to a temporary file first, then move into place. OneDrive begins
# uploading the moment a file appears, so writing directly to the synced folder
# can push a half-written workbook to everyone else.
dir.create(dirname(DEST), recursive = TRUE, showWarnings = FALSE)
tmp <- file.path(tempdir(), paste0("export_", as.integer(runif(1, 1e6, 9e6)), ".xlsx"))
store_export(st, tmp)

if (!file.exists(tmp) || file.size(tmp) < 1000) stop("Export produced no usable workbook.")
ok <- file.copy(tmp, DEST, overwrite = TRUE)
unlink(tmp)
if (!ok) stop("Could not write to ", DEST, " - is the folder synced and unlocked?")

cat(sprintf("Wrote %s (%.2f MB) at %s\n", DEST, file.size(DEST) / 1e6, now_utc()))
cat("This file is a snapshot. It is regenerated each run and is not the store.\n")
