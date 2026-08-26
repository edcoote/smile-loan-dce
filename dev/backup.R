# dev/backup.R --------------------------------------------------------------
# Timestamped hot backup of the SQLite response store. Safe to run while the
# survey is live: VACUUM INTO takes a consistent point-in-time snapshot of a
# database that is being written to.
#
# Do NOT back up by copying the .sqlite file with the OS, File Explorer or
# OneDrive. A file copy can catch a write mid-transaction and produce a
# snapshot that will not open — and you will not find out until you need it.
#
#   Rscript dev/backup.R [db_path] [backup_dir] [keep_n]
#
# Windows Task Scheduler, hourly during fielding:
#   Program:   Rscript.exe
#   Arguments: dev/backup.R
#   Start in:  C:\path\to\repo
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
if (!dir.exists("app/R")) stop("Run from the repository root: Rscript dev/backup.R")
for (f in sort(list.files("app/R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
DB   <- if (length(args) >= 1) args[1] else
  (if (nzchar(Sys.getenv("SURVEY_SQLITE"))) Sys.getenv("SURVEY_SQLITE")
   else file.path(CFG$store_path, "responses.sqlite"))
DIR  <- if (length(args) >= 2) args[2] else file.path(dirname(DB), "backups")
KEEP <- if (length(args) >= 3) as.integer(args[3]) else 48

if (!file.exists(DB)) stop("No database at ", DB)

st <- store_sqlite(DB)
on.exit(try(st$disconnect(), silent = TRUE), add = TRUE)

# Refuse to back up a database that is already damaged — otherwise the good
# snapshots age out of the retention window and get replaced by broken ones.
chk <- st$integrity()
if (!identical(chk, "ok")) {
  cat("INTEGRITY FAILURE:", chk, "\n")
  cat("Not overwriting backups. Restore from the most recent good snapshot in", DIR, "\n")
  quit(status = 1)
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
dest  <- file.path(DIR, sprintf("responses_%s.sqlite", stamp))
st$snapshot(dest)

# Verify the snapshot opens and matches before trusting it.
src_n <- vapply(st$read(), nrow, integer(1))
b  <- DBI::dbConnect(RSQLite::SQLite(), dest)
bc <- DBI::dbGetQuery(b, "pragma integrity_check")[[1]][1]
bn <- vapply(STORE_TABLES, function(t)
  as.integer(DBI::dbGetQuery(b, paste("select count(*) n from", t))$n), integer(1))
DBI::dbDisconnect(b)

if (!identical(bc, "ok")) { unlink(dest); stop("Snapshot failed its integrity check; removed.") }

cat(sprintf("%s  %s  %.2f MB\n", stamp, basename(dest), file.size(dest) / 1e6))
cat("  rows:", paste(names(bn), bn, sep = "=", collapse = "  "), "\n")
# The snapshot is a point in time, so it can legitimately hold slightly fewer
# rows than the live database if a respondent submitted mid-backup.
if (any(bn > src_n)) warning("Snapshot has more rows than the source; investigate.")

# Retention.
old <- sort(list.files(DIR, pattern = "^responses_.*[.]sqlite$", full.names = TRUE), decreasing = TRUE)
if (length(old) > KEEP) {
  drop <- old[(KEEP + 1):length(old)]
  unlink(drop)
  cat("  pruned", length(drop), "older snapshots (keeping", KEEP, ")\n")
}
cat("  ", length(list.files(DIR, pattern = "[.]sqlite$")), "snapshots in", DIR, "\n")
