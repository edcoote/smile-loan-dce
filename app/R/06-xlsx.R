# 06-xlsx.R -----------------------------------------------------------------
# Minimal multi-sheet .xlsx writer, base R only.
#
# Rationale: the export is the handover artefact and it must not fail on a
# machine where openxlsx or writexl happen not to be installed. If writexl IS
# available it is used (faster, better typed); otherwise this fallback writes a
# valid workbook using inline strings and utils::zip. Numeric columns are
# written as numbers, everything else as text, and dates as ISO strings.
# ---------------------------------------------------------------------------

.xl_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  # strip control characters Excel refuses
  gsub("[\x01-\x08\x0B\x0C\x0E-\x1F]", "", x)
}

.xl_col <- function(n) {
  s <- ""
  while (n > 0) { r <- (n - 1) %% 26; s <- paste0(LETTERS[r + 1], s); n <- (n - 1) %/% 26 }
  s
}

.xl_sheet_xml <- function(df) {
  nm <- names(df)
  rows <- character(nrow(df) + 1)
  hdr <- vapply(seq_along(nm), function(j)
    sprintf('<c r="%s1" t="inlineStr"><is><t xml:space="preserve">%s</t></is></c>',
            .xl_col(j), .xl_esc(nm[j])), character(1))
  rows[1] <- paste0('<row r="1">', paste(hdr, collapse = ""), "</row>")
  isnum <- vapply(df, function(v) is.numeric(v) && !inherits(v, "Date"), logical(1))
  chr <- lapply(df, function(v) {
    if (inherits(v, c("Date", "POSIXt"))) format(v) else as.character(v)
  })
  for (i in seq_len(nrow(df))) {
    cells <- character(length(nm))
    for (j in seq_along(nm)) {
      v <- chr[[j]][i]
      ref <- paste0(.xl_col(j), i + 1)
      if (is.na(v) || !nzchar(v)) { cells[j] <- ""; next }
      cells[j] <- if (isnum[j])
        sprintf('<c r="%s"><v>%s</v></c>', ref, v)
      else
        sprintf('<c r="%s" t="inlineStr"><is><t xml:space="preserve">%s</t></is></c>', ref, .xl_esc(v))
    }
    rows[i + 1] <- paste0(sprintf('<row r="%d">', i + 1), paste(cells, collapse = ""), "</row>")
  }
  paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
         '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
         "<sheetData>", paste(rows, collapse = ""), "</sheetData></worksheet>")
}

.xl_safe_name <- function(x) {
  x <- gsub("[\\\\/?*\\[\\]:]", "_", x)
  substr(x, 1, 31)
}

write_xlsx_base <- function(sheets, file) {
  stopifnot(is.list(sheets), length(sheets) > 0, !is.null(names(sheets)))
  nms <- .xl_safe_name(names(sheets))
  tmp <- file.path(tempdir(), paste0("xlsx_", as.integer(runif(1, 1e6, 9e6))))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "xl", "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "xl", "worksheets"), recursive = TRUE, showWarnings = FALSE)

  n <- length(sheets)
  ov <- paste0(vapply(seq_len(n), function(i)
    sprintf('<Override PartName="/xl/worksheets/sheet%d.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>', i),
    character(1)), collapse = "")
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
    '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
    ov, "</Types>"), file.path(tmp, "[Content_Types].xml"), useBytes = TRUE)

  writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
    "</Relationships>"), file.path(tmp, "_rels", ".rels"), useBytes = TRUE)

  sh <- paste0(vapply(seq_len(n), function(i)
    sprintf('<sheet name="%s" sheetId="%d" r:id="rId%d"/>', .xl_esc(nms[i]), i, i), character(1)), collapse = "")
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    "<sheets>", sh, "</sheets></workbook>"), file.path(tmp, "xl", "workbook.xml"), useBytes = TRUE)

  rel <- paste0(vapply(seq_len(n), function(i)
    sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet%d.xml"/>', i, i),
    character(1)), collapse = "")
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">', rel,
    sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>', n + 1),
    "</Relationships>"), file.path(tmp, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)

  writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    '<fonts count="1"><font><sz val="11"/><name val="Calibri"/></font></fonts>',
    '<fills count="1"><fill><patternFill patternType="none"/></fill></fills>',
    '<borders count="1"><border/></borders>',
    '<cellStyleXfs count="1"><xf/></cellStyleXfs>',
    '<cellXfs count="1"><xf xfId="0"/></cellXfs>',
    '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
    '</styleSheet>'),
    file.path(tmp, "xl", "styles.xml"), useBytes = TRUE)

  for (i in seq_len(n)) {
    con <- file(file.path(tmp, "xl", "worksheets", sprintf("sheet%d.xml", i)), open = "wb")
    writeBin(charToRaw(enc2utf8(.xl_sheet_xml(as.data.frame(sheets[[i]], stringsAsFactors = FALSE)))), con)
    close(con)
  }

  file <- file.path(normalizePath(dirname(file), mustWork = TRUE), basename(file))
  if (file.exists(file)) unlink(file)
  wd <- setwd(tmp); on.exit(setwd(wd), add = TRUE)
  utils::zip(file, files = c("[Content_Types].xml", "_rels", "xl"), flags = "-r9Xq")
  invisible(file)
}

# Public entry point: prefer writexl when present, fall back to the base writer.
write_workbook <- function(sheets, file) {
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(sheets, path = file)
    return(invisible(file))
  }
  write_xlsx_base(sheets, file)
}
