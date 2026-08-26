# dev/check.R ---------------------------------------------------------------
# Design and pipeline assertions. Run before locking the instrument, and again
# after any edit to the item bank or the designs.
#   Rscript dev/check.R
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
if (!dir.exists("app/R")) stop("Run from the repository root: Rscript dev/check.R")
for (f in sort(list.files("app/R", pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

PASS <- 0L; FAIL <- 0L
ok <- function(label, expr) {
  r <- tryCatch(isTRUE(expr), error = function(e) structure(FALSE, msg = conditionMessage(e)))
  if (isTRUE(r)) { PASS <<- PASS + 1L; cat(sprintf("  ok    %s\n", label)) }
  else { FAIL <<- FAIL + 1L; cat(sprintf("  FAIL  %s %s\n", label, attr(r, "msg") %||% "")) }
}

cat("\n== DCE design ==\n")
cand <- dce_candidates()
d <- dce_design(12)
ok("18 non-dominated candidate pairs", nrow(cand) == 18)
ok("12 choice sets served", nrow(d) == 12)
ok("no set is dominated", all(mapply(function(ar, af, br, bf)
  !((ar <= br && af <= bf) || (br <= ar && bf <= af)),
  d$a_rate, d$a_fee, d$b_rate, d$b_fee)))
ok("all sets distinct", !any(duplicated(paste(d$a_rate, d$a_fee, d$b_rate, d$b_fee))))
ok("effects-coded information matrix is non-singular", attr(d, "det_effects") > 0)
ok("linear model retains >80% relative efficiency", attr(d, "rel_eff_linear") > 0.80)
ok("every rate level appears", length(unique(c(d$a_rate, d$b_rate))) == 4)
ok("every fee level appears", length(unique(c(d$a_fee, d$b_fee))) == 3)
ok("two equal blocks", identical(as.integer(table(d$block)), c(6L, 6L)))
ok("design is deterministic across calls",
   identical(.dce_design_build(12)[, 1:6], .dce_design_build(12)[, 1:6]))

set.seed(1); tk <- dce_tasks_for(CFG)
ok("dominance task appended", sum(tk$dominance) == 1)
ok("dominance task is genuinely dominated", {
  r <- tk[tk$dominance == 1, ]
  (r$a_rate <= r$b_rate && r$a_fee <= r$b_fee) || (r$b_rate <= r$a_rate && r$b_fee <= r$a_fee)
})
ok("side assignment varies", length(unique(tk$side_flipped)) == 2)
ok("13 tasks served with dominance on", nrow(tk) == 13)
ok("dominance scoring identifies the worse loan", {
  r <- tk[tk$dominance == 1, ]
  worse <- if (r$a_rate > r$b_rate) "A" else "B"
  dce_dominance_failed(r, worse) == 1 && dce_dominance_failed(r, setdiff(c("A", "B"), worse)) == 0
})

cat("\n== BWS designs ==\n")
for (v in as.integer(names(BWS_CATALOGUE))) {
  p <- bws_properties(bws_build(v))
  ok(sprintf("v=%2d is a balanced BIBD (b=%d, r=%d, lambda=%d)", v, p$b, p$r[1], p$lambda[1]), p$balanced)
}
ok("item bank matches the configured design",
   nrow(BWS_ITEMS) >= max(bws_build(CFG$bws_items)))
ok("item ids are unique", !any(duplicated(BWS_ITEMS$item_id)))
set.seed(2); sets <- bws_sets_for(CFG)
ok("every item appears equally often across a respondent's sets", {
  tb <- table(unlist(lapply(sets, `[[`, "items")))
  length(unique(as.integer(tb))) == 1
})

cat("\n== Routing ==\n")
ok("every stage option routes", all(SCREENER$stage$options %in% names(STAGE_ROUTE)))
ok("every route key has a PATH_EXTRA entry (possibly NULL)",
   all(vapply(STAGE_ROUTE, `[[`, "", "key") %in% names(PATH_EXTRA)))
for (opt in SCREENER$stage$options) {
  a <- list(scr_age = "55\u201359", scr_uk = "Yes",
            scr_arches = SCREENER$arches$options[3], scr_stage = opt)
  set.seed(3); fl <- flow_build(a, CFG)
  ok(sprintf("flow builds for '%s' (%d pages, path %s)", substr(opt, 1, 34), length(fl), attr(fl, "path")),
     length(fl) > 5 && attr(fl, "path") %in% c("A", "B"))
}
ok("split-sample serves exactly one of BWS/DCE", {
  c2 <- CFG; c2$split_sample <- TRUE
  a <- list(scr_age = "55\u201359", scr_uk = "Yes", scr_arches = SCREENER$arches$options[3],
            scr_stage = SCREENER$stage$options[2])
  set.seed(4); fl <- flow_build(a, c2)
  !grepl("[+]", attr(fl, "modules_served"))
})
ok("screen-out rules fire", {
  identical(screen_out_reason(list(scr_age = "Under 18", scr_uk = "Yes", scr_arches = "x")), "under_18") &&
  identical(screen_out_reason(list(scr_age = "45\u201349", scr_uk = "No", scr_arches = "x")), "non_uk") &&
  is.null(screen_out_reason(list(scr_age = "45\u201349", scr_uk = "Yes",
                                 scr_arches = SCREENER$arches$options[1])))
})

cat("\n== Field controls ==\n")
ok("postcode district accepted", all(vapply(c("CW9", "M1", "SW1A", "cw9 "), valid_postcode_district, logical(1))))
ok("full postcode rejected", !any(vapply(c("CW9 5AB", "CW95AB", "SW1A 1AA", ""), valid_postcode_district, logical(1))))
ok("MDAS scores in range", { a <- setNames(as.list(rep(MDAS$options[5], 5)), vapply(MDAS$items, `[[`, "", "id"))
  mdas_score(a) == 25 })
ok("MDAS phobia cut-off at 19", isTRUE(mdas_flag(19)) && isFALSE(mdas_flag(18)))
ok("MDAS returns NA when incomplete", is.na(mdas_score(list(mdas_1 = MDAS$options[1]))))

cat("\n== Storage and export ==\n")
tmp <- file.path(tempdir(), paste0("chk", as.integer(runif(1, 1e5, 9e5))))
st <- store_init("csv", tmp)
ok("four tables created", st$healthy())
rid <- new_rid()
st$append("items", rows_items(rid, 1L, "core", list(core_srh = "Good"), 9))
st$append("items", rows_items(rid, 2L, "core", list(core_srh = "Fair"), 4))   # Back, changed answer
lat <- store_latest(st$read())
ok("append-only log keeps both revisions", nrow(st$read()$items) == 2)
ok("latest revision wins on read", lat$items$value == "Fair")
set.seed(5); r1 <- dce_tasks_for(CFG)[1, ]
st$append("dce", rows_dce(rid, 1L, r1, "A", "No", "pay_privately", 25000, 30))
ok("dce writes two rows per task", nrow(st$read()$dce) == 2)
ok("exactly one alternative marked chosen", sum(st$read()$dce$chosen) == 1)
ok("non-loan outcome retained separately", all(st$read()$dce$stage2_outcome == "pay_privately"))
set.seed(6); s1 <- bws_sets_for(CFG)[[1]]
ids <- BWS_ITEMS$item_id[s1$items]
st$append("bws", rows_bws(rid, 1L, s1, ids[1], ids[2], 20))
b <- st$read()$bws
ok("bws writes one row per item shown", nrow(b) == length(s1$items))
ok("exactly one best and one worst", sum(b$best) == 1 && sum(b$worst) == 1)
xl <- file.path(tmp, "x.xlsx")
store_export(st, xl)
ok("workbook written", file.exists(xl) && file.size(xl) > 1000)
unlink(tmp, recursive = TRUE)

cat("\n== Burden ==\n")
g <- admin_burden_grid(CFG)
ok("burden grid covers every catalogued design", nrow(g) == length(BWS_CATALOGUE) * 2 * 2)
ok("at least one configuration lands under 13 minutes", any(g$under_13min))
ok("current configuration burden is reported", burden_estimate(CFG) > 0)

cat(sprintf("\n%d passed, %d failed\n", PASS, FAIL))
if (FAIL > 0) quit(status = 1)
