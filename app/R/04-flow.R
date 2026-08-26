# 04-flow.R -----------------------------------------------------------------
# The flow is a plain list of page descriptors built once, immediately after
# the screener, from that respondent's answers and the run configuration. The
# server holds a pointer into it. Nothing downstream branches on state, so the
# routing is inspectable, testable headlessly (see dev/simulate.R), and the
# exact page sequence a respondent saw is recoverable from the stored record.
# ---------------------------------------------------------------------------

# Pages that exist before routing is known.
flow_preamble <- function() list(
  list(type = "landing",  module = "landing"),
  list(type = "screener", module = "screener")
)

# Build the remainder once the screener is answered.
flow_build <- function(answers, cfg = CFG) {
  route <- STAGE_ROUTE[[answers$scr_stage]]
  if (is.null(route)) stop("Unroutable stage response: ", answers$scr_stage)
  path <- route$path; key <- route$key

  fl <- list()
  extra <- PATH_EXTRA[[key]]
  if (!is.null(extra)) fl[[length(fl) + 1]] <- list(type = "single", module = "path_extra", item = extra)

  fl[[length(fl) + 1]] <- list(type = "single", module = "core", item = list(
    id = AOHS$id, type = "radio", label = AOHS$label, options = AOHS$options))
  fl[[length(fl) + 1]] <- list(type = "battery", module = "core_mdas",
    title = "How you feel about dental treatment",
    help = "There are no right answers \u2014 just how you would feel.",
    items = MDAS$items, options = MDAS$options)
  fl[[length(fl) + 1]] <- list(type = "battery_mixed", module = "core_enabling",
    title = "A few practical questions", items = ENABLING)

  # Section 3. Module order randomised 50/50 at entry, stored as `arm` and
  # entered in the choice model as an interaction with the price coefficient.
  dce_first <- if (isTRUE(cfg$randomise_modules)) sample(c(TRUE, FALSE), 1) else FALSE
  arm <- if (dce_first) "dce_first" else "bws_first"

  modules <- c("bws", "dce")
  if (isTRUE(cfg$split_sample)) modules <- sample(modules, 1)   # each respondent gets one
  if (dce_first) modules <- rev(modules)

  bws_pages <- function() {
    sets <- bws_sets_for(cfg)
    c(list(list(type = "bws_intro", module = "bws", stem = PATH_STEM[[path]], n = length(sets))),
      lapply(sets, function(s) list(type = "bws", module = "bws", set = s,
                                    stem = PATH_STEM[[path]], n = length(sets))))
  }
  dce_pages <- function() {
    tasks <- dce_tasks_for(cfg)
    c(list(list(type = "dce_intro", module = "dce", n = nrow(tasks))),
      lapply(seq_len(nrow(tasks)), function(i)
        list(type = "dce", module = "dce", row = tasks[i, ], n = nrow(tasks))))
  }
  for (m in modules) fl <- c(fl, if (m == "bws") bws_pages() else dce_pages())

  fl[[length(fl) + 1]] <- list(type = "demographics", module = "demographics")
  fl[[length(fl) + 1]] <- list(type = "thanks", module = "thanks")

  attr(fl, "path") <- path
  attr(fl, "stage_key") <- key
  attr(fl, "arm") <- arm
  attr(fl, "modules_served") <- paste(modules, collapse = "+")
  fl
}

# Progress is computed over answerable pages only, so intros and the landing
# page do not make the bar jump.
flow_progress <- function(flow, i) {
  answerable <- vapply(flow, function(p)
    !p$type %in% c("landing", "thanks", "bws_intro", "dce_intro"), logical(1))
  tot <- sum(answerable)
  done <- sum(answerable[seq_len(min(i, length(flow)))])
  if (tot == 0) 0 else max(0, min(1, (done - 1) / tot))
}

screen_out_reason <- function(answers) {
  for (nm in names(SCREEN_OUT_RULES)) {
    r <- SCREEN_OUT_RULES[[nm]](answers)
    if (!is.null(r)) return(r)
  }
  NULL
}
