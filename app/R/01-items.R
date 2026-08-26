# 01-items.R ----------------------------------------------------------------
# Item bank. Every item carries a stable item_id — the storage layer and the
# analysis pipeline key on these, so they must not be renamed once fielded.
#
# WORDING STATUS. Two blocks below are PARAPHRASED PLACEHOLDERS, marked
# VERBATIM_REQUIRED. They must be replaced with the source wording before the
# instrument is locked, because a paraphrased validated instrument is not the
# validated instrument and cannot be scored against published norms:
#   * AOHS  — transcribe the single self-rated oral health item from the Adult
#             Oral Health Survey 2023 questionnaire (OGL v3.0, no permission
#             needed, but the stem and the five response labels must match
#             exactly or the national benchmark comparison is not available).
#   * MDAS  — transcribe the five stems and the five response labels from
#             Humphris et al. Free to use with acknowledgement; the >=19 cut-off
#             is only interpretable against the published wording.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Section 1: screener ---------------------------------------------------
SCREENER <- list(
  age = list(
    id = "scr_age", label = "Which age group are you in?",
    options = c("Under 18", "18\u201324", "25\u201334", "35\u201344", "45\u201349",
                "50\u201354", "55\u201359", "60\u201365", "66\u201374", "75 or over")),
  uk = list(
    id = "scr_uk", label = "Do you currently live in the United Kingdom?",
    options = c("Yes", "No")),
  arches = list(
    id = "scr_arches",
    label = "Which best describes your teeth at the moment?",
    options = c("I have lost all the teeth in my upper jaw",
                "I have lost all the teeth in my lower jaw",
                "I have lost all the teeth in both jaws",
                "I still have some of my own teeth, but several are failing",
                "None of these",
                "I would rather not say",
                "I have had full-jaw rehabilitation")),
  stage = list(
    id = "scr_stage",
    label = "Which of these is closest to where you are with full-jaw implant treatment?",
    options = c("I had never considered it before now",
                "I am actively considering it",
                "I had a consultation but did not go ahead",
                "I considered it and decided against it",
                "I have had the treatment at 21D",
                "I have had the treatment somewhere else, or abroad"))
)

# Screen-out rules, evaluated in order. Each returns a reason code or NULL.
SCREEN_OUT_RULES <- list(
  under_18   = function(a) if (identical(a$scr_age, "Under 18")) "under_18" else NULL,
  non_uk     = function(a) if (identical(a$scr_uk, "No")) "non_uk" else NULL,
  not_target = function(a) if (identical(a$scr_arches, "None of these")) "not_target" else NULL
)

# --- Routing ---------------------------------------------------------------
# Item content is identical across paths. Only tense and one path-specific
# question differ, which keeps BWS and DCE responses poolable while preserving
# stage as a stratifier.
STAGE_ROUTE <- list(
  "I had never considered it before now"                       = list(path = "A", key = "never"),
  "I am actively considering it"                               = list(path = "A", key = "considering"),
  "I had a consultation but did not go ahead"                  = list(path = "A", key = "consulted"),
  "I considered it and decided against it"                     = list(path = "A", key = "ruled_out"),
  "I have had the treatment at 21D"                            = list(path = "B", key = "treated_21d"),
  "I have had the treatment somewhere else, or abroad"         = list(path = "B", key = "treated_other")
)

PATH_STEM <- list(
  A = "What is currently holding you back?",
  B = "What delayed you at the time?"
)

# One extra item per stage, per the v2 routing table.
PATH_EXTRA <- list(
  never = NULL,
  considering = NULL,
  consulted = list(id = "px_quote", type = "radio",
    label = "Roughly what were you quoted?",
    options = c("Under \u00A310,000", "\u00A310,000\u2013\u00A314,999",
                "\u00A315,000\u2013\u00A319,999", "\u00A320,000\u2013\u00A324,999",
                "\u00A325,000 or more", "I was not given a figure",
                "I would rather not say")),
  ruled_out = list(id = "px_decisive", type = "radio",
    label = "What was the single most decisive reason you decided against it?",
    options = c("The cost", "Fear of the surgery", "I was not convinced it would work",
                "My health made it unsuitable", "The time it would take",
                "Someone advised me against it", "Something else")),
  treated_21d = list(id = "px_months", type = "radio",
    label = "From first considering treatment, roughly how long was it before you went ahead?",
    options = c("Less than 3 months", "3\u20136 months", "7\u201312 months",
                "1\u20132 years", "More than 2 years")),
  treated_other = list(id = "px_where", type = "radio",
    label = "Where did you have the treatment, and what most influenced that choice?",
    options = c("Elsewhere in the UK \u2014 mainly cost",
                "Elsewhere in the UK \u2014 mainly convenience or reputation",
                "Abroad \u2014 mainly cost",
                "Abroad \u2014 mainly availability or speed",
                "Abroad \u2014 mainly a personal recommendation"))
)

# --- Section 2: core measures ---------------------------------------------
# VERBATIM_REQUIRED — AOHS 2023, single self-rated oral health item, OGL v3.0.
AOHS <- list(
  id = "core_srh",
  label = "Overall, how would you describe the health of your mouth?",
  options = c("Very good", "Good", "Fair", "Bad", "Very bad"),
  source = "Adult Oral Health Survey 2023, single five-point item, OGL v3.0")

# VERBATIM_REQUIRED — MDAS (Humphris). Stems below are short paraphrases so the
# flow can be exercised; replace before locking. Scored 1-5, total 5-25.
MDAS <- list(
  source = "MDAS (Humphris et al.) \u2014 free to use with acknowledgement",
  options = c("Not anxious", "Slightly anxious", "Fairly anxious",
              "Very anxious", "Extremely anxious"),
  items = list(
    list(id = "mdas_1", label = "If you were going to the dentist tomorrow, how would you feel?"),
    list(id = "mdas_2", label = "Waiting in the waiting room for your turn."),
    list(id = "mdas_3", label = "About to have a tooth drilled."),
    list(id = "mdas_4", label = "About to have your teeth scaled and polished."),
    list(id = "mdas_5", label = "About to have a local anaesthetic injection in your gum."))
)

mdas_score <- function(answers) {
  v <- vapply(MDAS$items, function(it) match(answers[[it$id]] %||% NA, MDAS$options), numeric(1))
  if (anyNA(v)) return(NA_integer_)
  as.integer(sum(v))
}
mdas_flag <- function(score) if (is.na(score)) NA else score >= 19   # published phobia cut-off

# Andersen enabling domain — bespoke.
ENABLING <- list(
  list(id = "enab_quote", type = "radio",
       label = "Have you ever been given a price for full-jaw implant treatment?",
       options = c("Yes, in writing", "Yes, verbally", "No", "I am not sure")),
  list(id = "enab_income", type = "radio",
       label = "Roughly what is your household income before tax?",
       options = c("Under \u00A312,570", "\u00A312,570\u2013\u00A320,000",
                   "\u00A320,000\u2013\u00A330,000", "\u00A330,000\u2013\u00A345,000",
                   "\u00A345,000\u2013\u00A360,000", "\u00A360,000 or more",
                   "I would rather not say")),
  list(id = "enab_employment", type = "radio",
       label = "Which best describes your situation?",
       options = c("Working full time", "Working part time", "Self-employed",
                   "Retired", "Not working because of health",
                   "Not working for another reason")),
  list(id = "enab_afford", type = "radio",
       label = "If you needed \u00A31,000 at short notice, how easily could you find it?",
       options = c("Very easily", "Fairly easily", "With some difficulty",
                   "With great difficulty", "I could not"))
)

# --- Section 3: BWS barrier items -----------------------------------------
# PLACEHOLDER. The real list comes from the qualitative phase (8-12
# semi-structured interviews, thematic analysis to saturation). Thirteen items
# are carried here to match the (13,4,1) design; cut to 11, 9 or 7 to move to a
# smaller balanced design (see 03-design-bws.R).
BWS_ITEMS <- data.frame(
  item_id = sprintf("bws_%02d", 1:13),
  label = c(
    "The overall cost of treatment",
    "Not being able to spread the cost over time",
    "Worry about whether the implants would last",
    "Fear of the surgery itself",
    "Not knowing where to go for reliable information",
    "Not wanting to take time off work",
    "Being told my health made me unsuitable",
    "Not being sure it would look natural",
    "Worry about being pushed into a decision",
    "Having managed so far with dentures",
    "Not wanting to travel for appointments",
    "Concern about what happens if something goes wrong",
    "Not feeling the problem was serious enough yet"),
  stringsAsFactors = FALSE)

# --- Section 4: demographics ----------------------------------------------
DEMOGRAPHICS <- list(
  list(id = "dem_sex", type = "radio", label = "How would you describe yourself?",
       options = c("Female", "Male", "In another way", "I would rather not say")),
  list(id = "dem_education", type = "radio",
       label = "What is the highest qualification you have?",
       options = c("No formal qualifications", "GCSEs or equivalent",
                   "A-levels or equivalent", "Degree", "Postgraduate degree",
                   "I would rather not say")),
  list(id = "dem_postcode", type = "text",
       label = "The first part of your postcode only \u2014 for example CW9, not CW9 5AB",
       help = "Used to attach an area deprivation measure. It cannot identify you.")
)

# Postcode DISTRICT validation. Rejects anything with a space or an inward code,
# which is the field control that stops a full postcode being captured.
valid_postcode_district <- function(x) {
  x <- toupper(trimws(x %||% ""))
  if (!nzchar(x)) return(FALSE)
  grepl("^[A-Z]{1,2}[0-9][0-9A-Z]?$", x)
}

FREETEXT <- list(id = "dem_freetext", type = "textarea",
                 label = "Is there anything else that held you back that we have not asked about?")

# --- Consent ---------------------------------------------------------------
CONSENT_POINTS <- c(
  "I have read the participant information and had the chance to think about it.",
  "I understand that taking part is voluntary and that I can stop at any point.",
  "I understand my answers are anonymous and will not affect my care in any way.",
  "I understand the loan terms shown are hypothetical and are not an offer.",
  "I agree to take part."
)
