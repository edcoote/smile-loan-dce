# analysis/02-apollo-dce.R --------------------------------------------------
# Apollo estimation of the Smile Loan DCE.
#
# THREE MODELS, IN ORDER
#   M1  Stage 1 only, MNL. The trade-off between repayment rate and upfront
#       fee, conditional on choosing one of the two loans.
#   M2  Stage 2 only, MNL over three outcomes. The participation margin:
#       take the loan / pay privately / do not proceed.
#   M3  The two jointly, sharing the attribute coefficients. This is the model
#       that separates substitution from induced demand, because it estimates
#       the participation decision as a function of the attributes rather than
#       inferring it afterwards from marginal frequencies.
#
# WHY JOINT ESTIMATION MATTERS HERE
# Running M1 and M2 separately treats the participation decision as independent
# of the trade-off, which it is not: someone who dislikes both loans on offer
# is more likely to decline whichever they nominally preferred. M3 shares the
# rate and fee coefficients across both stages, so the same preference
# parameters drive both margins and the two non-loan outcomes stay separately
# identified. Collapsing "pay privately" and "do not proceed" into a single
# opt-out would make substitution and induced demand indistinguishable.
#
#   Rscript analysis/02-apollo-dce.R [datadir]
# ---------------------------------------------------------------------------

suppressMessages(library(apollo))

args <- commandArgs(trailingOnly = TRUE)
DATA <- if (length(args) >= 1) args[1] else "analysis/data"
OUT  <- "analysis/output"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

db <- utils::read.csv(file.path(DATA, "dce_apollo_wide.csv"), stringsAsFactors = FALSE)

# The dominance task is excluded from ESTIMATION but was retained in the file
# and reported in preparation. It is a data-quality indicator, not a trade-off
# observation: it carries no information about preferences because one loan is
# better on every attribute.
database <- db[db$dominance == 0 & !is.na(db$choice), ]
database <- database[order(database$ID, database$task), ]

# Fee enters in £100s so its coefficient is on a comparable scale to rate;
# WTP below is converted back to pounds.
database$fee_A_100 <- database$fee_A / 100
database$fee_B_100 <- database$fee_B / 100
database$chosen_fee_100 <- database$chosen_fee / 100

cat(sprintf("Estimation sample: %d tasks, %d respondents\n",
            nrow(database), length(unique(database$ID))))

# ---------------------------------------------------------------------------
# M1 — stage 1, MNL
# ---------------------------------------------------------------------------
apollo_initialise()
apollo_control <- list(modelName = "M1_stage1_mnl", modelDescr = "Loan A vs Loan B",
                       indivID = "ID", outputDirectory = OUT, nCores = 1)

apollo_beta <- c(asc_B = 0, b_rate = 0, b_fee = 0)
apollo_fixed <- c("asc_B")   # A is the reference; ASC captures residual left/right effects

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs); on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list(); V <- list()
  V[["A"]] <- b_rate * rate_A + b_fee * fee_A_100
  V[["B"]] <- asc_B + b_rate * rate_B + b_fee * fee_B_100
  mnl_settings <- list(alternatives = c(A = 1, B = 2),
                       avail = list(A = av_A, B = av_B),
                       choiceVar = choice, utilities = V)
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_inputs <- apollo_validateInputs()
M1 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                      estimate_settings = list(silent = TRUE))
cat("\n===== M1: stage 1 =====\n"); apollo_modelOutput(M1)

# Willingness to pay: fee, in pounds, equivalent to one percentage point of
# repayment rate. Delta method for the ratio.
wtp <- apollo_deltaMethod(M1, list(operation = "ratio", parName1 = "b_rate", parName2 = "b_fee"))
cat("\nWTP: 1pp of repayment rate is worth this many x100 GBP of upfront fee (above)\n")

# ---------------------------------------------------------------------------
# M3 — joint stage 1 + stage 2
# ---------------------------------------------------------------------------
database2 <- database[!is.na(database$choice2), ]

apollo_initialise()
apollo_control <- list(modelName = "M3_joint_dual_response",
                       modelDescr = "Stage 1 trade-off and stage 2 participation, shared attribute coefficients",
                       indivID = "ID", outputDirectory = OUT, nCores = 1)
database <- database2

apollo_beta <- c(asc_B = 0, b_rate = 0, b_fee = 0,
                 asc_pay_privately = 0, asc_do_not_proceed = 0,
                 log_lambda = 0)
# lambda is the relative error scale of stage 2 against stage 1. It is
# estimated as exp(log_lambda) rather than directly, because the pair
# (beta, lambda) and (-beta, -lambda) give identical STAGE 2 utilities: with
# lambda unconstrained the optimiser can flip both signs, satisfy stage 2, and
# leave stage 1 with a positive price coefficient. That is exactly what happened
# on first run — b_rate came back +0.05 against a true -0.28, and predicted
# uptake rose with the upfront fee. Forcing lambda > 0 removes the reflection
# and the joint model recovers the same parameters as M1.
apollo_fixed <- c("asc_B")

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs); on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()

  # Stage 1: which loan, given a loan is taken.
  V1 <- list()
  V1[["A"]] <- b_rate * rate_A + b_fee * fee_A_100
  V1[["B"]] <- asc_B + b_rate * rate_B + b_fee * fee_B_100
  P[["stage1"]] <- apollo_mnl(list(alternatives = c(A = 1, B = 2),
                                   avail = list(A = av_A, B = av_B),
                                   choiceVar = choice, utilities = V1), functionality)

  # Stage 2: take the nominated loan, pay privately, or do not proceed.
  # The loan's utility depends on the attributes actually nominated, so the two
  # margins are linked through the same preference parameters.
  V2 <- list()
  V2[["take"]]       <- exp(log_lambda) * (b_rate * chosen_rate + b_fee * chosen_fee_100)
  V2[["pay"]]        <- asc_pay_privately
  V2[["notproceed"]] <- asc_do_not_proceed
  P[["stage2"]] <- apollo_mnl(list(alternatives = c(take = 1, pay = 2, notproceed = 3),
                                   choiceVar = choice2, utilities = V2), functionality)

  P <- apollo_combineModels(P, apollo_inputs, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_inputs <- apollo_validateInputs()
M3 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                      estimate_settings = list(silent = TRUE))
cat("\n===== M3: joint dual response =====\n"); apollo_modelOutput(M3)

# ---------------------------------------------------------------------------
# Policy quantities
# ---------------------------------------------------------------------------
# Predicted shares at a given loan offer. Substitution and induced demand are
# READ OFF these predicted probabilities, not backed out of marginal counts —
# which is the whole reason the two non-loan outcomes were never collapsed.
b <- M3$estimate
lam <- exp(b["log_lambda"])
share_at <- function(rate, fee) {
  v <- c(take = lam * (b["b_rate"] * rate + b["b_fee"] * fee / 100),
         pay = b["asc_pay_privately"], notproceed = b["asc_do_not_proceed"])
  e <- exp(v - max(v)); round(100 * e / sum(e), 1)
}
grid <- expand.grid(rate = c(6, 9, 12, 15), fee = c(0, 250, 750))
pred <- cbind(grid, t(mapply(share_at, grid$rate, grid$fee)))
names(pred)[3:5] <- c("take_loan_pct", "pay_privately_pct", "do_not_proceed_pct")
cat("\n===== Predicted outcome shares by loan offer =====\n")
print(pred, row.names = FALSE)
utils::write.csv(pred, file.path(OUT, "predicted_shares.csv"), row.names = FALSE)

cat("\nInduced demand at the most generous offer (6%, no fee):",
    pred$take_loan_pct[pred$rate == 6 & pred$fee == 0], "% take the loan\n")
cat("Of the rest, the split between paying privately and not proceeding is what\n")
cat("distinguishes substitution from genuinely new treatment.\n")
cat("\nOutputs written to", OUT, "\n")
