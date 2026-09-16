## Source global file
source(here::here("Code/global.R"))

## Source data extraction / preparation & SSC recommendation encoding
source(here::here("Code/define_ssc_variables.R"))

## Set seed to make results reproducible
set.seed(123)

## lgr is the logging package that mlr3 (ML framework for Double ML) uses to emit messages
## the command sets this logger's severity threshold to "warn", meaning the logger only
## emits messages at or above this threshold --> quiets the logging from the ML framework
lgr::get_logger("mlr3")$set_threshold("warn")

key <- c("subject_id",
         "hadm_id",
         "stay_id")

## Table of SSC recommendations
ssc_recommendations <- readRDS(here::here("Data/ssc_recommendations.rds")) %>%
  setDT()

## Load Covariates and Outcome Variables ---------------------------------------
baseline_covariates <- readRDS(here::here("Data/baseline_covariates.rds")) %>%
  setDT()

cohort_outcomes <- readRDS(here::here("Data/sepsis_base_cohort_data.rds")) %>%
  setDT()


## Define Confounder Variables -------------------------------------------------
## Every column is a pre-treatment common cause of adherence and outcome
## By construction, there are no mediators/colliders here 
## (all measured at or before suspected_infection_time)
W_demographics   <- c("anchor_age",
                      # "gender",
                      "gender_male")

W_comorbidities  <- c("charlson_comorbidity_index")

W_severity       <- c("sofa_score",
                      "sapsii",
                      "creatinine",
                      "bun",
                      "wbc",
                      "platelets",
                      "hemoglobin",
                      "bilirubin",
                      "albumin",
                      "inr",
                      "lactate",
                      "ph_blood",
                      "bicarbonate")

W_baseline_renal <- c("aki_baseline_stage")

W <- unique(c(W_demographics,
              W_comorbidities,
              W_severity,
              W_baseline_renal))


## Assemble Analysis Frame (A, Y, W) -------------------------------------------
treat_col   <- "rec_16_antimicrobial_within_1h" ## treatment A
outcome_col <- "mort_28d" ## outcome  Y

## treatment: eligible stays only (exclude NA entries, since 
## they are outside the recommendation's population)
DT_dml_raw <- rec_16[, .(subject_id,
                      hadm_id,
                      stay_id,
                      A = get(treat_col))] %>%
  .[!is.na(A)] %>%
  ## outcome
  merge(.,
        cohort_outcomes[, .(subject_id,
                            hadm_id,
                            stay_id,
                            Y = get(outcome_col))],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE) %>%
  ## confounders (plus observability tier flags for later stratification)
  merge(.,
        baseline_covariates[, c("subject_id",
                                "hadm_id",
                                "stay_id",
                                "gender",
                                setdiff(W, "gender_male"),
                                "well_observed",
                                "well_observed_strict"),
                            with = FALSE],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE)

## encode treatment, outcome & gender as numeric (0 / 1)
DT_dml_raw[, A := as.integer(A)]
DT_dml_raw[, Y := as.integer(Y)]
DT_dml_raw[, gender_male := as.integer(gender == "M")]


## Impute missing confounders (median + missingness indicator) -----------------
## DoubleML needs a complete design matrix; median imputation plus an explicit
## missingness flag keeps "was this measured?" as information rather than
## discarding incomplete stays
for (col in W) {

  miss <- is.na(DT_dml_raw[[col]])

  if (any(miss)) {

    ## add missingness indicator (1 = missing value / imputed, 0 = observed)
    DT_dml_raw[, (paste0(col, "_miss")) := as.integer(miss)]
    ## replace the missing entries in this column with its (observed) median
    DT_dml_raw[miss, (col) := median(DT_dml_raw[[col]], na.rm = TRUE)]
  }
}

## Missingness indicator columns
miss_inds <- grep("_miss$",
                  names(DT_dml_raw),
                  value = TRUE)

## All columns combined (fully observed, imputed & imputation indicator columns)
## This allows missingness to act as an additional confounder (the causal purpose)
## Also, median imputation gives every missing patient the same value. Without the flag, 
## the model cannot tell an imputed value from a genuinely measured one, so it would 
## treat imputed as real data and be biased by them. The _miss flag lets each forest 
## handle the imputed subgroup separately —-> it can split on e.g. creatinine_miss = 1 
## and adjust accordingly
X_cols <- c(W, miss_inds)


## Fit Double ML Interactive Regression Model (IRM) ----------------------------
## drop any residual incomplete rows, then build the Double ML data backend
DT_dml <- DT_dml_raw[complete.cases(DT_dml_raw[, c("Y", "A", X_cols), with = FALSE])]

## builds the data container that Double ML operates on
## no fitting yet, just labels which columns play which causal role 
DT_dml_data <- DoubleMLData$new(data  = as.data.table(DT_dml),
                             y_col = "Y",
                             d_cols = "A",
                             x_cols = X_cols)

## Select nuisance learners: random forest for both the outcome model E[Y|A,X] 
## (predicts a probability for the binary outcome) and the propensity E[A|X]
## regression learner for the outcome and classification learner for the propensity
dml_nuisance_g <- lrn("regr.ranger",
            num.trees = 500,
            min.node.size = 5)

dml_nuisance_m <- lrn("classif.ranger",
            num.trees = 500,
            min.node.size = 5,
            ## predict_type = "prob" returns the 
            ## propensity score, not a hard 0 / 1 label
            predict_type = "prob")


## Construct IRM model
dml_irm_mod <- DoubleMLIRM$new(DT_dml_data,
                               ## outcome learner
                               ml_g = dml_nuisance_g,
                               ## propensity learner
                               ml_m = dml_nuisance_m,
                               ## sets up 5-fold cross-fitting
                               ## removes overfitting bias
                               n_folds = 5,
                               ## trims propensity scores to [0.01, 0.99]
                               ## so no single near-deterministic patient
                               ## destabilizes the estimate
                               trimming_threshold = 0.01)
                       

## runs the whole Double ML algorithm and stores results inside dml_irm_mod
dml_irm_mod$fit(store_predictions = TRUE)

## Propensity Scores
## Every patient receives an estimated propensity score (P(A = 1|X)), which is the
## model's estimate of how likely that patient was to receive the treatment (A = 1)
## based on their confounders. The positivy (overlap) assumption states that every 
## patient must have a real chance of both treatment states (0 < P(A = 1|X) < 1)
## Quantifies how "hard" the propenstiy score trimming in the fitted model is "working"
dml_irm_ps <- dml_irm_mod$predictions$ml_m[, 1, 1]

## Check distribution
quantile(dml_irm_ps,
         c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1),
         na.rm = TRUE)


## Summary of the Double ML IRM model
## contains the ATE, SE, CI and p-value
dml_irm_mod$summary()
