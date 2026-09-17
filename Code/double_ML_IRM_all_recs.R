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

## run DoubleML sequentially: ordered progress output + reproducibility
future::plan("sequential")

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


## Double ML Wrapper Function --------------------------------------------------
## fits the Double ML IRM model for a given SSC recommendation & Sepsis Outcome

run_dml <- function(rec_dt,
                    treat_col,
                    outcome_col, ## sepsis outcome = c("mort_28d", "icu_mortality", "aki_new")
                    tier, ## cohort tier = c("full", "well_observed", "well_observed_strict")
                    W) {
  
  ## Assemble Analysis Frame (A, Y, W)
  ## treatment: eligible stays only (exclude NA entries, since 
  ## they are outside the recommendation's population)
  DT_dml_raw <- rec_dt[, .(subject_id,
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
  
  ## prevents one failing iteration to abort the whole implementation
  if (tier == "full") {

    DT_dml_raw <- DT_dml_raw
  }
  
  if (tier == "well_observed") {
    
    DT_dml_raw <- DT_dml_raw[well_observed == TRUE]
  }
  
  if (tier == "well_observed_strict") {
    
    DT_dml_raw <- DT_dml_raw[well_observed_strict == TRUE]
  }
  
  ## guard filter
  if (nrow(DT_dml_raw) < 100 || 
      DT_dml_raw[, uniqueN(A)] < 2) {
    
    return(data.table(treat_col, 
                      outcome = outcome_col, 
                      tier,
                      n = nrow(DT_dml_raw), 
                      n_treated = NA_integer_, 
                      p0 = NA_real_,
                      ate = NA_real_, 
                      se = NA_real_, 
                      ci_lo = NA_real_,
                      ci_hi = NA_real_,
                      pval = NA_real_, 
                      ps_p01 = NA_real_, 
                      ps_p99 = NA_real_))
  }
  
  
  ## Impute missing confounders (median + missingness indicator)
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
  
  
  ## Fit Double ML Interactive Regression Model (IRM)
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
  
  ## Confidence Interval
  ci <- dml_irm_mod$confint()
  
  ## Propensity Scores
  ps <- dml_irm_mod$predictions$ml_m[, 1, 1] 
  
  ## Propensity Score quantiles
  ps_q <- quantile(ps, c(0.01, 
                         0.99), 
                   na.rm = TRUE)
  
  ## Build one-row results data table
  data.table(treat_col = treat_col,
             outcome = outcome_col,
             tier = tier,
             n = nrow(DT_dml),
             n_treated = sum(DT_dml$A),
             p0 = DT_dml[A == 0, mean(Y)], ## baseline risk, needed for E-value later
             ate = unname(dml_irm_mod$coef),
             se = unname(dml_irm_mod$se),
             ci_lo = ci[1, 1],
             ci_hi = ci[1, 2],
             pval = unname(dml_irm_mod$pval),
             ps_p01 = ps_q[[1]],
             ps_p99 = ps_q[[2]])
}


## Test Case for the run_dml() function
rec_16_res <- run_dml(rec_dt = rec_16,
        treat_col = "rec_16_antimicrobial_within_1h",
        outcome_col = "mort_28d",
        tier = "full",
        W = W)


## Building SSC Recomenddation x Outcome x Observability Tier Grid -------------

## SSC Recommendations
ssc_recs <- data.table(rec_dt = c("rec_7",
                                  "rec_8",
                                  "rec_10",
                                  "rec_12",
                                  "rec_13",
                                  "rec_15",
                                  "rec_16",
                                  "rec_17",
                                  "rec_42"),
                       treat_col = c("rec_7_culture_before_antimicrobial",
                                     "rec_8_lactate_measured_within_1h",
                                     "rec_10_crystalloid_within_3h",
                                     "rec_12_vaso_without_central_delay",
                                     "rec_13_map_target_65",
                                     "rec_15_icu_6h",
                                     "rec_16_antimicrobial_within_1h",
                                     "rec_17_antimicrobial_within_1h",
                                     "rec_42_bp_monitoring"))

## Sepsis Outcomes
outcomes <- c("mort_28d",
              "icu_mortality",
              "aki_new")

## Observability Tiers
tiers <- c("full",
           "well_observed",
           "well_observed_strict")

## Full Grid
grid <- CJ(rec_row = seq_len(nrow(ssc_recs)),
           outcome = outcomes,
           tier = tiers,
           sorted = FALSE)

n_grid <- nrow(grid)

cli::cli_progress_bar("Fitting DML grid", total = n_grid)


## Apply Double ML IRM Model to full grid --------------------------------------
## Run all combinations and row-bind the one-row results
run_grid <- function() {
  
  ## capture the bar id
  pb <- cli::cli_progress_bar("Fitting DML grid", total = n_grid)
  
  dml_irm_results <- rbindlist(
    lapply(seq_len(nrow(grid)), function(i) {
      
      r <- ssc_recs[grid$rec_row[i]]
      
      ## progress update
      message(sprintf("[%d/%d] %s | %s | %s",
                      i,
                      n_grid,
                      r$treat_col,
                      grid$outcome[i],
                      grid$tier[i]))
      
      ## pass id = pb so it finds the bar from inside the nested function
      cli::cli_progress_update(id = pb)
      
      tryCatch(
        run_dml(rec = get(r$rec_dt),
                treat_col = r$treat_col,
                outcome_col = grid$outcome[i],
                tier = grid$tier[i],
                W = W),
        
        error = function(e) {
          
          ## report the failure but keep going
          message(" -> ERROR: ", conditionMessage(e))
          
          data.table(treat_col = r$treat_col,
                     outcome = grid$outcome[i],
                     tier = grid$tier[i],
                     n = NA_integer_,
                     n_treated = NA_integer_,
                     p0 = NA_real_,
                     ate = NA_real_,
                     se = NA_real_,
                     ci_lo = NA_real_,
                     ci_hi = NA_real_,
                     pval = NA_real_,
                     ps_p01 = NA_real_,
                     ps_p99 = NA_real_)
          }) 
      }),
    
    fill = TRUE)
  
  ## progress end
  cli::cli_progress_done(id = pb)
  
  dml_irm_results
  
}

if (!file.exists(here::here("Outputs/dml_irm_results.rds"))) {
  
  dml_irm_results <- run_grid()
  
  ## Save results
  saveRDS(dml_irm_results, here::here("Outputs/dml_irm_results.rds"))
  
}

dml_irm_results <- readRDS(here::here("Outputs/dml_irm_results.rds")) %>% 
  setDT()

## check positivity / overlap assumption
dml_irm_results[, ok_overlap := ps_p01 >= 0.01 & ps_p99 <= 0.99]
dml_irm_results[, estimable   := !is.na(ate) & ok_overlap]


res <- dml_irm_results[estimable == TRUE]

## RR and its CI on the risk-ratio scale
res[, rr := (p0 + ate) / p0]
res[, rr_lo := (p0 + ci_lo) / p0]
res[, rr_hi := (p0 + ci_hi) / p0]


## FDR (False Discovery Rate Correction) ---------------------------------------
## FDR within each outcome x tier family
res[, p_adj := p.adjust(pval, 
                        method = "BH"), 
    by = .(outcome, 
           tier)]

res[, significant := p_adj < 0.05]



## E-Value (Unmeasured Confounding Sensitivity) --------------------------------
## row-wise E-value: point estimate + CI-limit-nearest-null
ev_one <- function(rr, lo, hi) {
  
  e <- EValue::evalues.RR(est = rr, 
                          lo = lo,
                          hi = hi)
  
  data.table(evalue_point = e[2, "point"],
             evalue_ci = fcoalesce(e[2, "lower"], 
                                   e[2, "upper"])) ## non-NA bound
}

res[, c("evalue_point", 
        "evalue_ci") :=
      rbindlist(Map(ev_one, 
                    rr, 
                    rr_lo, 
                    rr_hi))]


final <- res[order(outcome, 
                   tier, 
                   pval),
             .(treat_col, 
               outcome, 
               tier, 
               n,
               n_treated,
               ate, 
               ci_lo, 
               ci_hi,
               pval, 
               p_adj, 
               significant,
               ps_p01, 
               ps_p99, 
               evalue_point,
               evalue_ci)]

## the candidate findings: primary tier, FDR-significant, good overlap
final[tier == "well_observed" & 
        significant == TRUE &
        ps_p01 >= 0.05 & 
        ps_p99 <= 0.95]


## the informative rows: real effect + robustness
res[, .(treat_col, 
        outcome, 
        tier, 
        ate,
        ci_lo,
        ci_hi,
        pval,
        evalue_point,
        evalue_ci)] %>% 
  .[evalue_ci > 1] %>% 
  .[order(-evalue_ci)]
