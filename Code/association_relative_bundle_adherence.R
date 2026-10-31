
## Association of relative bundle adherence with mortality ---------------------

source(here::here("Code/global.R"))

dt   <- setDT(readRDS(here::here("Data/DT_ssc_encoded.rds"))) %>% 
  setDT()

base <- setDT(readRDS(here::here("Data/baseline_covariates.rds"))) %>% 
  setDT()

coh  <- setDT(readRDS(here::here("Data/sepsis_base_cohort_data.rds"))) %>% 
  setDT()

id   <- c("subject_id", "hadm_id", "stay_id")

bundle <- c("rec_44_balanced_over_saline_bolus_causal",
            "rec_92_insulin_started_at_glucose_180_causal",
            "rec_89_active_fluid_removal_7d_causal",
            "rec_91_early_enteral_nutrition_causal")

b <- dt[, c(id, 
            bundle), 
        with = FALSE] %>% 
  .[, n_eligible := rowSums(!is.na(.SD)),        
    .SDcols = bundle] %>% 
  .[, n_adhered := rowSums(.SD == TRUE, 
                           na.rm = TRUE), 
    .SDcols = bundle] %>% 
  .[, completeness := fifelse(n_eligible > 0, 
                              n_adhered / n_eligible, 
                              NA_real_)] %>% 
## must be eligible for >= 1 component
  .[n_eligible >= 1]                      

W <- c("anchor_age",
       "charlson_comorbidity_index",
       "sofa_score",
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
       "bicarbonate",
       "aki_baseline_stage")

d <- b %>% 
  copy() %>% 
  merge(.,   
        coh[, c(id, 
                "mort_28d"),
            with = FALSE], 
        by = id, 
        all.x = TRUE) %>% 
  merge(.,   
        base[, c(id, 
                 "gender", 
                 W), 
             with = FALSE], 
        by = id, 
        all.x = TRUE) %>% 
  .[, gender_male := as.integer(gender == "M")]

## crude gradient: mortality by number of bundle components received,
## among patients eligible for at least two (so the counts are comparable)
print(d[n_eligible >= 2,
        .(n = .N, 
          mortality = round(mean(mort_28d, 
                                 na.rm = TRUE), 
                            3)),
        by = n_adhered] %>% 
        .[order(n_adhered)])

## adjusted association of completeness with 28-day mortality
f <- as.formula(paste("mort_28d ~ completeness + gender_male +",
                      paste(W, 
                            collapse = " + ")))

m <- glm(f, 
         data = d, 
         family = binomial)

print(round(exp(cbind(OR = coef(m), 
                      confint(m)))["completeness", ], 3))

## sensitivity: early components only (44, 92), not subject to immortal time
early <- bundle[1:2]

be <- dt[, c(id, 
             early), 
         with = FALSE] %>% 
  .[, n_eligible := rowSums(!is.na(.SD)), 
   .SDcols = early] %>% 
  .[, n_adhered := rowSums(.SD == TRUE, 
                           na.rm = TRUE), 
    .SDcols = early] %>% 
  .[, completeness := fifelse(n_eligible > 0, 
                              n_adhered / n_eligible, 
                              NA_real_)] %>% 
  .[n_eligible >= 1] %>% 
  merge(., coh[, c(id, 
                    "mort_28d"), 
                with = FALSE], 
         by = id, 
         all.x = TRUE) %>% 
  merge(., base[, c(id, 
                    "gender",
                    W), 
                with = FALSE], 
        by = id, 
        all.x = TRUE) %>% 
  .[, gender_male := as.integer(gender == "M")]

me <- glm(f,
          data = be, 
          family = binomial)

print(round(exp(cbind(OR = coef(me), confint(me)))["completeness", ], 3))


## Latex results tables --------------------------------------------------------
library(data.table)

### adjusted completeness-mortality association --------------------------------
or_row <- function(model, 
                   data, 
                   label, 
                   comps) {
  
  est <- exp(coef(model)["completeness"])
  
  ci  <- exp(suppressMessages(confint(model))["completeness", ])
  
  sprintf("%s & %s & %s & %.2f & [%.2f, %.2f] \\\\",
          label, 
          comps,
          formatC(nrow(data),
                  big.mark = ",", 
                  format = "d"),
          est, 
          ci[1], 
          ci[2])
}

assoc <- c(
  "\\begin{table}[ht]",
  "  \\centering",
  paste0("  \\caption{Adjusted association between bundle completeness and 28-day ",
         "mortality. The odds ratio is for a patient moving from receiving none to ",
         "all of the bundle components they were eligible for, adjusted for ",
         "baseline covariates.}"),
  "  \\label{tab:completeness-or}", 
  "  \\begin{tabular}{llrrc}", 
  "    \\toprule",
  "    Score & Components & $n$ & OR & 95\\% CI \\\\",
  "    \\midrule",
  paste0("    ", 
         or_row(m,  
                d,  
                "Full bundle",     
                "44, 92, 89, 91")),
  paste0("    ", 
         or_row(me,
                be,
                "Early components",
                "44, 92")),
  "    \\bottomrule",
  "  \\end{tabular}",
  "\\end{table}")

writeLines(assoc, here::here("Outputs/completeness_or_table.tex"))

### crude mortality by number of components received ---------------------------
grad <- d[n_eligible >= 2, .(patients  = .N,
                             mortality = 100 * mean(mort_28d, 
                                                    na.rm = TRUE)),
          by = n_adhered] %>% 
  .[order(n_adhered)]

grad_rows <- grad[, sprintf("    %d & %s & %.1f \\\\",
                            n_adhered,
                            formatC(patients, 
                                    big.mark = ",", 
                                    format = "d"),
                            mortality)]
tab2 <- c(
  "\\begin{table}[ht]", 
  "  \\centering",
  paste0("  \\caption{Crude 28-day mortality by the number of bundle components ",
         "received, among patients eligible for at least two. Mortality falls as ",
         "more of the bundle is delivered, the descriptive gradient that the ",
         "adjusted odds ratio in Table~\\ref{tab:completeness-or} summarizes.}"),
  "  \\label{tab:completeness-gradient}", 
  "  \\begin{tabular}{ccc}",
  "    \\toprule",
  "    Components received & Patients & 28-day mortality (\\%) \\\\",
  "    \\midrule",
  grad_rows,
  "    \\bottomrule", 
  "  \\end{tabular}",
  "\\end{table}")

writeLines(tab2, here::here("Outputs/completeness_gradient_table.tex"))
