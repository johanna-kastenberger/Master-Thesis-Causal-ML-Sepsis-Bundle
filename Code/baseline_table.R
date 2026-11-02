
source(here::here("Code/global.R"))
source(here::here("Code/data_preparation.R"))

## Load sepsis base cohort data
sepsis_base_cohort <- readRDS(here::here("Data/sepsis_base_cohort_data.rds")) %>% 
  setDT()

baseline_covariates <- readRDS(here::here("Data/baseline_covariates.rds")) %>%
  setDT()

## only the extra columns from baseline_covariates (avoid duplicating
## anchor_age / gender / sofa_score that already live in the cohort frame)
bc_extra <- c("sapsii",
              "charlson_comorbidity_index",
              "myocardial_infarct",
              "congestive_heart_failure",
              "peripheral_vascular_disease",
              "cerebrovascular_disease",
              "chronic_pulmonary_disease",
              "mild_liver_disease",
              "severe_liver_disease",
              "diabetes_without_cc",
              "diabetes_with_cc",
              "renal_disease",
              "malignant_cancer",
              "metastatic_solid_tumor",
              "creatinine",
              "wbc",
              "platelets",
              "hemoglobin",
              "bun",
              "lactate")

## Assemble one row per stay with every Table 1 variable -----------------------
DT <- merge(sepsis_base_cohort,
            baseline_covariates[, c("subject_id",
                                    "hadm_id",
                                    "stay_id",
                                    bc_extra), 
                                with = FALSE],
            by = c("subject_id",
                   "hadm_id",
                   "stay_id"),
            all.x = TRUE,
            all.y = FALSE)


## Grouping variable: sepsis without shock vs. septic shock --------------------
DT <- DT %>% 
  copy() %>% 
  .[, shock_group := factor(fifelse(septic_shock_proxy == TRUE,
                                    "Septic shock",
                                    "Sepsis without shock"),
                            levels = c("Sepsis without shock",
                                       "Septic shock"))]

## Derived variables -----------------------------------------------------------
DT <- DT %>% 
  copy() %>% 
  .[, los_icu_days := as.numeric(lubridate::time_length(icu_outtime -
                                                          icu_intime,
                                                        "days"))] %>% 
  ## sex as 0/1 indicator
  .[, male := as.integer(gender == "M")] %>% 
  ## collapse the Charlson components
  .[, liver_disease := as.integer(mild_liver_disease == 1 |
                                    severe_liver_disease == 1)] %>% 
  .[, cancer := as.integer(malignant_cancer == 1 |
                             metastatic_solid_tumor == 1)] %>% 
  .[, diabetes := as.integer(diabetes_without_cc == 1 |
                               diabetes_with_cc == 1)] %>% 
  ## collapse MIMIC-IV's many race strings into the reference categories
  .[, race_group := fcase(
    
    grepl("WHITE",
          race,
          ignore.case = TRUE),           
    "White",
    
    grepl("BLACK|AFRICAN",
          race,
          ignore.case = TRUE),            
    "African",
    
    grepl("ASIAN",
          race,
          ignore.case = TRUE),           
    "Asian",
    
    grepl("HISPANIC|LATINO",
          race,
          ignore.case = TRUE),          
    "Hispanic",
    
    grepl("NATIVE|INDIAN|ALASKA",
          race,
          ignore.case = TRUE),            
    "Indigenous",
    
    default = "Other / unknown")] %>% 
  .[, race_group := factor(race_group,
                           levels = c("African",
                                      "Asian",
                                      "Hispanic",
                                      "Indigenous",
                                      "White",
                                      "Other / unknown"))]

## Ensure the binary flags are integers (0 / 1) for percentage displays
flag_cols <- c("male",
               "myocardial_infarct",
               "congestive_heart_failure",
               "peripheral_vascular_disease",
               "cerebrovascular_disease",
               "chronic_pulmonary_disease",
               "renal_disease",
               "liver_disease",
               "cancer",
               "diabetes",
               "mort_28d",
               "icu_mortality",
               "aki_new")


DT <- DT %>% 
  copy() %>% 
  .[, (flag_cols) := lapply(.SD, 
                            as.integer),
    .SDcols = flag_cols]


## Helper function that builds labelled section --------------------------------
make_section <- function(vars,
                         labels) {
  
  DT[, c(vars, 
         "shock_group"), 
     with = FALSE] %>%
    tbl_summary(by = shock_group,
                missing = "no",
                type = list(all_dichotomous() ~ "dichotomous",
                            all_continuous() ~ "continuous"),
                ## all continuous variables are reported as median [Q1, Q3]:
                ## ICU cohort variables are mostly skewed, so a single robust
                ## summary is more faithful and keeps one consistent format
                statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]",
                                 all_categorical() ~ "{n} ({p}%)"),
                ## one decimal by default, whole numbers for the large-valued
                ## counts and scores (keeps the median cells narrow so the table
                ## scales down less and the font is larger); later rule wins
                digits = list(all_continuous() ~ 1,
                              all_of(intersect(c("anchor_age",
                                                 "weight_kg_for_fluids",
                                                 "platelets",
                                                 "bun",
                                                 "sofa_score",
                                                 "sapsii",
                                                 "charlson_comorbidity_index"),
                                               vars)) ~ 0),
                label = labels) %>%
    add_overall(last = FALSE,
                col_label = "**Whole cohort**, N = {style_number(N)}")
}


## Build the sections ----------------------------------------------------------
## Demographics section
sec_demo <- make_section(vars = c("male",
                                  "anchor_age",
                                  "race_group",
                                  "weight_kg_for_fluids"),
                         labels = list(male ~ "Male (%)",
                                       anchor_age ~ "Age (years)",
                                       race_group ~ "Ethnicity (%)",
                                       weight_kg_for_fluids ~ "Weight (kg)"))

## Comorbidities section
sec_comorb <- make_section(vars = c("myocardial_infarct",
                                    "congestive_heart_failure",
                                    "peripheral_vascular_disease",
                                    "cerebrovascular_disease",
                                    "chronic_pulmonary_disease",
                                    "renal_disease",
                                    "liver_disease",
                                    "cancer",
                                    "diabetes",
                                    "charlson_comorbidity_index"),
                           labels = list(myocardial_infarct ~ "Myocardial infarction",
                                         congestive_heart_failure ~ "Congestive heart failure",
                                         peripheral_vascular_disease ~ "Peripheral vascular disease",
                                         cerebrovascular_disease ~ "Cerebrovascular disease",
                                         chronic_pulmonary_disease ~ "Chronic pulmonary disease",
                                         renal_disease ~ "Renal disease",
                                         liver_disease ~ "Liver disease",
                                         cancer ~ "Cancer",
                                         diabetes ~ "Diabetes",
                                         charlson_comorbidity_index  ~ "Charlson comorbidity index"))

                
## Laboratory values section
sec_labs <- make_section(vars = c("lactate",
                                  "creatinine",
                                  "wbc",
                                  "platelets",
                                  "hemoglobin",
                                  "bun"),
                         labels = list(lactate ~ "Lactate (mmol/L)",
                                       creatinine ~ "Creatinine (mg/dL)",
                                       wbc ~ "WBC (10^9/L)",
                                       platelets ~ "Platelets (10^9/L)",
                                       hemoglobin ~ "Hemoglobin (g/dL)",
                                       bun ~ "BUN (mg/dL)"))


## Illness severity section
sec_severity <- make_section(vars = c("sofa_score",
                                      "sapsii"),
                             labels = list(sofa_score ~ "SOFA score",
                                           sapsii  ~ "SAPS II score"))


## Outcomes section
sec_outcomes <- make_section(vars = c("mort_28d",
                                      "icu_mortality",
                                      "aki_new",
                                      "los_icu_days"),
                             labels = list(mort_28d ~ "28-day mortality (%)",
                                           icu_mortality ~ "ICU mortality (%)",
                                           aki_new ~ "New acute kidney injury (%)",
                                           los_icu_days ~ "ICU length of stay (days)"))


## Stack the sections with group headers ---------------------------------------
baseline_tbl <- tbl_stack(list(sec_demo,
                               sec_comorb,
                               sec_labs,
                               sec_severity,
                               sec_outcomes),
                          group_header = c("Demographics",
                                           "Comorbidities",
                                           "Laboratory results at admission",
                                           "Severity scores",
                                           "Outcomes")) %>%
  modify_header(label ~ "**Characteristic**") %>%
  ## single-line stat headers (removes the "[c]" \makecell artifact);
  ## style_number adds the thousands separators to the group sizes
  modify_header(all_stat_cols() ~ "**{level}**, N = {style_number(n)}") %>%
  modify_caption("Baseline characteristics of the sepsis cohort")


## Export to LaTeX -------------------------------------------------------------
## escape = FALSE because gtsummary already escaped the body; scale the whole
## table to the text width (collapse_rows is avoided: its \multirow markup
## breaks under scale_down, and the section name already shows once per block)
baseline_tex <- baseline_tbl %>%
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 longtable = FALSE,
                 escape = FALSE,
                 linesep = "") %>%
  kableExtra::kable_styling(latex_options = "scale_down") %>%
  as.character()

## use a flexible float specifier: [!htbp] lets LaTeX place the table here, or at
## the top/bottom of this or the next page, and lets the surrounding text flow up
## to fill the page rather than leaving the large gaps that the locked [H] caused
baseline_tex <- sub("\\\\begin\\{table\\}(\\[[^]]*\\])?",
                    "\\\\begin{table}[!htbp]",
                    baseline_tex)

## inject \label{tab:baseline} right after the caption (gtsummary escapes the
## caption text itself, so the label cannot be placed inside modify_caption);
## reference with \ref{tab:baseline} or \autoref{tab:baseline}
baseline_tex <- sub("(\\\\caption\\{[^}]*\\})",
                    "\\1\\\\label{tab:baseline}",
                    baseline_tex)

## drop gtsummary's auto stat-type footnote and its superscript markers, then
## add a single explanatory note covering every statistic used in the table
baseline_tex <- gsub("\\\\textsuperscript\\{[0-9]+\\}",
                     "",
                     baseline_tex,
                     perl = TRUE)

baseline_tex <- gsub("\\\\multicolumn\\{5\\}\\{l\\}\\{[^\n]*\n",
                     "",
                     baseline_tex,
                     perl = TRUE)

stat_note <- paste0("\\multicolumn{5}{l}{\\footnotesize \\textit{Note.} ",
                    "Continuous variables are summarized as median [Q1, Q3]; ",
                    "categorical variables as n (\\%).}\\\\")

## double the backslashes: in a sub() replacement a single backslash before a
## letter is dropped, which would strip \multicolumn, \footnotesize, \% etc.
baseline_tex <- sub("(\\\\bottomrule\n)",
                    paste0("\\1", gsub("\\\\", "\\\\\\\\", stat_note), "\n"),
                    baseline_tex)

## render the 10^9/L lab units as proper math mode: gtsummary escaped the "^"
## in the label to "\textasciicircum{}", which otherwise prints a literal caret
baseline_tex <- gsub("10\\\\textasciicircum\\{\\}9/L",
                     "$10^{9}$/L",
                     baseline_tex,
                     perl = TRUE)

## add a horizontal rule between the variable categories for clearer separation,
## with vertical space (\addlinespace) before and after each rule so the groups
## are set further apart and the table is taller overall
baseline_tex <- gsub("\n(Comorbidities|Laboratory results at admission|Severity scores|Outcomes) &",
                     "\n\\\\addlinespace[8pt]\n\\\\midrule\n\\\\addlinespace[8pt]\n\\1 &",
                     baseline_tex)

## break the long group label onto two lines so the Group column is narrower
## (\shortstack is core LaTeX, no extra package needed); runs after the rule
## injection above so that still matches the plain "Laboratory results..." label.
## the backslashes are doubled because a sub() replacement halves them
lab_label <- "\\shortstack[l]{Laboratory results\\\\at admission} &"
baseline_tex <- sub("Laboratory results at admission &",
                    gsub("\\\\", "\\\\\\\\", lab_label),
                    baseline_tex)

cat(baseline_tex,
    file = here::here("Outputs/baseline_table.tex"))


                         
  

