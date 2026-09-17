## Source global file
source(here::here("Code/global.R"))

## Source data extraction / preparation
source(here::here("Code/data_preparation.R"))

## Table of SSC recommendations
ssc_recommendations <- readRDS(here::here("Data/ssc_recommendations.rds")) %>% 
  setDT()

## Sepsis & Septic Shock Base Cohort
sepsis_base_cohort_data <- readRDS(here::here("Data/sepsis_base_cohort_data.rds")) %>% 
  setDT()

## Define sepsis patient cohort ------------------------------------------------
sepsis_base_cohort_data <- sepsis_base_cohort_data %>% 
  ## patients with sepsis
  .[sepsis3 == TRUE] %>% 
  .[, time_to_sepsis :=
      lubridate::time_length(
          suspected_infection_time -
            icu_intime,
        "hours")] %>% 
  ## Keep only first ICU stay per patient
  .[order(subject_id,
          hadm_id,
          stay_id,
          icu_intime),
    .SD[1],
    by = .(subject_id)] %>% 
  ## sepsis diagnosis within +/-24h to ICU admission
  .[suspected_infection_time >= icu_intime - hours(24) &
      suspected_infection_time <= icu_intime + hours(24)] %>% 
  ## ICU length of stay >= 24h
  .[time_length(icu_outtime -
                  icu_intime,
                unit = "hours") >= 24] %>%
  ## Initial IV fluid resuscitation (crystalloids, colloids, other IV fluids)
  ## should start immediately after sepsis onset, but to account for delayed
  ## documentation, up to 1 hour before recorded sepsis onset is considered.
  ## Initial resuscitation is restricted to the first three hours are sepsis
  ## onset, as per the SSC guidelines and many other sources
  .[, fluid_resuscitation_start := suspected_infection_time -
      hours(1)] %>%
  .[, fluid_resuscitation_end := suspected_infection_time +
      hours(3)]


## Admission Weight Data Cleaning ----------------------------------------------
## Item IDs for admission weight (kg)
admission_weight_kg_itemid <- d_items %>%
  .[label == "Admission Weight (Kg)"]

## Admission weight (kg)
admission_weight_kgs <- all_chartevents %>%
  ## only stays associated with patients in the sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## only entries associated to daily weight (kgs)
  .[itemid == admission_weight_kg_itemid$itemid &
      !is.na(valuenum)] %>% 
  .[, .(stay_id,
        admission_weight_kg_time = charttime,
        admission_weight_kg = valuenum)] %>%
  unique() %>% 
  ## remove obvious outliers
  .[admission_weight_kg >= 30 &
      admission_weight_kg <= 300]

## Check that only one value for patient is included
stopifnot(nrow(admission_weight_kgs[, .N, by = stay_id] %>% .[N > 1]) == 0)

## Item IDs for admission weight (lbs)
admission_weight_lb_itemid <- d_items %>% 
  .[label == "Admission Weight (lbs.)"]

## Admission weight (lb)
admission_weight_lbs <- all_chartevents %>% 
  ## only stays associated with patients in the sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## only entries associated to daily weight (lbs)
  .[itemid == admission_weight_lb_itemid$itemid &
      !is.na(valuenum)] %>% 
  .[, .(stay_id,
        admission_weight_lb_time = charttime,
        admission_weight_lb = valuenum)] %>%
  unique() %>% 
  ## remove obvious outliers
  .[admission_weight_lb >= 30 * 2.20462 &
      admission_weight_lb <= 300 * 2.20462]

## Validate weight entries by comparing admission 
## weight (kg) and admission weight (lb)
admission_weight_merged <- admission_weight_lbs %>% 
  merge(.,
        admission_weight_kgs,
        by = "stay_id",
        all = TRUE) %>% 
  ## calculate the difference between admission weight (kg)
  ## and admissinon weight (lb) in lbs (1kg = 2.20462 lbs)
  .[, `:=` (
    weight_diff_lb = abs((admission_weight_kg * 2.20462) - 
                                   admission_weight_lb),
    weight_time_diff_hr = abs(lubridate::time_length(
      admission_weight_kg_time -
      admission_weight_lb_time,
      "hours")))] 
  
## select the entry of admission weight (lb) closest to 
## admission weight (kg) for each stay_id
## patients with both admission weight(kg) 
## and admission weight(lb) entries
admission_weight_both <- admission_weight_merged %>% 
    .[!is.na(admission_weight_kg) &
        !is.na(admission_weight_lb)] %>% 
    .[order(stay_id,
          weight_diff_lb,
          weight_time_diff_hr),
    .SD[1],
    by = stay_id] %>% 
  .[, weight_valid := fcase(
    !is.na(admission_weight_kg) &
      !is.na(admission_weight_lb) &
      weight_diff_lb <= 2,
    "valid: kg/lb agreement within 2lb",
    
    !is.na(admission_weight_kg) &
      !is.na(admission_weight_lb) &
      weight_diff_lb > 2,
    "kg/lb discrepancy >2 lb"
  )] %>% 
  .[, admission_weight_final_kg := fcase(
    weight_valid == "valid: kg/lb agreement within 2lb",
    admission_weight_kg,
    
    default = NA_real_)] %>% 
  .[, .(stay_id,
        admission_weight_lb_time,
        admission_weight_lb,
        admission_weight_kg_time,
        admission_weight_kg,
        weight_valid,
        admission_weight_final_kg,
        admission_weight_time = admission_weight_kg_time)]

## Check that there is only one row per stay_id
stopifnot(nrow(admission_weight_both[, .N, by = stay_id] %>% .[N > 1]) == 0)

## Patients with only admission weight (kg)
admission_weight_only_kg <- admission_weight_merged %>% 
  .[!is.na(admission_weight_kg) &
      is.na(admission_weight_lb)] %>% 
  .[, `:=` (
    weight_valid = "only one admission weight (kg) measurement available",
    admission_weight_final_kg = admission_weight_kg
  )] %>% 
  .[, .(stay_id,
        admission_weight_lb_time,
        admission_weight_lb,
        admission_weight_kg_time,
        admission_weight_kg,
        weight_valid,
        admission_weight_final_kg,
        admission_weight_time = admission_weight_kg_time)]

## Check that there is only one row per stay_id
stopifnot(nrow(admission_weight_only_kg[, .N, by = stay_id] %>% .[N > 1]) == 0)

## admission weight(lb) entries
admission_weight_only_lb <- admission_weight_merged %>% 
  .[!is.na(admission_weight_lb) &
      is.na(admission_weight_kg)] %>%
  .[order(stay_id,
          admission_weight_lb_time)] %>% 
  .[, n_weight := .N, 
    by = stay_id] %>% 
  .[, `:=` (diff_to_prev = 
              abs(admission_weight_lb -
              shift(admission_weight_lb,
                    type = "lag")),
            prev_weight_lb = 
              shift(admission_weight_lb,
                    type = "lag"),
            time_to_prev =
              abs(lubridate::time_length(admission_weight_lb_time -
              shift(admission_weight_lb_time,
                    type = "lag"),
              "hours"))),
    by = stay_id] %>% 
  .[, diff_to_prev := nafill(diff_to_prev,
                             type = "nocb"),
    by = stay_id] %>%
  .[, time_to_prev := nafill(time_to_prev,
                             type = "nocb"),
    by = stay_id] %>%
  # .[(!is.na(diff_to_prev) &
  #     !is.na(time_to_prev)) |
  #     n_weight == 1] %>%
  # add suspected sepsis onset time
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, time_to_sepsis := abs(lubridate::time_length(
    suspected_infection_time -
      admission_weight_lb_time,
    "hours"
  ))] %>% 
  .[, weight_valid := fcase(
    
    n_weight == 1,
    "only one admission weight (lb) measurement available",
    
    !is.na(diff_to_prev) &
      diff_to_prev == 0,
    "valid: weight (lb) equal to previous weight measurement",
    
    !is.na(diff_to_prev) &
      diff_to_prev <= 5,
    "valid: <=5lb difference between measurements",
    
    !is.na(diff_to_prev) &
      diff_to_prev > 5 &
      time_to_prev <= 1,
    "not valid: >5 lb difference within 1h",
    
    !is.na(diff_to_prev) &
      diff_to_prev > 5 &
      time_to_prev > 1,
    "uncertain: >5lb difference between measurements"
  )] %>% 
  .[, weight_valid := factor(weight_valid,
                             levels = c("valid: weight (lb) equal to previous weight measurement",
                                        "valid: <=5lb difference between measurements",
                                        "uncertain: >5lb difference between measurements",
                                        "only one admission weight (lb) measurement available",
                                        "not valid: >5 lb difference within 1h"
                                        ))] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          weight_valid,
          time_to_sepsis),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, admission_weight_final_kg := fcase(
    
    weight_valid %in% c("valid: weight (lb) equal to previous weight measurement",
                        "valid: <=5lb difference between measurements",
                        "uncertain: >5lb difference between measurements",
                        "only one admission weight (lb) measurement available"),
    admission_weight_lb / 2.20462
  )] %>% 
  .[, .(stay_id,
        admission_weight_lb_time,
        admission_weight_lb,
        admission_weight_kg_time,
        admission_weight_kg,
        weight_valid,
        admission_weight_final_kg,
        admission_weight_time = admission_weight_lb_time)]

## Check that there is only one row per stay ID
stopifnot(nrow(admission_weight_only_lb[, .N, by = stay_id] %>% .[N > 1]) == 0)

## Combine into one validated admission weight dataset
admission_weight_validated <- 
  rbindlist(list(
    admission_weight_both,
    admission_weight_only_kg,
    admission_weight_only_lb
  )) %>% 
  ## add subject and hospital admission IDs
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id)] %>% 
          unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE)

## Check that there is only one row per stay ID
stopifnot(nrow(admission_weight_validated[, .N, by = stay_id] %>% .[N > 1]) == 0)

# admission_weight_validated %>% 
#   copy() %>% 
#   ggplot(aes(
#     x = weight_valid,
#     fill = weight_valid
#   )) +
#   geom_bar() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90))

## Daily Weight Data Cleaning --------------------------------------------------
## Item ID for daily weight measurements
daily_weight_itemid <- chart_itemids %>% 
  .[grepl("Daily Weight",
          label,
          ignore.case = TRUE)]

## Closest daily weight to sepsis onset per stay ID
daily_weight_candidates <- all_chartevents %>%
  ## only stays associated with patients in the sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## only entries associated to daily weight
  .[itemid %in% daily_weight_itemid$itemid] %>%
  ## add item id unitname
  merge(.,
        daily_weight_itemid[, .(itemid,
                                unitname)],
        by = "itemid") %>% 
  .[, .(stay_id,
        daily_weight_time = charttime,
        daily_weight = valuenum,
        weight_unit = unitname)] %>%
  unique() %>% 
  ## remove obvious outliers
  .[daily_weight >= 30 &
      daily_weight <= 300] %>%
  .[order(stay_id,
          daily_weight_time)] %>% 
  .[, n_weight := .N,
    by = stay_id]


## Singular entry for daily weight per stay ID
## contains only daily weight entries which have been validated
## and are closer to sepsis onset than admission weight and 
## should therefore be used for fluid amount calculation
daily_weight_single <- daily_weight_candidates %>% 
  .[n_weight == 1] %>% 
  merge(.,
        admission_weight_validated[, .(stay_id,
                                       weight_valid,
                                       admission_weight_final_kg,
                                       admission_weight_time)],
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## Calculate difference to validated admission weight
  .[, diff_to_admission_weight := 
      abs(daily_weight -
      admission_weight_final_kg)] %>% 
  ## Time difference to validated admission weight
  .[, diff_to_admission_time := 
      abs(lubridate::time_length(
        daily_weight_time -
        admission_weight_time,
        "hours"))] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE
  ) %>% 
  ## Time difference between daily weight and sepsis onset
  .[, diff_to_onset_daily := 
      abs(lubridate::time_length(
        daily_weight_time -
          suspected_infection_time,
        "hours"))] %>% 
  ## Time difference between admission weight and sepsis onset
  .[, diff_to_onset_admission :=
      abs(lubridate::time_length(
        admission_weight_time -
          suspected_infection_time,
        "hours"))] %>% 
  .[, preferred_weight := 
      fcase(
        !is.na(diff_to_onset_admission) &
        diff_to_onset_admission <
          diff_to_onset_daily,
        "admission weight",
        
        is.na(diff_to_onset_admission) |
        (diff_to_onset_daily <=
          diff_to_onset_admission),
        "daily weight")] %>% 
  .[preferred_weight == "daily weight"] %>% 
  .[, daily_weight_valid := fcase(
    
    diff_to_admission_weight > 20 &
    diff_to_admission_time <= 48,
    "not valid: >20 kg weight change within 48h",
    
    diff_to_admission_weight <= 2,
    "valid: <=2 kg difference to admission weight",
    
    ## less than 1kg/day difference to admission weight
    diff_to_admission_time >= 24 &
    diff_to_admission_weight <= (diff_to_admission_time / 24),
    "valid: <=1 kg difference per day to admission weight",
    
    default = NA_character_)] %>% 
  .[, daily_weight_kg_final := fcase(
    daily_weight_valid %in% c("valid: <=2 kg difference to admission weight",
                              "valid: <=1 kg difference per day to admission weight"),
    daily_weight,
    
    daily_weight_valid == "not valid: >20 kg weight change within 48h" &
      !is.na(admission_weight_final_kg),
    admission_weight_final_kg,
    
    is.na(daily_weight_valid) &
    weight_valid %in% c("valid: kg/lb agreement within 2lb",
                        "valid: weight (lb) equal to previous weight measurement"),
    admission_weight_final_kg,
    
    is.na(daily_weight_valid) &
    (weight_valid %in% c("only one admission weight (kg) measurement available",
                        "only one admission weight (lb) measurement available") |
       is.na(admission_weight_final_kg)),
    daily_weight,
    
    default = NA_real_)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        daily_weight_time,
        daily_weight_valid,
        daily_weight_kg_final)]

## Check that there is only one row per patient
stopifnot(nrow(daily_weight_single[, .N, by = stay_id] %>% .[, N > 1]) == 0)

## Multiple entries for daily weight per stay ID
daily_weight_multiple <- daily_weight_candidates %>% 
  .[n_weight > 1] %>% 
  merge(.,
        admission_weight_validated[, .(stay_id,
                                       weight_valid,
                                       admission_weight_final_kg,
                                       admission_weight_time)],
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## Calculate difference to validated admission weight
  .[, diff_to_admission_weight := 
      abs(daily_weight -
            admission_weight_final_kg)] %>% 
  ## Time difference to validated admission weight
  .[, diff_to_admission_time := 
      abs(lubridate::time_length(
        daily_weight_time -
          admission_weight_time,
        "hours"))] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE
  ) %>% 
  ## Time difference between daily weight and sepsis onset
  .[, diff_to_onset_daily := 
      abs(lubridate::time_length(
        daily_weight_time -
          suspected_infection_time,
        "hours"))] %>% 
  ## Time difference between admission weight and sepsis onset
  .[, diff_to_onset_admission :=
      abs(lubridate::time_length(
        admission_weight_time -
          suspected_infection_time,
        "hours"))] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          daily_weight_time)] %>% 
  .[, `:=`(
    prev_daily_weight = shift(daily_weight),
    diff_to_prev_daily = abs(daily_weight - shift(daily_weight)),
    time_to_prev_daily = abs(lubridate::time_length(
      daily_weight_time - shift(daily_weight_time),
      "hours"
    ))
  ), by = .(subject_id,
            hadm_id,
            stay_id)] %>% 
  .[is.na(diff_to_onset_admission) |
      diff_to_onset_daily <= diff_to_onset_admission] %>% 
  .[, daily_weight_valid := fcase(
    
    is.na(admission_weight_final_kg),
    "only daily weight available",
    
    diff_to_admission_weight > 20 &
      diff_to_admission_time <= 48,
    "not valid: >20 kg difference to admission weight within 2 days",
    
    diff_to_admission_weight <= 2,
    "valid: <=2kg difference to admission weight",
    
    ## less than 1kg difference per day from admission weight
    diff_to_admission_time >= 24 &
    diff_to_admission_weight <= diff_to_admission_time / 24,
    "valid: <=1kg difference per day to admission weight",
    
    !is.na(diff_to_prev_daily) &
      diff_to_prev_daily <= 2,
    "valid: <=2 kg difference to previous daily weight",
    
    !is.na(diff_to_prev_daily) &
      diff_to_prev_daily <= time_to_prev_daily / 24,
    "valid: <=1 kg difference per day to previous daily weight",
    
    default = NA_character_
  )] %>% 
  .[, daily_weight_valid :=
      factor(daily_weight_valid,
             levels = c("valid: <=2kg difference to admission weight",
                        "valid: <=1kg difference per day to admission weight",
                        "valid: <=2 kg difference to previous daily weight",
                        "valid: <=1 kg difference per day to previous daily weight",
                        "only daily weight available",
                        "not valid: >20 kg difference to admission weight within 2 days"))] %>% 
  .[order(stay_id,
          daily_weight_valid,
          diff_to_onset_daily,
          daily_weight_time),
    ## select one daily weight value (most valid) per patient
    .SD[1],
    by = stay_id] %>% 
  .[, daily_weight_kg_final := fcase(
    daily_weight_valid %in% c(
      "valid: <=2kg difference to admission weight",
      "valid: <=1kg difference per day to admission weight",
      "valid: <=2 kg difference to previous daily weight",
      "valid: <=1 kg difference per day to previous daily weight",
      "only daily weight available"),
    daily_weight,
    
    daily_weight_valid == "not valid: >20 kg difference to admission weight within 2 days" &
      !is.na(admission_weight_final_kg),
    admission_weight_final_kg,
    
    default = NA_real_)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        daily_weight_time,
        daily_weight_valid,
        daily_weight_kg_final)]

## Check that there is one row per patient
stopifnot(nrow(daily_weight_multiple[, .N, by = stay_id] %>% .[N > 1]) == 0)

## Combine to one daily weight table
daily_weight_validated <- rbindlist(
  list(daily_weight_single,
       daily_weight_multiple),
  fill = TRUE)


## Patient Weight for Fluids ---------------------------------------------------
weight_for_fluids <- admission_weight_validated %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        weight_valid,
        admission_weight_final_kg,
        admission_weight_time)] %>% 
  merge(.,
        daily_weight_validated,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = TRUE) %>% 
  .[, weight_kg_for_fluids := fcase(
    !is.na(daily_weight_kg_final),
    daily_weight_kg_final,
    
    !is.na(admission_weight_final_kg) &
      is.na(daily_weight_kg_final),
    admission_weight_final_kg,
    
    default = NA_real_)] %>% 
  .[, weight_source := fcase(
    !is.na(daily_weight_kg_final),
    "validated daily weight",
    
    is.na(daily_weight_kg_final) &
      !is.na(admission_weight_final_kg),
    "validated admission weight",
    
    is.na(daily_weight_kg_final) &
      is.na(admission_weight_final_kg),
    "missing"
  )] %>% 
  .[!is.na(weight_kg_for_fluids)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        weight_kg_for_fluids,
        weight_source)] %>% 
  unique()

## Add to sepsis base cohort
# sepsis_base_cohort_data <-
sepsis_base_cohort_data <-
  sepsis_base_cohort_data %>% 
  merge(.,
        weight_for_fluids,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, time_sepsis_to_shock := 
      lubridate::time_length(
        septic_shock_onset_time -
          suspected_infection_time,
        "hours")]

sepsis_base_cohort_data[, summary(time_sepsis_to_shock)]

quantile(sepsis_base_cohort_data$time_sepsis_to_shock,
         probs = c(0.01,
                   0.05,
                   0.1,
                   0.5,
                   0.75,
                   0.9,
                   0.95,
                   0.99),
         na.rm = TRUE)

## First sepsis antimicrobial --------------------------------------------------
## First antimicrobial administration related to sepsis episode
first_sepsis_antimicrobial <- sepsis_base_cohort_data %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        antibiotic_time)] %>% 
  merge(.,
        all_antimicrobialevents[, .(subject_id,
                                    hadm_id,
                                    antimicrobial_start = starttime,
                                    antimicrobial_end = stoptime,
                                    drug,
                                    route)],
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  ## -24h to +72h window around sepsis onset
  ## matches sepsis3 definition
  .[antimicrobial_start >= suspected_infection_time - hours(24) &
      antimicrobial_start <= suspected_infection_time + hours(72)] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time,
                                    antibiotic_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "suspected_infection_time",
               "antibiotic_time"),
        all = TRUE) %>% 
  .[, antimicrobial_time := fcoalesce(antimicrobial_start, antibiotic_time)] %>%
  .[, antimicrobial_source := fifelse(is.na(antimicrobial_start),
                                      "sepsis3_antibiotic_time",
                                      "prescriptions")] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          antimicrobial_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        antimicrobial_time,
        antimicrobial_source)]

## Pre-ICU antibiotics, vasopressors, and crystalloids -------------------------
## quality check
## IV Antibiotic dispensed in ED
ed_abx_iv <- all_dispenseevents %>% 
  .[grepl(paste(c("Piperacillin",
                  "Tazobactam",
                  "Cefepime",
                  "Ceftriaxone", 
                  "Ceftazidime",
                  "Cefazolin",
                  "Ampicillin",
                  "Sulbactam",
                  "Cefoxitin",
                  "Vancomycin",
                  "Clindamycin",
                  "Gentamicin",
                  "Metronidazole",
                  "Azithromycin",
                  "Fluconazole",
                  "Ciprofloxacin",
                  "Levofloxacin",
                  "Acyclovir"),
                collapse = "|"),
          name,
          ignore.case = TRUE) &
      !grepl("\\bTAB\\b|\\bCAP\\b|Oral|Ophth|Oph|SPBL|Otic|Cream|Ointment|Lozenge|Suppository",
             name,
             ignore.case = TRUE) &
      !grepl("oxymetazoline|nitrofurantoin|benzathine|Clavulanic",
             name,
             ignore.case = TRUE),
    unique(name)]

## Vasopressors dispensed in ED
ed_vaso <- all_dispenseevents %>% 
  .[(grepl("^Epinephrine",
           name,
           ignore.case = TRUE) &
       !grepl(paste(c("Bristojet",
                      "auto injector",
                      "pen",
                      "kit",
                      "amp",
                      "syr",
                      "Epinephrine$"),
                    collapse = "|"),
              name,
              ignore.case = TRUE)) |
      grepl(paste(c("Norepinephrine",
                    "Phenylephrine",
                    "Vasopressin",
                    "Dopamine"),
                  collapse = "|"),
            name,
            ignore.case = TRUE), 
    unique(name)]


## Crystalloid fluids dispensed in ED
ed_crystalloid <- all_dispenseevents %>% 
  .[grepl(paste(c("0.9% NaCl",
                  "Sodium Chloride 0.9%",
                  "0.9% NaCl"),
                collapse = "|"),
          name,
          ignore.case = TRUE),
    unique(name)]



all_dispenseevents <- all_dispenseevents %>% 
  copy() %>% 
  ## only patients in sepsis base cohort
  .[, medication_type := fcase(
    
    name %in% ed_abx_iv,
    "IV antimicrobial",
    
    name %in% ed_vaso,
    "Vasopressor",
    
    name %in% ed_crystalloid,
    "Crystalloid",
    
    default = NA_character_)] %>% 
  .[!is.na(medication_type)]


## Link dispensing to the hospital admission 
dispense_linked <- all_dispenseevents %>% 
  merge(.,
        all_edstays[, .(stay_id,
                     hadm_id)],
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## only hospital admissions in sepsis base cohort
  .[hadm_id %in% sepsis_base_cohort_data$hadm_id]


## vasopressors excluding epinephrine (word-boundary keeps norepinephrine)
ed_vaso_noepi <- grep("\\bepinephrine", ed_vaso, value = TRUE,
                      invert = TRUE, ignore.case = TRUE)

## per admission first ED times + crystalloid presence
ed_abx_first <- dispense_linked %>% 
  .[name %in% ed_abx_iv,
    .(ed_first_abx_time = min(charttime)),
    by = .(subject_id,
           hadm_id)]

ed_vaso_first <- dispense_linked %>% 
  .[name %in% ed_vaso,
    .(ed_first_vaso_time = min(charttime)),
    by = .(subject_id,
           hadm_id)]

ed_vaso_first_noepi <- dispense_linked %>% 
  .[name %in% ed_vaso_noepi,
    .(ed_first_vaso_time_noepi = min(charttime)),
    by = .(subject_id,
           hadm_id)]

ed_crys <- dispense_linked %>% 
  .[name %in% ed_crystalloid,
    .(crystalloid_ed_present = TRUE),
    by = .(subject_id,
           hadm_id)]


## Assemble onto cohort 
ed_signals <- sepsis_base_cohort_data %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        suspected_infection_time,
        septic_shock_onset_time)] %>% 
  merge(.,
        first_sepsis_antimicrobial[, .(subject_id,
                                       hadm_id,
                                       stay_id,
                                       antimicrobial_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        ed_abx_first,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        ed_vaso_first,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        ed_vaso_first_noepi,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        ed_crys,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[is.na(crystalloid_ed_present),
    crystalloid_ed_present := FALSE]


## Unified ED Physiology -------------------------------------------------------
## quality check
## unified ED physiology series: 
## triage (at ED intime) + vitalsign (charttime)
ed_physio <- rbind(
  all_edtriage %>%
    merge(.,
          all_edstays[, .(stay_id, 
                          ed_intime)], 
          by = "stay_id") %>%
    .[, .(subject_id, 
          stay_id, 
          charttime = ed_intime, 
          resprate, 
          sbp)],
  all_edvitalsign[, .(subject_id, 
                      stay_id,
                      charttime, 
                      resprate, 
                      sbp)]) %>%
  merge(.,
        all_edstays[, .(stay_id, 
                        hadm_id)], 
        by = "stay_id") %>%
  .[hadm_id %in% sepsis_base_cohort_data$hadm_id] %>%
  ## plausibility bounds (drop implausible readings)
  .[, resprate := fifelse(
    resprate >= 4  &
      resprate <= 60,  
    as.numeric(resprate), 
    NA_real_)] %>%
  .[, sbp := fifelse(
    sbp >= 40 & 
      sbp <= 300, 
    as.numeric(sbp),      
    NA_real_)]

## earliest onset markers per admission
ed_qsofa2 <- ed_physio[resprate >= 22 & 
                         sbp <= 100,
                       .(ed_qsofa2_time = min(charttime, 
                                              na.rm = TRUE)), 
                       by = .(subject_id, 
                              hadm_id)]

ed_hypo90 <- ed_physio[sbp <= 90,
                       .(ed_hypo90_time = min(charttime,
                                              na.rm = TRUE)), 
                       by = .(subject_id, 
                              hadm_id)]

ed_tachypnea <- ed_physio[resprate >= 22,
                          .(ed_tachypnea_time = min(charttime, 
                                                    na.rm = TRUE)), 
                          by = .(subject_id, 
                                 hadm_id)]

## add SBP<=100 hypotension onset
ed_hypo100 <- ed_physio[sbp <= 100,
                        .(ed_hypo100_time = min(charttime, na.rm = TRUE)), by = .(subject_id, hadm_id)]

## assemble onto cohort
ed_physio_signals <- sepsis_base_cohort_data %>% 
  .[, .(subject_id, 
        hadm_id, 
        stay_id,
        septic_shock_proxy,
        suspected_infection_time,
        septic_shock_onset_time)] %>%
  merge(.,
        ed_qsofa2,   
        by = c("subject_id",
               "hadm_id"), 
        all.x = TRUE) %>%
  merge(.,
        ed_hypo90,    
        by = c("subject_id",
               "hadm_id"), 
        all.x = TRUE) %>%
  merge(.,
        ed_tachypnea, 
        by = c("subject_id",
               "hadm_id"), 
        all.x = TRUE) %>% 
  merge(.,
        ed_hypo100,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE)

## qSOFA-2 physiologic onset vs Sepsis-3 recognition 
## (lead = onset - physiology)
ed_physio_signals[!is.na(ed_qsofa2_time),
                  .(n = .N,
                    median_lead_h = 
                      round(median(
                        as.numeric(
                          difftime(suspected_infection_time, 
                                   ed_qsofa2_time,
                                   units = "hours"))), 
                        1),
                    qsofa_before_onset_pct = round(mean(
                      ed_qsofa2_time < suspected_infection_time),
                      3))]

## ED hypotension vs ICU-derived shock onset (shock patients)
ed_physio_signals[septic_shock_proxy == TRUE & 
                    !is.na(ed_hypo90_time),
                  .(n = .N,
                    median_lead_h = round(median(
                      as.numeric(
                        difftime(
                          septic_shock_onset_time, 
                          ed_hypo90_time,
                          units = "hours"))),
                      1),
                    hypo_before_shockonset_pct = round(mean(
                      ed_hypo90_time < septic_shock_onset_time), 
                      3))]

## lead of each single marker vs suspected_infection_time (positive = marker precedes onset)
ed_physio_signals[!is.na(ed_hypo100_time),
                  .(marker = "hypotension SBP<=100", 
                    n = .N,
                    median_lead_h = round(median(as.numeric(
                      difftime(suspected_infection_time, 
                               ed_hypo100_time, 
                               units = "hours"))), 
                      1),
                    before_onset_pct = round(mean(ed_hypo100_time < 
                                                    suspected_infection_time), 
                                             3))]

ed_physio_signals[!is.na(ed_tachypnea_time),
                  .(marker = "tachypnea RR>=22",
                    n = .N,
                    median_lead_h = round(median(
                      as.numeric(difftime(
                        suspected_infection_time, 
                        ed_tachypnea_time,
                        units = "hours"))),
                      1),
                    before_onset_pct = round(mean(ed_tachypnea_time < 
                                                    suspected_infection_time), 
                                             3))]



### Physiology-based Septic Shock Onset ----------------------------------------
## Re-times onset from vasopressor ADMINISTRATION (ICU-lagged treatment) to the
## vasopressor REQUIREMENT (physiology): sustained hypotension + lactate > 2.
## Hypotension = ED vitalsign (pre-ICU) UNION chartevents (ICU). Pre-ICU-corrected
## for the ED subset, ICU-bounded otherwise. Membership (vaso + lactate) UNCHANGED.

## BP itemids (mean + systolic; arterial, non-invasive, manual)
bp_itemids <- chart_itemids[label %in% c("Arterial Blood Pressure mean",
                                         "Non Invasive Blood Pressure mean",
                                         "Arterial Blood Pressure systolic",
                                         "Non Invasive Blood Pressure systolic",
                                         "Manual Blood Pressure Systolic Left",
                                         "Manual Blood Pressure Systolic Right")]


## low-BP readings: ED SBP + ICU MAP/SBP
ed_bp <- all_edvitalsign %>% 
  merge(.,
        all_edstays[, .(stay_id, 
                        hadm_id)], 
        by = "stay_id") %>%
  .[, .(subject_id, 
        hadm_id, 
        charttime, 
        val = as.numeric(sbp), 
        kind = "sbp")]

icu_bp <- all_chartevents %>% 
  .[itemid %in% bp_itemids$itemid & 
      !is.na(valuenum)] %>%
  merge(.,
        bp_itemids[, .(itemid, 
                       lbl = label)], 
        by = "itemid") %>%
  .[, kind := fifelse(grepl("mean", 
                            lbl), 
                      "map", 
                      "sbp")] %>%
  .[, .(subject_id,
        hadm_id,
        charttime, 
        val = valuenum, 
        kind)]

low_bp <- rbind(ed_bp, 
                icu_bp) %>%
  .[(kind == "map" & 
       val >= 20 & 
       val <= 200 & 
       val < 65) |
      (kind == "sbp" & 
         val >= 40 & 
         val <= 300 &
         val < 90)] %>%
  merge(.,
        sepsis_base_cohort_data[septic_shock_proxy == TRUE,
                                .(subject_id, 
                                  hadm_id, 
                                  suspected_infection_time)], 
        by = c("subject_id",
               "hadm_id")) %>%
  .[charttime >= suspected_infection_time - hours(6) &
      charttime <= suspected_infection_time + hours(24),
    .(subject_id, 
      hadm_id, t = charttime)] %>% 
  unique()

## sustained hypotension onset = earliest low reading with another low reading 1–6h later
sustained_hypo <- low_bp[low_bp, 
                         on = .(subject_id, 
                                hadm_id),
                         allow.cartesian = TRUE,
                         .(subject_id, 
                           hadm_id, 
                           t0 = x.t, 
                           t1 = i.t)] %>%
  .[t1 >= t0 + hours(1) & 
      t1 <= t0 + hours(6)] %>%
  .[, .(hypo_onset = min(t0)),
    by = .(subject_id, 
           hadm_id)]

## physio onset = pmax(sustained hypotension, first lactate>2);
## fall back to the vasopressor-based onset where hypotension is unobserved
physio_onset <- sepsis_base_cohort_data[septic_shock_proxy == TRUE,
                                        .(subject_id,
                                          hadm_id, 
                                          stay_id, 
                                          suspected_infection_time,
                                          septic_shock_onset_time, 
                                          shock_lactate_time)] %>%
  merge(.,
        sustained_hypo, 
        by = c("subject_id",
               "hadm_id"), 
        all.x = TRUE) %>%
  merge(.,
        lactate_hypoperfusion[, .(subject_id,
                                  hadm_id, 
                                  hp_lactate_time)],
        by = c("subject_id",
               "hadm_id"), 
        all.x = TRUE) %>%
  .[, lactate_onset := fcoalesce(hp_lactate_time, 
                                 shock_lactate_time)] %>%
  .[, physio_shock_onset := fifelse(!is.na(hypo_onset),
                                    hypo_onset,                      
                                    septic_shock_onset_time)]

## comparison
physio_onset[, .(n = .N,
                 with_sustained_hypo = sum(!is.na(hypo_onset)),
                 median_shift_vs_current_h = round(median(as.numeric(difftime(physio_shock_onset, 
                                                                              septic_shock_onset_time, 
                                                                              units = "hours")), 
                                                          na.rm=TRUE), 
                                                   1),
                 earlier_than_current_pct = round(mean(physio_shock_onset < septic_shock_onset_time, 
                                                       na.rm=TRUE), 
                                                  3),
                 physio_before_sepsis_pct  = round(mean(physio_shock_onset < suspected_infection_time, 
                                                        na.rm=TRUE), 
                                                   3))]


## 7 RECOMMENDATION ------------------------------------------------------------
rec7_label <- ssc_recommendations %>% 
  .[rec_num == 7, rec_label]

## Compliance was defined as blood culture collection at or before the first
## systemic antimicrobial administration. The antimicrobial time is derived from
## the `prescriptions` table (see rec7_antimicrobial): a curated set of systemic
## antibacterials, antifungals and antivirals, restricted to systemic routes,
## excluding oral vancomycin, topical/ophthalmic forms and allergy protocols.
## This is hospital-wide (capturing pre-ICU/ED first doses), broader than the
## Sepsis-3 antibiotic_time (which is antibacterials only), and reduces the
## circularity with suspected_infection_time. The Sepsis-3 antibiotic_time is
## retained only as a <0.1% fallback for stays with no matching prescription.

## Blood cultures were identified from the `microbiologyevents` table using the 
## specimen types "BLOOD CULTURE" and "BLOOD CULTURE ( MYCO/F LYTIC BOTTLE)". 
## Specimen types such as "Stem Cell - Blood Culture", "FLUID RECEIVED IN BLOOD 
## CULTURE BOTTLES", neonatal blood cultures, serological blood specimens, and 
## other specialized blood-derived specimens were excluded because they do not 
## represent routine diagnostic blood cultures obtained from adult patients with 
## suspected sepsis. Restricting the definition to routine patient blood cultures 
## ensured consistency with the intent of SSC Recommendation 7, which recommends 
## obtaining blood cultures before initiation of antimicrobial therapy to maximize 
## microbiological diagnostic yield.

## Although the study cohort was restricted to ICU admissions, blood cultures were 
## identified from the entire hospitalization rather than only during the ICU stay. 
## This approach was chosen because the SSC recommendation refers to the initial 
## management of the sepsis episode rather than care delivered exclusively after 
## ICU admission. In clinical practice, blood cultures are frequently obtained in 
## the emergency department or on hospital wards before transfer to the ICU, often 
## immediately before initiation of antimicrobial therapy. Restricting blood cultures 
## to the ICU stay would therefore underestimate adherence by excluding clinically 
## appropriate cultures obtained before ICU admission.

## A blood culture collected within 24 hours around sepsis onset is considered to 
## belong to the current sepsis episode. The recommendation itself specifies no 
## time threshold.

## First blood culture related to sepsis episode
first_sepsis_blood_culture <- all_microbioevents %>% 
  ## only patients in sepsis base cohort
  .[hadm_id %in% sepsis_base_cohort_data$hadm_id] %>% 
  ## relevant blood cultures
  .[spec_type_desc %in% c("BLOOD CULTURE",
                          "BLOOD CULTURE ( MYCO/F LYTIC BOTTLE)")] %>% 
  .[, .(subject_id,
        hadm_id,
        blood_culture_time = charttime,
        spec_type_desc)] %>% 
  unique() %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)],
        by = c("subject_id",
               "hadm_id"),
        all = FALSE) %>%
  ## Time between blood culture and sepsis onset (negative = culture precedes onset)
  .[blood_culture_time >= suspected_infection_time - hours(24) &
      blood_culture_time <= suspected_infection_time + hours(72)] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          blood_culture_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)]

## Adherent if the earliest qualifying blood culture was drawn at or before the
## first systemic antimicrobial. This is equivalent to testing whether any
## qualifying culture preceded it: if the earliest does not, none can.
## antimicrobial_time is never NA (prescriptions value, or antibiotic_time
## fallback), so a NA blood_culture_time means no qualifying culture -> FALSE,
## since failing to obtain cultures is itself a deviation from the recommendation.
rec_7 <- first_sepsis_antimicrobial %>% 
  merge(.,
        first_sepsis_blood_culture,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, blood_culture_collected := !is.na(blood_culture_time)] %>%
  .[, rec_7_culture_before_antimicrobial := fcase(
  
    !is.na(blood_culture_collected) &
      blood_culture_time <= antimicrobial_time,
    TRUE,
    
    blood_culture_time > antimicrobial_time |
      is.na(blood_culture_time),
    FALSE,
  
    default = NA)]

stopifnot(nrow(first_sepsis_antimicrobial) == nrow(unique(sepsis_base_cohort_data, 
                                                          by = c("subject_id",
                                                                 "hadm_id",
                                                                 "stay_id"))))

stopifnot(first_sepsis_antimicrobial[, sum(is.na(antimicrobial_time))] == 0)

stopifnot(nrow(rec_7) == nrow(first_sepsis_antimicrobial))


## 8 RECOMMENDATION ------------------------------------------------------------
rec8_label <- ssc_recommendations %>% 
  .[rec_num == 8, rec_label]

## Item IDs corresponding to blood lactate measurement
lactate_itemid <- lab_itemids %>% 
  .[label == "Lactate" &
      grepl("blood",
            fluid,
            ignore.case = TRUE)]

## Determine ED admission time to use as lower 
## bound for lactate measurement time stamp
ed_first_intime <- all_edstays %>% 
  .[!is.na(hadm_id)] %>% 
  ## earliest ED arrival per hospital admission
  .[, .(ed_intime = min(ed_intime,
                        na.rm = TRUE)),
    by = .(subject_id,
           hadm_id)]

## Lactate Events
## Consider lactate measurements during the entire hospitalization, not just
## during the ICU stay to avoid excluding relevant measurements
lactate_events <- all_labevents %>%
  .[itemid %in% lactate_itemid$itemid,
    .(subject_id,
      hadm_id,
      charttime,
      valuenum)]

## First lactate measurement within 6h of sepsis onset
first_lactate <- lactate_events %>% 
  .[, .(subject_id,
        hadm_id,
        lactate_time = charttime,
        lactate_value = valuenum)] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time,
                                    admittime,
                                    icu_intime)],
        by = c("subject_id",
               "hadm_id"),
        all = FALSE) %>% 
  merge(.,
        ed_first_intime,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, lactate_lower_bound := pmin(suspected_infection_time - 
                                    hours(1),                     
                                  pmax(fcoalesce(ed_intime,
                                                 admittime, 
                                                 icu_intime),       
                                       suspected_infection_time - 
                                         hours(24),               
                                       na.rm = TRUE),
                                  na.rm = TRUE)] %>% 
  ## meant to guide resuscitation and subsequent therapeutic interventions
  ## therefore guideline is restricted to initial fluid resuscitation window
  ## so lactate measurement can guide decision making
.[lactate_time >= lactate_lower_bound &
    lactate_time <= suspected_infection_time + hours(24)] %>% 
  # .[lactate_time >= suspected_infection_time - hours(1) &
  #     lactate_time <= suspected_infection_time + hours(24)] %>%
  .[order(subject_id,
          hadm_id,
          stay_id,
          lactate_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, time_to_sepsis := 
      lubridate::time_length(
        lactate_time -
          suspected_infection_time,
        "hours")]

## Analzye distribution of lactate time compared to sepsis onset
summary(first_lactate$time_to_sepsis)
quantile(first_lactate$time_to_sepsis,
         probs = c(0.01, 0.1, 0.5, 0.75, 0.9, 0.95, 0.99))

## Encode recommendation 8
rec_8 <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id, 
        suspected_infection_time)] %>% 
  unique() %>% 
  merge(.,
        first_lactate,
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "suspected_infection_time"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, lactate_measured := !is.na(lactate_time)] %>% 
  .[, rec_8_lactate_measured := fcase(
    
    !is.na(lactate_time),
    TRUE,
    
    is.na(lactate_time),
    FALSE,
    
    default = NA)] %>% 
  .[, rec_8_lactate_measured_within_1h := fcase(
    
    !is.na(lactate_time) &
      time_to_sepsis <= 1,
    
    TRUE,
    
    (!is.na(lactate_time) &
      time_to_sepsis > 1) |
      is.na(lactate_time),
    FALSE,
    
    default = NA)] %>% 
  .[, rec_8_lactate_measured_within_3h := fcase(
    
    !is.na(lactate_time) &
      time_to_sepsis <= 3,
    
    TRUE,
    
    (!is.na(lactate_time) &
       time_to_sepsis > 3) |
      is.na(lactate_time),
    FALSE,
    
    default = NA)] %>% 
  .[, rec_8_lactate_measured_within_6h := fcase(
    
    !is.na(lactate_time) &
      time_to_sepsis <= 6,
    
    TRUE,
    
    (!is.na(lactate_time) &
       time_to_sepsis > 6) |
      is.na(lactate_time),
    FALSE,
    
    default = NA)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        rec_8_lactate_measured,
        rec_8_lactate_measured_within_1h,
        rec_8_lactate_measured_within_3h,
        rec_8_lactate_measured_within_6h,
        time_to_sepsis)] %>% 
  unique() %>% 
  .[, rec_8_eligibility := fcase(
    
    !is.na(rec_8_lactate_measured),
    "eligible",
    
    default = NA_character_)]

## 10 RECOMMENDATION -----------------------------------------------------------
## Recommendation 10 targets patients with sepsis-induced hypoperfusion or
## septic shock. Following the 2026 SSC guideline, sepsis-induced hypoperfusion
## is operationalized (in data_preparation.R) as lactate > 2 mmol/L, hypotension
## (MAP < 65 or SBP < 90 mmHg), or septic shock, whichever occurs earliest
## within the sepsis episode (hypoperfusion_onset_time). Eligibility and the
## resuscitation window are anchored to this hypoperfusion onset, generalizing
## the earlier shock-only definition (septic_shock_proxy remains available as a
## baseline comparison rung).
rec10_label <- ssc_recommendations %>% 
  .[rec_num == 10, rec_label]

## Classification of administered fluids into Crystalloids (balanced/normal 
## saline), ## colloids, albumin, starches, etc.
input_fluid_classification <- d_items %>% 
  .[linksto == "inputevents"] %>% 
  .[grepl("\\bLR\\b|Ringers|Normosol",
          label,
          ignore.case = TRUE) &
      !grepl("/",
             label,
             ignore.case = TRUE), 
    fluid_type := "Balanced crystalloid"] %>% 
  .[grepl("NaCl 0.9%|Saline 0,9%",
          label,
          ignore.case = TRUE) &
      !grepl("/|Hetastarch",
             label,
             ignore.case = TRUE),
    fluid_type := "Unbalanced crystalloid (normal saline)"] %>% 
  .[is.na(fluid_type) &
      grepl("NaCl|Saline|Darrow",
            label,
            ignore.case = TRUE) &
      grepl("0.45%|0,18%|0,255%|0,3%|0,45%|0,65%|Darrow",
            label,
            ignore.case = TRUE) &
      !grepl("Dextrose",
             label,
             ignore.case = TRUE),
    fluid_type := "Hypotonic saline"] %>% 
  .[is.na(fluid_type) &
      grepl("Saline|NaCl",
            label,
            ignore.case = TRUE) &
      grepl("3%|23.4%|30%",
            label,
            ignore.case = TRUE) &
      !grepl("Dextrose",
             label,
             ignore.case = TRUE),
    fluid_type := "Hypertonic saline"] %>% 
  .[is.na(fluid_type) &
      grepl("Dextrose|\\bD5",
            label,
            ignore.case = TRUE) &
      !grepl("Dextrose PN|Dextran|Aminosyn",
             label,
             ignore.case = TRUE),
    fluid_type := "Dextrose-containing"] %>% 
  .[is.na(fluid_type) &
      grepl("Free Water|Sterile Water|Aquadestila",
            label,
            ignore.case = TRUE) &
      !grepl("GU Irrigant",
             label,
             ignore.case = TRUE),
    fluid_type := "Water"] %>% 
  .[is.na(fluid_type) &
      grepl("Sodium Bicarbonate|Bicarbonate Base|Prismasate|Citrate",
            label,
            ignore.case = TRUE),
    fluid_type := "Buffer/CRRT"] %>% 
  .[is.na(fluid_type) &
      grepl("Albumin",
            label,
            ignore.case = TRUE) &
      !grepl("Apache|ZAlbumin",
             label,
             ignore.case = TRUE),
    fluid_type := "Albumin (Colloid)"] %>% 
  .[is.na(fluid_type) &
      grepl("starch",
            label,
            ignore.case = TRUE),
    fluid_type := "Starches (Colloid)"] %>% 
  .[is.na(fluid_type) &
      grepl("Dextran",
            label,
            ignore.case = TRUE),
    fluid_type := "Dextrans (Colloid)"] %>% 
  .[is.na(fluid_type) &
      grepl("Gelofusine",
            label,
            ignore.case = TRUE),
    fluid_type := "Gelatin (Colloid)"] %>% 
  .[is.na(fluid_type) &
      grepl("Intake",
            label,
            ignore.case = TRUE),
    fluid_type := "Aggregate intake (non-ICU)"] %>% 
  .[is.na(fluid_type) &
      grepl(paste("Packed Red",
                  "Red Cells",
                  "erytrocytes",
                  "Whole Blood",
                  "Fresh Frozen Plasma",
                  "Octoplas",
                  "ESDEP",
                  "Platelets",
                  "Thrombocyte",
                  "Cryoprecipitate",
                  "Granulocytes",
                  "Cell Saver",
                  "Auto transfusion",
                  sep = "|"),
            label,
            ignore.case = TRUE),
    fluid_type := "Blood product"] %>% 
  .[!is.na(fluid_type)] %>% 
  .[, resuscitation_role := fcase(
    
    fluid_type %in% c("Balanced crystalloid",
                      "Unbalanced crystalloid (normal saline)"),
    "crystalloid",
    
    grepl("Colloid",
          fluid_type,
          ignore.case = TRUE),
    "colloid",
    
    fluid_type == "Blood product",
    "blood product",
    
    fluid_type == "Aggregate intake (non-ICU)" &
      grepl("crystalloid",
            label,
            ignore.case = TRUE),
    "aggregated crystalloid",
    
    default = "other fluid")]

## Only crystalloid given AS FLUID counts: ordercategorydescription in "Bolus"
## and "Continuous IV". Crystalloid recorded under "Continuous Med" (drip carrier
## bags) and "Drug Push" (medication diluents) is excluded -- it is incidental to
## drug delivery, infused at drug rather than resuscitation rates, and would
## confound adherence with illness severity (sicker patients on more drips accrue
## more carrier volume). PRIMARY = Bolus + Continuous IV; boluses-only added as a
## sensitivity. Target uses the validated weight_kg_for_fluids, not the
## inputevents patientweight field (which contains gross data-entry errors).
crystalloid_itemids <- input_fluid_classification %>% 
  .[fluid_type %in% c("Unbalanced crystalloid (normal saline)",
                      "Balanced crystalloid") |
      (fluid_type == "Aggregate intake (non-ICU)" &
         grepl("crystalloid",
               label,
               ignore.case = TRUE))]

### Aggregated crystalloid intake ----------------------------------------------
## Inspect aggregated crystalloid intake
crystalloid_events_aggregated <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>%
  ## aggregated crystalloid intake item IDs
  .[itemid %in% crystalloid_itemids[
    resuscitation_role == "aggregated crystalloid"]$itemid] %>% 
  merge(.,
        crystalloid_itemids[, .(itemid,
                                label)],
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## unify units (mL and L)
  .[, amount_ml := fcase(
    
    amountuom == "mL",
    amount,
    
    amountuom == "L",
    amount * 1000,
    
    default = NA_real_)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        starttime,
        endtime,
        storetime,
        itemid,
        label,
        amount_ml,
        ordercategoryname,
        ordercategorydescription,
        patientweight)] %>% 
  unique() %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time,
                                    icu_intime,
                                    icu_outtime,
                                    fluid_resuscitation_start,
                                    fluid_resuscitation_end)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, fluid_duration := 
      lubridate::time_length(
        endtime-
          starttime,
        "mins")] %>% 
  .[, time_to_sepsis :=
      lubridate::time_length(
        starttime -
          suspected_infection_time,
        "hours")]

## Confirm that all crystalloid stoptimes are within 1 min of crystalloid starttime
## Entries display bolus fluid administration over a short time span
stopifnot(nrow(crystalloid_events_aggregated[fluid_duration > 1]) == 0)

## Analyze distribution of time difference between 
## crystalloid administration and sepsis onset
summary(crystalloid_events_aggregated$time_to_sepsis)

quantile(crystalloid_events_aggregated$time_to_sepsis,
         probs = c(0.01, 
                   0.1, 
                   0.5, 
                   0.75,
                   0.8,
                   0.85,
                   0.9, 
                   0.95, 
                   0.99))

## Restrict to relevant sepsis episode
crystalloid_resuscitation_aggregated <- 
  crystalloid_events_aggregated %>% 
  .[starttime >= suspected_infection_time - hours(24) &
      starttime <= suspected_infection_time + hours(24)] %>% 
  .[, crystalloid_target := patientweight * 30] %>% 
  .[, total_crystalloid_reveived_over_30 := amount_ml >= crystalloid_target]


### Crystalloid intake ---------------------------------------------------------
crystalloid_events <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  .[itemid %in% crystalloid_itemids[resuscitation_role == "crystalloid"]$itemid] %>% 
  merge(.,
        crystalloid_itemids[, .(itemid,
                                label)],
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE)

## Determine units for resuscitation fluid administration amount
crystalloid_events[, unique(amountuom)]

## Determine units for resuscitation fluid administration rate
crystalloid_events[, unique(rateuom)]

## Unify fluid amount units
crystalloid_events <- crystalloid_events %>% 
  copy() %>% 
  .[, amount_ml := fcase(
    
    amountuom == "mL",
    amount,
    
    amountuom == "L",
    amount * 1000,
    
    default = NA)]

## Verify crystalloid bolus definition
crystalloid_events[, .(.N, total_ml = sum(amount_ml, 
                                          na.rm = TRUE)),
                   by = .(ordercategoryname,
                          ordercomponenttypedescription, 
                          ordercategorydescription)] %>% 
  .[order(-total_ml)]


## Define total fluid resuscitation volume per patient
total_crystalloid_resuscitation <- crystalloid_events %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        label,
        starttime,
        endtime,
        amount_ml,
        rate,
        rateuom,
        ordercategoryname,
        secondaryordercategoryname,
        ordercomponenttypedescription,
        ordercategorydescription,
        patientweight)] %>% 
  merge(.,
        sepsis_base_cohort_data[hypoperfusion_proxy == TRUE,
                                .(subject_id,
                                  hadm_id,
                                  stay_id,
                                  hypoperfusion_onset_time)],

        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>%
  ## hypoperfusion-anchored resuscitation window bounds (defined once, used everywhere)
  .[, resus_start := hypoperfusion_onset_time - lubridate::hours(1)] %>%
  .[, resus_end   := hypoperfusion_onset_time + lubridate::hours(3)] %>%
  ## a crystalloid event counts if it OVERLAPS the window
  .[, fluid_resuscitation_window :=
      starttime < resus_end &
      endtime   > resus_start] %>%
  ## clip each overlapping event to the window
  .[fluid_resuscitation_window == TRUE,
    `:=` (overlap_start = pmax(starttime, resus_start),
          overlap_end   = pmin(endtime,   resus_end))] %>%
  .[, overlap_hours :=
      lubridate::time_length(overlap_end - overlap_start, "hours")] %>%
  .[, fluid_ml_3h := fcase(
    
    rateuom == "mL/hour", 
    overlap_hours * rate,
    
    rateuom == "mL/min",  
    overlap_hours * 60 * rate,
    
    !is.na(amount_ml) & 
      starttime >= resus_start & 
      starttime < resus_end, 
    amount_ml,
    
    default = NA_real_)] %>% 
  ## Restrict to main order crystalloid administration
  .[ordercategoryname %in% c("02-Fluids (Crystalloids)",
                             "03-IV Fluid Bolus") &
      ordercategorydescription %in% c("Continuous IV",
                                      "Bolus")] %>% 
  ## PRIMARY volume: all crystalloid given as fluid (Bolus + Continuous IV)
  .[, crystalloid_ml_3h :=
      sum(fluid_ml_3h, na.rm = TRUE),
    by = .(subject_id, hadm_id, stay_id)] %>% 
  ## SENSITIVITY volume: resuscitation boluses only
  .[, crystalloid_ml_3h_bolus :=
      sum(fifelse(ordercategoryname == "03-IV Fluid Bolus",
                  fluid_ml_3h,
                  NA_real_),
          na.rm = TRUE),
    by = .(subject_id, hadm_id, stay_id)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        resus_start,
        resus_end,
        crystalloid_ml_3h,
        crystalloid_ml_3h_bolus)] %>%
  unique()

## SENSITIVITY volume table: identical crystalloid accounting as the primary,
## but the [-1h, +3h] window is anchored to SEPSIS onset (suspected_infection_time)
## rather than hypoperfusion/shock onset. This matches the anchor used by the
## guideline's cited evidence ("30 mL/kg within 3 h of sepsis onset") and, for the
## shock stratum, moves the window earlier (often pre-ICU), quantifying how much
## the anchor choice alone drives the measured adherence.
total_crystalloid_resuscitation_sepsisonset <- crystalloid_events %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        label,
        starttime,
        endtime,
        amount_ml,
        rate,
        rateuom,
        ordercategoryname,
        secondaryordercategoryname,
        ordercomponenttypedescription,
        ordercategorydescription,
        patientweight)] %>%
  merge(.,
        sepsis_base_cohort_data[hypoperfusion_proxy == TRUE,
                                .(subject_id,
                                  hadm_id,
                                  stay_id,
                                  suspected_infection_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>%
  .[, resus_start := suspected_infection_time - lubridate::hours(1)] %>%
  .[, resus_end   := suspected_infection_time + lubridate::hours(3)] %>%
  .[, fluid_resuscitation_window :=
      starttime < resus_end &
      endtime   > resus_start] %>%
  .[fluid_resuscitation_window == TRUE,
    `:=` (overlap_start = pmax(starttime, resus_start),
          overlap_end   = pmin(endtime,   resus_end))] %>%
  .[, overlap_hours :=
      lubridate::time_length(overlap_end - overlap_start, "hours")] %>%
  .[, fluid_ml_3h := fcase(

    rateuom == "mL/hour",
    overlap_hours * rate,

    rateuom == "mL/min",
    overlap_hours * 60 * rate,

    !is.na(amount_ml) &
      starttime >= resus_start &
      starttime < resus_end,
    amount_ml,

    default = NA_real_)] %>%
  .[ordercategoryname %in% c("02-Fluids (Crystalloids)",
                             "03-IV Fluid Bolus") &
      ordercategorydescription %in% c("Continuous IV",
                                      "Bolus")] %>%
  .[, crystalloid_ml_3h_sepsisonset :=
      sum(fluid_ml_3h, na.rm = TRUE),
    by = .(subject_id, hadm_id, stay_id)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        crystalloid_ml_3h_sepsisonset)] %>%
  unique()

## Adjusted body weight for the 30 mL/kg target (SENSITIVITY analysis).
## The 2026 guideline (Table 4) calculates the weight-based volume from actual
## body weight for BMI <= 30, and from ADJUSTED body weight for BMI > 30, where
## AdjBW = IBW + 0.4 * (ABW - IBW) and IBW is the Devine ideal body weight (the
## same formula as predicted_body_weight used for lung-protective ventilation).
## A validated height is required; obese patients without a valid height, and
## all non-obese patients, retain actual body weight.

## Confirm one height entry per patient
stopifnot(nrow(patient_height[, .N, by = stay_id] %>% .[N > 1]) == 0)

fluid_target_weight <- patient_height %>%
  .[height_valid == "valid: <= 2 cm difference"] %>%
  merge(.,
        sepsis_base_cohort_data[, .(stay_id,
                                    gender,
                                    weight_kg_for_fluids)],
        by = "stay_id",
        all.x = FALSE,
        all.y = TRUE) %>%
  .[, bmi := weight_kg_for_fluids / ((`Height (cm)` / 100)^2)] %>% 
  ## Devine ideal body weight (kg) from height and sex
  .[, ibw_kg := fcase(
    gender == "M", 50   + 0.905 * (`Height (cm)` - 152.4),
    gender == "F", 45.5 + 0.905 * (`Height (cm)` - 152.4),
    default = NA_real_)] %>%
  .[, adjbw_kg := ibw_kg + 0.4 * (weight_kg_for_fluids - ibw_kg)] %>%
  ## target weight: adjusted for BMI > 30 (valid height), else actual weight
  .[, weight_kg_target_adjbw := fifelse(
    !is.na(bmi) &
      bmi > 30 &
      !is.na(adjbw_kg),
    adjbw_kg,
    weight_kg_for_fluids)] %>%
  .[, .(stay_id,
        `Height (cm)`,
        bmi,
        ibw_kg,
        adjbw_kg,
        weight_kg_target_adjbw)]

## Define recommendation 10
rec_10 <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        hypoperfusion_proxy,
        hypoperfusion_onset_time,
        weight_kg_for_fluids)] %>%
  .[, crystalloid_target := 30 * weight_kg_for_fluids] %>%
  ## SENSITIVITY target: adjusted body weight for BMI > 30 (guideline Table 4);
  ## actual weight is retained where no adjusted weight applies or height is missing
  merge(.,
        fluid_target_weight[, .(stay_id,
                                weight_kg_target_adjbw)],
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, weight_kg_target_adjbw := fcoalesce(weight_kg_target_adjbw,
                                          weight_kg_for_fluids)] %>%
  .[, crystalloid_target_adjbw := 30 * weight_kg_target_adjbw] %>%
  merge(.,
        total_crystalloid_resuscitation,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  ## coalesce missing crystalloid entries
  .[, crystalloid_ml_3h := fifelse(
    !is.na(crystalloid_ml_3h),
    crystalloid_ml_3h,
    fifelse(
      is.na(crystalloid_ml_3h) &
        hypoperfusion_proxy == TRUE,
      0,
      NA_real_))] %>%
  .[, crystalloid_ml_3h_bolus := fifelse(
    !is.na(crystalloid_ml_3h_bolus),
    crystalloid_ml_3h_bolus,
    fifelse(
      is.na(crystalloid_ml_3h_bolus) &
        hypoperfusion_proxy == TRUE,
      0,
      NA_real_))] %>%
  ## PRIMARY adherence: >= 30 mL/kg crystalloid fluid (Bolus + Continuous IV)
  .[, rec_10_crystalloid_within_3h :=
      fifelse(
        hypoperfusion_proxy == TRUE &
        crystalloid_ml_3h >= crystalloid_target,
        TRUE,
        fifelse(hypoperfusion_proxy == TRUE &
                  crystalloid_ml_3h <
                  crystalloid_target,
                FALSE,
                NA))] %>%
  ## adjusted body weight sensitivity
  .[, rec_10_crystalloid_within_3h_adjbw :=
      fifelse(
        hypoperfusion_proxy == TRUE &
          crystalloid_ml_3h >= crystalloid_target_adjbw,
        TRUE,
        fifelse(hypoperfusion_proxy == TRUE &
                  crystalloid_ml_3h <
                  crystalloid_target_adjbw,
                FALSE,
                NA))] %>%
  ## SENSITIVITY adherence: resuscitation boluses only
  .[, rec_10_crystalloid_within_3h_bolus :=
      fifelse(
        hypoperfusion_proxy == TRUE &
          crystalloid_ml_3h_bolus >= crystalloid_target,
        TRUE,
        fifelse(hypoperfusion_proxy == TRUE &
                  crystalloid_ml_3h_bolus <
                  crystalloid_target,
                FALSE,
                NA))] %>%
  ## adjusted body weight sensitivity
  .[, rec_10_crystalloid_within_3h_bolus_adjbw :=
      fifelse(
        hypoperfusion_proxy == TRUE &
          crystalloid_ml_3h_bolus >= crystalloid_target_adjbw,
        TRUE,
        fifelse(hypoperfusion_proxy == TRUE &
                  crystalloid_ml_3h_bolus <
                  crystalloid_target_adjbw,
                FALSE,
                NA))] %>%
  ## SENSITIVITY adherence: window anchored to SEPSIS onset instead of
  ## hypoperfusion/shock onset (matches the guideline's cited-evidence anchor)
  merge(.,
        total_crystalloid_resuscitation_sepsisonset,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, crystalloid_ml_3h_sepsisonset := fifelse(
    !is.na(crystalloid_ml_3h_sepsisonset),
    crystalloid_ml_3h_sepsisonset,
    fifelse(
      is.na(crystalloid_ml_3h_sepsisonset) &
        hypoperfusion_proxy == TRUE,
      0,
      NA_real_))] %>%
  .[, rec_10_crystalloid_within_3h_sepsisonset :=
      fifelse(
        hypoperfusion_proxy == TRUE &
          crystalloid_ml_3h_sepsisonset >= crystalloid_target,
        TRUE,
        fifelse(hypoperfusion_proxy == TRUE &
                  crystalloid_ml_3h_sepsisonset <
                  crystalloid_target,
                FALSE,
                NA))] %>%
  .[, rec_10_eligibility := fcase(

    !is.na(rec_10_crystalloid_within_3h),
    "eligible",

    hypoperfusion_proxy == TRUE &
      is.na(weight_kg_for_fluids),
    "missing weight",

    hypoperfusion_proxy == FALSE |
      is.na(hypoperfusion_proxy),
    "no sepsis-induced hypoperfusion or unknown",

    default = NA_character_)]

## 12 RECOMMENDATION -----------------------------------------------------------
## Recommendation 12 (septic shock): start vasopressors peripherally to restore
## MAP rather than DELAYING initiation until central venous access is secured.
## MIMIC-IV does not record the administration route, so adherence is measured as
## vasopressor initiation WITHOUT delay for central access, inferred from the
## timing of vasopressors relative to central-line placement. Three groups:
##  - vasopressor started before any central line (or with no central line) ->
##    peripheral start -> adherent;
##  - a central line already in place before the sepsis episode and still present
##    at shock onset -> immediate central administration, no delay -> adherent;
##  - a central line placed DURING the episode, with vasopressors only afterwards
##    -> initiation delayed to secure central access -> non-adherent.
## Central venous lines only (arterial lines, midlines -- per the guideline remark
## "midline catheters were not considered" -- and non-venous access are excluded).
rec12_label <- ssc_recommendations %>%
  .[rec_num == 12, rec_label]

# ## Peripheral IV lines
# peripheral_itemids <- d_items %>% 
#   copy() %>% 
#   .[linksto == "procedureevents" &
#       category ==  "Access Lines - Peripheral"]
# 
# peripheral_events <- all_procedureevents %>% 
#   copy() %>% 
#   ## only patients in sepsis base cohort
#   .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
#   .[itemid %in% peripheral_itemids$itemid]

## Central venous access lines
cvc_itemids <- d_items %>% 
  .[linksto == "procedureevents" &
      label %in% c("Multi Lumen",
                   "PICC Line",
                   "Cordis/Introducer",
                   "Trauma line",
                   "CCO PAC",
                   "Dialysis Catheter",
                   "Presep Catheter",
                   "PA Catheter",
                   "Triple Introducer",
                   "Indwelling Port (PortaCath)",
                   "Pheresis Catheter",
                   "Tunneled (Hickman) Line",
                   "Sheath (Venous)",
                   "AVA",
                   "Single Lumen",
                   "Multi Lumen Cooling Catheter",
                   "MAC",
                   "Tunneled Access Line",
                   "Indwelling Port",
                   "Sheath Insertion" ## generic (arterial or venous unspecified)
                   )]
  
cvc_events <- all_procedureevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  .[itemid %in% cvc_itemids$itemid] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        itemid,
        cvc_start = starttime,
        cvc_end = endtime)] %>% 
  ## collapse duplicate records of the same line
  ## keep latest documented removal
  .[order(subject_id,
          hadm_id,
          stay_id,
          itemid,
          cvc_start,
          -as.numeric(cvc_end)), .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id,
           itemid,
           cvc_start)] %>% 
    merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time,
                                    septic_shock_onset_time,
                                    shock_vaso_start)],
                                
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE)

## Was a central line in place at septic shock onset?
cvc_at_shock <- cvc_events %>% 
  ## only count lines actually in place for sepsis
  ## not e.g. placed immediately before septic shock onset
  ## which is arguably the "placed for vasopressor" delay scenario
  .[cvc_start <= suspected_infection_time &
      ## placed before the sepsis episode and 
      ## still in place at septic shock onset
      cvc_end >= septic_shock_onset_time,
    .(cvc_at_shock = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)]

## First central line placement 
first_cvc <- cvc_events %>% 
  .[, .(first_cvc_time = min(cvc_start,
                             na.rm = TRUE)),
    by = .(subject_id,
           hadm_id,
           stay_id)]

rec_12 <- sepsis_base_cohort_data %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time,
        shock_vaso_start)] %>% 
  merge(.,
        cvc_at_shock,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        first_cvc,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  ## Coalacesce NA values
  .[, cvc_at_shock := fcoalesce(cvc_at_shock,
                                FALSE)] %>% 
  .[, rec_12_vaso_without_central_delay := fcase(
    
    septic_shock_proxy != TRUE |
      is.na(shock_vaso_start),
    NA,
    
    cvc_at_shock == TRUE,
    TRUE,
    
    is.na(first_cvc_time) |
      shock_vaso_start < first_cvc_time,
    TRUE,
    
    default = FALSE)] %>% 
  .[, rec_12_eligibility := fcase(
    
    !is.na(rec_12_vaso_without_central_delay),
    "eligible",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_)]

## 13 RECOMMENDATION -----------------------------------------------------------
rec13_label <- ssc_recommendations %>%
  .[rec_num == 13, rec_label]

## MAP item IDs (mean arterial and non-invasive)
map_itemids <- chart_itemids %>%
  .[label %in% c("Arterial Blood Pressure mean",
                 "Non Invasive Blood Pressure mean")]

## MAP events
map_events <- all_chartevents %>%
  ## keep only sepsis base cohort stay IDs
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## MAP item IDs
  .[itemid %in% map_itemids$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  ## remove obvious outliers / data entry errors
  ## implausible MAP measurements (<20 mmHG or >200 mmHg) were 
  ## treated as invalid and excluded from analyis
  .[valuenum >= 20 &
      valuenum <= 200] %>% 
  ## add item ID labels
  merge(.,
        map_itemids[, .(itemid,
                        map_source = label)],
        by = "itemid") %>% 
  # .[, map_source := fifelse(label == "Arterial Blood Pressure mean",
  #                           "Arterial",
  #                           fifelse(label == "Non Invasive Blood Pressure mean",
  #                                   "Non-invasive",
  #                                   NA_character_)
  # )] %>% 
  .[, map_source := factor(map_source,
                           levels = c("Arterial Blood Pressure mean",
                                      "Non Invasive Blood Pressure mean"))] %>%
  ## If available, arterial MAP is preferred over non-invasive MAP
  .[order(stay_id,
          charttime,
          map_source),
    .SD[1],
    by = .(stay_id,
           charttime)]

## Median MAP within the initial [-1h, +3h] of septic shock onset within the
## 60-70 mmHg band (initial MAP target of 65 +/- 5, titrated with vasopressors).
## The window is anchored to septic SHOCK onset -- when vasopressors and MAP
## titration begin -- rather than sepsis onset, which for a majority of stays
## precedes ICU admission where chartevents MAP is not recorded.
rec_13 <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time)] %>%
  unique() %>%
  .[, map_window_start := septic_shock_onset_time - lubridate::hours(1)] %>%
  .[, map_window_end   := septic_shock_onset_time + lubridate::hours(3)] %>%
  merge(.,
        map_events,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  ## MAP values within the initial [-1h, +3h] septic-shock window
  .[, map_window := charttime >= map_window_start &
      charttime <= map_window_end] %>%
  .[map_window == TRUE,
    median_map := median(valuenum, na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  ## propagate the per-stay median to all rows of the stay
  .[, median_map := nafill(median_map,
                           type = "locf"),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, median_map := nafill(median_map,
                           type = "nocb"),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time,
        median_map)] %>%
  unique() %>%
  .[septic_shock_proxy == TRUE, rec_13_map_target_65 :=
      !is.na(median_map) &
      median_map >= 60 &
      median_map <= 70] %>%
  .[, rec_13_eligibility := fcase(

    !is.na(rec_13_map_target_65),
    "eligible",

    septic_shock_proxy == FALSE | is.na(septic_shock_proxy),
    "no septic shock or unknown")]

## 15 RECOMMENDATION -----------------------------------------------------------
rec15_label <- ssc_recommendations %>% 
  .[rec_num == 15, rec_label]

## Time between sepsis onset and ICU admission <= 6 hours.
## Primary measure (rec_15_icu_6h) matches the reference bundle exactly:
## icu_intime - suspected_infection_time <= 6h, counting in-ICU onset (negative
## difference) as adherent. Because ~45% of the cohort develops sepsis AFTER ICU
## admission -- for whom a 6h admission target is moot -- a second, intent-faithful
## variant (rec_15_icu_6h_preicu) restricts to patients whose onset is at or before
## ICU admission and marks in-ICU-onset stays non-evaluable (NA).
rec_15 <- sepsis_base_cohort_data %>%
  copy() %>%
  .[, hours_to_icu :=
      lubridate::time_length(icu_intime - suspected_infection_time,
                             unit = "hours")] %>%
  .[, rec_15_icu_6h := hours_to_icu <= 6] %>%
  ## intent-faithful: only patients with onset at/before ICU admission
  ## (in-ICU onset -> recommendation does not apply -> NA)
  .[, rec_15_icu_6h_preicu := fifelse(
    hours_to_icu >= 0,
    hours_to_icu <= 6,
    NA)] %>%
  .[, rec_15_eligibility := fcase(

    !is.na(rec_15_icu_6h),
    "eligible",

    default = NA_character_
  )]


## 16 & 17 RECOMMENDATION -----------------------------------------------------------
## Recommendations 16 and 17 share an identical measurable criterion --
## antimicrobial therapy within 1 hour of recognition -- and differ only in
## population: septic shock (16) vs probable/definite sepsis WITHOUT shock (17).
## The confirmed-Sepsis-3 cohort contains only probable/definite patients, so
## every stay falls under exactly one, split by septic_shock_proxy. They are
## encoded and reported SEPARATELY because the guideline separates them and
## adherence is expected to differ. (Recommendation 18 -- possible sepsis without
## shock, 3h target -- is a different population and is NOT encoded here; the 3h
## variables below are a sensitivity on the 16/17 population, not rec 18.)
##
## Antimicrobial timing uses the prescriptions-based first systemic antimicrobial
## (first_sepsis_antimicrobial, from rec 7), NOT the Sepsis-3 antibiotic_time:
## broader coverage (antibacterials + antifungals + antivirals), the true first
## dose, and it at least partly mitigates the circularity of measuring
## antimicrobial timing against a recognition time that is itself partly derived
## from antibiotic_time. Recognition = suspected_infection_time. The primary
## measure is ONE-SIDED (first antimicrobial no later than 1h AFTER recognition):
## because the recommendation concerns DELAY, earlier administration counts as
## adherent, and the antimicrobial is already episode-scoped by rec 7's
## [-24h, +72h] window. A strict two-sided [-1h, +1h] variant is retained as a
## sensitivity. NOTE: median time_to_antimicrobial is ~ +5h -- most patients
## receive antimicrobials well AFTER recognition, which is substantially a
## structural property of the culture-anchored Sepsis-3 onset (antimicrobials
## follow cultures under the culture->antibiotic pairing), not solely clinical
## delay; report stratified by culture- vs antibiotic-anchored onset (cf rec 7).

## Time from recognition to first systemic antimicrobial (shared by rec 16 & 17)
## onset_anchor records whether the Sepsis-3 recognition time coincides with the
## culture timestamp (culture-anchored) or the antibiotic timestamp
## (antibiotic-anchored). suspected_infection_time = COALESCE(culture, antibiotic),
## so for antibiotic-anchored stays the antimicrobial is close to recognition by
## construction -- this stratifier isolates how much of the low "within 1h" rate
## is a structural property of the onset definition vs clinical antimicrobial delay.
antimicrobial_timing <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        suspected_infection_time,
        culture_time,
        antibiotic_time)] %>%
  unique() %>%
  .[, onset_anchor := fcase(
    !is.na(culture_time) &
      suspected_infection_time == culture_time,
    "culture-anchored",

    !is.na(antibiotic_time) &
      suspected_infection_time == antibiotic_time,
    "antibiotic-anchored",

    default = "other/unresolved")] %>%
  merge(.,
        first_sepsis_antimicrobial[, .(subject_id,
                                       hadm_id,
                                       stay_id,
                                       antimicrobial_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, time_to_antimicrobial :=
      time_length(antimicrobial_time - suspected_infection_time,
                  unit = "hours")]

### 16 Recommendation 16 -------------------------------------------------------
## septic shock: antimicrobial within 1h of recognition
rec16_label <- ssc_recommendations %>%
  .[rec_num == 16, rec_label]

rec_16 <- antimicrobial_timing %>%
  copy() %>%
  ## PRIMARY: no delay beyond 1h after recognition (one-sided; early counts)
  .[septic_shock_proxy == TRUE,
    rec_16_antimicrobial_within_1h := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial <= 1] %>%
  ## SENSITIVITY: strict two-sided [-1h, +1h]
  .[septic_shock_proxy == TRUE,
    rec_16_antimicrobial_within_1h_strict := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial >= -1 &
      time_to_antimicrobial <= 1] %>%
  ## SENSITIVITY: 3h, one-sided (NOT rec 18)
  .[septic_shock_proxy == TRUE,
    rec_16_antimicrobial_within_3h := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial <= 3] %>%
  .[, rec_16_eligibility := fcase(

    !is.na(rec_16_antimicrobial_within_1h),
    "eligible",

    septic_shock_proxy == FALSE,
    "sepsis without shock (rec 17)",

    default = NA_character_)]

### 17 Recommendation ---------------------------------------------------------- 
## probable/definite sepsis WITHOUT shock:
## antimicrobial within 1h of recognition
rec17_label <- ssc_recommendations %>%
  .[rec_num == 17, rec_label]

rec_17 <- antimicrobial_timing %>%
  copy() %>%
  ## PRIMARY: no delay beyond 1h after recognition (one-sided; early counts)
  .[septic_shock_proxy == FALSE,
    rec_17_antimicrobial_within_1h := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial <= 1] %>%
  ## SENSITIVITY: strict two-sided [-1h, +1h]
  .[septic_shock_proxy == FALSE,
    rec_17_antimicrobial_within_1h_strict := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial >= -1 &
      time_to_antimicrobial <= 1] %>%
  ## SENSITIVITY: 3h, one-sided (NOT rec 18)
  .[septic_shock_proxy == FALSE,
    rec_17_antimicrobial_within_3h := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial <= 3] %>%
  .[, rec_17_eligibility := fcase(

    !is.na(rec_17_antimicrobial_within_1h),
    "eligible",

    septic_shock_proxy == TRUE,
    "septic shock (rec 16)",

    default = NA_character_)]

## xx 33 RECOMMENDATION -----------------------------------------------------------
rec33_label <- ssc_recommendations %>% 
  .[rec_num == 33, rec_label]

## This recommendation was not encoded because prolonged versus bolus beta-lactam
## infusion required administration-level infusion duration or rate information,
## which could not be reliably determined from the structured medication data available

## 42 RECOMMENDATION -----------------------------------------------------------
rec42_label <- ssc_recommendations %>% 
  .[rec_num == 42, rec_label]

## Item IDs corresponding to any blood pressure monitoring
blood_pressure_monitoring_itemids <- d_items %>% 
  .[grepl("blood pressure",
          label,
          ignore.case = TRUE) &
      category == "Routine Vital Signs"]

## Blood pressure monitoring events
bp_events <- all_chartevents %>% 
  ## only stay IDs from sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## blood pressure monitoring item IDs
  .[itemid %in% blood_pressure_monitoring_itemids$itemid] %>% 
  .[!is.na(valuenum) &
      ## remove obvious outliers
      valuenum > 0 &
      valuenum < 300] %>% 
  .[, .(stay_id,
        charttime,
        itemid,
        valuenum)]

## BP monitoring within the initial [-1h, +3h] of septic SHOCK onset. Anchored to
## shock onset (vasopressor-timed, in-ICU) rather than sepsis onset, which for a
## majority of stays precedes ICU admission where chartevents BP is not recorded
## -- otherwise a permissive, near-universally-met monitoring recommendation is
## depressed to a pure observability artefact.
rec_42 <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time)] %>%
  unique() %>%
  .[, bp_window_start := septic_shock_onset_time - lubridate::hours(1)] %>%
  .[, bp_window_end   := septic_shock_onset_time + lubridate::hours(3)] %>%
  merge(.,
        bp_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, bp_window :=
      charttime >= bp_window_start &
      charttime <= bp_window_end] %>% 
  .[, n_bp_window := sum(bp_window,
                         na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[bp_window == TRUE, .(subject_id,
                         hadm_id,
                         stay_id,
                         septic_shock_proxy,
                         bp_window,
                         n_bp_window)] %>%
  unique() %>% 
  ## add back full sepsis patient cohort
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    septic_shock_proxy)],
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "septic_shock_proxy"),
        all.x = FALSE,
        all.y = TRUE) %>% 
  .[is.na(bp_window),
    bp_window := FALSE] %>%
  .[is.na(n_bp_window),
    n_bp_window := 0] %>%
  .[septic_shock_proxy == TRUE, rec_42_bp_monitoring :=
      n_bp_window > 0] %>%
  .[, rec_42_eligibility := fcase(
    
    !is.na(rec_42_bp_monitoring),
    "eligible",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown"
  )]
  

## 43 RECOMMENDATION -----------------------------------------------------------
## What is the comprehensive list of IV fluids used for resuscitation in sepsis patients?
rec43_label <- ssc_recommendations %>% 
  .[rec_num == 43, rec_label]

## Non-crystalloid resuscitation fluids
## Find source / confirm this list
non_crystalloid_patterns <- c(
  "albumin",
  "hetastarch",
  "hydroxyethyl",
  "dextran",
  "gelatin")

## Item IDs for non-crystalloid resuscitation fluids
non_crystalloid_itemids <- d_items %>% 
  .[linksto == "inputevents"] %>% 
  .[grepl(paste(non_crystalloid_patterns,
                collapse = "|"),
          label,
          ignore.case = TRUE)] %>% 
  .[!grepl("ingredient|OR|PACU",
           label,
           ignore.case = TRUE)]

## Resuscitation fluids
resuscitation_fluid_itemids <- rbindlist(
  list(
    crystalloid_itemids[, .(
      itemid,
      fluid_label = label,
      fluid_type = "Crystalloid"
    )],
    non_crystalloid_itemids[, .(
      itemid,
      fluid_label = label,
      fluid_type = "Colloid"
    )]
  ),
  fill = TRUE
)

## IV fluid resuscitation events
resuscitation_fluid_events <- all_inputevents %>% 
  ## patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## IV resuscitation fluids item IDs
  .[itemid %in% resuscitation_fluid_itemids$itemid] %>%
  # .[!is.na(amount) & amount >= 250] %>% 
  merge(.,
        resuscitation_fluid_itemids,
        by = "itemid",
        all.x = TRUE) %>% 
  .[, .(stay_id,
       fluid_start = starttime,
       fluid_end = endtime,
       fluid_rate = rate,
       rate_unit = rateuom,
       fluid_amount = amount,
       fluid_unit = amountuom,
       fluid_label,
       fluid_type)]

## Check whether fluid rate and amount are plausible
quantile(resuscitation_fluid_events$fluid_rate,
         probs = c(0.5, 0.75, 0.9, 0.95, 0.99),
                   na.rm = TRUE)

quantile(resuscitation_fluid_events$fluid_amount,
         probs = c(0.5, 0.75, 0.9, 0.95, 0.99),
         na.rm = TRUE)

## First line resuscitation fluid
first_line_fluid <- resuscitation_fluid_events %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time,
                                    fluid_resuscitation_start,
                                    fluid_resuscitation_end)],
        by = "stay_id",
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  .[fluid_start >= fluid_resuscitation_start &
      fluid_start <= fluid_resuscitation_end + hours(12)] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          fluid_start),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_fluid_time = fluid_start,
        first_fluid_type = fluid_type,
        first_fluid_rate = fluid_rate,
        first_fluid_amount = fluid_amount)] %>% 
  unique()

rec_43 <- sepsis_base_cohort_data %>% 
  merge(.,
        first_line_fluid,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, rec_43_crystalloid_first_line := fcase(
    
    !is.na(first_fluid_time) &
      first_fluid_type == "Crystalloid",
    TRUE,
    
    !is.na(first_fluid_time) &
      first_fluid_type == "Colloid",
    FALSE,
    
    default = NA)] %>% 
  .[, rec_43_eligibility := fcase(
    
    !is.na(first_fluid_time),
    "eligible",
    
    is.na(first_fluid_time),
    "no initial fluid resuscitation within 3h",
    
    default = NA_character_)]
   

## 44 & 45 RECCOMENDATION -----------------------------------------------------------
rec44_label <- ssc_recommendations %>% 
  .[rec_num == 44, rec_label]

rec_45_label <- ssc_recommendations %>% 
  .[rec_num == 45, rec_label]

## Define balanced crystalloid item IDs
balanced_crystalloid_itemids <- crystalloid_itemids %>% 
  .[grepl("LR|Normosol",
          label,
          ignore.case = TRUE)]

## Define 0.9% Saline item ID
saline_crystalloid_itemids <- crystalloid_itemids %>% 
  .[grepl("Saline|NaCl",
          label,
          ignore.case = TRUE)]

## Albumin administration item IDs
albumin_itemids <- d_items %>% 
  .[category %in% c("Blood Products/Colloids",
                    "Fluids - Other (Not In Use)")] %>% 
  .[grepl("albumin",
          label,
          ignore.case = TRUE)]


## Resuscitation IV Fluid Intake
resuscitation_fluid_events <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Resuscitation IV fluid item IDs
  .[itemid %in% c(crystalloid_itemids$itemid,
                  albumin_itemids$itemid)] %>% 
  .[,
    fluid_type := ifelse(itemid %in% balanced_crystalloid_itemids$itemid,
                               "Balanced",
                               ifelse(
                                 itemid %in% saline_crystalloid_itemids$itemid,
                                 "0.9% Saline",
                                 ifelse(
                                   itemid %in% albumin_itemids$itemid,
                                   "Albumin",
                                   "Other/Unknown Crystalloid")
                               ))]

## Total IV resuscitation fluids administered within initital resuscitation window
iv_fluids_initial_window <- sepsis_base_cohort_data %>% 
  merge(.,
        resuscitation_fluid_events,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## define initital resuscitation window as the first 3 hours after 
  ## sepsis onset and 1 hour before to account for delayed documentation
  ## define crystalloid administration within initial resuscitation window
  .[starttime < fluid_resuscitation_end &
      endtime > fluid_resuscitation_start, `:=` (overlap_start = pmax(starttime,
                                 fluid_resuscitation_start),
            overlap_end = pmin(endtime,
                               fluid_resuscitation_end))] %>% 
  .[, overlap_time := ifelse(rateuom == "mL/hour",
                             time_length(overlap_end -
                                                      overlap_start,
                                                    unit = "hours"),
                             ifelse(rateuom == "mL/min",
                                    time_length(overlap_end - 
                                                             overlap_start,
                                                           unit = "minutes"),
                                    NA_real_))] %>% 
  .[, fluid_ml_initial_window := overlap_time * rate] %>% 
  .[, total_fluid_initial_window := sum(fluid_ml_initial_window, 
                               na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id,
           fluid_type)] %>% 
  .[, .(subject_id,
        hadm_id, 
        stay_id,
        fluid_type,
        total_fluid_initial_window)] %>% 
  unique()

  
## Define Rec 44 as more Balanced crystalloid than 0.9% Saline within first 6h
rec_44 <- iv_fluids_initial_window %>% 
  copy() %>% 
  .[is.na(fluid_type),
    fluid_type := "no crystalloid"] %>%
  dcast(subject_id + 
          hadm_id +
          stay_id  ~ fluid_type,
          value.var = "total_fluid_initial_window",
      fill = 0) %>% 
  .[, rec_44_balanced_over_saline := fifelse(
    Balanced > 0 &
      Balanced > `0.9% Saline`,
    TRUE,
    fifelse(
      `0.9% Saline` > 0 &
        Balanced <= `0.9% Saline`,
      FALSE,
      NA))] %>% 
  .[, rec_44_eligibility := fcase(
    
    !is.na(rec_44_balanced_over_saline),
    "eligible",
    
    Balanced == 0 & `0.9% Saline` == 0,
    "no crystalloid fluids within initital resuscitation window"
  )]


rec_45 <- iv_fluids_initial_window %>% 
  copy() %>% 
  .[is.na(fluid_type),
    fluid_type := "no crystalloid"] %>%
  dcast(subject_id + 
          hadm_id +
          stay_id  ~ fluid_type,
        value.var = "total_fluid_initial_window",
        fill = 0) %>% 
  .[, rec_45_crystalloid_no_albumin := fifelse(
    (Balanced > 0 |
      `0.9% Saline` > 0) &
      Albumin == 0,
    TRUE,
    fifelse(
      (Balanced > 0 |
        `0.9% Saline` > 0) &
        Albumin > 0,
      FALSE,
      NA))] %>% 
  .[, rec_45_eligibility := fcase(
    
    !is.na(rec_45_crystalloid_no_albumin),
    "eligible",
    
    Balanced == 0 & `0.9% Saline` == 0,
    "no crystalloid fluids within initital resuscitation window"
  )]
 
# 45 RECOMMENDATION
# rec45_label <- ssc_recommendations %>% 
#   copy() %>% 
#   .[rec_num == 45, rec_label]
# 
# ## Albumin administration item IDs
# albumin_itemids <- d_items %>% 
#   copy() %>% 
#   .[category %in% c("Blood Products/Colloids",
#                     "Fluids - Other (Not In Use)")] %>% 
#   .[grepl("albumin",
#           label,
#           ignore.case = TRUE)]
# 
# ## Albumin and Crystalloid administration
# albumin_crystalloid_events <- all_inputevents %>% 
#   copy() %>%
#   ## only patients in sepsis base cohort
#   .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
#   ## Albumin item IDs
#   .[itemid %in% crystalloid_itemids$itemid |
#       itemid %in% albumin_itemids$itemid] %>% 
#   .[, iv_resuscitation := fifelse(
#     itemid %in% crystalloid_itemids$itemid,
#     "Crystalloid",
#     fifelse(
#       itemid %in% albumin_itemids$itemid,
#       "Albumin",
#       NA_character_
#     )
#   )]
# 
# ## Crystallid administration within 24 hours without any albumin administration
# rec_45 <- sepsis_base_cohort_data %>% 
#   copy() %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         suspected_infection_time,
#         first_vaso_time)] %>% 
#   unique() %>% 
#   merge(.,
#         albumin_crystalloid_events,
#         by = "stay_id", 
#         all.x = TRUE,
#         all.y = FALSE)
#   .[starttime >= suspected_infection_time &
#       starttime <= suspected_infection_time + hours(24)] %>% 
#   .[, .(any_crystalloid = any(iv_resuscitation == "Crystalloid"),
#         any_albumin = any(iv_resuscitation == "Albumin"),
#         first_fluid_time = min(starttime,
#                                na.rm = TRUE)),
#     by = .(subject_id,
#            hadm_id,
#            stay_id)] %>% 
#   ## adherance can only be determined if crystalloids were administered
#   .[any_crystalloid == TRUE] %>% 
#   .[, rec_45_crystalloid_without_albumin := any_crystalloid == TRUE &
#       any_albumin == FALSE]
  

## 53 RECOMMENDATION -----------------------------------------------------------
rec53_label <- ssc_recommendations %>% 
  .[rec_num == 53, rec_label]

## First line vasopressor Norepinephrine over Dopamine or Epinephrine
## If first line vasopressor not Norepinephrine, Dopamine, or Epinephrine,
## NA instead of non-adherance 

first_vasopressor <- sepsis_base_cohort_data %>% 
  merge(.,
        all_vasopressorevents[, .(stay_id,
                                  vaso_start,
                                  vaso_end,
                                  label = vaso_label,
                                  amount = vaso_amount,
                                  rate = vaso_rate)],
                               
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## In patients with septic shock, vasopressors should be administered
  ## after sepsis onset, however, we consider up to 1 hour before sepsis 
  ## onset to account for delayed documentation
  .[vaso_start >= suspected_infection_time - hours(1)] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          vaso_start),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        septic_shock_proxy,
        vaso_start,
        vaso_end,
        first_vaso_amount = amount,
        first_vaso_rate = rate,
        first_vaso_type = label)] %>% 
  unique()

rec_53 <- sepsis_base_cohort_data %>% 
  copy %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        suspected_infection_time)] %>% 
  merge(.,
        first_vasopressor,
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "suspected_infection_time",
               "septic_shock_proxy"),
        all = TRUE) %>% 
  .[, rec_53_first_vaso_norepi := fifelse(
    grepl("Norepinephrine",
          first_vaso_type,
          ignore.case = TRUE),
    TRUE,
    ifelse(
      grepl("Epinephrine|Dopamine",
            first_vaso_type,
            ignore.case = TRUE),
      FALSE,
      NA
    )
  )] %>% 
  .[, rec_53_eligbility := fcase(
    
    !is.na(rec_53_first_vaso_norepi),
    "eligible",
    
    septic_shock_proxy == TRUE &
      !is.na(first_vaso_type) &
      is.na(rec_53_first_vaso_norepi),
    "other vasoporessor administered",
    
    septic_shock_proxy == FALSE | is.na(septic_shock_proxy),
    "no septic shock or unknown"
  )]


## 55 RECOMMENDATION -----------------------------------------------------------
rec55_label <- ssc_recommendations %>% 
  .[rec_num == 55, rec_label]

## First line vasopressor Norepinephrine over Vasopressin or Angiotensin II
## If first line vasopressor not Norepinephrine, Vasopressin, or Angiotensin II,
## NA instead of non-adherance 
# first_vaso <- all_vasopressorevents %>% 
#   copy() %>% 
#   .[order(stay_id,
#           vaso_start),
#     .SD[1],
#     by = stay_id]

first_vaso <- vasopressor_events_shock %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    septic_shock_proxy,
                                    septic_shock_onset_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>% 
  .[septic_shock_proxy == TRUE &
      vaso_start >= septic_shock_onset_time] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          vaso_start),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)]

rec_55 <- sepsis_base_cohort_data %>% 
  copy %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time)] %>% 
  merge(.,
        first_vaso,
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "septic_shock_onset_time",
               "septic_shock_proxy"),
        all = TRUE) %>% 
  .[, rec_55_first_vaso_norepi := fifelse(
    grepl("Norepinephrine",
          vaso_label,
          ignore.case = TRUE),
    TRUE,
    ifelse(
      grepl("Vasopressin|Angiotensin",
            vaso_label,
            ignore.case = TRUE),
      FALSE,
      NA
    )
  )] %>% 
  .[, rec_55_eligbility := fcase(
    
    !is.na(rec_55_first_vaso_norepi),
    "eligible",
    
    septic_shock_proxy == TRUE &
      !is.na(vaso_label) &
      is.na(rec_55_first_vaso_norepi),
    "other vasoporessor administered",
    
    septic_shock_proxy == FALSE | is.na(septic_shock_proxy),
    "no septic shock or unknown"
  )]


## 56 RECOMMENDATION -----------------------------------------------------------
rec56_label <- ssc_recommendations %>% 
  .[rec_num == 56, rec_label]

## SSC 2026 describes adding vasopressin in patients with septic shock on 
## escalating doses of norepinephrine. "Escalating doses" of Norepinephrine
## are considered as median dose of 0.3 μg/kg/min of norepinephrine 
## (IQR 0.2–0.5 µg/kg/min). This dosing is similar to a recent observational 
## study in which a reinforcement machine learning model suggested initiating 
## vasopressin at a median 0.20 μg/kg/min (IQR 0.08–0.45 μg/kg/min), as 
## compared with clinician initiation of vasopressin at a median 0.37 μg/kg/min 
## (IQR 0.17–0.69 μg/kg/min) (428).

## Item ID for Norepinephrine
norepi_itemid <- d_items %>% 
  .[grepl("Norepinephrine",
          label,
          ignore.case = TRUE)]

## Escalating doses of Norepinephrine
norepi_escalating_doses <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Norepinephrine item ID
  .[itemid == norepi_itemid$itemid] %>% 
  .[, norepi_dose_mcg_kg_min := fcase(
    rateuom == "mcg/kg/min", rate,
    rateuom == "mg/kg/min", rate * 1000,
    default = NA_real_
  )] %>% 
  .[, norepi_escalating_dose := fifelse(
    norepi_dose_mcg_kg_min >= 0.3,
    TRUE,
    fifelse(
      norepi_dose_mcg_kg_min < 0.3,
      FALSE,
      NA
    )
  )] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## first escalating norepinephrine dose after sepsis onset
  ## up to 1 hour before sepsis onset to account for delayed documentation
  .[norepi_escalating_dose == TRUE &
      starttime >= suspected_infection_time -
      hours(1)] %>% 
  ## first escalating dose per patient
  .[order(stay_id,
          starttime),
    .SD[1],
    by = stay_id] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        norepi_first_escalated = starttime,
        norepi_dose_mcg_kg_min,
        norepi_escalating_dose)] %>% 
  unique()

## Vasopressin item ID
vasopressin_itemid <- d_items %>% 
  .[grepl("Vasopressin",
          label,
          ignore.case = TRUE)]

## Vasopressin administration events
vasopressin_events <- all_vasopressorevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
## vasopressin item ID
  .[vaso_label == "Vasopressin"] %>% 
  .[!is.na(vaso_amount) & vaso_amount > 0 &
      !is.na(vaso_rate) & vaso_rate > 0] %>% 
  .[, .(stay_id,
        vasopressin_start = vaso_start)] %>% 
  unique() %>% 
  setorder(stay_id,
           vasopressin_start)

## Define vasopressin added after escalating doses of Norepinephrine
vasopressin_afer_escalating_norepi <- norepi_escalating_doses %>% 
  merge( .,
      vasopressin_events,
      by = "stay_id",
      all.x = TRUE,
      all.y = FALSE) %>% 
  .[, vasopressin_after_escalating_norepi := 
      vasopressin_start >= norepi_first_escalated -
      ## account for delayed documentation
      hours(1) &
      ## restrict to at most 6h after escalated norepinephrine
      vasopressin_start <= norepi_first_escalated +
      hours(6)] %>% 
  .[, vasopressin_after_escalating_norepi := fifelse(
    !is.na(vasopressin_after_escalating_norepi),
           vasopressin_after_escalating_norepi,
           fifelse(all(is.na(vasopressin_after_escalating_norepi)),
                   FALSE,
                   NA)),
    by = stay_id] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          vasopressin_start),
    .SD[1],
     by = stay_id]


rec_56 <- sepsis_base_cohort_data %>% 
  merge(.,
        vasopressin_afer_escalating_norepi,
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "suspected_infection_time"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        septic_shock_proxy,
        norepi_escalating_dose,
        norepi_first_escalated,
        norepi_dose_mcg_kg_min,
        vasopressin_after_escalating_norepi,
        vasopressin_start)] %>% 
  .[, rec_56_vasopressin_after_escalating_norepi :=
      fifelse(vasopressin_after_escalating_norepi == TRUE,
             TRUE,
             fifelse(
               vasopressin_after_escalating_norepi == FALSE,
               FALSE,
               NA
             ))] %>% 
  .[, rec_56_eligibility := fcase(
    !is.na(rec_56_vasopressin_after_escalating_norepi),
    "eligible",
    
    septic_shock_proxy == TRUE &
      (is.na(norepi_escalating_dose) |
      norepi_escalating_dose == FALSE),
    "no escalated dose of Norepinephrine",
    
    septic_shock_proxy == FALSE | is.na(septic_shock_proxy),
    "no septic shock or unknown"
  )]

# ## Define recommendation as vasopressor administration after first 
# ## escalating dose of Norepinephrine
# rec_56 <- norepi_escalating_doses %>%
#   copy() %>% 
#   merge(.,
#           sepsis_base_cohort_data[, .(subject_id,
#                                       hadm_id,
#                                       stay_id,
#                                       septic_shock_proxy,
#                                       septic_shock_onset_time)] %>% unique(),
#           by = "stay_id",
#           all.x = TRUE,
#           all.y = FALSE) %>% 
#   .[septic_shock_proxy == TRUE &
#       starttime >= septic_shock_onset_time &
#       norepi_escalating_dose == TRUE] %>% 
#   ## select first escalated dose after septic shock onset
#   setorder(stay_id,
#            starttime) %>% 
#   .[, .SD[1],
#     by = .(subject_id,
#            hadm_id,
#            stay_id)] %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         ne_escalation_time = starttime,
#         norepi_dose_mcg_min,
#         norepi_escalating_dose)] %>% 
#   unique() %>% 
#   merge(.,
#         vasopressin_events[, .(stay_id,
#                                vasopressin_time = starttime,
#                                received_vasopressin)],
#         by = "stay_id",
#         all.x = TRUE,
#         all.y = FALSE,
#         allow.cartesian = TRUE) %>% 
#   .[is.na(vasopressin_time) | vasopressin_time >= ne_escalation_time] %>% 
#   setorder(stay_id,
#            vasopressin_time) %>% 
# ## select first vasopressin administration after norepinephrine escalating dose
#   .[, .SD[1],
#      by = .(subject_id,
#             hadm_id,
#             stay_id)] %>% 
#   .[, rec_56_vasopressin_after_norepi := !is.na(vasopressin_time) &
#       vasopressin_time >= ne_escalation_time]
#   

## 57 RECOMMENDATION -----------------------------------------------------------
## This recommendation applies to patients with septic shock and inadequate MAP
## levels despite norepinehrine and vasopressin. However, the guideline does not
## define what consitutes "inadequate MAP" or "escalating doses of norepinephrine",
## nor does it specify a quantitative norepinephrine threshold for initiating
## epinephrine. To enable reproducible implentation in MIMIC-IV, escalating norepi
## therapy was defined using the expert panel's reported clinical practice. The
## SSC 2026 panel reported initiating epinephrine at a median norepinephrine dose
## of 0.8µg/kg/min (IQR 0.5-1 µg/kg/min). This value was therefore used as the threshold indicating
## escalating norepinephrine therapy.

## Persistent inadequate MAP could not be operationalized reliably because the 
## guideline does not specify a duration or measurement frequency for hypotension. 
## Mean arterial pressure measurements in MIMIC-IV are intermittent and vasopressor 
## doses are frequently titrated between measurements. Applying an additional MAP 
## criterion would therefore require an arbitrary definition that is not supported 
## by the guideline. Consequently, the norepinephrine dose threshold reported by 
## the SSC expert panel was used as the primary indicator of escalating vasopressor 
## requirements.

## Where the SSC 2026 recommendations contained qualitative terms (e.g., "escalating 
## doses of norepinephrine") without explicit operational definitions, these were 
## translated into reproducible variables using the quantitative practice patterns 
## reported by the SSC expert panel whenever available.
rec57_label <- ssc_recommendations %>% 
  .[rec_num == 57, rec_label]

## Item ID for Norepinephrine
norepi_itemid <- d_items %>% 
  .[grepl("Norepinephrine",
          label,
          ignore.case = TRUE)]

## Escalating doses of Norepinephrine >= 0.8µg/kg/min
norepi_escalating_doses_0.8 <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Norepinephrine item ID
  .[itemid == norepi_itemid$itemid] %>% 
  .[, norepi_dose_mcg_kg_min := fcase(
    rateuom == "mcg/kg/min", rate,
    rateuom == "mg/kg/min", rate * 1000,
    default = NA_real_
  )] %>% 
  .[, norepi_escalating_dose_0.8 := fifelse(
    norepi_dose_mcg_kg_min >= 0.8,
    TRUE,
    fifelse(
      norepi_dose_mcg_kg_min < 0.8,
      FALSE,
      NA
    )
  )] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## first escalating norepinephrine dose >= 0.8µg/kg/min after sepsis onset
  ## up to 1 hour before sepsis onset to account for delayed documentation
  .[norepi_escalating_dose_0.8 == TRUE &
      starttime >= suspected_infection_time -
      hours(1)] %>% 
  ## first escalating dose per patient
  .[order(stay_id,
          starttime),
    .SD[1],
    by = stay_id] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        norepi_first_escalated = starttime,
        norepi_dose_mcg_kg_min,
        norepi_escalating_dose_0.8)] %>% 
  unique()

## Epinephrine item ID
epinephrine_itemids <- d_items %>% 
  .[grepl("Epinephrine",
          label,
          ignore.case = TRUE) &
      !grepl("Norepinephrine",
             label,
             ignore.case = TRUE)]

## Epinephrine administration events
epinephrine_events <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## epinpehrine item ID
  .[itemid %in% epinephrine_itemids$itemid] %>% 
  .[!is.na(amount) & amount > 0 &
      !is.na(rate) & rate > 0] %>% 
  .[, .(stay_id,
        epi_start = starttime)] %>% 
  unique() %>% 
  setorder(stay_id,
           epi_start)

## Define Epinephrine added after escalating doses 
## of Norepinephrine >= 0.8µg/kg/min
epi_after_escalating_norepi_vaso <- norepi_escalating_doses_0.8 %>% 
  merge(.,
        vasopressin_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  merge(.,
         epinephrine_events,
         by = "stay_id",
         all.x = TRUE,
         all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  .[, rec_57_trigger_time := pmax(norepi_first_escalated,
                                  vasopressin_start,
                                  ## patients without vasopressin are not 
                                  ## eligible and should not get a trigger time
                                  na.rm = FALSE)] %>% 
  .[, epi_after_escalating_norepi_vaso := 
      !is.na(epi_start) &
      !is.na(rec_57_trigger_time) &
      epi_start >= rec_57_trigger_time -
      ## account for delayed documentation
      hours(1) &
      ## restrict to at most 6h after escalated norepinephrine
      epi_start <= rec_57_trigger_time +
      hours(6)] %>% 
  .[, epi_after_escalating_norepi_vaso := fifelse(
    !is.na(epi_after_escalating_norepi_vaso),
    epi_after_escalating_norepi_vaso,
    fifelse(all(is.na(epi_after_escalating_norepi_vaso)),
            FALSE,
            NA)),
    by = stay_id] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          epi_start),
    .SD[1],
    by = stay_id]


rec_57 <- sepsis_base_cohort_data %>% 
  merge(.,
        epi_after_escalating_norepi_vaso,
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "suspected_infection_time"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        septic_shock_proxy,
        norepi_escalating_dose_0.8,
        norepi_first_escalated,
        norepi_dose_mcg_kg_min,
        vasopressin_start,
        rec_57_trigger_time,
        epi_after_escalating_norepi_vaso,
        epi_start)] %>% 
  .[, rec_57_epi_after_escalating_norepi_vaso :=
      fifelse(epi_after_escalating_norepi_vaso == TRUE,
              TRUE,
              fifelse(
                epi_after_escalating_norepi_vaso == FALSE,
                FALSE,
                NA
              ))] %>% 
  .[, rec_57_eligibility := fcase(
    !is.na(rec_57_epi_after_escalating_norepi_vaso),
    "eligible",
    
    septic_shock_proxy == TRUE &
      (is.na(norepi_escalating_dose_0.8) |
         norepi_escalating_dose_0.8 == FALSE),
    "no escalated dose of Norepinephrine",
    
    septic_shock_proxy == TRUE &
      !is.na(norepi_first_escalated) &
      is.na(vasopressin_start),
    "no Vasopressin",
    
    septic_shock_proxy == FALSE | is.na(septic_shock_proxy),
    "no septic shock or unknown"
  )]

# ## First Norepinephrine administration after septic shock onset
# first_norepi <- vasopressor_events %>% 
#   copy() %>% 
#   merge(.,
#         vasopressor_lookup,
#         by = "itemid",
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[label == "Norepinephrine"] %>% 
#   merge(sepsis_base_cohort_data[, .(subject_id,
#                                     hadm_id,
#                                     stay_id,
#                                     septic_shock_proxy,
#                                     septic_shock_onset_time)] %>% unique(),
#         by = "stay_id",
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[starttime >= septic_shock_onset_time &
#       amount > 0 &
#       rate > 0] %>% 
#   .[, first_norepi := min(starttime,
#                         na.rm = TRUE),
#     by = .(subject_id,
#            hadm_id,
#            stay_id)]
# 
# ## First Vasopressin administration after septic shock onset
# first_vasopressin <- vasopressor_events %>% 
#   copy() %>% 
#   merge(.,
#         vasopressor_lookup,
#         by = "itemid",
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[label == "Vasopressin"] %>% 
#   merge(sepsis_base_cohort_data[, .(subject_id,
#                                     hadm_id,
#                                     stay_id,
#                                     septic_shock_proxy,
#                                     septic_shock_onset_time)] %>% unique(),
#         by = "stay_id",
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[starttime >= septic_shock_onset_time &
#       amount > 0 &
#       rate > 0] %>% 
#   .[, first_vasopressin := min(starttime,
#                           na.rm = TRUE),
#     by = .(subject_id,
#            hadm_id,
#            stay_id)]
# 
# ## Administration of Norepinephrine & Vasopressin after septic shock onset
# norepi_and_vasopressin <- 
# merge(first_norepi %>% 
#   copy() %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         first_norepi)] %>% 
#   unique(),
# first_vasopressin %>% 
#   copy() %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         first_vasopressin)] %>% 
#   unique(),
# by = c("subject_id",
#        "hadm_id",
#        "stay_id"),
# all = FALSE) %>% 
#   .[, norepi_and_vasopressin_time := max(first_norepi,
#                                        first_vasopressin,
#                                        na.rm = TRUE),
#     by = .(subject_id,
#            hadm_id,
#            stay_id)] %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         norepi_and_vasopressin_time)] %>% 
#   unique()
# 
# ## Persistent hypotension (MAP < 65) after Norepinephrine & Vasopressor administration
# persistent_hypotension <- norepi_and_vasopressin %>% 
#   copy() %>% 
#   merge(.,
#         map_events,
#         by = "stay_id",
#         all = FALSE,
#         allow.cartesian = TRUE) %>% 
#   .[chart_hour >= lubridate::floor_date(norepi_and_vasopressin_time,
#                                         "hour") &
#       chart_hour  <= lubridate::floor_date(norepi_and_vasopressin_time + hours(6),
#                                            "hour")] %>% 
#   .[, `:=` (persistent_hypotension = any(map < 65,
#                                      na.rm = TRUE),
#         median_map_6h = median(map, 
#                                na.rm = TRUE),
#         min_map_6h = min(map,
#                          na.rm = TRUE)),
#     by = .(subject_id,
#            hadm_id,
#            stay_id)] %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         persistent_hypotension,
#         median_map_6h,
#         min_map_6h,
#         norepi_and_vasopressin_time)] %>% 
#   unique()
# 
# ## Epinephrine administration
# first_epi_after_shock <- vasopressor_events %>% 
#   copy() %>% 
#   merge(vasopressor_lookup,
#         by = "itemid",
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[label == "Epinephrine"] %>% 
#   merge(.,
#         sepsis_base_cohort_data[, .(subject_id,
#                                     hadm_id,
#                                     stay_id,
#                                     septic_shock_proxy,
#                                     septic_shock_onset_time)] %>% unique(),
#         by = "stay_id",
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[septic_shock_proxy == TRUE &
#       starttime >= septic_shock_onset_time &
#       amount > 0 &
#       rate > 0] %>% 
#   .[, .(first_epinephrine = min(starttime, 
#                                 na.rm = TRUE)),
#         by = .(subject_id,
#                hadm_id,
#                stay_id)]
# 
# ## Define recommendation as persistent hypotension (MAP < 65) within 6 hours
# ## after both Norepinephrine & Vasopressin administration after septic shock onset
# rec_57 <- persistent_hypotension %>% 
#   copy() %>% 
#   .[persistent_hypotension == TRUE] %>% 
#   merge(.,
#         first_epi_after_shock,
#         by = c("subject_id",
#                "hadm_id",
#                "stay_id"),
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[, rec_57_epinephrine_added := !is.na(first_epinephrine) &
#       first_epinephrine >= norepi_and_vasopressin_time]


## 67 RECOMMENDATION -----------------------------------------------------------
rec67_label <- ssc_recommendations %>% 
  .[rec_num == 67, rec_label]

## AHRF: broad sepsis-related hypoxemic respiratory failure
## ARDS: stricter subset of AHRF with Global ARDS criteria

## The 2023 Global ARDS definition supports including HFNO >=30 L/min,
## P/F <= 300 or S/F <= 315 when SpO2 <= 97%, bilateral opacities, and
## acute timing; Berlin remains the conceptual backbone for timing, imaging,
## and noncardiogenic edema logic. AHRF can be operationalized more broadly
## as hypoxemia on oxygen therapy, commonly using P/F <= 300 in patients
## receiving oxygen suuport.

## Respiratory support therapy patterns
resp_support_patterns <- c("oxygen device",
                             "o2 device",
                             "o2 delivery device",
                             "delivery device",
                             "high flow",
                             "nasal cannula",
                             "cannula",
                             "face tent",
                             "aerosol mask",
                             "venturi",
                             "non-rebreather",
                             "partial rebreather",
                             "simple mask",
                             "mask",
                             "high flow",
                             "hfnc",
                             "high-flow")

## Determine oxygen therapy item IDs
resp_support_itemids <- chart_itemids %>% 
  .[grepl(paste0(resp_support_patterns,
                 collapse = "|"),
          label,
          ignore.case = TRUE) &
      !grepl("O2 Flow",
             label,
             ignore.case = TRUE)]

## Respiratory support events
resp_support_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## resp support item IDs
  .[itemid %in% resp_support_itemids$itemid] %>%
  ## add item id names
  merge(.,
        resp_support_itemids[, .(itemid,
                                 label)],
        by = "itemid") %>% 
  .[, .(stay_id,
        resp_support_time = charttime,
        resp_support_device = label,
        ## Remove leading and trailing spaces
        resp_support = trimws(value),
        valuenum,
        valueuom)] %>%  
  .[, resp_support_type := fcase(
    
    resp_support == "High flow nasal cannula",
    "HFNC",
    
    resp_support %in% c("Nasal cannula",
                        "Nasal",
                        "Face tent",
                        "Venti mask",
                        "Non-rebreather",
                        "Medium conc mask",
                        "Oxymizer"),
    "Conventional oxygen",
    
    resp_support %in% c("CPAP mask",
                        "Bipap mask",
                        "Full",
                        "Oral/Nasal",
                        "Pillows",
                        "Total face"),
    "NIV",
    
    resp_support %in% c("Endotracheal tube",
                        "Tracheostomy tube"),
    "Invasive ventilation",
    
    resp_support %in% c("Trach mask",
                        "T-piece",
                        "Aerosol-cool",
                        ## classify as other oxygen because
                        ## it is not clearly high-flow nasal oxygen
                        "High flow neb",
                        "Ultrasonic neb",
                        "Vapomist"),
    "Other oxygen",
    
    resp_support %in% c("None",
                        "Not applicable"),
    "None",
    
    resp_support == "Other",
    "Other",
    
    default = NA_character_)]

## Check that there are no missing values for resp_support_type
stopifnot(nrow(resp_support_events[is.na(resp_support_type)]) == 0)

## Define respiratory support for AHRF and ARDS
resp_support_events <- resp_support_events %>% 
  copy() %>% 
  .[, oxygen_therapy :=
      resp_support_type %in% c("Conventional oxygen",
                               "HFNC",
                               "NIV",
                               "Invasive ventilation",
                               "Other oxygen")] %>% 
  .[, ards_resp_support :=
      resp_support_type %in% c("HFNC",
                               "NIV",
                               "Invasive ventilation")]

setkey(resp_support_events,
       stay_id,
       resp_support_type)

## Add oxygen flow
flow_patterns <- c(
  "O2 Flow",
  "Flow Rate",
  "O2 Flow (additional cannula)",
  "BiPap O2 Flow")

## Oxygen flow item IDs
flow_itemids <- chart_itemids %>% 
  copy() %>% 
  .[, label := trimws(label)] %>% 
  .[grepl(paste(flow_patterns,
                collapse = "|"),
          label,
          ignore.case = TRUE) &
      !grepl("impella|purge|variable|peak",
             label,
             ignore.case = TRUE)]

## Oxygen flow events
resp_flow_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## oxygen flow item IDs
  .[itemid %in% flow_itemids$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  ## remove obvious outliers
  .[valuenum >= 0 &
      valuenum <= 100] %>% 
  merge(.,
        flow_itemids[, .(itemid,
                         label)],
        by = "itemid") %>% 
  .[, .(stay_id,
        flow_time = charttime,
        itemid,
        flow_label = label,
        flow_rate = valuenum)]

## Check which flow variables correspond to HFNC
flow_dt <- resp_flow_events %>% 
  copy() %>% 
  .[, time := flow_time]

resp_dt <- resp_support_events %>% 
  copy() %>% 
  .[, time := resp_support_time]

setkey(flow_dt,
       stay_id,
       time)

setkey(resp_dt,
       stay_id,
       time)

## Nearest respiratory support device per oxygen flow entry
flow_support_match <- resp_dt[
  flow_dt,
  on = .(stay_id,
         time),
  roll = "nearest"] %>% 
  .[, time_diff_min := abs(
    lubridate::time_length(
      resp_support_time -
        flow_time,
      "mins"))] %>% 
  .[time_diff_min <= 30]

## Add flow rate to respiratory support events
resp_support_events <- resp_support_events %>% 
  merge(.,
        flow_support_match[, .(stay_id,
                               resp_support_time,
                               resp_support_device,
                               resp_support,
                               flow_time,
                               flow_label,
                               flow_rate,
                               flow_time_diff_min = time_diff_min)],
        by = c("stay_id",
               "resp_support_time",
               "resp_support_device",
               "resp_support"),
        all.x = TRUE,
        all.y = FALSE)

## Item ID for PaO2 (partial pressure of oxygen in arterial blood)
pao2_itemid <- chart_itemids %>% 
  .[grepl("PaO2|PO2",
          abbreviation,
          ignore.case = TRUE) &
      grepl("arterial",
            label,
            ignore.case = TRUE) &
      category == "Labs"]

## PaO2 events
pao2_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## PaO2 item ID
  .[itemid == pao2_itemid$itemid] %>% 
  .[, .(stay_id,
        pao2_time = charttime,
        pao2 = valuenum,
        pao2_unit = valueuom)]

## Analyze for outliers
summary(pao2_events$pao2)

## Check distribution
quantile(pao2_events$pao2,
         probs = c(0.01, 0.1, 0.5, 0.75, 0.9, 0.95, 0.99),
         na.rm = FALSE)

## Remove obvious outliers based on distribution
pao2_events <- pao2_events %>% 
  .[pao2 >= 30 &
      pao2 <= 600]

## Item ID for FiO2 (fraction of inspired oxygen)
fio2_itemid <- chart_itemids %>% 
  .[grepl("Inspired O2 Fraction",
          label,
          ignore.case = TRUE)]

## FiO2 events
fio2_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## FiO2 item ID
  .[itemid == fio2_itemid$itemid] %>% 
  .[, .(stay_id,
        fio2_time = charttime,
        fio2 = valuenum,
        fio2_unit = valueuom)]

## Analzye for outliers
summary(fio2_events$fio2)

## Check distribution
quantile(fio2_events$fio2,
         probs = c(0.01, 0.1, 0.5, 0.75, 0.9, 0.95, 0.99),
         na.rm = FALSE)

## Check suspicious values 1 < FiO2 < 21
setkey(resp_support_events,
       stay_id,
       resp_support_time)

setkey(fio2_events,
       stay_id,
       fio2_time)

fio2_check <- resp_support_events %>% 
  copy() %>% 
  .[, time := resp_support_time] %>% 
  .[fio2_events[, time := fio2_time],
  on = .(stay_id,
         time),
  roll = "nearest"] %>% 
  ## check only suspicious values
  .[fio2 > 1 & 
      fio2 < 21] %>% 
  .[, time_diff_min := abs(
    lubridate::time_length(
      fio2_time -
        resp_support_time,
      "mins"))] %>% 
  .[time_diff_min <= 30] %>% 
  .[, .N,
    by = .(fio2,
           resp_support_type)] %>% 
  .[order(fio2)]

## FiO2 values were recorded in both percentage (21-100%) and fractional (0.21-1)
## formats. Values within these ranges were retained and normalized to fractional
## FiO2 for calculation of the PaO2/FiO2 ratio. Values outside both physiologically
## ranges (e.g. 2-20 or >100%) were considered implausible and excluded. To 
## investigate whether these values represented oxygen flow rates rather than FiO2,
## they were compared with simultaneously documented respiratory support devices.
## No consistent association with a specific oxygen delivery modality was observed,
## supporting their classification as invalid FiO2 measurements rather than an
## alternative encoding.

## Remove obvious outliers based on distribution
fio2_events <- fio2_events %>% 
  ## not physiologically plausible, since room air
  ## is approximately 21% FiO2
  .[(fio2 >= 21 &
      fio2 <= 100) |
      ## preserve entries already available in fraction format
      (fio2 >= 0.21 & fio2 <= 1.00)] %>% 
  ## convert to percentage for P/F ratio,
  ## if not already in fraction format
  .[, fio2_fraction := 
      fifelse(
        fio2 > 1,
        fio2 / 100,
        fio2)]

## Item ID for SpO2
spo2_itemid <- chart_itemids %>% 
  .[grepl("SpO2",
          abbreviation,
          ignore.case = TRUE) &
      category == "Respiratory"]

## SpO2 events
spo2_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## SpO2 item ID
  .[itemid == spo2_itemid$itemid] %>% 
  .[, .(stay_id,
        spo2_time = charttime,
        spo2 = valuenum,
        spo2_unit = valueuom)]

## Analzye for outliers
summary(spo2_events$spo2)

## Check distribution
quantile(spo2_events$spo2,
         probs = c(0.01, 0.1, 0.5, 0.75, 0.9, 0.95, 0.99),
         na.rm = FALSE)

## Exclude obvious outliers
spo2_events <- spo2_events %>% 
  .[spo2 > 0 &
      spo2 <= 100 ]

## Calculate P/F and S/F ratio
fio2_dt <- fio2_events %>% 
  copy() %>% 
  .[, fio2_time := as.POSIXct(fio2_time)] %>% 
  .[, time := fio2_time]

pao2_dt <- pao2_events %>% 
  copy() %>% 
  .[, pao2_time := as.POSIXct(pao2_time)] %>% 
  .[, time := pao2_time]

spo2_dt <- spo2_events %>% 
  copy() %>% 
  .[, spo2_time := as.POSIXct(spo2_time)] %>% 
  .[, time := spo2_time]

setkey(fio2_dt,
       stay_id,
       time)

setkey(pao2_dt,
       stay_id,
       time)

setkey(spo2_dt,
       stay_id,
       time)

## For each FiO2, find nearest PaO2 and SpO2
pf_sf_ratios <- pao2_dt[
  fio2_dt,
    on = .(stay_id,
           time),
    roll = "nearest"] %>% 
  spo2_dt[.,
       on = .(stay_id,
              time),
       roll = "nearest"] %>% 
  .[, time_diff_pf :=
      abs(lubridate::time_length(
        fio2_time -
        pao2_time,
        "mins"))] %>%
  .[, time_diff_sf :=
      abs(lubridate::time_length(
        fio2_time -
          spo2_time,
        "mins"))] %>% 
  .[, pf_ratio := fifelse(
    ## PaO2 and FiO2 within 2h
    time_diff_pf <= 120,
    pao2 / fio2_fraction,
    NA_real_
  )] %>% 
  .[, sf_ratio := fifelse(
    ## SpO2 and FiO2 within 2h
    time_diff_sf <= 120,
    spo2 / fio2_fraction,
    NA_real_
  )] %>% 
  .[, .(stay_id,
        fio2_time,
        fio2_fraction,
        pao2_time,
        pao2,
        spo2_time,
        spo2,
        time_diff_pf,
        time_diff_sf,
        pf_ratio,
        sf_ratio)] %>% 
  ## keep rows with at least one valid ratio
  .[!is.na(pf_ratio) |
      !is.na(sf_ratio)]

setkey(pf_sf_ratios,
       stay_id,
       fio2_time)

## Check PF and SF ratio distribution
summary(pf_sf_ratios$pf_ratio)
summary(pf_sf_ratios$sf_ratio)

## Radiology reports for bilateral chest imaging
## Chest imaging reports only
# chest_patterns <- c("chest",
#                     "portable chest",
#                     "single frontal chest",
#                     "chest radiograph",
#                     "cxr",
#                     "xray chest",
#                     "ct chest",
#                     "cta chest")
# 
# bilateral_patterns <- c("bilateral",
#                         "bilaterally",
#                         "both lungs",
#                         "both bases",
#                         "bibasal",
#                         "bibasilar",
#                         "multifocal",
#                         "widespread")
# 
# lung_abnormality_patterns <- c("opacit",
#                                "airspace",
#                                "infiltrat",
#                                "consolidat",
#                                "pulmonary edema",
#                                "alveolar edema",
#                                "ground[- ]glass")
# 
# 
# negative_patterns <- c("no focal consolidation",
#                        "no airspace opacity",
#                        "no pulmonary edema",
#                        "no acute cardiopulmonary",
#                        "clear lungs",
#                        "lungs are clear",
#                        "without focal consolidation",
#                        "without pulmonary edema",
#                        "no evidence of edema")
# 
# unilateral_patterns <- c("right lower lobe",
#                          "right upper lobe",
#                          "right middle lobe",
#                          "left lower lobe",
#                          "left upper lobe",
#                          "right base",
#                          "left base",
#                          "right lung",
#                          "left lung",
#                          "unilateral",
#                          "focal")
# 
# ## Define proxy definition for bilateral chest opacities
# bilateral_chest_opacities_proxy <- all_radiologyevents %>%
#   copy() %>%
#   .[, text_lower := str_to_lower(fifelse(is.na(text), "", text))] %>%
#   ## chest imaging only
#   .[grepl(paste(chest_patterns,
#                 collapse = "|"),
#           text_lower,
#           ignore.case = TRUE)] %>%
#   ## raw positive signal
#   .[, bilateral_abnormality_raw := 
#       grepl(paste(bilateral_patterns,
#                   collapse = "|"),
#             text_lower,
#             ignore.case = TRUE) &
#       grepl(paste(lung_abnormality_patterns,
#                   collapse = "|"),
#             text_lower,
#             ignore.case = TRUE)] %>% 
#   ## negative report language
#   .[, has_negative_statement :=
#       grepl(paste(negative_patterns,
#                   collapse = "|"),
#             text_lower,
#             ignore.case = TRUE)] %>% 
#   ## unilateral/focal-only findings
#   .[, unilateral_or_focal := 
#       grepl(paste(unilateral_patterns,
#                   collapse = "|"),
#             text_lower,
#             ignore.case = TRUE) &
#       !grepl("bilateral|both|diffuse|multifocal|widespread|bibasal|bibasilar",
#              text_lower,
#              ignore.case = TRUE)] %>% 
#   ## final proxy
#   .[, bilateral_chest_opacities_proxy :=
#       bilateral_abnormality_raw == TRUE &
#       has_negative_statement == FALSE &
#       unilateral_or_focal == FALSE] %>% 
#     .[bilateral_chest_opacities_proxy == TRUE & 
#         !is.na(charttime),
#       .(first_bilateral_chest_imaging_time = min(charttime),
#         any_bilateral_chest_opacities = TRUE),
#       by = .(subject_id, 
#              hadm_id)]

## Define proxy definition for bilateral chest opacities
ards_note <- all_radiologyevents %>%
  .[grepl("INDICATION:",
          text,
          ignore.case = FALSE) &
      grepl("CHEST RADIOGRAPH|CHEST \\(PA AND LAT\\)|FRONTAL CHEST RADIOGRAPH|PORTABLE AP CHEST",
            text,
            ignore.case = TRUE)] %>% 
  .[, indication := sub(
      "(?s).*?INDICATION:\\s*(.*?)\\n\\n[A-Z][A-Z /()\\-]+:.*",
      "\\1",
      text,
      perl = TRUE)] %>% 
  .[, text := NULL] %>% 
  .[grepl("ARDS|AHRF",
          indication,
          ignore.case = FALSE)]
          

## Combine results into AHRF and ARDS proxy
ahrf_ards_proxies <- resp_support_events %>% 
  copy() %>% 
  .[, time := resp_support_time] %>% 
  .[
    pf_sf_ratios[, time := fio2_time],
    on = .(stay_id,
           time),
    roll = "nearest"] %>% 
  .[, resp_support_time_diff_min := 
      abs(lubridate::time_length(
        resp_support_time -
          fio2_time,
        "mins"))] %>% 
  .[resp_support_time_diff_min <= 120] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)],
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, ahrf_timing :=
      ## acount for documentation delay
      ## restrict to 7 days after sepsis onset
      fio2_time >= suspected_infection_time - hours(1) &
      fio2_time <= suspected_infection_time + days(7)] %>% 
  .[, ahrf_event := fcase(
    
    ahrf_timing == TRUE &
    pf_ratio <= 300 &
      oxygen_therapy == TRUE,
    TRUE,
    
    ahrf_timing == TRUE &
    (!is.na(pf_ratio) &
      !is.na(oxygen_therapy) &
      (pf_ratio > 300 |
         oxygen_therapy == FALSE)),
    FALSE,
    
    default = NA)] %>% 
  ## ARDS timing criteria: within 7 days after sepsis onset
  .[, ards_timing :=
      ## acount for documentation delay
      fio2_time >= suspected_infection_time - hours(1) &
      fio2_time <= suspected_infection_time + days(7)] %>% 
  ## Global ARDS hypoxemia criterion
  .[, ards_hypoxemia := 
      fcase(
        
        !is.na(pf_ratio) & pf_ratio <= 300,
        TRUE,
        
        is.na(pf_ratio) &
          !is.na(sf_ratio) &
          spo2 <= 97 &
          sf_ratio <= 315,
        TRUE,
        
        !is.na(pf_ratio) & pf_ratio > 300,
        FALSE,
        
        is.na(pf_ratio) &
          !is.na(sf_ratio) &
          (spo2 > 97 |
             sf_ratio > 315),
        FALSE,
        
        default = NA)] %>% 
  ## strict ARDS respiratory support criterion
  .[, ards_resp_support_strict := fcase(
    
    resp_support_type %in% c("NIV",
                             "Invasive ventilation"),
    TRUE,
    
    resp_support_type == "HFNC" &
      flow_label == "O2 Flow" &
      flow_rate >= 30 &
      flow_time_diff_min <= 30,
    TRUE,
    
    !is.na(resp_support_type),
    FALSE,
    
    default = NA)] %>% 
  ## ARDS respiratory support criterion
  .[, ards_resp_support_proxy := resp_support_type %in% c(
    "HFNC",
    "NIV",
    "Invasive ventilation"
  )] %>% 
  ## Structured ARDS proxy without imaging
  .[, ards_global_structured_proxy := fcase(
    
    ards_timing == TRUE &
      ards_hypoxemia == TRUE &
      ards_resp_support_strict == TRUE,
    TRUE,
    
    !is.na(ards_timing) &
      !is.na(ards_hypoxemia) &
      !is.na(ards_resp_support_strict) &
      (ards_timing == FALSE |
         ards_hypoxemia == FALSE |
         ards_resp_support_strict == FALSE),
    FALSE,
    
    default = NA)]

## First AHRF event per stay ID
ahrf_stay <- ahrf_ards_proxies %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.numeric(ahrf_event),
          fio2_time)] %>% 
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        ahrf_event,
        ahrf_onset_time = fifelse(
          ahrf_event == TRUE,
          fio2_time,
          as.POSIXct(NA)
        ),
        ahrf_first_pf_ratio = pf_ratio,
        ahrf_first_sf_ratio = sf_ratio,
        ahrf_first_resp_support_type = resp_support_type)]

## First ARDS event per stay ID
ards_stay <- ahrf_ards_proxies %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.numeric(ards_global_structured_proxy),
          fio2_time)] %>% 
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        ards_global_structured_proxy,
        ards_onset_time = fifelse(
          ards_global_structured_proxy == TRUE,
          fio2_time,
          as.POSIXct(NA)
        ),
        ards_first_pf_ratio = pf_ratio,
        ards_first_sf_ratio = sf_ratio,
        ards_first_resp_support_type = resp_support_type)]
  

## Define AHRF around sepsis onset
rec_67 <- sepsis_base_cohort_data %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time)] %>% 
  merge(.,
        ahrf_stay[, .(subject_id,
                      hadm_id,
                      stay_id,
                      ahrf_event,
                      ahrf_onset_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        resp_support_events[, .(stay_id,
                                resp_support_time,
                                resp_support,
                                resp_support_type)] %>% unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## account for documentation delay
  .[, resp_timing := resp_support_time >= ahrf_onset_time - hours(1) &
      ## first oxygen therapy within 6 hours of AHRF onset
      resp_support_time <= ahrf_onset_time + hours(6)] %>% 
  .[order(-as.numeric(resp_timing),
          resp_timing)] %>% 
    .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        ahrf_event,
        resp_timing,
        first_resp_time = resp_support_time,
        first_resp_device = resp_support,
        first_resp_type = resp_support_type)] %>% 
  .[, rec_67_first_oxygen_hfnc := fcase(
    
    resp_timing == TRUE &
      ahrf_event == TRUE &
      first_resp_type == "HFNC",
    TRUE,
    
    resp_timing == TRUE &
      ahrf_event == TRUE &
      first_resp_type == "Conventional oxygen",
    FALSE,
    
    default = NA)] %>% 
  .[, rec_67_eligbility := fcase(
    
    !is.na(rec_67_first_oxygen_hfnc),
    "eligible",
    
    resp_timing == TRUE &
      ahrf_event == TRUE &
      first_resp_type %in% c("Invasive ventilation",
                               "Other oxygen",
                               "NIV"),
    "other oxygen besides HFNC or Conventional oxygen",
    
    resp_timing == TRUE &
      ahrf_event == TRUE &
      (first_resp_type %in% c("None",
                               "Other") |
         is.na(first_resp_type)),
    "no oxygen administered",
    
    resp_timing == FALSE |
      is.na(resp_timing) |
      ahrf_event == FALSE |
      is.na(ahrf_event),
      
    "no AHRF event")]

## 68 RECOMMENDATION -----------------------------------------------------------
rec68_label <- ssc_recommendations %>% 
  .[rec_num == 68, rec_label]

## For adults with sepsis and AHRF, HFNC as initial therapy over NIPPV/NIV
rec_68 <- sepsis_base_cohort_data %>% 
  merge(.,
        ahrf_stay[, .(subject_id,
                hadm_id,
                stay_id,
                ahrf_event,
                ahrf_onset_time)],
            by = c("subject_id",
                   "hadm_id",
                   "stay_id"),
            all.x = TRUE,
            all.y = FALSE) %>% 
  ## compare initial HFNC vs. NIV/NIPPV
  merge(.,
        resp_support_events[, .(stay_id,
                                resp_support_time,
                                resp_support,
                                resp_support_type)] %>% unique(),
                       
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## account for documentation delay
  .[, resp_timing := resp_support_time >= ahrf_onset_time - hours(1) &
      ## first oxygen therapy within 6 hours of AHRF onset
      resp_support_time <= ahrf_onset_time + hours(6)] %>% 
  .[order(-as.numeric(resp_timing),
          resp_support_time)] %>% 
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        ahrf_event,
        resp_timing,
        first_resp_time = resp_support_time,
        first_resp_device = resp_support,
        first_resp_type = resp_support_type)] %>% 
  ## encode recommendation 68
  .[, rec_68_first_oxygen_hfnc_over_niv := fcase(
    
    resp_timing == TRUE &
      ahrf_event == TRUE &
      first_resp_type == "HFNC",
    TRUE,
    
    resp_timing == TRUE &
      ahrf_event == TRUE &
      first_resp_type == "NIV",
    FALSE,
    
    default = NA)] %>% 
  .[, rec_68_eligbility := fcase(
    
    !is.na(rec_68_first_oxygen_hfnc_over_niv),
    "eligible",
    
    resp_timing == TRUE &
      ahrf_event == TRUE &
      first_resp_type %in% c("Invasive ventilation",
                               "Other oxygen",
                               "Conventional oxygen"),
    "other oxygen besides HFNC or NIV",
    
    resp_timing == TRUE &
      ahrf_event == TRUE &
      (first_resp_type %in% c("None",
                                "Other") |
         is.na(first_resp_type)),
    "no oxygen administered",
    
    resp_timing == FALSE |
      is.na(resp_timing) |
      ahrf_event == FALSE |
      is.na(ahrf_event),
    
    "no AHRF event")]

## 69 RECOMMENDATION -----------------------------------------------------------
rec69_label <- ssc_recommendations %>% 
  .[rec_num == 69, rec_label]

## Define HFNC/NIV respiratory support events
## For adults with sepsis and AHRF, HFNC as initial therapy over NIPPV/NIV
hfnc_niv_events <- resp_support_events %>% 
  .[resp_support_type %in% c("HFNC",
                             "NIV"),
    .(stay_id,
           resp_support_time,
           resp_support,
           resp_support_type)] %>% 
      unique()

rec_69 <- sepsis_base_cohort_data %>% 
  merge(.,
        ahrf_stay[, .(subject_id,
                      hadm_id,
                      stay_id,
                      ahrf_event,
                      ahrf_onset_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## compare initial HFNC vs. NIV/NIPPV
  merge(.,
        resp_support_events[, .(stay_id,
                                resp_support_time,
                                resp_support,
                                resp_support_type)] %>% unique(),
        
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## account for documentation delay
  .[, resp_timing := resp_support_time >= ahrf_onset_time - hours(1) &
      ## first oxygen therapy within 6 hours of AHRF onset
      resp_support_time <= ahrf_onset_time + hours(6)] %>% 
  .[order(-as.numeric(resp_timing),
          resp_support_time)] %>% 
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  # .[, .(subject_id,
  #       hadm_id,
  #       stay_id,
  #       ahrf_event,
  #       resp_timing,
  #       first_resp_time = resp_support_time,
  #       first_resp_device = resp_support,
  #       first_resp_type = resp_support_type)] %>% 
  ## Respiratory support after AHRF onset
  merge(.,
        hfnc_niv_events[, .(stay_id,
                            time = resp_support_time,
                            support = resp_support,
                            type = resp_support_type)],
        by = "stay_id",
        all = FALSE) %>% 
  .[, resp_timing := resp_support_time >= ahrf_onset_time &
      resp_support_time <= ahrf_onset_time + hours(6)] %>% 
  ## Identify first therapy and whether both HFNC and NIV ocurred
  .[order(-as.numeric(resp_timing),
          resp_support_time),
    .(first_resp_time = resp_support_time[1],
      first_resp_device = resp_support[1],
      first_resp_type = resp_support_type[1],
      used_hfnc = any(resp_support_type == "HFNC"),
      used_niv = any(resp_support_type == "NIV"),
      n_switches = sum(resp_support_type[-1] != resp_support_type[-.N])),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  ## Alternating HFNC/NIV proxy
  ## both HFNC and NIV used within the early AHRF window, at least one switch
  .[, hfnc_niv_alternating := used_hfnc == TRUE &
      used_niv == TRUE &
      n_switches >= 1] %>% 
  .[, rec_69_hfnc_over_hfnc_niv_alternating := 
      first_resp_type == "HFNC" &
      hfnc_niv_alternating == FALSE]

## 71 RECOMMENDATION -----------------------------------------------------------
rec71_label <- ssc_recommendations %>% 
  .[rec_num == 71, rec_label]

## Oxygen therapy HFNC & NIV
oxygen_therapy_hfnc_niv <- resp_support_events %>% 
  copy() %>% 
  .[, resp_support_type := fcase(
    resp_support == "HFNC", "HFNC",
    resp_support == "NIV", "NIV",
    grepl("Endotracheal tube|Tracheostomy tube",
          resp_support,
          ignore.case = TRUE),
    "Invasive ventilation",
    default = NA_character_
  )] %>% 
  .[resp_support_type %in% c("HFNC",
                             "NIV",
                             "Invasive ventilation")] %>% 
  .[, chart_hour := lubridate::floor_date(resp_support_time,
                                          "hour")] %>% 
  .[, .(stay_id,
        chart_hour,
        resp_support,
        resp_support_type)]

## Calculate PBW (predicted body weight) from height and sex
predicted_body_weight <- patient_height %>% 
  .[!is.na(`Height (cm)`)] %>% 
  .[`Height (cm)` >= 120 &
      `Height (cm)` <= 230] %>% 
  .[, .(`Height (cm)` = median(`Height (cm)`, 
                           na.rm = TRUE)),
    by = stay_id] %>% 
  merge(.,
        sepsis_base_cohort_data,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        gender,
        `Height (cm)`)] %>% 
  unique() %>% 
  .[, pbw_kg := fcase(
    gender == "M", 50 + 0.905 * (`Height (cm)` - 152.4),
    gender == "F", 45.5 + 0.905 * (`Height (cm)` - 152.4),
    default = NA_real_
  )]

## Item ID for hourly set tidal volume
tidal_volume_itemids <- chart_itemids %>% 
  .[grepl("tidal volume",
          label,
          ignore.case = TRUE) &
      grepl("set",
            label,
            ignore.case = TRUE)]

## Extract hourly set tidal volume (mL/kg) for oxygenation events
tidal_volume_events <- all_chartevents %>% 
  .[itemid == tidal_volume_itemids$itemid] %>% 
  .[!is.na(valuenum) &
      valuenum > 0 &
      ## conservative upper bound that removed obvious outliers
      valuenum <= 1500] %>% 
  .[, chart_hour := lubridate::floor_date(charttime, 
                                          "hour")] %>% 
  .[, .(tidal_volume_ml = mean(valuenum,
                               na.rm = TRUE)),
    by = .(stay_id,
           chart_hour)] %>% 
  merge(.,
        predicted_body_weight,
        by = "stay_id",
        all = FALSE) %>% 
  ## Tidal volume per kg of predicted body weight
  .[, tidal_vol_per_kg := tidal_volume_ml / pbw_kg] %>% 
  .[, tidal_volume_strategy := fcase(
    tidal_vol_per_kg <= 6, "Low tidal volume",
    tidal_vol_per_kg > 10, "High tidal volume",
    tidal_vol_per_kg > 6 & tidal_vol_per_kg <= 10, "Intermediate",
    default = NA_character_
  )]

# ## Item IDs for ARDS definition oxgenation events
# ards_oxygenation_itemids <- chart_itemids %>% 
#   copy() %>% 
#   .[grepl("PO2|FiO2|SpO2",
#         abbreviation,
#         ignore.case = TRUE) &
#       !grepl("venous",
#              label,
#              ignore.case = TRUE)] %>% 
#   .[category %in% c("Labs",
#                     "Respiratory")]
# 
# ## Oxgenation events for ARDS definition
# ards_oxygenation_events <- all_chartevents %>% 
#   copy() %>% 
#   .[itemid %in% ards_oxygenation_itemids$itemid] %>% 
#   merge(.,
#         ards_oxygenation_itemids[, .(itemid, 
#                                     abbreviation)],
#         by = "itemid",
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[, abbreviation := fifelse(
#     abbreviation == "PO2 (Arterial)",
#     "PaO2",
#     abbreviation
#   )] %>% 
#   .[!is.na(valuenum)] %>% 
#   ## Remove obvious outliers
#   .[(abbreviation == "PaO2" & valuenum > 20 & valuenum <= 600) |
#       (abbreviation == "SpO2" & valuenum >= 50 & valuenum <= 100) |
#       (abbreviation == "FiO2" & valuenum >= 21 & valuenum <= 100)] %>% 
#   ## FiO2 needs to be on 0-1 scale for ratio calculation
#   .[, valuenum_clean := fifelse(
#     abbreviation == "FiO2",
#     valuenum / 100,
#     valuenum
#   )] %>% 
# .[, chart_hour := lubridate::floor_date(charttime, "hour")] %>% 
#   .[, .(value = mean(valuenum_clean, na.rm = TRUE)),
#     by = .(stay_id,
#            chart_hour,
#            abbreviation)] %>% 
#   dcast(stay_id +
#         chart_hour ~ abbreviation,
#         value.var = "value") %>% 
#   .[, pf_ratio := PaO2 / FiO2] %>% 
#   .[, sf_ratio := SpO2 / FiO2] %>% 
#   .[!is.na(pf_ratio) |
#       !is.na(sf_ratio)] %>% 
#   .[, oxygenation_failure := 
#       (!is.na(pf_ratio) & pf_ratio <= 300) |
#       (!is.na(sf_ratio) & !is.na(SpO2) & SpO2 <= 97 & sf_ratio <= 315)]
# 
# ## ARDS proxy definition
# ards_proxy <- ards_oxygenation_events %>% 
#   copy() %>% 
#   .[oxygenation_failure == TRUE] %>% 
#   merge(.,
#         sepsis_base_cohort_data[, .(subject_id,
#                                     hadm_id,
#                                     stay_id,
#                                     suspected_infection_time)] %>% unique(),
#         by = "stay_id",
#         all = FALSE,
#         allow.cartesian = TRUE) %>% 
#   .[chart_hour >= lubridate::floor_date(suspected_infection_time,
#                                         "hour") &
#       chart_hour <= lubridate::floor_date(suspected_infection_time + days(7),
#                                           "hour")] %>% 
#   merge(.,
#         oxygen_therapy_hfnc_niv[, .(stay_id,
#                                     support_hour = chart_hour,
#                                     resp_support_type)] %>% unique(),
#         by = "stay_id",
#         all = FALSE,
#         allow.cartesian = TRUE) %>% 
#   .[abs(as.numeric(difftime(support_hour,
#                             chart_hour,
#                             units = "hours"))) <= 6] %>% 
#   .[order(chart_hour),
#     .SD[1],
#     by = .(subject_id,
#            hadm_id,
#            stay_id)] %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         chart_hour,
#         PaO2,
#         SpO2,
#         FiO2,
#         pf_ratio,
#         sf_ratio,
#         resp_support_type,
#         ards_proxy = TRUE)]


## Define recommendation 71
## Restrict to invasive ventilation because tidal volume strategy
## only applies to ventilated patients
rec_71 <- sepsis_base_cohort_data %>%
  merge(.,
        ards_stay[, .(subject_id, 
                      hadm_id, 
                      stay_id,
                      ards_global_structured_proxy, 
                      ards_onset_time)],
        by = c("subject_id", 
               "hadm_id", 
               "stay_id"),
        all.x = TRUE, 
        all.y = FALSE)

## Pick ONE tidal-volume reading per stay BEFORE joining back (no cartesian blow-up).
## Bring ARDS onset onto the (small) tidal-volume table, window it, keep first per stay.
first_tv <- tidal_volume_events %>% 
  .[, .(subject_id, 
        hadm_id, 
        stay_id,
        tv_hour = chart_hour,
        tidal_volume_ml, pbw_kg,
        tidal_vol_per_kg, tidal_volume_strategy)] %>%
  unique() %>%
  merge(.,
        rec_71[, .(subject_id,
                   hadm_id,
                   stay_id, 
                   ards_onset_time)],
        by = c("subject_id", 
               "hadm_id", 
               "stay_id"),
        all.x = FALSE, 
        all.y = FALSE) %>%          # inner: only cohort stays
  .[, ards_timing := tv_hour >= ards_onset_time - hours(1) &
      tv_hour <= ards_onset_time + hours(24)] %>%
  .[order(ards_timing, 
          tv_hour), 
    .SD[1],
    by = .(subject_id, 
           hadm_id, 
           stay_id)] %>%
  .[, .(subject_id, 
        hadm_id, 
        stay_id,
        ards_timing,
        first_tidal_volume_time = tv_hour,
        tidal_volume_ml,
        pbw_kg, 
        tidal_vol_per_kg,
        tidal_volume_strategy)]

rec_71 <- merge(rec_71, 
                first_tv,
                by = c("subject_id", 
                       "hadm_id", 
                       "stay_id"),
                all.x = TRUE, 
                all.y = FALSE) %>%
  .[, `:=`(rec_71_low_tidal_volume  = tidal_volume_strategy == "Low tidal volume",
           rec_71_high_tidal_volume = tidal_volume_strategy == "High tidal volume")]


## 72 RECOMMENDATION -----------------------------------------------------------
rec72_label <- ssc_recommendations %>% 
  .[rec_num == 72, rec_label]

## Define AHRF with ARDS
ahrf_without_ards <- ahrf_stay %>% 
  .[, .(stay_id,
        ahrf_onset_time,
        ahrf_event)] %>% 
  unique() %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = "stay_id",
        all = FALSE) %>% 
  ## AHRF around sepsis onset
  .[ahrf_onset_time >= suspected_infection_time - hours(6) &
      ahrf_onset_time <= suspected_infection_time + hours(6) &
      ahrf_event == TRUE] %>% 
  .[, .(ahrf_time = min(ahrf_onset_time)),
    by = .(subject_id,
           hadm_id,
           stay_id)]
  ## Exclude ARDS patients
  # .[stay_id %notin% ards_proxy$stay_id]

## Recommendation 72:
## AHRF without ARDS, tidal volume 6-8 ml/kg PBW
## over lower tidal volume 4 to <6 ml/kg PBW
rec_72 <- ahrf_without_ards %>% 
  ## Restrict to invasive ventilation
  merge(.,
        oxygen_therapy_hfnc_niv[resp_support_type == "Invasive ventilation",
        .(stay_id,
          vent_hour = chart_hour)] %>% 
  unique(),
  by = c("stay_id"),
  all = FALSE) %>% 
  ## Invasive ventilation close to AHRF onset
  .[vent_hour >= lubridate::floor_date(ahrf_time,
                                       "hour") &
      vent_hour <= lubridate::floor_date(ahrf_time + hours(24),
                                         "hour")] %>% 
  merge(.,
        tidal_volume_events[, .(stay_id,
                                tv_hour = chart_hour,
                                tidal_volume_ml,
                                pbw_kg,
                                tidal_vol_per_kg)] %>% 
          unique(),
        by = c("stay_id"),
        all = FALSE,
        allow.cartesian = TRUE,) %>% 
  ## First tidal volume after AHRF onset
  .[tv_hour >= lubridate::floor_date(ahrf_time,
                                     "hour") &
      tv_hour <= lubridate::floor_date(ahrf_time + hours(24),
                                       "hour")] %>% 
  .[order(tv_hour),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, tidal_volume_strategy_rec72 := fcase(
    tidal_vol_per_kg >= 6 & tidal_vol_per_kg <= 8,
    "Recommended 6-8 ml/kg",
    tidal_vol_per_kg >= 4 & tidal_vol_per_kg < 6,
    "Lower tidal volume 4-<6 ml/kg",
    tidal_vol_per_kg > 8,
    "Higher than recommended",
    tidal_vol_per_kg < 4,
    "Very low tidal volume",
    default = NA_character_
  )] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        ahrf_time,
        first_tidal_volume_time = tv_hour,
        tidal_volume_ml,
        pbw_kg,
        tidal_vol_per_kg,
        tidal_volume_strategy_rec72)] %>% 
  .[, `:=` (
    rec_72_tidal_volume_6_8 = 
      tidal_volume_strategy_rec72 == "Recommended 6-8 ml/kg",
    
    rec_72_tower_tidal_volume_4_to_less_6 =
      tidal_volume_strategy_rec72 == "Lower tidal volume 4-<6 ml/kg"
  )]

## 79 RECOMMENDATION -----------------------------------------------------------
rec79_label <- ssc_recommendations %>% 
  .[rec_num == 79, rec_label]

## Corticosteroid administration
## Prednisone & prednisolone are oral corticosteroids --> not IV
## Acetate steroids are not administered intravenously
corticosteroid_events <- corticosteroid_emar_icu %>% 
  .[event_txt %in% c("Administered",
                     "Delayed Administration",
                     "Administered in Other Location",
                     "Partial Administration") &
      !grepl(paste(c("cream",
                     "oint",
                     "ophth",
                     "rectal","enema",
                     "foam",
                     "suppository",
                     "study",
                     "inv-",
                     "tobramycin",
                     "sulfacetamide",
                     "oral",
                     "^prednisone",
                     "^prednisolone",
                     ## type of prednisolone
                     "predforte",
                     "acetate"),
                   collapse = "|"),
             medication,
             ignore.case = TRUE)] %>% 
  .[, medication := trimws(tolower(medication))] %>% 
  .[, corticosteroid_type := fcase(
    grepl("hydrocortisone na succ",
          medication), "Hydrocortisone sodium succinate",
    grepl("^hydrocortisone$",
          medication), "Hydrocortisone",
    grepl("^dexamethasone$",
          medication), "Dexamethasone",
    grepl("methylprednisolone sodium succ",
          medication), "Methylprednisolone sodium succ",
    grepl("^methylprednisolone$",
          medication), "Methylprednisolone",
    default = medication
  )] %>% 
  .[, .(subject_id,
        hadm_id,
        steroid_time = charttime,
        corticosteroid_type,
        event_txt,
        administration_type,
        dose_given,
        dose_given_unit)]
 
## Identify patients with septic shock
## who received IV corticosteroids
rec_79 <- sepsis_base_cohort_data %>% 
  merge(.,
        corticosteroid_events,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## corticosteroid after septic shock onset
  .[, window_24h_after_onset :=
      !is.na(steroid_time) &
      !is.na(septic_shock_onset_time) &
      steroid_time >= septic_shock_onset_time - hours(1) &
         steroid_time <= septic_shock_onset_time + hours(24)] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.integer(window_24h_after_onset),
          steroid_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, rec_79_iv_corticosteroids := fcase(
    septic_shock_proxy == TRUE &
      window_24h_after_onset == TRUE,
    TRUE,
    
    septic_shock_proxy == TRUE &
      window_24h_after_onset == FALSE,
    FALSE,
    
    default = NA)] %>% 
  .[, rec_79_eligibility := fcase(
    
    !is.na(rec_79_iv_corticosteroids),
    "eligible",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_)]


## 89 RECOMMENDATION -----------------------------------------------------------
rec89_label <- ssc_recommendations %>% 
  .[rec_num == 89, rec_label]

## Define active fluid removal
## Diuretics and/or extracorporeal fluid removal
diuretic_patterns <- c(
  "diur",
  
  ## Loop diuretics
  "furosemide",
  ## brand of furosemide
  "lasix",
  "torsemide",
  "torasemide",
  ## brand of torsemide
  "demadex",
  "bumetanide",
  ## brand of bumetanide
  "bumex",
  "ethacrynic acid",
  
  ## Thiazide diuretics
  "metolazone",
  "chlorothiazide",
  
  "hydrochlorothiazide",
  "thiazide")

## Diuretics item IDs
diuretic_itemids <- d_items %>% 
  .[grepl(paste(diuretic_patterns,
                 collapse = "|"),
          label,
          ignore.case = TRUE)]

## Diuretic administration
diuretic_events <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Diuretics item IDs
  .[itemid %in% diuretic_itemids$itemid] %>%
  merge(.,
        diuretic_itemids[, .(itemid,
                             label)] %>% 
          unique(),
        by = "itemid") %>% 
  .[, .(stay_id,
        fluid_removal_time = starttime,
        fluid_removal_type = "Diuretic",
        fluid_removal_detail = label)]

## Extracorporeal fluid removal
extracorporeal_patterns <-c(
  "haemofiltration",
  "hemodialysis",
  "ultrafiltration",
  "fluid removal",
  # "dialysis",
  ## continuous renal replacement therapy
  # "crrt",
  ## Continuous Venovenous Hemofiltration
  "cvvh",
  ## Continuous Veno-Venous Hemodialysis
  "cvvhd"
)

## Item IDs for hourly patients fluid removal & hemodiaylsis output
extracorporeal_fluid_removal_itemids <- chart_itemids %>% 
  .[grepl(paste(extracorporeal_patterns,
                collapse = "|"),
          label,
          ignore.case = TRUE)]

## Extracorporeal fluid removal events
extracorporeal_fluid_removal_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Extracorporeal fluid removal item IDs
  .[itemid %in% extracorporeal_fluid_removal_itemids$itemid] %>% 
  merge(.,
        extracorporeal_fluid_removal_itemids[, .(itemid,
                                                  label)],
        by = "itemid") %>% 
  .[!is.na(valuenum) & valuenum > 0] %>% 
  .[, .(stay_id,
        fluid_removal_time = charttime,
        fluid_removal_type = "Extracorporeal/Ultrafiltration",
        fluid_removal_detail = label)]

## Active fluid removal events
active_fluid_removal_events <- rbindlist(
  list(
    diuretic_events,
    extracorporeal_fluid_removal_events
  ),
  fill = TRUE
) %>% 
  unique()

## Hourly fluid boluses / volume expansion item IDs
fluid_bolus_itemids <- input_fluid_classification %>% 
  copy()

## Fluid bolus events (bedside resuscitation)
fluid_bolus_hourly <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## fluid bolus item IDs
  .[itemid %in% fluid_bolus_itemids$itemid] %>% 
  .[ordercategoryname %in% c("02-Fluids (Crystalloids)",
                             "03-IV Fluid Bolus") &
      ordercategorydescription %in% c("Continuous IV",
                                      "Bolus")] %>% 
  merge(.,
        input_fluid_classification[, .(itemid,
                                       label,
                                       resuscitation_role)],
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[amount >= 250] %>% 
  .[, chart_hour := lubridate::floor_date(starttime,
                                          "hour")] %>% 
  .[, .(fluid_bolus_ml = sum(amount,
                             na.rm = TRUE)),
    by = .(stay_id,
           chart_hour)] %>% 
  .[, ongoing_volume_expansion := fluid_bolus_ml >= 500]

## Vasopressor hourly administration events
vasopressor_hourly <- all_vasopressorevents %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    septic_shock_proxy,
                                    septic_shock_onset_time)] %>% unique(),
        by = "stay_id",
        all = FALSE) %>% 
  .[septic_shock_proxy == TRUE] %>% 
  .[!is.na(vaso_start) & !is.na(vaso_end)] %>% 
  .[, norepi_equiv_rate := fcase(
    vaso_label == "Norepinephrine",
    vaso_rate,
    
    vaso_label == "Epinephrine",
    vaso_rate,
    
    grepl("Phenylephrine",
          vaso_label,
          ignore.case = TRUE),
    vaso_rate / 10,
    
    vaso_label == "Dopamine",
    vaso_rate / 100,
    
    vaso_label == "Vasopressin" & vaso_rate_unit == "units/hour",
    (vaso_rate / 60) / 0.03 * 0.1,
    
    ## Angiotensin is not reliably convertible to NE-equivalent
    grepl("Angiotensin II",
          vaso_label),
    NA_real_,
    
    default = NA_real_
  )] %>% 
  .[!is.na(norepi_equiv_rate) & norepi_equiv_rate > 0] %>% 
  .[, vp_event_id := .I] %>% 
  .[, .(
    chart_hour = seq(
      lubridate::floor_date(vaso_start, "hour"),
      lubridate::floor_date(vaso_end, "hour"),
      by = "hour"
    ),
    norepi_equiv_rate = norepi_equiv_rate
  ),
  by = .(vp_event_id, stay_id, vaso_label)] %>% 
  .[, .(
    vasopressor_rate = max(norepi_equiv_rate, na.rm = TRUE)
  ),
  by = .(stay_id, chart_hour)] %>% 
  .[order(stay_id,
          chart_hour)] %>% 
  .[, previous_rate := shift(vasopressor_rate,
                             type = "lag"),
    by = stay_id] %>% 
  .[, vasopressor_escalating := !is.na(previous_rate) &
      vasopressor_rate > previous_rate] %>% 
  .[, high_dose_vasopressor := vasopressor_rate >= 0.25]


## Acute resuscitation hours
acute_resuscitation_hours <- sepsis_base_cohort_data %>% 
  .[septic_shock_proxy == TRUE] %>% 
  .[, start_hour := lubridate::floor_date(septic_shock_onset_time,
                                          "hour")] %>% 
  .[, end_hour := start_hour + hours(7 * 24)] %>% 
  .[, .(chart_hour = seq(start_hour,
                         end_hour,
                         by = "hour")),
    by = .(subject_id,
           hadm_id,
           stay_id,
           septic_shock_onset_time)] %>% 
  merge(.,
        vasopressor_hourly,
        by = c("stay_id",
               "chart_hour"),
        all.x = TRUE) %>% 
  merge(.,
        fluid_bolus_hourly,
        by = c("stay_id",
               "chart_hour"),
        all.x = TRUE) %>% 
  .[, `:=` (
    vasopressor_rate = fifelse(is.na(vasopressor_rate), 
                               0,
                               vasopressor_rate),
    vasopressor_escalating = fifelse(is.na(vasopressor_escalating),
                                           FALSE,
                                           vasopressor_escalating),
    high_dose_vasopressor = fifelse(is.na(high_dose_vasopressor),
                                    FALSE,
                                    high_dose_vasopressor),
    fluid_bolus_ml = fifelse(is.na(fluid_bolus_ml),
                             0,
                             fluid_bolus_ml),
    ongoing_volume_expansion = fifelse(is.na(ongoing_volume_expansion),
                                       FALSE,
                                       ongoing_volume_expansion))] %>% 
  .[, acute_resuscitation :=
      vasopressor_escalating |
      high_dose_vasopressor |
      ongoing_volume_expansion]

## First consecutive 6 hours after acute resuscitation
post_resuscitation_time <- acute_resuscitation_hours %>% 
  .[order(stay_id,
          chart_hour)] %>% 
  .[, no_acute_resuscitation := !acute_resuscitation] %>% 
  .[, no_acute_resuscitation_6h := frollsum(as.integer(no_acute_resuscitation),
                                           n = 6,
                                           align = "right",
                                           fill = NA) == 6,
    by = stay_id] %>% 
  .[no_acute_resuscitation_6h == TRUE] %>% 
  .[, .(post_resuscitation_time = min(chart_hour)),
    by = stay_id]

## Recommendation 89
rec_89 <- sepsis_base_cohort_data %>% 
  .[septic_shock_proxy == TRUE] %>% 
  merge(.,
        post_resuscitation_time,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        active_fluid_removal_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## Active fluid removal after acute resuscitation
  .[is.na(fluid_removal_time) |
      (fluid_removal_time >= post_resuscitation_time &
         fluid_removal_time <= post_resuscitation_time + hours(48))] %>% 
  .[order(fluid_removal_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_onset_time,
        post_resuscitation_time,
        first_fluid_removal_time = fluid_removal_time,
        first_fluid_removal_detail = fluid_removal_detail)] %>% 
  .[, rec_89_active_fluid_removal := !is.na(first_fluid_removal_time)] %>% 
  .[, rec_89_eligibility := fcase(
    
    !is.na(rec_89_active_fluid_removal),
    "eligible",
    
    septic_shock_proxy == FALSE | is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_
  )]


## 90 RECOMMENDATION -----------------------------------------------------------
rec90_label <- ssc_recommendations %>% 
  .[rec_num == 90, rec_label]

## Red blood cell transfusion item IDs
rbc_transfusion_itemids <- d_items %>% 
  .[grepl("packed red blood cells|packed red cells",
          label,
          ignore.case = TRUE)]

## RBC transfusions
rbc_transfusion_events <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## RBC transfusion item IDs
  .[itemid %in% rbc_transfusion_itemids$itemid] %>% 
  .[!is.na(amount) & amount > 0] %>% 
  .[, .(stay_id,
        transfusion_time = starttime,
        transfused_ml = amount)]

## Hemoglobin item ID
hemoglobin_itemid <- d_items %>% 
  .[grepl("hemoglobin",
          label,
          ignore.case = TRUE)]

## Hemoglobin measurements
hemoglobin_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Hemoglobin item ID
  .[itemid == hemoglobin_itemid$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  .[valuenum >= 3 & valuenum <= 20] %>% 
  .[, .(stay_id,
        hb_time = charttime,
        hemoglobin = valuenum)]

## Last hemoglobin value before each transfusion
pre_transfusion_hb <- rbc_transfusion_events %>% 
  merge(.,
        hemoglobin_events,
        by = "stay_id",
        allow.cartesian = TRUE) %>% 
  .[hb_time <= transfusion_time &
      hb_time >= transfusion_time - hours(24)] %>% 
  .[order(stay_id,
          transfusion_time,
          -hb_time)] %>% 
  .[, .SD[1],
    by = .(stay_id,
           transfusion_time)]

## Classify transfusion threshold
rec_90 <- sepsis_base_cohort_data %>% 
  merge(.,
        pre_transfusion_hb,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  .[, transfusion_strategy := fcase(
    hemoglobin < 7,
    "Restrictive",
    hemoglobin >= 9,
    "Liberal",
    hemoglobin >= 7 & hemoglobin < 9,
    "Intermediate",
    default = NA_character_
  )] %>% 
  ## use first transfusion after sepsis onset
  .[, transfusion_after_sepsis := transfusion_time >= suspected_infection_time] %>% 
  .[order(-as.numeric(transfusion_after_sepsis),
          transfusion_time),
    .SD[1],
    by = stay_id] %>% 
  .[, rec_90_restrictive_transfusion := fcase(
    transfusion_after_sepsis == TRUE &
      transfusion_strategy == "Restrictive",
    TRUE,
    
    transfusion_after_sepsis == TRUE &
      transfusion_strategy %in% c("Liberal",
                                  "Intermediate"),
    FALSE,
    
    default = NA)] %>% 
  .[, rec_90_eligibility := fcase(
    
    !is.na(rec_90_restrictive_transfusion),
    "eligible",
    
    transfusion_after_sepsis == FALSE |
      is.na(transfusion_after_sepsis),
    "no blood transfusion after sepsis onset",
    
    default = NA_character_
  )]


## 91 RECOMMENDATION -----------------------------------------------------------
## cannot be assessed within the first 24h
## was excluded in previous paper, due to this reason
rec91_label <- ssc_recommendations %>% 
  .[rec_num == 91, rec_label]

## Item ID for enteral nutrition
enteral_nutrition_itemid <- d_items %>% 
  .[grepl("^enteral nutrition ingredient",
          label,
          ignore.case = TRUE)]

## Enteral nutrition administration
enteral_nutrition_events <- all_ingredients %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Enteral nutrition item ID
  .[itemid == enteral_nutrition_itemid$itemid] %>% 
  ## clinically meaninful feed intitiation
  .[amount > 0] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        enteral_time = starttime,
        enteral_amount = amount,
        enteral_rate = rateuom)]

## Check for outliers
summary(enteral_nutrition_events$enteral_amount)

## Check distribution of enteral nutrition amount
quantile(enteral_nutrition_events$enteral_amount,
         probs = c(0.01, 0.1, 0.5, 0.75, 0.9, 0.95, 0.99),
         na.rm = FALSE)

## Enteral nutrition within 72 hours of sepsis onset
rec_91 <- sepsis_base_cohort_data %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time)] %>% 
  unique() %>% 
  merge(.,
        enteral_nutrition_events,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, enteral_within_72h := enteral_time >= suspected_infection_time - hours(1) &
      enteral_time <= suspected_infection_time + hours(72)] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.numeric(enteral_within_72h),
          enteral_time)] %>% 
  ## select first enteral feeding time after sepsis onset
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, time_to_enteral_hours :=
      abs(lubridate::time_length(enteral_time -
                                 suspected_infection_time,
                                 "hours"))] %>% 
  .[, rec_91_early_enteral_nutrition := fcase(
    
    enteral_within_72h == TRUE &
      time_to_enteral_hours <= 72,
    TRUE,
    
    enteral_within_72h == FALSE |
      time_to_enteral_hours <= 72 |
      is.na(enteral_within_72h) |
      is.na(time_to_enteral_hours),
    FALSE,
    
    default = NA)] %>% 
  .[, rec_91_eligibility := fcase(
    
    !is.na(rec_91_early_enteral_nutrition),
    "eligible",
    
    default = NA_character_)]

## 92 RECOMMENDATION -----------------------------------------------------------
rec92_label <- ssc_recommendations %>% 
  .[rec_num == 92, rec_label]

## Insulin item IDs
insulin_itemids <- d_items %>% 
  .[grepl("insulin",
          label,
          ignore.case = TRUE) &
      category == "Medications"]

## Insulin administration
insulin_events <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Insulin item IDs
  .[itemid %in% insulin_itemids$itemid] %>% 
  .[!is.na(amount) & amount > 0] %>% 
  merge(.,
        insulin_itemids[, .(itemid,
                            insulin_type = label)],
        by = "itemid") %>% 
  .[, .(stay_id,
        insulin_time = starttime,
        insulin_amount = amount,
        insulin_type)]

## Glucose item IDs
glucose_itemids <- d_items %>% 
  .[grepl("glucose",
          label,
          ignore.case = TRUE) &
      !grepl("apache",
             label,
             ignore.case = TRUE) &
      category == "Labs"]

## Glucose measuremnts
glucose_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Glucose item IDs
  .[itemid %in% glucose_itemids$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  .[valuenum >= 20 & valuenum <= 1000] %>% 
  .[, .(stay_id,
        glucose_time = charttime,
        glucose_mg_dl = valuenum)] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)],
        by = "stay_id",
        all = FALSE) %>% 
  ## account for delayed documentation
  .[glucose_time >= suspected_infection_time - hours(1)] %>% 
  ## Define hyperclycemia: glucose >= 180mg/dl
  .[, hyperglycemia_180 := glucose_mg_dl >= 180]

## Select first hyperglycemic event
first_glucose_180 <- glucose_events %>% 
  .[hyperglycemia_180 == TRUE] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          glucose_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_glucose_180_time = glucose_time,
        first_glucose_180_value = glucose_mg_dl)]

## First insulin after sepsis onset
first_insulin <- insulin_events %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = "stay_id",
        all = FALSE) %>% 
  .[insulin_time >= suspected_infection_time - hours(1)] %>% 
  .[order(insulin_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)]

## First insulin after first glucose >= 180 mg/dL
rec_92 <- sepsis_base_cohort_data %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time)] %>% 
  unique() %>% 
  merge(.,
        first_glucose_180,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        insulin_events,
        by = c("stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
 ## keep insulin administration after the first qualifying glucose
.[, insulin_after_glucose_180 := 
    !is.na(insulin_time) &
    !is.na(first_glucose_180_time) &
    insulin_time >= first_glucose_180_time - hours(1) &
    ## insulin must be administerd within 6h of hyperglycemia
    insulin_time <= first_glucose_180_time + hours(6)] %>% 
.[order(subject_id,
        hadm_id,
        stay_id,
        -as.integer(insulin_after_glucose_180),
        insulin_time)] %>% 
  .[, .SD[1],
  by = .(subject_id,
         hadm_id,
         stay_id)] %>% 
  .[, time_to_insulin_after_glucose := 
      lubridate::time_length(
        insulin_time -
          first_glucose_180_time,
        "hours")] %>% 
  .[, rec_92_insulin_started_at_glucose_180 := fcase(
    
    !is.na(first_glucose_180_time) &
      insulin_after_glucose_180 == TRUE,
    TRUE,
    
    !is.na(first_glucose_180_time) &
      insulin_after_glucose_180 == FALSE,
    FALSE,
    
    default = NA)] %>% 
  .[, rec_92_eligibility := fcase(
    
    !is.na(rec_92_insulin_started_at_glucose_180),
    "eligible",
    
    is.na(first_glucose_180_time),
    "no glucose value >= 180",
    
    default = NA_character_)]

## 96 RECOMMENDATION -----------------------------------------------------------
rec96_label <- ssc_recommendations %>% 
  .[rec_num == 96, rec_label]

## Arterial blood gas
ph_itemids <- d_items %>% 
  .[grepl("ph",
          abbreviation,
          ignore.case = TRUE) &
      grepl("arterial",
            label,
            ignore.case = TRUE) &
      !grepl("venous",
             label,
             ignore.case = TRUE)]

## Define severe acidemia
severe_acidemia_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## pH item ID
  .[itemid == ph_itemids$itemid] %>% 
  .[!is.na(valuenum)]
  
## Check for outliers
summary(severe_acidemia_events$valuenum)

## Analyze distribution
quantile(severe_acidemia_events$valuenum,
         probs = c(0.1, 0.5, 0.75, 0.9, 0.95, 0.99),
         na.rm = TRUE)

## Remove obvious outliers
# severe_acidemia_events <- 
severe_acidemia_events <-
  severe_acidemia_events %>% 
.[valuenum >= 6.5 & valuenum <= 8.0] %>% 
.[, severe_acidemia := valuenum <= 7.2] %>% 
  .[, .(stay_id,
        acidemia_time = charttime,
        ph = valuenum,
        severe_acidemia)] %>% 
  unique() %>% 
## select first severe acidemia event per patient
merge(.,
      sepsis_base_cohort_data[, .(subject_id,
                                  hadm_id,
                                  stay_id,
                                  suspected_infection_time,
                                  septic_shock_proxy)],
      by = "stay_id",
      all = FALSE)

## Item IDs for serum cystatin
serum_cystatin_itemid <- 
  lab_itemids %>% 
  .[grepl("cystatin|scr",
          label,
          ignore.case = TRUE)]

## Item ID for serum creatinine
serum_creatinine_itemid <- d_items %>% 
  .[grepl("creatinine",
          label,
          ignore.case = TRUE) &
      grepl("serum",
            label,
            ignore.case = TRUE) &
      !grepl("apache",
             label,
             ignore.case = TRUE)]

## Serum creatinine measurements
creatinine_events <- all_chartevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Serum creatinine item ID
  .[itemid == serum_creatinine_itemid$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  ## upper bound for serum creatinine
  .[, .(stay_id,
        creatinine_time = charttime,
        creatinine = valuenum,
        creatinine_unit = valueuom)] 

## Check for outliers
summary(creatinine_events$creatinine)

## Analyze distribution
quantile(creatinine_events$creatinine,
         probs = c(0.1, 0.5, 0.75, 0.9, 0.95, 0.99),
         na.rm = TRUE)

## Remove obvious outliers
creatinine_events <- creatinine_events %>% 
  ## use 40 mg/dL as a data quality threshold
  ## after inspecting the distribution
  .[creatinine > 0 & creatinine <= 40] 

## Serum creatinine values were restricted to the range 0–40 mg/dL. 
## Following inspection of the data distribution, values above 40 mg/dL 
## were considered implausible data entry or recording errors (e.g., isolated 
## values of 80, 117, 159, and 999999 mg/dL), whereas repeated measurements up 
## to approximately 32 mg/dL were retained because they were internally consistent
## and considered clinically plausible in patients with severe kidney failure.
  

## Define baseline creatinine value per patient before or at sepsis onset
creatinine_baseline <- creatinine_events %>% 
   merge(.,
         sepsis_base_cohort_data[, .(subject_id,
                                     hadm_id,
                                     stay_id,
                                     suspected_infection_time)],
         by = "stay_id",
         all = FALSE) %>% 
   ## only values before or at sepsis onset
   ## acount for documentation delay
   .[creatinine_time <= suspected_infection_time &
       ## restrict to 7 days before sepsis onset as per KDIGO AKI definition
       creatinine_time >= suspected_infection_time - days(7)] %>% 
   .[, time_before_onset := 
       lubridate::time_length(
         creatinine_time -
         suspected_infection_time,
         "hours")] %>% 
   .[order(subject_id,
           hadm_id,
           stay_id,
           time_before_onset),
     .SD[1],
     by = .(subject_id,
            hadm_id,
            stay_id)] %>% 
   .[, .(subject_id,
         hadm_id,
         stay_id,
         baseline_creatinine_time = creatinine_time,
         baseline_creatinine = creatinine)]

## Calculate creatinine change to baseline
## AKI criteria fulfilled when creatinine has increased  
## at least x1.5 compared to baseline
 creatinine_change_to_baseline <- creatinine_events %>% 
   merge(.,
         sepsis_base_cohort_data[, .(subject_id,
                                     hadm_id,
                                     stay_id,
                                     suspected_infection_time)],
         by = "stay_id",
         all = FALSE) %>% 
   merge(.,
         creatinine_baseline,
         by = c("subject_id",
                "hadm_id",
                "stay_id"),
         all = FALSE) %>% 
   ## compare creatinine values after sepsis onset to baseline value
   ## acount for delayed documentation
   .[creatinine_time >= suspected_infection_time - hours(1)] %>% 
   .[, increase_factor := creatinine / baseline_creatinine] %>% 
   .[, creatinine_aki_stage := fcase(
     
     increase_factor >= 1.5 &
       increase_factor < 2,
     "stage 1",
     
     increase_factor >= 2 &
       increase_factor < 3,
     "stage 2",
     
     increase_factor >= 3,
     "stage 3",
    
     increase_factor < 1.5,
     NA,
     
     default = NA_character_)] %>% 
   .[, creatinine_aki_baseline_stage := 
       factor(creatinine_aki_stage,
              levels = c("stage 3",
                         "stage 2",
                         "stage 1"))] %>% 
   .[order(subject_id,
           hadm_id,
           stay_id,
           creatinine_aki_baseline_stage),
     .SD[1],
     by = .(subject_id,
            hadm_id,
            stay_id)]
 
## Creatinine values after sepsis onset
creatinine_after_sepsis_onset <- 
   creatinine_events %>% 
   merge(.,
         sepsis_base_cohort_data[, .(subject_id,
                                     hadm_id,
                                     stay_id,
                                     suspected_infection_time)],
         by = "stay_id",
         all = FALSE) %>% 
   .[creatinine_time >= suspected_infection_time - hours(1)]
 
## Calculate difference between creatinine values within 48h of each other
## AKI criteria fulfilled when creatinine has increased >= 0.3 mg/dL within 48h
creatinine_change_48h <- merge(
   creatinine_after_sepsis_onset[, .(subject_id,
                                     hadm_id,
                                     stay_id,
                                     creatinine_x = creatinine,
                                     creatinine_time_x = creatinine_time)],
   creatinine_after_sepsis_onset[, .(subject_id,
                                     hadm_id,
                                     stay_id,
                                     creatinine_y = creatinine,
                                     creatinine_time_y = creatinine_time)],
   by = c("subject_id",
          "hadm_id",
          "stay_id"),
   all = TRUE,
   allow.cartesian = TRUE) %>% 
   ## keep only pairs within 48h of each other 
   .[creatinine_time_y > creatinine_time_x &
     lubridate::time_length(
       creatinine_time_y -
       creatinine_time_x,
     "hours") <= 48] %>% 
   .[, creatinine_diff := creatinine_y - creatinine_x] %>% 
   .[, creatinine_aki_diff_stage := fcase(
     
     creatinine_y >= 4,
     "stage 3",
     
     creatinine_diff >= 0.3,
     "stage 1",
     
     default = NA_character_)] %>% 
  .[, creatinine_aki_diff_stage := 
      factor(creatinine_aki_diff_stage,
             levels = c("stage 3",
                        "stage 1"))] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          creatinine_aki_diff_stage)] %>% 
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)]

## Calculate hourly urine output and length or oliguria / anuria
## AKI criteria fulfilled if oliguria / anuria persist long enough 
## Urinary output itemids
urine_output_itemids <- d_items %>% 
  .[linksto == "outputevents"] %>% 
  .[grepl("foley|void|cath|suprapubic|nephrostomy|urine",
          label,
          ignore.case = TRUE) &
      !grepl("irrigant|incontinent|cath lab",
             label,
             ignore.case = TRUE)]

## Hourly urine output
## Define oliguria as < 0.5mL/kg for 6 consecutive hours
urine_output_hourly <- all_outputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## urinary output item IDs
  .[itemid %in% urine_output_itemids$itemid] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time,
                                    icu_intime,
                                    icu_outtime,
                                    weight_kg_for_fluids)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>% 
  .[!is.na(value) & value >= 0] %>%
  .[, chart_hour := lubridate::floor_date(charttime,
                                          "hour")] %>% 
  ## after sepsis onset, account for documentation delay
  .[chart_hour >= suspected_infection_time - hours(1) &
      chart_hour <= suspected_infection_time + hours(48)] %>% 
  .[, .(urine_output_ml = sum(value,
                            na.rm = TRUE),
        weight_kg_for_fluids = first(weight_kg_for_fluids)),
    by = .(subject_id,
           hadm_id,
           stay_id,
           chart_hour)]


## Create complete hourly gird
urine_hour_grid <- sepsis_base_cohort_data %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        weight_kg_for_fluids,
        start_hour = 
          lubridate::floor_date(suspected_infection_time -
                                  hours(1),
                                "hour"),
        end_hour = 
          lubridate::floor_date(suspected_infection_time +
                                  hours(48),
                                "hour"))] %>% 
  .[, .(
    chart_hour = seq(start_hour,
                     end_hour,
                     by = "hour")
  ),
  by = .(subject_id,
         hadm_id,
         stay_id,
         weight_kg_for_fluids)]

## Complete urine output 
urine_output_complete <- urine_hour_grid %>% 
  merge(.,
        urine_output_hourly,
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "weight_kg_for_fluids",
               "chart_hour"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, urine_output_ml := ifelse(
    is.na(urine_output_ml),
    0,
    urine_output_ml)] %>% 
  .[, urine_ml_kg_h := urine_output_ml / weight_kg_for_fluids] %>% 
  .[, oliguric_0.5 := urine_ml_kg_h < 0.5] %>% 
  .[, oliguric_0.3 := urine_ml_kg_h < 0.3] %>% 
  .[, anuric := urine_output_ml == 0] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          chart_hour)] %>%
  .[, run_0.5 := rleid(oliguric_0.5),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, run_0.3 := rleid(oliguric_0.3),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, run_anuric := rleid(anuric),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[oliguric_0.5 == TRUE,
    duration_0.5_h := .N,
    by = .(subject_id,
           hadm_id,
           stay_id,
           run_0.5)] %>%
  .[oliguric_0.3 == TRUE,
    duration_0.3_h := .N,
    by = .(subject_id,
           hadm_id,
           stay_id,
           run_0.3)] %>%
  .[anuric == TRUE,
    duration_anuric_h := .N,
    by = .(subject_id,
           hadm_id,
           stay_id,
           run_anuric)]

## Collapse to patient-level
aki_urine_output <- urine_output_complete %>% 
  copy() %>% 
  .[, aki_urine_output_stage := fcase(
    
    duration_0.3_h >= 24 |
      duration_anuric_h >= 12,
    "stage 3",
    
    duration_0.5_h >= 12,
    "stage 2",
    
    duration_0.5_h >= 6,
    "stage 1",
    
    default = NA_character_)] %>% 
  .[, aki_urine_output_stage := 
      factor(aki_urine_output_stage,
             levels = c("stage 3",
                        "stage 2",
                        "stage 1"))] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          aki_urine_output_stage,
          chart_hour)] %>% 
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)]
  # .[, `:=` (max_duration_0.5_h = max(duration_0.5_h,
  #                               na.rm = TRUE),
  #       max_duration_0.3_h = max(duration_0.3_h,
  #                                na.rm = TRUE),
  #       max_duration_anuric_h = max(duration_anuric_h,
  #                                   na.rm = TRUE)),
  #   by = .(subject_id,
  #          hadm_id,
  #          stay_id)] %>% 
  # .[, `:=` (
  #   max_duration_0.5_h = fifelse(is.infinite(max_duration_0.5_h),
  #                                            0,
  #                                            max_duration_0.5_h),
  #   
  #   max_duration_0.3_h = fifelse(is.infinite(max_duration_0.3_h),
  #                                            0,
  #                                            max_duration_0.3_h),
  #   
  #   max_duration_anuric_h = fifelse(is.infinite(max_duration_anuric_h),
  #                                             0,
  #                                             max_duration_anuric_h))] %>% 
  # .[, aki_urine_output_stage := fcase(
  # 
  #   max_duration_0.3_h >= 24 |
  #     max_duration_anuric_h >= 12,
  #   "stage 3",
  # 
  #   max_duration_0.5_h >= 12,
  #   "stage 2",
  # 
  #   max_duration_0.5_h >= 6,
  #   "stage 1",
  # 
  #   default = NA_character_)]

## Define KDIGO AKI criteria
aki_classification <- sepsis_base_cohort_data %>% 
  ## Creatinine change to baseline
  merge(.,
        creatinine_change_to_baseline[, .(subject_id,
                                          hadm_id,
                                          stay_id,
                                          creatinine_baseline_time = creatinine_time,
                                          creatinine_aki_baseline_stage)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## Creatinine change wihtin 48h
  merge(.,
        creatinine_change_48h[, .(subject_id,
                                  hadm_id,
                                  stay_id,
                                  creatinine_time_y,
                                  creatinine_diff,
                                  creatinine_aki_diff_stage)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## Urine oliguria / anuria
  merge(.,
        aki_urine_output[, .(subject_id,
                             hadm_id,
                             stay_id,
                             aki_urine_output_stage,
                             aki_urine_time = chart_hour)],
                             
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, aki_stage := fcase(
    
    creatinine_aki_baseline_stage == "stage 3" |
      creatinine_aki_diff_stage == "stage 3" |
      aki_urine_output_stage == "stage 3",
    "stage 3",
    
    creatinine_aki_baseline_stage == "stage 2" |
      creatinine_aki_diff_stage == "stage 2" |
      aki_urine_output_stage == "stage 2",
    "stage 2",
    
    creatinine_aki_baseline_stage == "stage 1" |
      creatinine_aki_diff_stage == "stage 1" |
      aki_urine_output_stage == "stage 1",
    "stage 1",
    
    default = NA_character_)] %>% 
  .[, aki_stage_time := 
      min(creatinine_baseline_time,
          creatinine_time_y,
          aki_urine_time,
          na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)]


## Sodium bicarbonate item IDs
sodium_bicarb_itemids <- d_items %>% 
  .[grepl("bicarbonate|nahco3|sodium bicarb",
        label,
        ignore.case = TRUE) &
      !grepl("ingredient",
             category,
             ignore.case = TRUE)]

## Sodium bicarbonate admininstration
sodium_bicarb_events <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## Sodium bicarbonate therapy item IDs
  .[itemid %in% sodium_bicarb_itemids$itemid] %>% 
  merge(.,
        sodium_bicarb_itemids[, .(itemid,
                                  label)] %>% 
          unique(),
        by = "itemid") %>%
  .[!is.na(amount) &
      amount > 0] %>% 
  .[, .(stay_id,
        bicarbonate_time = starttime,
        bicarbonate_type = label,
        bicarbonate_amount = amount,
        bicarbonate_unit = amountuom)]

## Check for outliers
summary(sodium_bicarb_events$bicarbonate_amount)

## Analyze distribution
quantile(sodium_bicarb_events$bicarbonate_amount,
         probs = c(0.1, 0.5, 0.75, 0.9, 0.95, 0.99),
         na.rm = TRUE)

## Define recommendation 96
rec_96 <- sepsis_base_cohort_data %>% 
  merge(.,
        aki_classification[, .(subject_id,
                               hadm_id,
                               stay_id,
                               aki_stage,
                               aki_stage_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  # .[septic_shock_proxy == TRUE] %>% 
  merge(.,
        severe_acidemia_events[severe_acidemia == TRUE],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  ## require acidemia and AKI resasonably close together
  .[abs(as.numeric(difftime(acidemia_time,
                            aki_stage_time,
                            units = "hours"))) <= 24] %>% 
  .[, eligibility_time := pmax(acidemia_time,
                               aki_stage_time)] %>% 
  .[order(eligibility_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  merge(.,
        sodium_bicarb_events,
        by = "stay_id",
        all.x = TRUE,
        allow.cartesian = TRUE) %>% 
  ## Bicarbonate given after meeting all criteria
  .[bicarbonate_time >= eligibility_time] %>% 
  .[order(bicarbonate_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_onset_time,
        acidemia_time,
        ph,
        aki_stage_time,
        aki_stage,
        eligibility_time,
        first_bicarbonate_time = bicarbonate_time,
        bicarbonate_type,
        bicarbonate_amount,
        bicarbonate_unit)] %>% 
  .[, rec_96_sodium_bicarb := !is.na(first_bicarbonate_time)]


## Combine all recommendations into one dataset --------------------------------
DT_ssc_encoded <- sepsis_base_cohort_data %>% 
  merge(.,
        rec_7[, .(subject_id,
                hadm_id,
                stay_id,
                rec_7_culture_before_antimicrobial)],
         by = c("subject_id",
                "hadm_id",
                "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
      rec_8[, .(subject_id,
                hadm_id,
                stay_id,
                rec_8_lactate_measured,
                rec_8_lactate_measured_within_1h,
                rec_8_lactate_measured_within_3h,
                rec_8_lactate_measured_within_6h)] %>% 
        unique(),
      by = c("subject_id",
             "hadm_id",
             "stay_id"),
      all.x = TRUE,
      all.y = FALSE,
      allow.cartesian = FALSE) %>% 
  merge(.,
        rec_10[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_10_crystalloid_within_3h,
                   rec_10_crystalloid_within_3h_bolus,
                   rec_10_eligibility)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_12[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_12_vaso_without_central_delay,
                   rec_12_eligibility)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_13[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_13_map_target_65,
                   rec_13_eligibility)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_15[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_15_icu_6h,
                   rec_15_icu_6h_preicu)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_16[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_16_antimicrobial_within_1h,
                   rec_16_antimicrobial_within_1h_strict,
                   rec_16_antimicrobial_within_3h,
                   rec_16_eligibility)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_17[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_17_antimicrobial_within_1h,
                   rec_17_antimicrobial_within_1h_strict,
                   rec_17_antimicrobial_within_3h,
                   rec_17_eligibility)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_42[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_42_bp_monitoring,
                   rec_42_eligibility)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_43[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_43_crystalloid_first_line,
                   rec_43_eligibility)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_44[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_44_balanced_over_saline)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_45[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_45_crystalloid_no_albumin)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_53[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_53_first_vaso_norepi)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_55[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_55_first_vaso_norepi)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_56[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_56_vasopressin_after_escalating_norepi)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_57[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_57_epi_after_escalating_norepi_vaso)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_67[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_67_first_oxygen_hfnc)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_68[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_68_first_oxygen_hfnc_over_niv)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_69[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_69_hfnc_over_hfnc_niv_alternating)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_71[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_71_high_tidal_volume,
                   rec_71_low_tidal_volume)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_72[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_72_tidal_volume_6_8,
                   rec_72_tower_tidal_volume_4_to_less_6)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_79[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_79_iv_corticosteroids)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_89[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_89_active_fluid_removal)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_90[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_90_restrictive_transfusion)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_91[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_91_early_enteral_nutrition)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_92[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_92_insulin_started_at_glucose_180)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_96[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_96_sodium_bicarb)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE)

## Save dataset
saveRDS(DT_ssc_encoded,
        here::here("Data/DT_ssc_encoded.rds"))

ssc_recommendations_encoded <- ssc_recommendations %>% 
  .[rec_num %in% c(7,
                   8,
                   10,
                   12,
                   13,
                   15,
                   16,
                   17,
                   42,
                   43,
                   44,
                   45,
                   53,
                   55,
                   56,
                   57,
                   67,
                   68,
                   69,
                   71,
                   72,
                   79,
                   89,
                   90,
                   91,
                   96)]
  
