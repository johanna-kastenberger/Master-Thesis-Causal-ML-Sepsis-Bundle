## Source global file
source(here::here("Code/global.R"))

## Source data extraction / preparation
source(here::here("Code/data_preparation.R"))

## Source functions
source(
  list.files(
    here::here("Code", "Functions"),
    pattern = "\\.R$",
    full.names = TRUE))

## Table of SSC recommendations
ssc_recommendations <- readRDS(here::here("Data/ssc_recommendations.rds")) %>% 
  setDT()

## Sepsis & Septic Shock Base Cohort
sepsis_base_cohort_data <- readRDS(here::here("Data/sepsis_base_cohort_data.rds")) %>% 
  setDT()

## Define sepsis patient cohort ------------------------------------------------
sepsis_base_cohort_data <- sepsis_base_cohort_data %>% 
  copy() %>% 
  ## patients with sepsis
  .[sepsis3 == TRUE] %>% 
  .[, time_to_sepsis :=
      lubridate::time_length(
          suspected_infection_time -
            icu_intime,
        "hours")]
  ## sepsis diagnosis within +/-24h to ICU admission
  .[suspected_infection_time >= icu_intime - hours(24) &
      suspected_infection_time <= icu_intime + hours(24)] %>% 
  ## Keep only first hospitalization & ICU stay per patient
  .[order(subject_id,
          hadm_id,
          stay_id,
          icu_intime),
    .SD[1],
    by = .(subject_id)] %>% 
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
  copy() %>%
  .[label == "Admission Weight (Kg)"]

## Admission weight (kg)
admission_weight_kgs <- all_chartevents %>%
  copy() %>%
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
  .[admission_weight_kg >= 40 &
      admission_weight_kg <= 200]

## Check that only one value for patient is included
stopifnot(nrow(admission_weight_kgs[, .N, by = stay_id] %>% .[N > 1]) == 0)

## Item IDs for admission weight (lbs)
admission_weight_lb_itemid <- d_items %>% 
  copy() %>% 
  .[label == "Admission Weight (lbs.)"]

## Admission weight (lb)
admission_weight_lbs <- all_chartevents %>% 
  copy() %>% 
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
  .[admission_weight_lb >= 40 * 2.20462 &
      admission_weight_lb <= 200 * 2.20462]

## Validate weight entries by comparing admission 
## weight (kg) and admission weight (lb)
admission_weight_merged <- admission_weight_lbs %>% 
  copy() %>% 
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
    copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[grepl("Daily Weight",
          label,
          ignore.case = TRUE)]

## Closest daily weight to sepsis onset per stay ID
daily_weight_candidates <- all_chartevents %>%
  copy() %>% 
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
  .[daily_weight >= 40 &
      daily_weight <= 200] %>% 
  .[order(stay_id,
          daily_weight_time)] %>% 
  .[, n_weight := .N,
    by = stay_id]


## Singular entry for daily weight per stay ID
## contains only daily weight entries which have been validated
## and are closer to sepsis onset than admission weight and 
## should therefore be used for fluid amount calculation
daily_weight_single <- daily_weight_candidates %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
sepsis_base_cohort_data <-
  sepsis_base_cohort_data %>% 
  copy() %>% 
  merge(.,
        weight_for_fluids,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE)


## xx 4 RECOMMENDATION ---------------------------------------------------------
rec4_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 4, rec_label]

## 7 RECOMMENDATION ------------------------------------------------------------
rec7_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 7, rec_label]

## recommendation 7 was encoded using the MIMIC-IV Sepsis-3 variables culture_time 
## and antibiotic_time. Compliance was defined as blood culture collection before 
## the first recorded antibiotic administration. Because timing of non-antibiotic 
## antimicrobial agents was not incorporated into the Sepsis-3 derived table, this 
## represents a proxy for culture collection before antimicrobial therapy.

rec_7 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        culture_time,
        antibiotic_time)] %>% 
  # .[, `:=` (blood_culture_collected = !is.na(culture_time),
  #           
  #           blood_culture_before_antibiotics = !is.na(culture_time) &
  #             !is.na(antibiotic_time) &
  #             culture_time <= antibiotic_time,
  #           
  #           blood_culture_within_3h = !is.na(culture_time) &
  #             !is.na(suspected_infection_time) &
  #             culture_time >= suspected_infection_time &
  #             culture_time <= suspected_infection_time + hours(3))] %>% 
  .[, rec_7_culture_before_antimicrobial := !is.na(culture_time) &
      !is.na(antibiotic_time) &
      !is.na(suspected_infection_time) &
      !is.na(suspected_infection_time) &
      culture_time >= suspected_infection_time &
      culture_time <= suspected_infection_time + hours(3) &
      culture_time <= antibiotic_time] %>% 
  .[, rec_7_eligibility := fcase(
    !is.na(rec_7_culture_before_antimicrobial),
    "eligible",
    
    default = NA_character_)]

## 8 RECOMMENDNA_character_## 8 RECOMMENDATION ------------------------------------------------------------
rec8_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 8, rec_label]

## Item IDs corresponding to blood lactate measurement
lactate_itemid <- lab_itemids %>% 
  copy() %>% 
  .[label == "Lactate" &
      grepl("blood",
            fluid,
            ignore.case = TRUE)]

## Lactate Events
lactate_events <- all_labevents_icu %>%
  copy() %>% 
  .[itemid %in% lactate_itemid$itemid,
    .(subject_id,
      hadm_id,
      stay_id,
      charttime,
      valuenum)]

## Encode recommendation 8
rec_8 <- sepsis_base_cohort_data %>%
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id, 
        suspected_infection_time,
        fluid_resuscitation_start,
        fluid_resuscitation_end)] %>% 
  unique() %>% 
  merge(.,
        lactate_events,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, lactate_measured := !is.na(valuenum) &
      ## restrict measurement to initial resuscitation window
                            charttime >= fluid_resuscitation_start &
                            charttime <= fluid_resuscitation_end] %>% 
  .[, rec_8_lactate_measured_at_suspicion := any(lactate_measured),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        rec_8_lactate_measured_at_suspicion)] %>% 
  unique() %>% 
  .[, rec_8_eligibility := fcase(
    
    !is.na(rec_8_lactate_measured_at_suspicion),
    "eligible",
    
    default = NA_character_
  )]

## 10 RECOMMENDATION -----------------------------------------------------------
## Recommendation 10 targets patients with sepsis-induced hypoperfusion or 
## septic shock. Because sepsis-induced hypoperfusino is not explicitly defined
## in the Sepsis-3 framework and cannot be directly identified in MIMIC-IV, 
## eligibility for this recommendation was restricted to patients meeting the 
## septic shock proxy.
rec10_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 10, rec_label]

## List of crystalloid fluids (isotonic saline and balanced electrolyte solutions)
patterns_crystalloid <- c("nacl 0.9%",
                          "crystalloid",
                          "lactated",
                          "\\blr\\b",
                          "plasma-lyte",
                          "normosol",
                          "normasol",
                          "plasmalyte")

## Crystalloid item IDs
crystalloid_itemids <- d_items %>%
  copy() %>% 
  .[grepl(paste(patterns_crystalloid,
                collapse = "|"),
          label,
          ignore.case = TRUE) &
      !grepl("OR Crystalloid Intake|PACU",
             label,
             ignore.case = TRUE)]

## Determine units for crystalloid administration rate
crystalloid_units <- all_inputevents[itemid %in% crystalloid_itemids$itemid, unique(rateuom)]

## Crystalloid fluids within 3h
rec_10 <- sepsis_base_cohort_data %>%
  copy() %>%
  # .[septic_shock_proxy == TRUE |
  #     hypoperfusion_proxy == TRUE] %>% 
  merge(.,
        all_inputevents[itemid %in% crystalloid_itemids$itemid],
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, fluid_resuscitation_window := 
      starttime < fluid_resuscitation_end &
      endtime > fluid_resuscitation_start] %>% 
  .[fluid_resuscitation_window == TRUE,
    `:=` (overlap_start = pmax(starttime,
                              fluid_resuscitation_start),
         overlap_end = pmin(endtime,
                            fluid_resuscitation_end))] %>% 
  .[, overlap_hours := 
      lubridate::time_length(
        overlap_end -
          overlap_start,
        "hours")] %>% 
  .[, fluid_ml_3h := fcase(
    rateuom == "mL/hour",
    overlap_hours * rate,
    
    rateuom == "mL/min",
    overlap_hours * 60 * rate,
    
    !is.na(amountuom) & amountuom == "mL" &
      starttime >= fluid_resuscitation_start &
      starttime < fluid_resuscitation_end,
    amount,
    default = NA_real_)] %>% 
  .[, crystalloid_ml_3h := 
      sum(fluid_ml_3h,
          na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  # .[, start_day := lubridate::floor_date(starttime,
  #                                        "days")] %>% 
  .[, crystalloid_target := 30 * weight_kg_for_fluids] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        fluid_resuscitation_start,
        fluid_resuscitation_end,
        weight_kg_for_fluids,
        crystalloid_ml_3h,
        crystalloid_target)] %>% 
  unique() %>% 
  .[, rec_10_crystalloid_within_3h := 
      fifelse(
        septic_shock_proxy == TRUE &
        crystalloid_ml_3h >= crystalloid_target,
        TRUE,
        fifelse(septic_shock_proxy == TRUE &
                  crystalloid_ml_3h < 
                  crystalloid_target,
                FALSE,
                NA))] %>% 
  .[, rec_10_eligibility := fcase(
    
    !is.na(rec_10_crystalloid_within_3h),
    "eligible",
    
    septic_shock_proxy == TRUE &
      is.na(weight_kg_for_fluids),
    "missing weight",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_)]


## 12 RECOMMENDATION -----------------------------------------------------------
## Proxy: first vasopressor time before first central line time
## implies peripheral vasopressor administration
rec12_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 12, rec_label]

## cvc itemids
cvc_patterns <- c("PICC Line",
                  "Trauma line",
                  "Tunneled (Hickman) Line",
                  "Tunneled Access Line")

## Central line item IDs
cvc_itemids <- d_items %>% 
  copy() %>% 
  .[category %in% c("Access Lines - Invasive",
                    "CVL Insertion",
                    "PICC Line Insertion")] %>% 
  .[label %in% cvc_patterns]

## First central line
first_cvc <- all_procs %>%
  copy() %>% 
  .[itemid %in% cvc_itemids$itemid] %>% 
  .[!is.na(starttime)] %>% 
  .[order(starttime), 
    .SD[1],
    by = stay_id] %>% 
  .[, .(stay_id,
        first_cvc_time = starttime,
        first_cvc_itemid = itemid,
        first_cvc_label = label)]

## Define first vasopressor use before first central line
rec_12 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  # .[septic_shock_proxy == TRUE] %>% 
  merge(.,
        first_cvc,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, rec_12_vaso_before_cvc := septic_shock_proxy == TRUE &
      !is.na(first_vaso_time) &
      (is.na(first_cvc_time) |
      first_vaso_time < first_cvc_time)] %>% 
  .[, rec_12_eligibility := fcase(
    
    !is.na(rec_12_vaso_before_cvc),
    "eligible",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    is.na(first_vaso_time),
    "no vasopressors administered",
    
    is.na(first_cvc_label), 
    "no central line")]


## 13 RECOMMENDATION -----------------------------------------------------------
rec13_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 13, rec_label]

## MAP item IDs
map_itemids <- chart_itemids %>% 
  copy() %>% 
  .[label %in% c("Arterial Blood Pressure mean",
                 "Non Invasive Blood Pressure mean")]

## MAP events
map_events <- all_chartevents %>% 
  copy() %>% 
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
  .[, chart_hour := lubridate::floor_date(charttime,
                                          "hour")] %>% 
  .[, .(map = mean(valuenum,
                   na.rm = TRUE)),
    by = .(stay_id,
           chart_hour,
           map_source)] %>% 
  ## If available, arterial MAP is preferred over non-invasive MAP
  .[order(stay_id,
          chart_hour,
          map_source),
    .SD[1],
    by = .(stay_id,
           chart_hour)]

## Median MAP 24h after septic shock onset around 65mmHg +/- 5mmHg
rec_13 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        suspected_infection_time,
        septic_shock_onset_time,
        fluid_resuscitation_start,
        fluid_resuscitation_end
        )] %>% 
  unique() %>% 
  # .[septic_shock_proxy == TRUE &
  #     !is.na(first_vaso_time)] %>% 
  merge(.,
        map_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## Define "initial" resuscitation window as the first 3 hours or vasopressor inititated (if administered)
  .[, fluid_resuscitation_window := chart_hour >= fluid_resuscitation_start &
      chart_hour <= fluid_resuscitation_end] %>% 
  ## only look at MAP values within early resuscitation window
  # .[initial_resuscitation_window == TRUE] %>% 
  .[fluid_resuscitation_window == TRUE, 
     median_map := median(map, na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  ## fill rows of missing MAP values for patients with actual MAP values
  .[any(fluid_resuscitation_window == TRUE),
    fluid_resuscitation_window := TRUE,
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
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
        fluid_resuscitation_start,
        fluid_resuscitation_end,
        fluid_resuscitation_window,
        septic_shock_proxy,
        septic_shock_onset_time,
        median_map)] %>% 
  unique() %>% 
  .[septic_shock_proxy == TRUE, rec_13_map_target_65 :=
      fluid_resuscitation_window == TRUE &
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
  copy() %>% 
  .[rec_num == 15, rec_label]

## Time between sepsis onset and ICU admission <= 6 hours
rec_15 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, hours_to_icu :=
      lubridate::time_length(icu_intime - suspected_infection_time,
                             unit = "hours")] %>% 
  .[, rec_15_icu_6h := hours_to_icu <= 6] %>% 
  .[, rec_15_eligibility := fcase(
    
    !is.na(rec_15_icu_6h),
    "eligible",
    
    default = NA_character_
  )] %>% 
  .[, summary(rec_15_icu_6h)]


## 16 & 17 RECOMMENDATION -----------------------------------------------------------
## Combine into one recommendation
rec16_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 16, rec_label]

rec17_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 17, rec_label]

## Antibiotic therapy within 1 hour of septic shock onset
rec_16_17 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  # .[septic_shock_proxy == TRUE] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        suspected_infection_time,
        antibiotic_time)] %>% 
  unique() %>% 
  .[, time_to_antib := time_length(antibiotic_time - suspected_infection_time,
                                   unit = "hours")] %>% 
  .[, rec_16_17_antib_within_1h := !is.na(time_to_antib) &
      ## up to one hour before sepsis onset to account for delayed documentation
      time_to_antib >= -1 &
      time_to_antib <= 1] %>% 
  .[, rec_16_17_antib_within_3h := !is.na(time_to_antib) &
      ## up to one hour before sepsis onset to account for delayed documentation
      time_to_antib >= -1 &
      time_to_antib <= 3] %>% 
  .[, rec_16_17_eligbility := fcase(
    
    !is.na(rec_16_17_antib_within_1h),
    "eligible",
    
    default = NA_character_
  )]
  
# ## 17 RECOMMENDATION 
# rec17_label <- ssc_recommendations %>% 
#   copy() %>% 
#   .[rec_num == 17, rec_label]
# 
# rec_17 <- sepsis_base_cohort_data %>% 
#   copy() %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         suspected_infection_time,
#         antibiotic_time,
#         septic_shock_proxy)] %>% 
#   unique() %>% 
#   .[, time_to_antib := time_length(antibiotic_time - suspected_infection_time,
#                                    unit = "hours")] %>% 
#   .[septic_shock_proxy == FALSE |
#       is.na(septic_shock_proxy), rec_17_sepsis_antib_within_1h := !is.na(time_to_antib) &
#       time_to_antib >= 0 &
#       time_to_antib <= 1] %>% 
#   .[, rec_16_eligibility := fcase(
#     
#     !is.na(rec_17_sepsis_antib_within_1h),
#     "eligible",
#     
#     septic_shock_proxy == TRUE,
#     "no septic shock or unknown")]

## xx 33 RECOMMENDATION -----------------------------------------------------------
rec33_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 33, rec_label]

## This recommendation was not encoded because prolonged versus bolus beta-lactam
## infusion required administration-level infusion duration or rate information,
## which could not be reliably determined from the structured medication data available

## 42 RECOMMENDATION -----------------------------------------------------------
rec42_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 42, rec_label]

## Item IDs corresponding to any blood pressure monitoring
blood_pressure_monitoring_itemids <- d_items %>% 
  copy() %>% 
  .[grepl("blood pressure",
          label,
          ignore.case = TRUE) &
      category == "Routine Vital Signs"]

## Blood pressure monitoring events
bp_events <- all_chartevents %>% 
  copy() %>% 
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

rec_42 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time,
        fluid_resuscitation_start,
        fluid_resuscitation_end)] %>%
  unique() %>% 
  ## define initital resuscitation window as the first 3 hours
  ## after sepsis onset and 1 hour before sepsis onset to account
  ## for delayed documentation
  merge(.,
        bp_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, fluid_resuscitation_window := 
      charttime >= fluid_resuscitation_start &
      charttime <= fluid_resuscitation_end] %>% 
  .[, n_bp_fluid_window := 
      ifelse(fluid_resuscitation_window == TRUE,
             sum(fluid_resuscitation_window,
                                 na.rm = TRUE),
             NA_real_),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[fluid_resuscitation_window == TRUE, .(subject_id,
                                          hadm_id,
                                          stay_id,
                                          septic_shock_proxy,
                                          fluid_resuscitation_window,
                                          n_bp_fluid_window)] %>% 
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
        all = TRUE) %>% 
  .[is.na(fluid_resuscitation_window),
    fluid_resuscitation_window := FALSE] %>% 
  .[is.na(n_bp_fluid_window),
    n_bp_fluid_window := 0] %>% 
  .[septic_shock_proxy == TRUE, rec_42_bp_monitoring :=
      n_bp_fluid_window > 0] %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
rec_43 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  merge(.,
        resuscitation_fluid_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  ## define initital resuscitation window as the first 3 hours
  ## after sepsis onset and 1 hour before to account for 
  ## delayed documentation
  .[, fluid_resuscitation_window := 
      !is.na(fluid_start) &
      fluid_start >= fluid_resuscitation_start &
      fluid_start <= fluid_resuscitation_end] %>% 
  .[, fluid_status := fifelse(
    all(fluid_resuscitation_window == FALSE),
    "no fluid within window",
    fifelse(
      any(fluid_resuscitation_window == TRUE),
      "at least one fluid within window",
      NA_character_
    )
  ),
  by = .(subject_id,
         hadm_id,
         stay_id)] %>% 
  ## keep only rows within initial resuscitation window for 
  ## patients who received at least one fluid within window
  .[fluid_status == "no fluid within window" |
      (fluid_status == "at least one fluid within window" &
         fluid_resuscitation_window == TRUE)] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          fluid_start), 
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, rec_43_crystalloid_first_line := 
      fifelse(fluid_status == "at least one fluid within window" &
                fluid_type == "Crystalloid",
              TRUE,
              fifelse(
                fluid_status == "at least one fluid within window" &
                  fluid_type == "Colloid",
                FALSE,
                NA))] %>% 
  .[, rec_43_eligibility := fcase(
    
    !is.na(rec_43_crystalloid_first_line),
    "eligible",
    
    fluid_status == "no fluid within window",
    "no fluid administered within initial resuscitation window"
  )]

## 44 & 45 RECCOMENDATION -----------------------------------------------------------
rec44_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 44, rec_label]

rec_45_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 45, rec_label]

## Define balanced crystalloid item IDs
balanced_crystalloid_itemids <- crystalloid_itemids %>% 
  copy() %>% 
  .[grepl("LR|Normosol",
          label,
          ignore.case = TRUE)]

## Define 0.9% Saline item ID
saline_crystalloid_itemids <- crystalloid_itemids %>% 
  copy() %>% 
  .[grepl("Saline|NaCl",
          label,
          ignore.case = TRUE)]

## Albumin administration item IDs
albumin_itemids <- d_items %>% 
  copy() %>% 
  .[category %in% c("Blood Products/Colloids",
                    "Fluids - Other (Not In Use)")] %>% 
  .[grepl("albumin",
          label,
          ignore.case = TRUE)]


## Resuscitation IV Fluid Intake
resuscitation_fluid_events <- all_inputevents %>% 
  copy() %>% 
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
  copy() %>% 
  merge(.,
        resuscitation_fluid_events,
        by = "stay_id",
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
  copy() %>% 
  .[rec_num == 53, rec_label]

## First line vasopressor Norepinephrine over Dopamine or Epinephrine
## If first line vasopressor not Norepinephrine, Dopamine, or Epinephrine,
## NA instead of non-adherance 

first_vasopressor <- sepsis_base_cohort_data %>% 
  copy() %>% 
  merge(.,
        vasopressor_events[, .(stay_id,
                               vaso_start = starttime,
                               vaso_end = endtime,
                               itemid,
                               amount,
                               rate)],
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        vasopressor_lookup,
        by = "itemid") %>%
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
  copy() %>% 
  .[rec_num == 55, rec_label]

## First line vasopressor Norepinephrine over Vasopressin or Angiotensin II
## If first line vasopressor not Norepinephrine, Vasopressin, or Angiotensin II,
## NA instead of non-adherance 
rec_55 <- sepsis_base_cohort_data %>% 
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
  .[, rec_55_first_vaso_norepi := fifelse(
    grepl("Norepinephrine",
          first_vaso_type,
          ignore.case = TRUE),
    TRUE,
    ifelse(
      grepl("Vasopressin|Angiotensin",
            first_vaso_type,
            ignore.case = TRUE),
      FALSE,
      NA
    )
  )] %>% 
  .[, rec_55_eligbility := fcase(
    
    !is.na(rec_55_first_vaso_norepi),
    "eligible",
    
    septic_shock_proxy == TRUE &
      !is.na(first_vaso_type) &
      is.na(rec_55_first_vaso_norepi),
    "other vasoporessor administered",
    
    septic_shock_proxy == FALSE | is.na(septic_shock_proxy),
    "no septic shock or unknown"
  )]


## 56 RECOMMENDATION -----------------------------------------------------------
rec56_label <- ssc_recommendations %>% 
  copy() %>% 
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
  copy() %>% 
  .[grepl("Norepinephrine",
          label,
          ignore.case = TRUE)]

## Escalating doses of Norepinephrine
norepi_escalating_doses <- all_inputevents %>% 
  copy() %>% 
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
        by = "stay_id",
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
  copy() %>% 
  .[grepl("Vasopressin",
          label,
          ignore.case = TRUE)]

## Vasopressin administration events
vasopressin_events <- vasopressor_events %>% 
  copy() %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
## vasopressin item ID
  .[itemid == vasopressin_itemid$itemid] %>% 
  .[!is.na(amount) & amount > 0 &
      !is.na(rate) & rate > 0] %>% 
  .[, .(stay_id,
        vasopressin_start = starttime)] %>% 
  unique() %>% 
  setorder(stay_id,
           vasopressin_start)

## Define vasopressin added after escalating doses of Norepinephrine
vasopressin_afer_escalating_norepi <- norepi_escalating_doses %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[rec_num == 57, rec_label]

## Item ID for Norepinephrine
norepi_itemid <- d_items %>% 
  copy() %>% 
  .[grepl("Norepinephrine",
          label,
          ignore.case = TRUE)]

## Escalating doses of Norepinephrine >= 0.8µg/kg/min
norepi_escalating_doses_0.8 <- all_inputevents %>% 
  copy() %>% 
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
        by = "stay_id",
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
  copy() %>% 
  .[grepl("Epinephrine",
          label,
          ignore.case = TRUE) &
      !grepl("Norepinephrine",
             label,
             ignore.case = TRUE)]

## Epinephrine administration events
epinephrine_events <- all_inputevents %>% 
  copy() %>% 
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
  copy() %>%
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[grepl(paste0(resp_support_patterns,
                 collapse = "|"),
          label,
          ignore.case = TRUE) &
      !grepl("O2 Flow",
             label,
             ignore.case = TRUE)]

## Respiratory support events
resp_support_events <- all_chartevents %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[grepl("PaO2|PO2",
          abbreviation,
          ignore.case = TRUE) &
      grepl("arterial",
            label,
            ignore.case = TRUE) &
      category == "Labs"]

## PaO2 events
pao2_events <- all_chartevents %>% 
  copy() %>% 
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
  copy() %>%
  .[pao2 >= 30 &
      pao2 <= 600]

## Item ID for FiO2 (fraction of inspired oxygen)
fio2_itemid <- chart_itemids %>% 
  copy() %>% 
  .[grepl("Inspired O2 Fraction",
          label,
          ignore.case = TRUE)]

## FiO2 events
fio2_events <- all_chartevents %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[grepl("SpO2",
          abbreviation,
          ignore.case = TRUE) &
      category == "Respiratory"]

## SpO2 events
spo2_events <- all_chartevents %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>%
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[rec_num == 68, rec_label]

## For adults with sepsis and AHRF, HFNC as initial therapy over NIPPV/NIV
rec_68 <- sepsis_base_cohort_data %>% 
  copy() %>% 
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
  copy() %>% 
  .[rec_num == 69, rec_label]

## Define HFNC/NIV respiratory support events
## For adults with sepsis and AHRF, HFNC as initial therapy over NIPPV/NIV
hfnc_niv_events <- resp_support_events %>% 
  copy() %>% 
  .[resp_support_type %in% c("HFNC",
                             "NIV"),
    .(stay_id,
           resp_support_time,
           resp_support,
           resp_support_type)] %>% 
      unique()

rec_69 <- sepsis_base_cohort_data %>% 
  copy() %>% 
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
  copy() %>% 
  .[rec_num == 71, rec_label]

## Oxygen therapy HFNC & NIV
oxygen_therapy_hfnc_niv <- oxygen_therapy %>% 
  copy() %>% 
  .[, resp_support_type := fcase(
    oxygen_type == "HFNC", "HFNC",
    oxygen_type == "NIV", "NIV",
    grepl("Endotracheal tube|Tracheostomy tube",
          value,
          ignore.case = TRUE),
    "Invasive ventilation",
    default = NA_character_
  )] %>% 
  .[resp_support_type %in% c("HFNC",
                             "NIV",
                             "Invasive ventilation")] %>% 
  .[, chart_hour := lubridate::floor_date(charttime,
                                          "hour")] %>% 
  .[, .(stay_id,
        chart_hour,
        itemid,
        resp_support_type)]

## Calculate PBW (predicted body weight) from height and sex
predicted_body_weight <- all_heightevents %>% 
  copy() %>% 
  .[!is.na(height_cm)] %>% 
  .[height_cm >= 120 &
      height_cm <= 230] %>% 
  .[, .(height_cm = median(height_cm, 
                           na.rm = TRUE)),
    by = subject_id] %>% 
  merge(.,
        sepsis_base_cohort_data,
        by = "subject_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        gender,
        height_cm)] %>% 
  unique() %>% 
  .[, pbw_kg := fcase(
    gender == "M", 50 + 0.905 * (height_cm - 152.4),
    gender == "F", 45.5 + 0.905 * (height_cm - 152.4),
    default = NA_real_
  )]

## Item ID for hourly set tidal volume
tidal_volume_itemids <- chart_itemids %>% 
  copy() %>% 
  .[grepl("tidal volume",
          label,
          ignore.case = TRUE) &
      grepl("set",
            label,
            ignore.case = TRUE)]

## Extract hourly set tidal volume (mL/kg) for oxygenation events
tidal_volume_events <- all_chartevents %>% 
  copy() %>% 
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
  copy() %>% 
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
        all.y = FALSE) %>% 
  merge(.,
        resp_support_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  # .[resp_support_type == "Invasive ventilation"] %>% 
  merge(.,
        tidal_volume_events[, .(subject_id,
                                hadm_id,
                                stay_id,
                                tv_hour = chart_hour,
                                tidal_volume_ml,
                                pbw_kg,
                                tidal_vol_per_kg,
                                tidal_volume_strategy)] %>% unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, ards_timing := tv_hour >= ards_onset_time - hours(1) &
      tv_hour <= ards_onset_time + hours(24)] %>% 
  .[order(tv_hour),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        ards_time,
        first_tidal_volume_time = tv_hour,
        PaO2,
        SpO2,
        FiO2,
        pf_ratio,
        sf_ratio,
        tidal_volume_ml,
        pbw_kg,
        tidal_vol_per_kg,
        tidal_volume_strategy)] %>% 
        .[, `:=` (rec_71_low_tidal_volume = tidal_volume_strategy == "Low tidal volume",
                  rec_71_high_tidal_volume = tidal_volume_strategy == "High tidal volume")]


## 72 RECOMMENDATION -----------------------------------------------------------
rec72_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 72, rec_label]

## Define AHRF with ARDS
ahrf_without_ards <- ahrf_events %>% 
  copy() %>% 
  .[, .(stay_id,
        charttime,
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
  .[charttime >= suspected_infection_time - hours(6) &
      charttime <= suspected_infection_time + hours(6) &
      ahrf_event == TRUE] %>% 
  .[, .(ahrf_time = min(charttime)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  ## Exclude ARDS patients
  .[stay_id %notin% ards_proxy$stay_id]

## Recommendation 72:
## AHRF without ARDS, tidal volume 6-8 ml/kg PBW
## over lower tidal volume 4 to <6 ml/kg PBW
rec_72 <- ahrf_without_ards %>% 
  copy() %>% 
  ## Restrict to invasive ventilation
  merge(.,
        oxygen_therapy_hfnc_niv[resp_support_type == "Invasive ventilation",
        .(stay_id,
          vent_hour = chart_hour)] %>% 
  unique(),
  by = "stay_id",
  all = FALSE) %>% 
  ## Invasive ventilation close to AHRF onset
  .[vent_hour >= lubridate::floor_date(ahrf_time,
                                       "hour") &
      vent_hour <= lubridate::floor_date(ahrf_time + hours(24),
                                         "hour")] %>% 
  merge(.,
        tidal_volume_events[, .(subject_id,
                                hadm_id,
                                stay_id,
                                tv_hour = chart_hour,
                                tidal_volume_ml,
                                pbw_kg,
                                tidal_vol_per_kg)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>% 
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
  copy() %>% 
  .[rec_num == 79, rec_label]

## Corticosteroid administration
## Prednisone & prednisolone are oral corticosteroids --> not IV
## Acetate steroids are not administered intravenously
corticosteroid_events <- corticosteroid_emar_icu %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[grepl(paste(diuretic_patterns,
                 collapse = "|"),
          label,
          ignore.case = TRUE)]

## Diuretic administration
diuretic_events <- all_inputevents %>% 
  copy() %>% 
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
  copy() %>% 
  .[grepl(paste(extracorporeal_patterns,
                collapse = "|"),
          label,
          ignore.case = TRUE)]

## Extracorporeal fluid removal events
extracorporeal_fluid_removal_events <- all_chartevents %>% 
  copy() %>% 
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
fluid_bolus_itemids <- d_items %>% 
  copy() %>% 
  .[grepl(paste(c(patterns_crystalloid,
                  "fluid bolus"),
                collapse = "|"),
          label,
          ignore.case = TRUE) &
      !grepl("PACU|OR",
             label,
             ignore.case = FALSE)]

## Fluid bolus events (bedside resuscitation)
fluid_bolus_hourly <- all_inputevents %>% 
  copy() %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## fluid bolus item IDs
  .[itemid %in% fluid_bolus_itemids$itemid] %>% 
  .[amount >= 250] %>% 
  .[, chart_hour := lubridate::floor_date(starttime,
                                          "hour")] %>% 
  .[, .(fluid_bolus_ml = sum(amount,
                             na.rm = TRUE)),
    by = .(stay_id,
           chart_hour)] %>% 
  .[, ongoing_volume_expansion := fluid_bolus_ml >= 500]

## Vasopressor hourly administration events
vasopressor_hourly <- vasopressor_events %>% 
  copy() %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    septic_shock_proxy,
                                    septic_shock_onset_time)] %>% unique(),
        by = "stay_id",
        all = FALSE) %>% 
  .[septic_shock_proxy == TRUE] %>% 
  .[!is.na(starttime) & !is.na(endtime)] %>% 
  merge(.,
        vasopressor_itemids[, .(itemid,
                                label)],
        by = "itemid",
        all.x = TRUE) %>% 
  .[, norepi_equiv_rate := fcase(
    label == "Norepinephrine",
    rate,
    
    label == "Epinephrine",
    rate,
    
    grepl("Phenylephrine",
          label,
          ignore.case = TRUE),
    rate / 10,
    
    label == "Dopamine",
    rate / 100,
    
    label == "Vasopressin" & rateuom == "units/hour",
    (rate / 60) / 0.03 * 0.1,
    
    ## Angiotensin is not reliably convertible to NE-equivalent
    grepl("Angiotensin II",
          label),
    NA_real_,
    
    default = NA_real_
  )] %>% 
  .[!is.na(norepi_equiv_rate) & norepi_equiv_rate > 0] %>% 
  .[, vp_event_id := .I] %>% 
  .[, .(
    chart_hour = seq(
      lubridate::floor_date(starttime, "hour"),
      lubridate::floor_date(endtime, "hour"),
      by = "hour"
    ),
    norepi_equiv_rate = norepi_equiv_rate
  ),
  by = .(vp_event_id, stay_id, itemid)] %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[rec_num == 90, rec_label]

## Red blood cell transfusion item IDs
rbc_transfusion_itemids <- d_items %>% 
  copy() %>% 
  .[grepl("packed red blood cells|packed red cells",
          label,
          ignore.case = TRUE)]

## RBC transfusions
rbc_transfusion_events <- all_inputevents %>% 
  copy() %>% 
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
  copy() %>% 
  .[grepl("hemoglobin",
          label,
          ignore.case = TRUE)]

## Hemoglobin measurements
hemoglobin_events <- all_chartevents %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[rec_num == 91, rec_label]

## Item ID for enteral nutrition
enteral_nutrition_itemid <- d_items %>% 
  copy() %>% 
  .[grepl("^enteral nutrition ingredient",
          label,
          ignore.case = TRUE)]

## Enteral nutrition administration
enteral_nutrition_events <- all_ingredients %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[rec_num == 92, rec_label]

## Insulin item IDs
insulin_itemids <- d_items %>% 
  copy() %>% 
  .[grepl("insulin",
          label,
          ignore.case = TRUE) &
      category == "Medications"]

## Insulin administration
insulin_events <- all_inputevents %>% 
  copy() %>% 
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
  copy() %>% 
  .[grepl("glucose",
          label,
          ignore.case = TRUE) &
      !grepl("apache",
             label,
             ignore.case = TRUE) &
      category == "Labs"]

## Glucose measuremnts
glucose_events <- all_chartevents %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[rec_num == 96, rec_label]

## Arterial blood gas
ph_itemids <- d_items %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[grepl("cystatin|scr",
          label,
          ignore.case = TRUE)]

## Item ID for serum creatinine
serum_creatinine_itemid <- d_items %>% 
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
   copy() %>% 
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
   copy() %>% 
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
   copy() %>% 
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
  copy() %>%
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
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
  copy() %>% 
  .[grepl("bicarbonate|nahco3|sodium bicarb",
        label,
        ignore.case = TRUE) &
      !grepl("ingredient",
             category,
             ignore.case = TRUE)]

## Sodium bicarbonate admininstration
sodium_bicarb_events <- all_inputevents %>% 
  copy() %>% 
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
  copy() %>% 
  merge(.,
        aki_classification[, .(aki_stage,
                               aki_stage_time)],
        )
  # .[septic_shock_proxy == TRUE] %>% 
  merge(.,
        severe_acidemia_events[severe_acidemia == TRUE],
        by = "stay_id",
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  merge(.,
        aki_2_3_events,
        by = "stay_id",
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  ## require acidemia and AKI resasonably close together
  .[abs(as.numeric(difftime(acidemia_time,
                            aki_time,
                            units = "hours"))) <= 24] %>% 
  .[, eligibility_time := pmax(acidemia_time,
                               aki_time)] %>% 
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
        aki_time,
        aki_stage,
        eligibility_time,
        first_bicarbonate_time = bicarbonate_time,
        bicarbonate_type,
        bicarbonate_amount,
        bicarbonate_unit)] %>% 
  .[, rec_96_sodium_bicarb := !is.na(first_bicarbonate_time)]


## Combine all recommendations into one dataset --------------------------------
DT_ssc_encoded <- sepsis_base_cohort_data %>% 
  copy() %>% 
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
                rec_8_lactate_measured_at_suspicion)] %>% 
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
                   rec_12_vaso_before_cvc,
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
                   rec_15_icu_6h)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_16_17[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_16_17_antib_within_1h,
                   rec_16_17_antib_within_3h)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  # merge(.,
  #       rec_17[, .(subject_id,
  #                 hadm_id,
  #                 stay_id,
  #                 rec_17_sepsis_antib_within_1h)] %>% unique(),
  #       by = c("subject_id",
  #              "hadm_id",
  #              "stay_id"),
  #       all.x = TRUE,
  #       all.y = FALSE,
  #       allow.cartesian = FALSE) %>% 
  merge(.,
        rec_43[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_43_crystalloid_first_line)] %>% 
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
  # merge(.,
  #       rec_71[, .(subject_id,
  #                  hadm_id,
  #                  stay_id,
  #                  rec_71_high_tidal_volume,
  #                  rec_71_low_tidal_volume)] %>% 
  #         unique(),
  #       by = c("subject_id",
  #              "hadm_id",
  #              "stay_id"),
  #       all.x = TRUE,
  #       all.y = FALSE,
  #       allow.cartesian = FALSE)
  # merge(.,
  #       rec_72[, .(subject_id,
  #                  hadm_id,
  #                  stay_id,
  #                  rec_72_tidal_volume_6_8,
  #                  rec_72_tower_tidal_volume_4_to_less_6)] %>% 
  #         unique(),
  #       by = c("subject_id",
  #              "hadm_id",
  #              "stay_id"),
  #       all.x = TRUE,
  #       all.y = FALSE,
  #       allow.cartesian = FALSE) %>% 
  # merge(.,
  #       rec_79[, .(subject_id,
  #                  hadm_id,
  #                  stay_id,
  #                  rec_79_iv_corticosteroids)] %>% 
  #         unique(),
  #       by = c("subject_id",
  #              "hadm_id",
  #              "stay_id"),
  #       all.x = TRUE,
  #       all.y = FALSE,
  #       allow.cartesian = FALSE) %>% 
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
                   rec_92_insulin_at_glucose_180)] %>%
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
  copy() %>% 
  .[rec_num %in% c(7,
                   8,
                   10,
                   12,
                   13,
                   15,
                   16,
                   17,
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
  
