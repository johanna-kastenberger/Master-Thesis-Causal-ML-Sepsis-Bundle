source(here::here("Code/global.R"))
source(here::here("Code/data_preparation.R"))

## Table of SSC recommendations
ssc_recommendations <- readRDS(here::here("Data/ssc_recommendations.rds")) %>% 
  setDT()

## -----------------------
## 1. Define septic shock
## -----------------------
## sepsis + vasopressor administration (vasporessor = 1) + lactate > 2

## Define sepsis onset
sepsis_onset <- ssc_mimic_variables %>% 
  copy() %>% 
  .[, sepsis_onset := suspected_infection_time] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        sepsis_onset)]

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

## 12 hour window around sepsis onset time
lac_window <- lactate_events %>% 
  copy() %>% 
  merge(.,
        sepsis_onset,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = FALSE,
        allow.cartesian = TRUE) %>% 
  .[charttime >= sepsis_onset - hours(6) &
      charttime <= sepsis_onset + hours(6)] %>% 
  .[!is.na(valuenum),
    .SD[which.max(valuenum)],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        max_lactate = valuenum,
        lactate_time = charttime)]

## Vasopressor itemids
## Vasopressors include Norepinephrine, Epinephrine, Dopamine, Angiotensin II,
## Vasopressin, Terlipressin, Selepressin, and Phenylephrine
vasopressors_patterns <- c("Norepinephrine",
                           "Epinephrine",
                           "Dopamine",
                           "Angiotensin II",
                           "Vasopressin",
                           "Terlipressin",
                           "Selepressin",
                           "Phenylephrine")

## Determine Vasopressor itemids
vasopressor_itemids <- d_items %>% 
  copy() %>% 
  .[grepl(paste(vasopressors_patterns,
                collapse = "|"),
        label,
        ignore.case = TRUE),
    itemid]

## Vasopressor administration
vasopressor_events <- all_inputevents %>% 
  copy() %>% 
  .[itemid %in% vasopressor_itemids]

## 12 hour window around sepsis onset
vaso_window <- vasopressor_events %>% 
  copy() %>% 
  merge(.,
        sepsis_onset,
        by = "stay_id",
        all.x = FALSE,
        allow.cartesian = TRUE) %>% 
  .[starttime >= sepsis_onset - hours(6) &
      starttime <= sepsis_onset + hours(6)] %>% 
  .[, .(first_vaso_time = min(starttime, na.rm = TRUE),
       any_vasopressor = TRUE),
   by = .(subject_id,
          hadm_id,
          stay_id)]

## Define septic shock proxy
septic_shock <- sepsis_onset %>% 
  copy() %>% 
  merge(.,
        lac_window,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE) %>% 
  merge(.,
        vaso_window,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
               all.x = TRUE) %>% 
  .[, `:=`(lactate_over2 = !is.na(max_lactate) & max_lactate > 2,
           any_vasopressor = fifelse(is.na(any_vasopressor), 
                                     FALSE, 
                                     any_vasopressor))] %>% 
  .[, septic_shock_proxy := lactate_over2 & any_vasopressor]



## RECOMMENDATION 4 ------------------------------------------------------------
rec4_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 4, rec_label]

## RECOMMENDATION 7 ------------------------------------------------------------
rec7_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 7, rec_label]

## RECOMMENDATION 8 ------------------------------------------------------------
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
rec8_lactate <- ssc_mimic_variables %>%
  copy() %>% 
  .[, .(stay_id, 
        suspected_infection_time)] %>% 
  unique() %>% 
  merge(.,
        lactate_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, time_diff := abs(as.numeric(difftime(charttime,
                                           suspected_infection_time,
                                           units = "hours")))] %>% 
  .[, rec_8_lactate_measured_at_suspicion := any(time_diff <= 6, na.rm = TRUE),
    by = stay_id]
  
## Merge with SSC mimic table  
ssc_mimic_variables <- ssc_mimic_variables %>% 
  copy() %>% 
  merge(.,
        rec8_lactate,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE)

## RECOMMENDATION 10 -----------------------------------------------------------
rec10_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 10, rec_label]

# STATUS: partially_encodable
# GOAL: Calculate fluid volume given in first 3h of septic shock.
# LIMITATION: Missing ED data is a major issue.
# PROXY: Analysis restricted to a sub-cohort who develop septic shock > 6 hours
# after ICU admission to increase confidence that resuscitation is captured.

cat("Processing REC 10: 30ml/kg Crystalloid\n")

## List of crystalloid fluids (isotonic saline and balanced electrolyte solutions)
patterns_crystalloid <- c("nacl 0.9%",
                          "crystalloid",
                          "lactated",
                          "\\blr\\b",
                          "plasma-lyte",
                          "normosol",
                          "normasol",
                          "plasma",
                          "plasmalyte")

# PROXY DEFINITION: Crystalloid itemids. Non-exhaustive.
crystalloid_itemids <- d_items %>%
  copy() %>% 
  .[category %in% c("Fluids/Intake",
                    "Blood Products/Colloids",
                    "Medications")] %>% 
  .[grepl(paste(patterns_crystalloid,
                collapse = "|"),
          label,
          ignore.case = TRUE)]

# PROXY DEFINITION: Use first recorded weight.
weight_itemid <- d_items %>% 
  copy() %>% 
  .[label == "Admission Weight (Kg)", itemid]

first_weight <- all_chartevents %>%
  copy() %>% 
  .[itemid == as.integer(weight_itemid)] %>% 
  .[, weight_kg := first(valuenum),
    by = stay_id]

rec10_cohort <- ssc_mimic_variables %>%
  copy() %>% 
  .[cardiovascular >= 2] %>% 
  .[, .(stay_id,
        suspected_infection_time,
        icu_intime)] %>% 
  merge(.,
        first_weight,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE)

# fluids_3hr <- all_inputevents %>%
#   copy() %>% 
#   merge(.,
#         rec10_cohort,
#         by = "stay_id",
#         all = FALSE) %>% 
#   .[itemid %in% crystalloid_itemids &
#       starttime >= shock_time &
#       starttime <= shock_time + hours(3)] %>% 
#   .[, total_fluid_ml = sum(amount, na.rm = TRUE), by = .(stay_id,
#                                                          weight_kg)] %>% 
#   .[, rec_10_fluid_vol_3h_ml_kg = total_fluid_ml / weight_kg] %>% 
#   .[, .(stay_id,
#         rec_10_fluid_vol_3h_ml_kg)]

# ssc_mimic_variables <- ssc_mimic_variables %>% left_join(fluids_3hr, by = "stay_id")

# --- REC 12: Peripheral Vasopressors ---
# STATUS: partially_encodable
# GOAL: Determine if vasopressors were started before a central line was placed.
# PROXY: `vasopressor_start < cvc_placement_time` indicates peripheral start.

cat("Processing REC 12: Peripheral Vasopressors\n")
# PROXY DEFINITION: Vasopressor itemids
vaso_itemids <- d_items %>%
  copy() %>% 
  .[category == "Vasopressors" | 
      grepl(paste(c("Norepinephrine", 
                   "Epinephrine",
                   "Dopamine",
                   "Vasopressin"),
                   collapse = "|"),
            label,
            ignore.case = TRUE),
    itemid]

first_vaso <- all_inputevents %>%
  copy() %>% 
  .[itemid %in% vaso_itemids & rate > 0] %>% 
  .[, first_vaso_time := min(starttime, 
                            na.rm = TRUE), 
    by = stay_id] %>% 
  unique()

first_cvc <- all_procs %>%
  copy() %>% 
  .[, first_cvc_time := min(starttime, 
                            na.rm = TRUE),
    by = stay_id] %>% 
  unique()

rec12_timing <- merge(
  ssc_mimic_variables[, .(stay_id)],
  first_vaso,
  by = "stay_id",
  all.x = TRUE
)

rec12_timing <- merge(
  rec12_timing,
  first_cvc,
  by = "stay_id",
  all.x = TRUE
)

rec12_timing[
  ,
  rec_12_vaso_start_before_cvc :=
    !is.na(first_vaso_time) &
    (is.na(first_cvc_time) | first_vaso_time < first_cvc_time)
]

rec12_timing <- rec12_timing[
  ,
  .(stay_id, rec_12_vaso_start_before_cvc)
]

rec12_timing <- ssc_mimic_variables %>% 
  select(stay_id) %>%
  left_join(first_vaso, by = "stay_id") %>%
  left_join(first_cvc, by = "stay_id") %>%
  mutate(
    rec_12_vaso_start_before_cvc = if_else(
      !is.na(first_vaso_time) & (is.na(first_cvc_time) | first_vaso_time < first_cvc_time),
      TRUE,
      FALSE,
      missing = FALSE # if no vaso, not applicable
    )
  ) %>%
  select(stay_id, rec_12_vaso_start_before_cvc)

ssc_mimic_variables <- ssc_mimic_variables %>% left_join(rec12_timing, by = "stay_id")

