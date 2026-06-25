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

## xx 4 RECOMMENDATION ------------------------------------------------------------
rec4_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 4, rec_label]

## 7 RECOMMENDATION ------------------------------------------------------------
rec7_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 7, rec_label]

## recommendation 7 was operationalized using the MIMIC-IV Sepsis-3 variables culture_time 
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
  .[, `:=` (blood_culture_collected = !is.na(culture_time),
            
            blood_culture_before_antibiotics = !is.na(culture_time) &
              !is.na(antibiotic_time) &
              culture_time <= antibiotic_time,
            
            blood_culture_after_antibiotics = !is.na(culture_time) &
              !is.na(antibiotic_time) &
              culture_time > antibiotic_time,
            
            blood_culture_within_3h = !is.na(culture_time) &
              !is.na(suspected_infection_time) &
              culture_time >= suspected_infection_time &
              culture_time <= suspected_infection_time + hours(3))] %>% 
  .[, rec_7_culture_before_antimicrobial := blood_culture_within_3h &
      blood_culture_before_antibiotics]

## 8 RECOMMENDATION ------------------------------------------------------------
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
# sepsis_base_cohort_data <- sepsis_base_cohort_data %>% 
#   copy() %>% 
#   merge(.,
#         rec8_lactate,
#         by = "stay_id",
#         all.x = TRUE,
#         all.y = FALSE)

## 10 RECOMMENDATION -----------------------------------------------------------
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
          ignore.case = TRUE)]

# PROXY DEFINITION: Use first recorded weight.
weight_itemid <- d_items %>% 
  copy() %>% 
  .[label == "Admission Weight (Kg)"]

first_weight <- all_chartevents %>%
  copy() %>% 
  .[itemid == as.integer(weight_itemid$itemid) &
      !is.na(valuenum)] %>% 
  .[, weight_kg := first(valuenum),
    by = stay_id] %>% 
  .[, .(stay_id,
        weight_kg)] %>% 
  unique()

## Determine units for crystalloid administration rate
crystalloid_units <- all_inputevents[itemid %in% crystalloid_itemids$itemid, unique(rateuom)]

## Crystalloid fluids within 3h
rec_10 <- all_inputevents %>%
  copy() %>%
  .[itemid %in% crystalloid_itemids$itemid] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time,
                                    septic_shock_proxy,
                                    hypoperfusion_proxy)] %>% unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[septic_shock_proxy == TRUE |
      hypoperfusion_proxy == TRUE] %>% 
  .[starttime < suspected_infection_time + hours(3) &
      endtime > suspected_infection_time] %>%
  .[, `:=` (overlap_start = pmax(starttime,
                                 suspected_infection_time),
            overlap_end = pmin(endtime,
                              suspected_infection_time + hours(3)))] %>% 
  .[, overlap_time := ifelse(rateuom == "mL/hour",
                              as.numeric(time_length(overlap_end -
                                                  overlap_start,
                                                  unit = "hours")),
                              ifelse(rateuom == "mL/min",
                                     as.numeric(time_length(overlap_end - 
                                                         overlap_start,
                                                         unit = "minutes")),
                                     NA_real_))] %>% 
  .[, fluid_ml_3h := overlap_time * rate] %>% 
  .[, crystalloid_ml_3h := sum(fluid_ml_3h, 
                               na.rm = TRUE),
    by = stay_id] %>% 
  merge(.,
        first_weight,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, crystalloid_target := 30 * weight_kg] %>% 
  .[, rec_10 := crystalloid_ml_3h >= crystalloid_target]

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
  .[septic_shock_proxy == TRUE] %>% 
  # merge(.,
  #       first_vaso,
  #       by = "stay_id",
  #       all.x = TRUE,
  #       all.y = FALSE) %>% 
  merge(.,
        first_cvc,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, rec_12_vaso_before_cvc := !is.na(first_vaso_time) &
      (is.na(first_cvc_time) |
      first_vaso_time < first_cvc_time)]


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
  .[itemid %in% map_itemids$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  ## remove obvious outliers / data entry errors
  .[valuenum >= 20 &
      valuenum <= 200] %>% 
  .[, map_source := fifelse(
    itemid == map_itemids[label == "Arterial Blood Pressure mean", itemid],
    "Arterial",
    fifelse(itemid == map_itemids[label == "Non Invasive Blood Pressure mean", itemid],
            "Non-invasive",
            NA_character_)
  )] %>% 
  .[, map_source := factor(map_source,
                           levels = c("Arterial",
                                      "Non-invasive"))] %>% 
  .[, chart_hour := lubridate::floor_date(charttime,
                                          "hour")] %>% 
  .[, .(map = mean(valuenum,
                   na.rm = TRUE)),
    by = .(stay_id,
           chart_hour,
           map_source)] %>% 
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
        first_vaso_time,
        septic_shock_proxy,
        septic_shock_onset_time)] %>% 
  unique() %>% 
  .[septic_shock_proxy == TRUE &
      !is.na(first_vaso_time)] %>% 
  merge(.,
        map_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[chart_hour >= lubridate::floor_date(first_vaso_time,
                                        "hour") &
      chart_hour <= lubridate::floor_date(first_vaso_time + hours(24),
                                          "hour")] %>% 
  .[, .(median_map_24h = median(map, na.rm = TRUE),
        mean_map_24h = mean(map, na.rm = TRUE),
        min_map_24h = min(map, na.rm = TRUE),
        max_map_24h = max(map, na.rm = TRUE)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, rec_13_map_target_65 := !is.na(median_map_24h) &
      median_map_24h >= 60 &
      median_map_24h <= 70]

## 15 RECOMMENDATION -----------------------------------------------------------
rec15_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 15, rec_label]

## Time between sepsis onset and ICU admission <= 6 hours
rec_15 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, hours_to_icu :=
      as.numeric(time_length(icu_intime - suspected_infection_time,
                             unit = "hours"))] %>% 
  .[, rec_icu_6h := hours_to_icu <= 6]


## 16 RECOMMENDATION -----------------------------------------------------------
rec16_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 16, rec_label]

## Antibiotic therapy within 1 hour of septic shock onset
rec_16 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[septic_shock_proxy == TRUE] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_onset_time,
        antibiotic_time)] %>% 
  unique() %>% 
  .[, time_to_antib := time_length(antibiotic_time - septic_shock_onset_time,
                                   unit = "hours")] %>% 
  .[, rec_shock_antib_within_1h := !is.na(time_to_antib) &
      time_to_antib >= 0 &
      time_to_antib <= 1]
  

## 17 RECOMMENDATION -----------------------------------------------------------
rec17_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 17, rec_label]

rec_17 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        antibiotic_time)] %>% 
  unique() %>% 
  .[, time_to_antib := time_length(antibiotic_time - suspected_infection_time,
                                   unit = "hours")] %>% 
  .[, rec_17_sepsis_antib_within_1h := !is.na(time_to_antib) &
      time_to_antib >= 0 &
      time_to_antib <= 1]

## xx 33 RECOMMENDATION -----------------------------------------------------------
rec33_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 33, rec_label]

## This recommendation was not encoded because prolonged versus bolus beta-lactam
## infusion required administration-level infusion duration or rate information,
## which could not be reliably determined from the structured medication data available

## xx 42 RECOMMENDATION -----------------------------------------------------------
rec42_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 42, rec_label]

## Not separately encoded because the recommendation recommends either 
## invasive or non-invasive monitoring, both of which are routinely used and 
## recorded in ICU care. The recommendation therefore lacks sufficient 
## discriminatory power for adherence assessment in MIMIC-IV.

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
  .[itemid %in% resuscitation_fluid_itemids$itemid] %>% 
  .[!is.na(amount) & amount >= 250] %>% 
  merge(.,
        resuscitation_fluid_itemids,
        by = "itemid",
        all.x = TRUE) %>% 
  .[, .(stay_id,
       fluid_time = starttime,
       fluid_amount = amount,
       fluid_unit = amountuom,
       fluid_label,
       fluid_type)]

## First line resuscitation fluid
rec_43 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  merge(.,
        resuscitation_fluid_events,
        by = "stay_id",
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  .[fluid_time >= suspected_infection_time &
      fluid_time <= suspected_infection_time + hours(6)] %>% 
  .[order(fluid_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, rec_43_crystalloid_first_line := fluid_type == "Crystalloid"]

## 44 RECCOMENDATION -----------------------------------------------------------
rec44_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 44, rec_label]

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


## Crystalloid Intake within first 3h
crystalloid_initial_3h <- all_inputevents %>% 
  copy() %>% 
  .[itemid %in% crystalloid_itemids$itemid] %>% 
  .[,
    crystalloid_type := ifelse(itemid %in% balanced_crystalloid_itemids$itemid,
                               "Balanced",
                               ifelse(
                                 itemid %in% saline_crystalloid_itemids$itemid,
                                 "0.9% Saline",
                                 "Other/Unknown Crystalloid"
                               ))] %>%
  merge(.,
        sepsis_base_cohort_data,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
.[starttime < suspected_infection_time + hours(3) &
    endtime > suspected_infection_time] %>%
  .[, `:=` (overlap_start = pmax(starttime,
                                 suspected_infection_time),
            overlap_end = pmin(endtime,
                               suspected_infection_time + hours(3)))] %>% 
  .[, overlap_time := ifelse(rateuom == "mL/hour",
                             as.numeric(time_length(overlap_end -
                                                      overlap_start,
                                                    unit = "hours")),
                             ifelse(rateuom == "mL/min",
                                    as.numeric(time_length(overlap_end - 
                                                             overlap_start,
                                                           unit = "minutes")),
                                    NA_real_))] %>% 
  .[, fluid_ml_3h := overlap_time * rate] %>% 
  .[, crystalloid_ml_3h := sum(fluid_ml_3h, 
                               na.rm = TRUE),
    by = .(stay_id,
           crystalloid_type)] %>% 
  .[, .(subject_id,
        hadm_id, 
        stay_id,
        crystalloid_type,
        crystalloid_ml_3h)] %>% 
  unique() %>% 
  .[crystalloid_type %in% c("0.9% Saline",
                            "Balanced")]
  
## Define Rec 44 as more Balanced crystalloid than 0.9% Saline within first 3h
rec_44 <- crystalloid_initial_3h %>% 
  copy() %>% 
dcast(subject_id + 
          hadm_id +
          stay_id ~ crystalloid_type,
        value.var = "crystalloid_ml_3h",
      fill = 0) %>% 
  .[, rec_44_balanced_over_saline := Balanced > 0 &
      Balanced > `0.9% Saline`]
 
## 45 RECOMMENDATION -----------------------------------------------------------
rec45_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 45, rec_label]

## Albumin administration item IDs
albumin_itemids <- d_items %>% 
  copy() %>% 
  .[category %in% c("Blood Products/Colloids",
                    "Fluids - Other (Not In Use)")] %>% 
  .[grepl("albumin",
          label,
          ignore.case = TRUE)]

## Albumin and Crystalloid administration
albumin_crystalloid_events <- all_inputevents %>% 
  copy() %>%
  .[itemid %in% crystalloid_itemids$itemid |
      itemid %in% albumin_itemids$itemid] %>% 
  .[, iv_resuscitation := fifelse(
    itemid %in% crystalloid_itemids$itemid,
    "Crystalloid",
    fifelse(
      itemid %in% albumin_itemids$itemid,
      "Albumin",
      NA_character_
    )
  )]

## Crystallid administration within 24 hours without any albumin administration
rec_45 <- albumin_crystalloid_events %>% 
  copy() %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                     hadm_id,
                                     stay_id,
                                     suspected_infection_time)] %>% unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[starttime >= suspected_infection_time &
      starttime <= suspected_infection_time + hours(24)] %>% 
  .[, .(any_crystalloid = any(iv_resuscitation == "Crystalloid"),
        any_albumin = any(iv_resuscitation == "Albumin"),
        first_fluid_time = min(starttime,
                               na.rm = TRUE)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  ## adherance can only be determined if crystalloids were administered
  .[any_crystalloid == TRUE] %>% 
  .[, rec_45_crystalloid_without_albumin := any_crystalloid == TRUE &
      any_albumin == FALSE]
  

## 53 RECOMMENDATION -----------------------------------------------------------
rec53_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 53, rec_label]

## First line vasopressor Norepinephrine over Dopamine or Epinephrine
## If first line vasopressor not Norepinephrine, Dopamine, or Epinephrine,
## NA instead of non-adherance 
rec_53 <- vaso_window %>% 
  copy() %>% 
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
  )]


## 55 RECOMMENDATION -----------------------------------------------------------
rec55_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 55, rec_label]

## First line vasopressor Norepinephrine over Vasopressin or Angiotensin II
## If first line vasopressor not Norepinephrine, Vasopressin, or Angiotensin II,
## NA instead of non-adherance 
rec_55 <- vaso_window %>% 
  copy() %>% 
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
  )] 

## 56 RECOMMENDATION -----------------------------------------------------------
rec56_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 56, rec_label]

## Item ID for daily weight measurements
daily_weight_itemid <- chart_itemids %>% 
  copy() %>% 
  .[grepl("Daily Weight",
          label,
          ignore.case = TRUE) &
      !grepl("lbs",
             label,
             ignore.case = TRUE)]

## Median daily weight per stay ID
daily_weight <- all_chartevents %>% 
  copy() %>% 
  .[itemid %in% daily_weight_itemid$itemid] %>% 
  setorder(stay_id,
           charttime) %>% 
  .[, chart_day := lubridate::floor_date(charttime,
                                         "days")] %>% 
  .[, daily_weight := median(valuenum,
                             na.rm = TRUE),
    by = .(stay_id,
           chart_day)] %>% 
  .[, .(stay_id,
        chart_day,
        daily_weight)] %>% 
  unique() %>% 
  ## remove obvious outliers
  .[daily_weight >= 40 &
      daily_weight <= 200]

## Vasopressin item ID
vasopressin_itemid <- d_items %>% 
  copy() %>% 
  .[grepl("Vasopressin",
          label,
          ignore.case = TRUE)]

## Vasopressin administration events
vasopressin_events <- vasopressor_events %>% 
  copy() %>% 
  .[itemid == vasopressin_itemid$itemid] %>% 
  .[!is.na(amount) & amount > 0 &
      !is.na(rate) & rate > 0] %>% 
  .[, .(stay_id,
        starttime)] %>% 
  unique() %>% 
  .[, received_vasopressin := TRUE] %>% 
  setorder(stay_id,
           starttime)

## Item ID for Norepinephrine
norepi_itemid <- d_items %>% 
  copy() %>% 
  .[grepl("Norepinephrine",
          label,
          ignore.case = TRUE)]

## Define patients with septic shock on escalating doses of Norepinephrine 
## Escalating does refers to rate >= 5mcg/min (as per VASST trial)
## Calculated by multiplying rate mcg/kg/min by patient's body weight (kg)
norepi_escalating_doses <- all_inputevents %>% 
  copy() %>% 
  .[itemid == norepi_itemid$itemid] %>% 
  .[, norepi_dose_mcg_kg_min := fcase(
    rateuom == "mcg/kg/min", rate,
    rateuom == "mg/kg/min", rate * 1000,
    default = NA_real_
  )] %>% 
  ## Norepinephrine doses administered for at least 1 hour
  .[as.numeric(difftime(endtime,
                        starttime,
                        units = "hours")) >= 1] %>% 
  .[, start_day := lubridate::floor_date(starttime,
                                         "days")] %>% 
  .[, .(stay_id,
        start_day,
        starttime,
        endtime,
        norepi_dose_mcg_kg_min)] %>% 
  unique() %>% 
  merge(.,
        daily_weight,
        by.x = c("stay_id",
                 "start_day"),
        by.y = c("stay_id",
                 "chart_day"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, any_weight := fifelse(
    any(!is.na(daily_weight)),
        TRUE,
        FALSE)] %>% 
  .[any_weight == TRUE] %>% 
  ## NA-fill daily weight entries
  .[, daily_weight := nafill(daily_weight,
                             type = "locf"),
    by = stay_id] %>% 
  .[, daily_weight := nafill(daily_weight,
                             type = "nocb"),
    by = stay_id] %>% 
  .[, norepi_dose_mcg_min := norepi_dose_mcg_kg_min * daily_weight] %>% 
  .[, norepi_escalating_dose := norepi_dose_mcg_min >= 5]


## Define recommendation as vasopressor administration after first 
## escalating dose of Norepinephrine
rec_56 <- norepi_escalating_doses %>%
  copy() %>% 
  merge(.,
          sepsis_base_cohort_data[, .(subject_id,
                                      hadm_id,
                                      stay_id,
                                      septic_shock_proxy,
                                      septic_shock_onset_time)] %>% unique(),
          by = "stay_id",
          all.x = TRUE,
          all.y = FALSE) %>% 
  .[septic_shock_proxy == TRUE &
      starttime >= septic_shock_onset_time &
      norepi_escalating_dose == TRUE] %>% 
  ## select first escalated dose after septic shock onset
  setorder(stay_id,
           starttime) %>% 
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        ne_escalation_time = starttime,
        norepi_dose_mcg_min,
        norepi_escalating_dose)] %>% 
  unique() %>% 
  merge(.,
        vasopressin_events[, .(stay_id,
                               vasopressin_time = starttime,
                               received_vasopressin)],
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  .[is.na(vasopressin_time) | vasopressin_time >= ne_escalation_time] %>% 
  setorder(stay_id,
           vasopressin_time) %>% 
## select first vasopressin administration after norepinephrine escalating dose
  .[, .SD[1],
     by = .(subject_id,
            hadm_id,
            stay_id)] %>% 
  .[, rec_56_vasopressin_after_norepi := !is.na(vasopressin_time) &
      vasopressin_time >= ne_escalation_time]
  

## 57 RECOMMENDATION -----------------------------------------------------------
rec57_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 57, rec_label]

## First Norepinephrine administration after septic shock onset
first_norepi <- vasopressor_events %>% 
  copy() %>% 
  merge(.,
        vasopressor_lookup,
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[label == "Norepinephrine"] %>% 
  merge(sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    septic_shock_proxy,
                                    septic_shock_onset_time)] %>% unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[starttime >= septic_shock_onset_time &
      amount > 0 &
      rate > 0] %>% 
  .[, first_norepi := min(starttime,
                        na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)]

## First Vasopressin administration after septic shock onset
first_vasopressin <- vasopressor_events %>% 
  copy() %>% 
  merge(.,
        vasopressor_lookup,
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[label == "Vasopressin"] %>% 
  merge(sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    septic_shock_proxy,
                                    septic_shock_onset_time)] %>% unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[starttime >= septic_shock_onset_time &
      amount > 0 &
      rate > 0] %>% 
  .[, first_vasopressin := min(starttime,
                          na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)]

## Administration of Norepinephrine & Vasopressin after septic shock onset
norepi_and_vasopressin <- 
merge(first_norepi %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_norepi)] %>% 
  unique(),
first_vasopressin %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_vasopressin)] %>% 
  unique(),
by = c("subject_id",
       "hadm_id",
       "stay_id"),
all = FALSE) %>% 
  .[, norepi_and_vasopressin_time := max(first_norepi,
                                       first_vasopressin,
                                       na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        norepi_and_vasopressin_time)] %>% 
  unique()

## Persistent hypotension (MAP < 65) after Norepinephrine & Vasopressor administration
persistent_hypotension <- norepi_and_vasopressin %>% 
  copy() %>% 
  merge(.,
        map_events,
        by = "stay_id",
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  .[chart_hour >= lubridate::floor_date(norepi_and_vasopressin_time,
                                        "hour") &
      chart_hour  <= lubridate::floor_date(norepi_and_vasopressin_time + hours(6),
                                           "hour")] %>% 
  .[, `:=` (persistent_hypotension = any(map < 65,
                                     na.rm = TRUE),
        median_map_6h = median(map, 
                               na.rm = TRUE),
        min_map_6h = min(map,
                         na.rm = TRUE)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        persistent_hypotension,
        median_map_6h,
        min_map_6h,
        norepi_and_vasopressin_time)] %>% 
  unique()

## Epinephrine administration
first_epi_after_shock <- vasopressor_events %>% 
  copy() %>% 
  merge(vasopressor_lookup,
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[label == "Epinephrine"] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    septic_shock_proxy,
                                    septic_shock_onset_time)] %>% unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[septic_shock_proxy == TRUE &
      starttime >= septic_shock_onset_time &
      amount > 0 &
      rate > 0] %>% 
  .[, .(first_epinephrine = min(starttime, 
                                na.rm = TRUE)),
        by = .(subject_id,
               hadm_id,
               stay_id)]

## Define recommendation as persistent hypotension (MAP < 65) within 6 hours
## after both Norepinephrine & Vasopressin administration after septic shock onset
rec_57 <- persistent_hypotension %>% 
  copy() %>% 
  .[persistent_hypotension == TRUE] %>% 
  merge(.,
        first_epi_after_shock,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, rec_57_epinephrine_added := !is.na(first_epinephrine) &
      first_epinephrine >= norepi_and_vasopressin_time]


## 67 RECOMMENDATION -----------------------------------------------------------
rec67_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 67, rec_label]
## REMARK: This recommendation pertains to patients 
## with a PaO2:FiO2 ratio < 200 or SpO2:FiO2 <235

## Oxygen therapy patterns
oxygen_therapy_patterns <- c("oxygen device",
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
oxygen_events_itemids <- chart_itemids %>% 
  copy() %>% 
  .[grepl(paste0(oxygen_therapy_patterns,
                 collapse = "|"),
          label,
          ignore.case = TRUE)]

## Unique reference list of oxygen therapies
oxygen_therapy_reference <- all_chartevents %>% 
  copy() %>% 
  .[itemid %in% oxygen_events_itemids$itemid] %>% 
  .[grepl(paste0(oxygen_therapy_patterns,
                 collapse = "|"),
          value,
          ignore.case = TRUE), unique(value)]

## Oxygen delivery modes with unclear equivalence to HFNC or conventional 
## oxygen therapy (e.g., Vapomist) were excluded from classification.
## NIV stands for nasal intermittent cannula or 
## nasal intermittent continuous-flow oxygen
oxygen_therapy <- all_chartevents %>% 
  copy() %>% 
  .[itemid %in% oxygen_events_itemids$itemid] %>% 
  .[, oxygen_type := fifelse(
    grepl("High flow nasal cannula",
          value,
          ignore.case = TRUE),
    "HFNC",
    fifelse(
      grepl(paste0(c("Face tent",
                   "High flow neb",
                   "Venti mask",
                   "Non-rebreather",
                   "Medium conc mask",
                   "Nasal cannula",
                   "Oxymizer"),
                   collapse = "|"),
            value,
            ignore.case = TRUE),
      "Conventional",
      fifelse(
        grepl(paste0(c("CPAP mask",
                     "Bipap mask"),
                     collapse = "|"),
              value,
              ignore.case = TRUE),
        "NIV",
        "Other/Excluded"
      )
    )
  )]

## Create proxy for acute hypoxemic respiratory failure (AHRF)
## PROXY: AHRF = PO2 (Arterial) < 60 mmHG or SaO2 < 88%
## Item IDs for variables pertaining to AHRF definition
ahrf_itemids <- chart_itemids %>% 
  copy() %>% 
  .[grepl("po2|spo2|sao2",
          abbreviation,
          ignore.case = TRUE) &
      !grepl("venous",
             label,
             ignore.case = TRUE)] %>% 
  .[category %in% c("Labs",
                    "Respiratory")]

## Item ID for PO2 (Arterial)
pao2_itemid <- ahrf_itemids[abbreviation == "PO2 (Arterial)", itemid]

## Item IDs for SpO2 and SaO2
sao2_spo2_itemids <- ahrf_itemids[abbreviation %in% c("SpO2", "SaO2"), itemid]

## Define AHRF
## Use variable "SaO2 < 90% > 2 min" as additional proxy,
## in case PO2 (Arterial), SpO2, and SaO2 are missing
ahrf_events <- all_chartevents %>% 
  copy() %>% 
  .[itemid %in% ahrf_itemids$itemid] %>% 
  .[, ahrf_event := 
      (itemid == pao2_itemid &
         !is.na(valuenum) &
         valuenum < 60) |
      (itemid %in% sao2_spo2_itemids &
         !is.na(valuenum) &
         valuenum < 88)]

## Define AHRF around sepsis onset
rec_67 <- ahrf_events %>% 
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
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  ## 12 hour window around sepsis onset
  .[charttime >= suspected_infection_time - hours(6) &
      charttime <= suspected_infection_time + hours(6) &
      ahrf_event == TRUE] %>% 
  .[,
    .(ahrf_time = min(charttime)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  merge(.,
        oxygen_therapy[oxygen_type %in% c("HFNC",
                                          "Conventional"),
                       .(stay_id,
                         oxygen_time = charttime,
                         oxygen_device = value,
                         oxygen_type)] %>% unique(),
        by = "stay_id",
        all = FALSE) %>% 
  .[oxygen_time >= ahrf_time &
      ## first oxygen therapy within 6 hours of AHRF onset
      oxygen_time <= ahrf_time + hours(6)] %>% 
  .[order(oxygen_time), 
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_oxygen_time = oxygen_time,
        first_oxygen_device = oxygen_device,
        first_oxygen_type = oxygen_type)] %>% 
  .[, rec_67_first_oxygen_hfnc := first_oxygen_type == "HFNC"]

## 68 RECOMMENDATION -----------------------------------------------------------
rec68_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 68, rec_label]

## For adults with sepsis and AHRF, HFNC as initial therapy over NIPPV/NIV
rec_68 <- ahrf_events %>% 
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
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  ## AHRF around sepsis onset
  .[charttime >= suspected_infection_time - hours(6) &
      charttime <= suspected_infection_time + hours(6) &
      ahrf_event == TRUE] %>% 
  .[,
    .(ahrf_time = min(charttime)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  ## compare initial HFNC vs. NIV/NIPPV
  merge(.,
        oxygen_therapy[oxygen_type %in% c("HFNC",
                                          "NIV"),
                       .(stay_id,
                         oxygen_time = charttime,
                         oxygen_device = value,
                         oxygen_type)] %>% unique(),
        by = "stay_id",
        all = FALSE) %>% 
  ## inital respiratory support within 6 hours after AHRF onset
  .[oxygen_time >= ahrf_time &
      ## first oxygen therapy within 6 hours of AHRF onset
      oxygen_time <= ahrf_time + hours(6)] %>% 
  ## first qualifying therapy
  .[order(oxygen_time), 
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_oxygen_time = oxygen_time,
        first_oxygen_device = oxygen_device,
        first_oxygen_type = oxygen_type)] %>% 
  ## encode recommendation 68
  .[, rec_68_first_oxygen_hfnc_over_niv := first_oxygen_type == "HFNC"]

## 69 RECOMMENDATION -----------------------------------------------------------
rec69_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 69, rec_label]

## Define HFNC/NIV respiratory support events
hfnc_niv_events <- oxygen_therapy %>% 
  copy() %>% 
  .[oxygen_type %in% c("HFNC",
                       "NIV"),
    .(stay_id,
           oxygen_time = charttime,
           oxygen_device = value,
           oxygen_type)] %>% 
      unique()

rec_69 <- ahrf_events %>% 
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
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  ## AHRF around sepsis onset
  .[charttime >= suspected_infection_time - hours(6) &
      charttime <= suspected_infection_time + hours(6) &
      ahrf_event == TRUE] %>% 
  .[, .(ahrf_time = min(charttime)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  ## Respiratory support after AHRF onset
  merge(.,
        hfnc_niv_events,
        by = "stay_id",
        all = FALSE) %>% 
  .[oxygen_time >= ahrf_time &
      oxygen_time <= ahrf_time + hours(6)] %>% 
  ## Identify first therapy and whether both HFNC and NIV ocurred
  .[order(oxygen_time),
    .(first_oxygen_time = oxygen_time[1],
      first_oxygen_device = oxygen_device[1],
      first_oxygen_type = oxygen_type[1],
      used_hfnc = any(oxygen_type == "HFNC"),
      used_niv = any(oxygen_type == "NIV"),
      n_switches = sum(oxygen_type[-1] != oxygen_type[-.N])),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  ## Alternating HFNC/NIV proxy
  ## both HFNC and NIV used within the early AHRF window, at least one switch
  .[, hfnc_niv_alternating := used_hfnc == TRUE &
      used_niv == TRUE &
      n_switches >= 1] %>% 
  .[, rec_69_hfnc_over_hfnc_niv_alternating := 
      first_oxygen_type == "HFNC" &
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

## Item IDs for ARDS definition oxgenation events
ards_oxygenation_itemids <- chart_itemids %>% 
  copy() %>% 
  .[grepl("PO2|FiO2|SpO2",
        abbreviation,
        ignore.case = TRUE) &
      !grepl("venous",
             label,
             ignore.case = TRUE)] %>% 
  .[category %in% c("Labs",
                    "Respiratory")]

## Oxgenation events for ARDS definition
ards_oxygenation_events <- all_chartevents %>% 
  copy() %>% 
  .[itemid %in% ards_oxygenation_itemids$itemid] %>% 
  merge(.,
        ards_oxygenation_itemids[, .(itemid, 
                                    abbreviation)],
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, abbreviation := fifelse(
    abbreviation == "PO2 (Arterial)",
    "PaO2",
    abbreviation
  )] %>% 
  .[!is.na(valuenum)] %>% 
  ## Remove obvious outliers
  .[(abbreviation == "PaO2" & valuenum > 20 & valuenum <= 600) |
      (abbreviation == "SpO2" & valuenum >= 50 & valuenum <= 100) |
      (abbreviation == "FiO2" & valuenum >= 21 & valuenum <= 100)] %>% 
  ## FiO2 needs to be on 0-1 scale for ratio calculation
  .[, valuenum_clean := fifelse(
    abbreviation == "FiO2",
    valuenum / 100,
    valuenum
  )] %>% 
.[, chart_hour := lubridate::floor_date(charttime, "hour")] %>% 
  .[, .(value = mean(valuenum_clean, na.rm = TRUE)),
    by = .(stay_id,
           chart_hour,
           abbreviation)] %>% 
  dcast(stay_id +
        chart_hour ~ abbreviation,
        value.var = "value") %>% 
  .[, pf_ratio := PaO2 / FiO2] %>% 
  .[, sf_ratio := SpO2 / FiO2] %>% 
  .[!is.na(pf_ratio) |
      !is.na(sf_ratio)] %>% 
  .[, oxygenation_failure := 
      (!is.na(pf_ratio) & pf_ratio <= 300) |
      (!is.na(sf_ratio) & !is.na(SpO2) & SpO2 <= 97 & sf_ratio <= 315)]

## ARDS proxy definition
ards_proxy <- ards_oxygenation_events %>% 
  copy() %>% 
  .[oxygenation_failure == TRUE] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% unique(),
        by = "stay_id",
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  .[chart_hour >= lubridate::floor_date(suspected_infection_time,
                                        "hour") &
      chart_hour <= lubridate::floor_date(suspected_infection_time + days(7),
                                          "hour")] %>% 
  merge(.,
        oxygen_therapy_hfnc_niv[, .(stay_id,
                                    support_hour = chart_hour,
                                    resp_support_type)] %>% unique(),
        by = "stay_id",
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  .[abs(as.numeric(difftime(support_hour,
                            chart_hour,
                            units = "hours"))) <= 6] %>% 
  .[order(chart_hour),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        chart_hour,
        PaO2,
        SpO2,
        FiO2,
        pf_ratio,
        sf_ratio,
        resp_support_type,
        ards_proxy = TRUE)]

## Define recommendation 71
## Restrict to invasive ventilation because tidal volume strategy
## only applies to ventilated patients
rec_71 <- ards_proxy %>% 
  copy() %>% 
  .[resp_support_type == "Invasive ventilation"] %>% 
  .[, ards_time := chart_hour] %>% 
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
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  .[tv_hour >= ards_time &
      tv_hour <= ards_time + hours(24)] %>% 
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
corticosteroid_events <- corticosteroid_emar_details %>% 
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
  .[septic_shock_proxy == TRUE] %>% 
  .[, shock_time := septic_shock_onset_time] %>% 
  merge(.,
        corticosteroid_events,
        by = c("subject_id",
               "hadm_id"),
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  ## corticosteroid after septic shock onset
  .[is.na(steroid_time) |
      (steroid_time >= shock_time &
         steroid_time <= shock_time + hours(24))] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          steroid_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id,
           shock_time,
           first_corticosteroid_time = steroid_time,
           first_corticosteroid_type = corticosteroid_type)] %>% 
  .[, rec_79_iv_corticosteroids := 
      !is.na(first_corticosteroid_time)]


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
        all = FALSE) %>% 
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
  .[, rec_89_active_fluid_removal := !is.na(first_fluid_removal_time)]


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
rec_90 <- pre_transfusion_hb %>% 
  copy() %>% 
  .[, transfusion_strategy := fcase(
    hemoglobin < 7,
    "Restrictive",
    hemoglobin >= 9,
    "Liberal",
    hemoglobin >= 7 & hemoglobin < 9,
    "Intermediate",
    default = NA_character_
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
  ## use first transfusion after sepsis onset
  .[transfusion_time >= suspected_infection_time] %>% 
  .[order(transfusion_time),
    .SD[1],
    by = stay_id] %>% 
  .[, rec_90_restrictive_transfusion := hemoglobin < 7]


## 91 RECOMMENDATION -----------------------------------------------------------
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
  .[itemid == enteral_nutrition_itemid$itemid] %>% 
  ## clinically meaninful feed intitiation
  .[amount >= 50] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        enteral_time = starttime,
        enteral_amount = amount)]

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
        all.x = FALSE,
        all.y = TRUE) %>% 
  .[enteral_time >= suspected_infection_time] %>% 
  .[, .(first_enteral_time = min(enteral_time,
                                 na.rm = TRUE)),
    by = .(subject_id,
           hadm_id,
           stay_id,
           suspected_infection_time)] %>% 
  .[, time_to_enteral_hours := as.numeric(difftime(first_enteral_time,
                                                suspected_infection_time,
                                                units = "hours"))] %>% 
  .[, rec_91_early_enteral_nutrition := time_to_enteral_hours <= 72]

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
  .[itemid %in% glucose_itemids$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  .[valuenum >= 20 & valuenum <= 1000] %>% 
  .[, .(stay_id,
        glucose_time = charttime,
        glucose_mg_dl = valuenum)]

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
  .[insulin_time >= suspected_infection_time] %>% 
  .[order(insulin_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)]

## Last glucose within 6h before first insulin
rec_92 <- first_insulin %>% 
  copy() %>% 
  merge(.,
        glucose_events,
        by = "stay_id",
        allow.cartesian = TRUE) %>% 
  .[glucose_time <= insulin_time &
      glucose_time >= insulin_time - hours(6)] %>% 
  .[order(stay_id,
          insulin_time,
          -glucose_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, rec_92_insulin_at_glucose_180 := glucose_mg_dl >= 180]

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
  .[itemid == ph_itemids$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  ## remove obvious outliers / data entry errors
  .[valuenum >= 6.5 & valuenum <= 8.0] %>% 
  .[, severe_acidemia := valuenum <= 7.2] %>% 
  .[, .(stay_id,
        acidemia_time = charttime,
        ph = valuenum,
        severe_acidemia)] %>% 
  unique()

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
  .[itemid == serum_creatinine_itemid$itemid] %>% 
  .[!is.na(valuenum)] %>% 
  ## upper bound for serum creatinine
  .[valuenum > 0 & valuenum <= 20] %>% 
  .[, .(stay_id,
        creatinine_time = charttime,
        creatinine = valuenum)] %>% 
  merge(.,
        sepsis_base_cohort_data,
        by = "stay_id",
        all = FALSE)

## Create input dataset for detect_aki function
aki_dt <- creatinine_events %>% 
   copy() %>% 
  .[septic_shock_proxy == TRUE] %>% 
  merge(.,
        severe_acidemia_events[, .(stay_id,
                                   severe_acidemia,
                                   acidemia_time)] %>% 
          unique(),
        by = "stay_id",
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  .[severe_acidemia == TRUE & acidemia_time >= septic_shock_onset_time] %>% 
  .[, .(stay_id,
        creatinine_time,
        creatinine)] %>% 
  unique() %>% 
    setnames(c("stay_id",
               "creatinine_time",
               "creatinine"),
             c("ID",
               "COLLECTED_DT",
               "RESULT")) %>% 
    .[, BIRTH_DT := NA_Date_]


## Determine AKI events
aki_creatinine_events <- detect_aki(aki_dt) %>% 
  setDT()

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
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>% 
  .[itemid %in% urine_output_itemids$itemid] %>% 
  # .[!is.na(value) & value >= 0] %>% 
  .[, chart_hour := lubridate::floor_date(charttime,
                                          "hour")] %>% 
  .[, .(urine_output_ml = sum(value,
                              na.rm = TRUE)),
    by = .(subject_id,
           hadm_id,
           stay_id,
           chart_hour)] %>% 
  .[, chart_day := lubridate::floor_date(chart_hour,
                                         "days")] %>% 
  merge(.,
        daily_weight,
        by = c("stay_id",
               "chart_day"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, any_weight := any(!is.na(daily_weight)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[any_weight == TRUE] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          chart_hour)] %>% 
  .[, daily_weight := nafill(daily_weight,
                             type = "locf"),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, daily_weight := nafill(daily_weight,
                             type = "nocb"),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, urine_ml_kg_h := urine_output_ml / daily_weight] %>% 
  .[, oliguric_0.5 := urine_ml_kg_h < 0.5] %>% 
  .[, oliguric_0.3 := urine_ml_kg_h < 0.3] %>% 
  .[, anuric := urine_ml_kg_h == 0] %>% 
  .[, run_id_0.5 := rleid(oliguric_0.5),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[oliguric_0.5 == TRUE,
    `:=` (duration_0.5_h = .N,
      start_time = min(chart_hour),
      end_time = max(chart_hour)),
      by = .(stay_id,
             run_id_0.5)] %>% 
    .[, run_id_0.3 := rleid(oliguric_0.3),
      by = .(subject_id,
             hadm_id,
             stay_id)] %>% 
    .[oliguric_0.3 == TRUE,
      `:=` (duration_0.3_h = .N,
            start_time = min(chart_hour),
            end_time = max(chart_hour)),
      by = .(stay_id,
             run_id_0.3)] %>% 
    .[, run_id_anuric := rleid(anuric),
      by = .(subject_id,
             hadm_id,
             stay_id)] %>% 
    .[anuric == TRUE,
      `:=` (duration_anuric_h = .N,
            start_time = min(chart_hour),
            end_time = max(chart_hour)),
      by = .(stay_id,
             run_id_anuric)] %>% 
  .[, aki_oliguria := fcase(
    duration_0.5_h >= 6 & duration_0.5_h < 12,
    "stage 1",
    
    duration_0.5_h >= 12,
    "stage 2",
    
    duration_0.3_h >= 24 |
      duration_anuric_h >= 12,
    "stage 3"
  )]

## Acute kidney injury is defined according to KDIGO criteria.
## Patients with KDIGO stage 2 or stage 3 AKI were considered to meet 
## the severe AKI criterion specified in SSC Recommendation 96.

## Severe AKI from creatinine
aki_2_3_creatinine <- aki_creatinine_events %>% 
  copy() %>% 
  .[AKI_STAGE %in% c(2, 
                     3)] %>% 
  .[, .(stay_id = ID,
        aki_time = COLLECTED_DT,
        aki_stage = AKI_STAGE,
        aki_source = "Creatinine")]

## Severe AKI from urine output
aki_2_3_urine <- urine_output_hourly %>% 
  copy() %>% 
  .[, aki_stage_urine := fcase(
    duration_0.3_h >= 24 |
      duration_anuric_h >= 12,
    3,
    
    duration_0.5_h >= 12,
    2,
    
    duration_0.5_h >= 6,
    1,
    
    default = NA_real_)] %>% 
  .[aki_stage_urine %in% c(2,
                           3)] %>% 
  .[, .(stay_id,
        aki_time = chart_hour,
        aki_stage = aki_stage_urine,
        aki_source = "Urine output")] %>% 
  unique()

## Combine creatinine and urine output AKI events
aki_2_3_events <- rbindlist(
  list(
    aki_2_3_creatinine,
    aki_2_3_urine
  ),
  fill = TRUE
) %>% 
  unique()

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

## Define recommendation 96
rec_96 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[septic_shock_proxy == TRUE] %>% 
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
                   rec_10)] %>% 
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
                   rec_12_vaso_before_cvc)] %>% 
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
                   rec_13_map_target_65)] %>% 
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
                   stay_id)] %>% 
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
                  rec_17_sepsis_antib_within_1h)] %>% unique(),
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
                   stay_id)] %>% 
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
                   rec_56_vasopressin_after_norepi)] %>% 
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
                   rec_57_epinephrine_added)] %>% 
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
  
