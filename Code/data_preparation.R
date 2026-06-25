source(here::here("Code/global.R"))

## 1. SET UP BIG QUERY CONNECTION ----------------------------------------------
project_id <- "master-thesis-492309"

## Define MIMIC-IV dataset names
hosp_db <- "`physionet-data.mimiciv_3_1_hosp`"
icu_db  <- "`physionet-data.mimiciv_3_1_icu`"
derived_db <- "`physionet-data.mimiciv_3_1_derived`"

## Helper function to execute and download queries
run_query <- function(sql) {
  cat("Executing query...\n")
  bq_table_download(bq_project_query(project_id, sql))
}

## 2. BASE COHORT DEFINITION AND CORE DATA PULLS -------------------------------
## Define the SQL to get the base cohort (adults, from sepsis3 definition)
## The sepsis3 table includes adult patients (age >= 18) in the ICU with sepsis3 
## criteria == TRUE. It provides a standardized definition of sepsis 
## and sepsis onset time.
sql_base_cohort <- glue::glue(
"SELECT
    s3.subject_id,
    icu.hadm_id,
    s3.stay_id,

    s3.sepsis3,
    s3.antibiotic_time,
    s3.culture_time,
    s3.suspected_infection_time,
    s3.sofa_time,
    s3.sofa_score,

    s3.respiration,
    s3.coagulation,
    s3.liver,
    s3.cardiovascular,
    s3.cns,
    s3.renal,

    icu.intime AS icu_intime,
    icu.outtime AS icu_outtime,
    icu.los AS icu_los,

    adm.admittime,
    adm.dischtime,
    adm.deathtime,

    pat.anchor_age,
    pat.gender,

    CASE
      WHEN pat.dod IS NOT NULL
       AND pat.dod BETWEEN adm.admittime AND adm.dischtime
      THEN 1 ELSE 0
    END AS hospital_mortality,

    CASE
      WHEN pat.dod IS NOT NULL
       AND pat.dod <= adm.admittime + INTERVAL 30 DAY
      THEN 1 ELSE 0
    END AS thirty_day_mortality

  FROM `physionet-data.mimiciv_3_1_derived.sepsis3` AS s3

  LEFT JOIN {icu_db}.icustays AS icu
    ON s3.stay_id = icu.stay_id

  LEFT JOIN {hosp_db}.admissions AS adm
    ON icu.hadm_id = adm.hadm_id

  LEFT JOIN {hosp_db}.patients AS pat
    ON s3.subject_id = pat.subject_id
  
  WHERE s3.sepsis3 IS TRUE")

## Create the main analysis dataframe
## SQL commands only need to run once to create and save RDS file
# base_cohort_variables <- run_query(sql_base_cohort)
# saveRDS(base_cohort_variables, here::here("Data/base_cohort_variables.rds"))
base_cohort_variables <- readRDS(here::here("Data/base_cohort_variables.rds")) %>% 
  setDT()

## Unique set of Subject IDs for adults with sepsis
subject_ids_sql_list <- paste(unique(base_cohort_variables$subject_id), collapse = ",")

## Unique set of Stay IDs for adults with sepsis
stay_ids_sql_list <- paste(unique(base_cohort_variables$stay_id), collapse = ",") 

## Unique set of Hospital Admission IDs for adults with sepsis
hadm_ids_sql_list <- paste(unique(base_cohort_variables$hadm_id), collapse = ",")

## Look-up table with subject ID, hospital admission ID and stay ID
stay_lookup <- base_cohort_variables %>%
  copy() %>% 
  .[, .(subject_id, 
        hadm_id, 
        stay_id, 
        icu_intime, 
        icu_outtime)] %>% 
  unique()

## Variable look up table
sql_d_items <- glue::glue(
  "SELECT
      itemid,
      label,
      abbreviation,
      category,
      unitname,
      param_type,
      lownormalvalue,
      highnormalvalue,
      linksto
  FROM {icu_db}.d_items")

# d_items <- run_query(sql_d_items)
# saveRDS(d_items, here::here("Data/d_items.rds"))
d_items <- readRDS(here::here("Data/d_items.rds")) %>% 
  setDT()

## Pull height events separately
sql_heighevents <- glue::glue(
  "SELECT subject_id, stay_id, charttime, height
  FROM {derived_db}.height
  WHERE subject_id IN ({subject_ids_sql_list})"
)

# all_heightevents <- run_query(sql_heighevents)
# saveRDS(all_heightevents, here::here("Data/all_heightevents.rds"))
all_heightevents <- readRDS(here::here("Data/all_heightevents.rds")) %>% 
  setDT() %>% 
  .[, height_cm := sapply(height, 
                          function(x)
                            x$height)] %>% 
  .[, .(subject_id,
        stay_id,
        charttime,
        height_cm)]

## 3. PULL ALL INPUT EVENTS ----------------------------------------------------
## This contains ICU-administered inputs: fluids, vasopressors, medications, infusion 
## rates, volumes. It is needed for variables like IV fluid administration, vasopressor 
## use, medication dose/rate, etc.
sql_inputevents <- glue::glue(
"SELECT stay_id, starttime, endtime, itemid, amount, amountuom, rate, rateuom
  FROM {icu_db}.inputevents
  WHERE stay_id IN ({stay_ids_sql_list})")

# all_inputevents <- run_query(sql_inputevents)
# saveRDS(all_inputevents, here::here("Data/all_inputevents.rds"))
all_inputevents <- readRDS(here::here("Data/all_inputevents.rds")) %>% 
  setDT()


## 4. PULL ALL MICROBIOLOGY EVENTS ---------------------------------------------
## This contains cultures, organisms, antibiotic sensitivity results, specimen type, 
## and interpretation. It is hospital-level rather than ICU-only, so the code later 
## maps it back to ICU stays using subject_id + hadm_id + charttime.
sql_microbio <- glue::glue(
"SELECT
    subject_id,
    hadm_id,
    charttime,
    spec_type_desc,
    org_name,
    ab_name,
    interpretation
  FROM {hosp_db}.microbiologyevents
  WHERE hadm_id IN ({hadm_ids_sql_list})")

# all_microbioevents <- run_query(sql_microbio)
# saveRDS(all_microbioevents, here::here("Data/all_microbioevents.rds"))
all_microbioevents <- readRDS(here::here("Data/all_microbioevents.rds")) %>% 
  setDT()

all_microbioevents_icu <- all_microbioevents %>% 
  copy() %>% 
  merge(.,
        stay_lookup,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  .[charttime >= icu_intime &
      charttime <= icu_outtime]


## 5. PULL ALL LAB EVENTS ------------------------------------------------------
## Pull lab event look-up table to select relevant item IDs
sql_lab_itemids <- glue::glue(
  "SELECT
      itemid,
      label,
      fluid,
      category
  FROM {hosp_db}.d_labitems"
)

# lab_itemids <- run_query(sql_lab_itemids)
# saveRDS(lab_itemids, here::here("Data/lab_itemids.rds"))
labitem_patterns <- c("lactate",
              "wbc",
              "creatinine",
              "hemoglobin",
              "platelets",
              "inr",
              "traponin",
              "vancomycin",
              "\\bph\\b"
              )

## The code keeps labs such as lactate, WBC, creatinine, hemoglobin, platelets, INR, 
## troponin, vancomycin level, and pH. These are relevant for sepsis severity, monitoring, 
## organ dysfunction, and SSC-related indicators.
lab_itemids <- readRDS(here::here("Data/lab_itemids.rds")) %>%
  setDT() %>%
  .[grepl(
    paste(labitem_patterns, collapse = "|"),
    label,
    ignore.case = TRUE
  )]

## String of target lab item IDs
sql_lab_itemids <- paste(unique(lab_itemids$itemid), collapse = ", ")

## Pull relevant labevents
sql_labevents <- glue::glue(
"SELECT
    subject_id,
    hadm_id,
    charttime,
    itemid,
    valuenum
  FROM {hosp_db}.labevents
  WHERE hadm_id IN ({hadm_ids_sql_list})
    AND itemid IN ({sql_lab_itemids})")

# all_labevents <- run_query(sql_labevents)
# saveRDS(all_labevents, here::here("Data/all_labevents.rds"))
all_labevents <- readRDS(here::here("Data/all_labevents.rds")) %>% 
  setDT()

all_labevents_icu <- all_labevents %>% 
  copy() %>% 
  merge(.,
        stay_lookup,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  .[charttime >= icu_intime &
      charttime <= icu_outtime]


## 6. PULL ALL CHART EVENTS ---------------------------------------------------- 
## Pull chart event look-up table to select relevant item IDs
sql_chartevents_itemids <- glue::glue(
  "SELECT
      itemid,
      label,
      abbreviation,
      category,
      unitname,
      param_type,
      lownormalvalue,
      highnormalvalue
  FROM {icu_db}.d_items
  WHERE linksto = 'chartevents'")

# chartevents_itemids <- run_query(sql_chartevents_itemids)
# saveRDS(chartevents_itemids, here::here("Data/chartevents_itemids.rds"))
chartevents_patterns <- c("heart rate",
                          "blood pressure",
                          "hemoglobin",
                          "glucose",
                          "respiratory rate",
                          "temperature",
                          "spo2",
                          "gcs",
                          "fio2",
                          "Inspired O2 Fraction",
                          "pao2",
                          "o2 saturation",
                          "oxygen saturation",
                          "pao2",
                          "po2",
                          "Arterial O2 pressure",
                          "sao2",
                          "spo2",
                          "peep",
                          "plateau pressure",
                          "tidal volume",
                          "height",
                          "map",
                          "oxygen device",
                          "o2 device",
                          "delivery device",
                          "high flow",
                          "nasal cannula",
                          "face tent",
                          "mask",
                          "weight",
                          "enteral",
                          "nutrition",
                          "enteral nutrition",
                          "arterial",
                          "ph",
                          "creatinine",
                          
                          ## Extracoporeal fluid removal
                          "haemofiltration",
                          "haemodialysis",
                          "ultrafiltration",
                          "fluid removal",
                          "dialysis",
                          ## continuous renal replacement therapy
                          "crrt",
                          ## Continuous Venovenous Hemofiltration
                          "cvvh",
                          ## Continuous Veno-Venous Hemodialysis
                          "cvvhd",
                          
                          ## Diuretics
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

## The code keeps labs such as lactate, WBC, creatinine, hemoglobin, platelets, INR, 
## troponin, vancomycin level, and pH. These are relevant for sepsis severity, monitoring, 
## organ dysfunction, and SSC-related indicators.
chart_itemids <- readRDS(here::here("Data/chartevents_itemids.rds")) %>% 
  setDT() %>% 
  .[grepl(
    paste(chartevents_patterns, collapse = "|"),
    label,
    ignore.case = TRUE
  )]

## String of target lab item IDs
sql_chart_itemids <- paste(unique(chart_itemids$itemid), collapse = ", ")

## Pull relevant chartevents
## This includes ICU bedside measurements: heart rate, blood pressure, respiratory 
## rate, temperature, SpO₂, GCS, FiO₂, PEEP, plateau pressure, tidal volume, height, 
## MAP, and oxygen device. These are needed for hemodynamics, ventilation, oxygenation, 
## and monitoring variables.
sql_chartevents <- glue::glue("
  SELECT stay_id, charttime, itemid, valuenum, value
  FROM {icu_db}.chartevents
  WHERE stay_id IN ({stay_ids_sql_list}) AND itemid IN ({sql_chart_itemids})")

# all_chartevents <- run_query(sql_chartevents)
# saveRDS(all_chartevents, here::here("Data/all_chartevents.rds"))
all_chartevents <- readRDS(here::here("Data/all_chartevents.rds")) %>% 
  setDT()

## 7. PULL ALL PROCEDURE EVENTS ------------------------------------------------
## Pull procedure event event look-up table to select relevant item IDs
sql_procedureevents_itemids <- glue::glue(
  "SELECT
      itemid,
      label,
      abbreviation,
      category,
      unitname,
      param_type,
      lownormalvalue,
      highnormalvalue
  FROM {icu_db}.d_items
  WHERE linksto = 'procedureevents'")

# procedureevents_itemids <- run_query(sql_procedureevents_itemids)
# saveRDS(procedureevents_itemids, here::here("Data/procedureevents_itemids.rds"))
procedureevents_patterns <- c("picc",
                              "hickamn",
                              "tunneled",
                              "trauma line")

## Pull procedureevents for CVC lines
procedure_itemids <- readRDS(here::here("Data/procedureevents_itemids.rds")) %>% 
  setDT() %>% 
  .[grepl(
    paste(procedureevents_patterns, collapse = "|"),
    label,
    ignore.case = TRUE
  )]

## String of target lab item IDs
sql_procedure_itemids <- paste(unique(procedure_itemids$itemid), collapse = ", ")

sql_procs <- glue::glue(
"SELECT 
    proc.stay_id,
    proc.starttime,
    proc.itemid,
    item.label
  FROM {icu_db}.procedureevents AS proc
  LEFT JOIN {icu_db}.d_items AS item 
    ON proc.itemid = item.itemid
  WHERE proc.stay_id IN ({stay_ids_sql_list})
    AND proc.itemid IN ({sql_procedure_itemids})")

# all_procs <- run_query(sql_procs)
# saveRDS(all_procs, here::here("Data/all_procs.rds"))
all_procs <- readRDS(here::here("Data/all_procs.rds")) %>% 
  setDT()

## 8. PULL ALL PRESCRIPTIONS ---------------------------------------------------
# Pull prescriptions
sql_prescriptions <- glue::glue(
"SELECT subject_id, hadm_id, starttime, drug, route, dose_val_rx, dose_unit_rx
  FROM {hosp_db}.prescriptions
  WHERE hadm_id IN ({hadm_ids_sql_list}) AND drug IS NOT NULL")

# all_prescriptions <- run_query(sql_prescriptions)
# saveRDS(all_prescriptions, here::here("Data/all_prescriptions.rds"))
all_prescriptions <- readRDS(here::here("Data/all_prescriptions.rds")) %>% 
  setDT()


## 9. Extract EMAR (Electronic Medication Administration Record) data ----------
## Corticosteroids patterns
corticosteroids_patterns <- paste(c("hydrocortisone",
                              "hydrocostrisone na succ.",
                              "methylprednisolone",
                              "dexamethasone",
                              "prednisolone",
                              "prednisone"),
                              collapse = "|")

sql_emarevents <- glue::glue(
    "SELECT *
    FROM {hosp_db}.emar
    WHERE subject_id IN ({subject_ids_sql_list})")
  
# all_emarevents <- run_query(sql_emarevents)
# saveRDS(all_emarevents, here::here("Data/all_emarevents.rds"))
all_emarevents <- readRDS(here::here("Data/all_emarevents.rds")) %>% 
    setDT() %>% 
    .[grepl(corticosteroids_patterns,
            medication,
            ignore.case = TRUE)]
  
  
## SQL query for emar_details
sql_corticosteroid_emar_details <- glue::glue("
  SELECT
    e.subject_id,
    e.hadm_id,
    e.emar_id,
    e.charttime,
    e.medication,
    e.event_txt,
    d.administration_type,
    d.route,
    d.dose_given,
    d.dose_given_unit
  FROM {hosp_db}.emar AS e
  LEFT JOIN {hosp_db}.emar_detail AS d
    ON e.emar_id = d.emar_id
  WHERE e.subject_id IN ({subject_ids_sql_list})
    AND REGEXP_CONTAINS(
      LOWER(e.medication),
      '{corticosteroids_patterns}')")
  
# corticosteroid_emar_details <- run_query(sql_corticosteroid_emar_details)
# saveRDS(corticosteroid_emar_details, here::here("Data/corticosteroid_emar_details.rds"))
corticosteroid_emar_details <- readRDS(here::here("Data/corticosteroid_emar_details.rds")) %>% 
  setDT()

## 10. PULL ALL INGREDIENTS ----------------------------------------------------
# Pull ingredients
sql_ingredients <- glue::glue(
  "SELECT subject_id, hadm_id, stay_id, starttime, endtime, itemid, amount, rateuom
  FROM {icu_db}.ingredientevents
  WHERE hadm_id IN ({hadm_ids_sql_list})")

# all_ingredients <- run_query(sql_ingredients)
# saveRDS(all_ingredients, here::here("Data/all_ingredients.rds"))
all_ingredients <- readRDS(here::here("Data/all_ingredients.rds")) %>% 
  setDT()

## 11. PULL ALL OUTPUTEVENTS ---------------------------------------------------
# Pull outputevents
sql_outputevents <- glue::glue(
  "SELECT subject_id, hadm_id, stay_id, charttime, itemid, value, valueuom
  FROM {icu_db}.outputevents
  WHERE hadm_id IN ({hadm_ids_sql_list})")

# all_outputevents <- run_query(sql_outputevents)
# saveRDS(all_outputevents, here::here("Data/all_outputevents.rds"))
all_outputevents <- readRDS(here::here("Data/all_outputevents.rds")) %>% 
  setDT()

## 12. DEFINE SEPTIC SHOCK AND SEPSIS-INDUCED HYPOPERFUSION --------------------
## Define septic shock
## sepsis + vasopressor administration (vasporessor = 1) + lactate > 2

## Define sepsis onset
sepsis_onset <- base_cohort_variables %>% 
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
      valuenum)] %>% 
  unique()

## 12 hour window around sepsis onset time
lac_window <- lactate_events %>% 
  copy() %>% 
  merge(.,
        sepsis_onset,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  .[charttime >= sepsis_onset - hours(6) &
      charttime <= sepsis_onset + hours(6) &
      !is.na(valuenum) &
      valuenum > 2] %>% 
  .[order(charttime),
    .SD[1], 
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        lactate_over2 = TRUE,
        first_lactate_over2 = valuenum,
        first_lactate_over2_time = charttime)]

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
          ignore.case = TRUE)]

## Vasopressor lookup table
vasopressor_lookup <- d_items %>% 
  copy() %>% 
  .[grepl(paste(vasopressors_patterns,
                collapse = "|"),
          label,
          ignore.case = TRUE),
    .(itemid, label)] %>% 
  unique()

## Vasopressor administration
vasopressor_events <- all_inputevents %>% 
  copy() %>% 
  .[itemid %in% vasopressor_itemids$itemid &
      !is.na(starttime) &
      rate > 0]

## 12 hour window around sepsis onset
vaso_window <- vasopressor_events %>% 
  copy() %>% 
  merge(.,
        sepsis_onset,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  .[starttime >= sepsis_onset - hours(6) &
      starttime <= sepsis_onset + hours(6)] %>% 
  merge(.,
        vasopressor_lookup,
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  setnames("label",
            "vaso_type") %>% 
  .[order(starttime), 
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_vaso_time = starttime,
        any_vasopressor = TRUE,
        first_vaso_type = vaso_type)]

## Define septic shock proxy
septic_shock <- sepsis_onset %>% 
  copy() %>% 
  merge(.,
        lac_window,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        vaso_window,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, any_vasopressor := fifelse(is.na(any_vasopressor),
                                FALSE,
                                any_vasopressor)] %>% 
  .[, septic_shock_proxy := lactate_over2 & any_vasopressor] %>% 
  ## Sepsis-induced hypoperfusion
  ## Since there is no universally accepted precise clinical definition of sepsis-induced 
  ## hypoperfusion, rationale for recommenation 10 e.g. comes from studies of early 
  ## fluid resuscitation with hypotension and/or elevated lactate
  ## We use serum lactate > 2mmol/L within +/- 6 hours of suspected infection time
  ## as a proxy for sepsis-induced hypoperfusion
  .[, hypoperfusion_proxy := lactate_over2] %>% 
  .[, septic_shock_onset_time := fifelse(
    septic_shock_proxy,
    pmax(first_lactate_over2_time,
         first_vaso_time,
         na.rm = TRUE),
    as.POSIXct(NA)
  )]

## Add septic shock and hypoperfusion variables to ssc variable data
sepsis_base_cohort_data <- base_cohort_variables %>% 
  copy() %>% 
  merge(.,
        septic_shock[, .(subject_id,
                         hadm_id,
                         stay_id,
                         septic_shock_proxy,
                         hypoperfusion_proxy,
                         any_vasopressor,
                         first_vaso_time,
                         first_lactate_over2_time,
                         septic_shock_onset_time)] %>% unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE)

saveRDS(sepsis_base_cohort_data,
        here::here("Data/sepsis_base_cohort_data.rds"))
