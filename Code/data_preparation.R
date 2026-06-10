source(here::here("Code/global.R"))

## 1. SET UP BIG QUERY CONNECTION ----------------------------------------------
project_id <- "master-thesis-492309"

## Define MIMIC-IV dataset names
hosp_db <- "`physionet-data.mimiciv_3_1_hosp`"
icu_db  <- "`physionet-data.mimiciv_3_1_icu`"

## Helper function to execute and download queries
run_query <- function(sql) {
  cat("Executing query...\n")
  bq_table_download(bq_project_query(project_id, sql))
}

## 2. BASE COHORT DEFINITION AND CORE DATA PULLS -------------------------------
cat("--- Section 2: Pulling base sepsis cohort and core data tables ---\n")

## Define the SQL to get the base cohort (adults, from sepsis3 definition)
## The sepsis3 table includes adult patients (age >= 18) in the ICU with sepsis3 criteria == TRUE.
## It provides a standardized definition of sepsis and sepsis onset time.
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
# ssc_mimic_variables <- run_query(sql_base_cohort)
# saveRDS(ssc_mimic_variables, here::here("Data/ssc_mimic_variables.rds"))
ssc_mimic_variables <- readRDS(here::here("Data/ssc_mimic_variables.rds")) %>% 
  setDT()

## Unique set of Stay IDs for adults with sepsis
stay_ids_sql_list <- paste(unique(ssc_mimic_variables$stay_id), collapse = ",") 

## Unique set of Hospital Admission IDs for adults with sepsis
hadm_ids_sql_list <- paste(unique(ssc_mimic_variables$hadm_id), collapse = ",")

## Look-up table with subject ID, hospital admission ID and stay ID
stay_lookup <- ssc_mimic_variables %>%
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
      highnormalvalue
  FROM {icu_db}.d_items")

# d_items <- run_query(sql_d_items)
# saveRDS(d_items, here::here("Data/d_items.rds"))
d_items <- readRDS(here::here("Data/d_items.rds")) %>% 
  setDT()

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
  WHERE linksto = 'chartevents' &
  WHERE stay_id IN ({stay_ids_sql_list})")

# chartevents_itemids <- run_query(sql_chartevents_itemids)
# saveRDS(chartevents_itemids, here::here("Data/chartevents_itemids.rds"))
chartevents_patterns <- c("heart rate",
                          "blood pressure",
                          "respiratory rate",
                          "temperature",
                          "spo2",
                          "gcs",
                          "fio2",
                          "peep",
                          "plateau pressure",
                          "tidal volume",
                          "height",
                          "map",
                          "oxygen device",
                          "weight")

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
  WHERE stay_id IN ({stay_ids_sql_list}) AND itemid IN ({sql_chart_itemids})
")

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
procedureevents_patterns <- c("centr",
                              "line",
                              "cvc")

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