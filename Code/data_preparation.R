source(here::here("Code/global.R"))

## 1. SET UP BIG QUERY CONNECTION ----------------------------------------------
project_id <- "master-thesis-492309"

## Define MIMIC-IV dataset names
hosp_db <- "`physionet-data.mimiciv_3_1_hosp`"
icu_db  <- "`physionet-data.mimiciv_3_1_icu`"
derived_db <- "`physionet-data.mimiciv_3_1_derived`"
note_db <- "`physionet-data.mimiciv_note`"

## Helper function to execute and download queries
run_query <- function(sql) {
  cat("Executing query...\n")
  bq_table_download(bq_project_query(project_id, sql))
}

## 2. BASE SEPSIS PATIENT COHORT -----------------------------------------------
## MIMIV-IV Patient table
sql_patient <- glue::glue(
  "SELECT
  *
  FROM {hosp_db}.patients")

# patient_data <- run_query(sql_patient)
# saveRDS(patient_data, here::here("Data/patient_data.rds"))
patient_data <- readRDS(here::here("Data/patient_data.rds")) %>% 
  setDT()

## Check number of subject IDs
patient_data[, uniqueN(subject_id)]

## MIMIC-IV ICU table
sql_icu <- glue::glue(
  "SELECT
  
  subject_id,
  hadm_id,
  stay_id,
  intime as icu_intime,
  outtime as icu_outtime,
  los as icu_los
  FROM {icu_db}.icustays")

# icu_data <- run_query(sql_icu)
# saveRDS(icu_data, here::here("Data/icu_data.rds"))
icu_data <- readRDS(here::here("Data/icu_data.rds")) %>% 
  setDT()

## Check number of subject IDs
icu_data[, uniqueN(subject_id)]

## Check number of hadm IDs
icu_data[, uniqueN(hadm_id)]

## Check number of stay IDs
icu_data[, uniqueN(stay_id)]

## MIMIC-IV hospital admission table
sql_hadm <- glue::glue(
  "SELECT
  *
  FROM {hosp_db}.admissions")

# hadm_data <- run_query(sql_hadm)
# saveRDS(hadm_data, here::here("Data/hadm_data.rds"))
hadm_data <- readRDS(here::here("Data/hadm_data.rds")) %>% 
  setDT() %>% 
  .[order(subject_id,
          hadm_id)]

## Check number of subject IDs
hadm_data[, uniqueN(subject_id)]

## Check number of hadm IDs
hadm_data[, uniqueN(hadm_id)]

## MIMIC-IV Sepsis 3 table
sql_sepsis3 <- glue::glue(
  "SELECT
  s3.subject_id,
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
  s3.renal
    
  FROM {derived_db}.sepsis3 as s3")
  
# sepsis3_data <- run_query(sql_sepsis3)
# saveRDS(sepsis3_data, here::here("Data/sepsis3_data.rds"))
sepsis3_data <- readRDS(here::here("Data/sepsis3_data.rds")) %>% 
  setDT()

## Check number of subject IDs
subjects_sepsis3 <- sepsis3_data[, unique(subject_id)]

## Check number of stay IDs
stays_sepsis3 <- sepsis3_data[, unique(stay_id)]

## Combine into patient base cohort
base_cohort <- merge(
  sepsis3_data,
  patient_data,
  by = c("subject_id"),
  all.x = TRUE,
  all.y = FALSE) %>% 
  ## only patients with sepsis3 == TRUE
  .[sepsis3 == TRUE] %>% 
  merge(.,
        icu_data,
        by = c("subject_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  merge(.,
        hadm_data[, .(subject_id,
                      hadm_id,
                      admittime,
                      dischtime,
                      deathtime,
                      insurance,
                      language,
                      marital_status,
                      race)],
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, hospital_mortality :=
      fifelse(
        !is.na(dod) &
          dod >= admittime &
          dod <= dischtime,
        1,
        0)] %>% 
  .[, thirty_day_mortality := 
      fifelse(
        !is.na(dod) &
          dod >= admittime &
          dod <= admittime + days(30),
        1,
        0)]

stopifnot(base_cohort[, uniqueN(stay_id)] == length(stays_sepsis3))

## Define the SQL to get the base cohort (adults, from sepsis3 definition)
## The sepsis3 table includes adult patients (age >= 18) in the ICU with sepsis3 
## criteria == TRUE. It provides a standardized definition of sepsis 
## and sepsis onset time.
# sql_base_cohort <- glue::glue(
# "SELECT
#     s3.subject_id,
#     icu.hadm_id,
#     s3.stay_id,
# 
#     s3.sepsis3,
#     s3.antibiotic_time,
#     s3.culture_time,
#     s3.suspected_infection_time,
#     s3.sofa_time,
#     s3.sofa_score,
# 
#     s3.respiration,
#     s3.coagulation,
#     s3.liver,
#     s3.cardiovascular,
#     s3.cns,
#     s3.renal,
# 
#     icu.intime AS icu_intime,
#     icu.outtime AS icu_outtime,
#     icu.los AS icu_los,
# 
#     adm.admittime,
#     adm.dischtime,
#     adm.deathtime,
# 
#     pat.anchor_age,
#     pat.gender,
# 
#     CASE
#       WHEN pat.dod IS NOT NULL
#        AND pat.dod BETWEEN adm.admittime AND adm.dischtime
#       THEN 1 ELSE 0
#     END AS hospital_mortality,
# 
#     CASE
#       WHEN pat.dod IS NOT NULL
#        AND pat.dod <= adm.admittime + INTERVAL 30 DAY
#       THEN 1 ELSE 0
#     END AS thirty_day_mortality
# 
#   FROM `physionet-data.mimiciv_3_1_derived.sepsis3` AS s3
# 
#   LEFT JOIN {icu_db}.icustays AS icu
#     ON s3.stay_id = icu.stay_id
# 
#   LEFT JOIN {hosp_db}.admissions AS adm
#     ON icu.hadm_id = adm.hadm_id
# 
#   LEFT JOIN {hosp_db}.patients AS pat
#     ON s3.subject_id = pat.subject_id
#   
#   WHERE s3.sepsis3 IS TRUE")
# 
# ## Create the main analysis dataframe
# ## SQL commands only need to run once to create and save RDS file
# # base_cohort_variables <- run_query(sql_base_cohort)
# # saveRDS(base_cohort_variables, here::here("Data/base_cohort_variables.rds"))
# base_cohort_variables <- readRDS(here::here("Data/base_cohort_variables.rds")) %>% 
#   setDT()

## Unique set of Subject IDs for adults with sepsis
subject_ids_sql_list <- paste(unique(base_cohort$subject_id), collapse = ",")

## Unique set of Stay IDs for adults with sepsis
stay_ids_sql_list <- paste(unique(base_cohort$stay_id), collapse = ",") 

## Unique set of Hospital Admission IDs for adults with sepsis
hadm_ids_sql_list <- paste(unique(base_cohort$hadm_id), collapse = ",")

## Look-up table with subject ID, hospital admission ID and stay ID
stay_lookup <- base_cohort %>%
  copy() %>% 
  .[, .(subject_id, 
        hadm_id, 
        stay_id, 
        icu_intime, 
        icu_outtime)] %>% 
  unique()

stopifnot(nrow(stay_lookup) == length(stays_sepsis3))

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


## Categories of labels in d_items lookup table
d_items_categories <- d_items %>% 
  copy() %>% 
  .[, unique(category)]

## 3. INPUT EVENTS ----------------------------------------------------
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

## Check number of stay IDs
all_inputevents[, uniqueN(stay_id)]


## 4. MICROBIOLOGY EVENTS ---------------------------------------------
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

## Check number of subject IDs
all_microbioevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_microbioevents[, uniqueN(hadm_id)]


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

## Check number of subject IDs
all_microbioevents_icu[, uniqueN(subject_id)]

## Check number of hadm IDs
all_microbioevents_icu[, uniqueN(hadm_id)]

## 5. LAB EVENTS ------------------------------------------------------
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
labitem_patterns <- c(
              "lactate",
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
    valuenum,
    valueuom
  FROM {hosp_db}.labevents
  WHERE hadm_id IN ({hadm_ids_sql_list})
    AND itemid IN ({sql_lab_itemids})")

# all_labevents <- run_query(sql_labevents)
# saveRDS(all_labevents, here::here("Data/all_labevents.rds"))
all_labevents <- readRDS(here::here("Data/all_labevents.rds")) %>% 
  setDT()

## Check number of subject IDs
all_labevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_labevents[, uniqueN(hadm_id)]

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

## Check number of subject IDs
all_labevents_icu[, uniqueN(subject_id)]

## Check number of hadm IDs
all_labevents_icu[, uniqueN(hadm_id)]


## 6. CHART EVENTS ---------------------------------------------------- 
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
                          "thiazide",
                          
                          ## oxygen flow
                          "o2 flow",
                          "oxygen flow",
                          "flow rate",
                          "flow",
                          "l/min",
                          "liter")

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
  SELECT 
  stay_id, 
  charttime, 
  itemid, 
  valuenum, 
  value, 
  valueuom
  FROM {icu_db}.chartevents
  WHERE stay_id IN ({stay_ids_sql_list}) AND itemid IN ({sql_chart_itemids})")

# all_chartevents <- run_query(sql_chartevents)
# saveRDS(all_chartevents, here::here("Data/all_chartevents.rds"))
all_chartevents <- readRDS(here::here("Data/all_chartevents.rds")) %>% 
  setDT()

## Check number of stay IDs
all_chartevents[, uniqueN(stay_id)]
  
## 7. HEIGHT -----------------------------------------------------------
## Pull patient admission height from chart data
height_itemids <- d_items %>% 
  copy() %>% 
  .[grepl("height",
          label,
          ignore.case = TRUE)]

height_events <- all_chartevents %>% 
  copy() %>% 
  .[itemid %in% height_itemids$itemid] %>% 
  merge(.,
        height_itemids[, .(itemid,
                           label,
                           unitname)],
        by = "itemid")

## Make sure there is only one measurement per patient and unit 
stopifnot(nrow(height_events[, .N, by = .(stay_id, itemid)] %>% .[N > 1]) == 0)

patient_height <- height_events %>% 
  copy() %>% 
  .[label == "Height",
    label := "Height (in)"] %>% 
  dcast(
    stay_id + 
      charttime ~ 
      label,
    value.var = "valuenum"
  )

## Check for outliers
summary(patient_height$`Height (cm)`)
summary(patient_height$`Height (in)`)

## Analzye distributions
quantile(patient_height$`Height (cm)`,
         probs = c(0.01,
                   0.1, 
                   0.5,
                   0.75,
                   0.9,
                   0.95,
                   0.99),
         na.rm = TRUE)

quantile(patient_height$`Height (in)`,
         probs = c(0.01,
                   0.1,
                   0.5, 
                   0.75, 
                   0.9, 
                   0.95, 
                   0.99),
         na.rm = TRUE)

## Remove obvious outliers
patient_height <- patient_height %>% 
  copy() %>% 
  .[, height_diff := 
      abs(`Height (in)` * 2.54 - `Height (cm)`)] %>% 
  .[, height_valid := fcase(
    
    !is.na(`Height (cm)`) &
      !is.na(`Height (in)`) &
      ## convert height (inches) to height (cm)
      ## 1 inch = 2.54 cm
      height_diff <= 2,
    "valid: <= 2 cm difference",
    
    !is.na(`Height (cm)`) &
      !is.na(`Height (in)`) &
      height_diff > 2,
    "not valid: >2 cm difference",
    
    is.na(`Height (cm)`) |
      is.na(`Height (in)`),
    "only one height measurement available",
    
    default = NA_character_)] %>% 
  .[`Height (cm)` >= 120 &
      `Height (cm)` <= 230 &
      `Height (in)` >= 47 &
      `Height (in)` <= 91] %>% 
  .[, .(stay_id,
        charttime,
        `Height (cm)`,
        `Height (in)`,
        height_valid)]

## check that all height measurements are valid
stopifnot(nrow(patient_height[height_valid != "valid: <= 2 cm difference"]) == 0)


## 8. PROCEDURE EVENTS ------------------------------------------------
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

## Check number of stay IDs
all_procs[, uniqueN(stay_id)]


## 9. PRESCRIPTIONS ---------------------------------------------------
# Pull prescriptions
sql_prescriptions <- glue::glue(
"SELECT 
subject_id, 
hadm_id, 
starttime, 
drug, 
route, 
dose_val_rx, 
dose_unit_rx
  FROM {hosp_db}.prescriptions
  WHERE hadm_id IN ({hadm_ids_sql_list}) AND drug IS NOT NULL")

# all_prescriptions <- run_query(sql_prescriptions)
# saveRDS(all_prescriptions, here::here("Data/all_prescriptions.rds"))
all_prescriptions <- readRDS(here::here("Data/all_prescriptions.rds")) %>% 
  setDT()

## Check number of subject IDs
all_prescriptions[, uniqueN(subject_id)]

## Check number of hadm IDs
all_prescriptions[, uniqueN(hadm_id)]

all_prescriptions_icu <- all_prescriptions %>% 
  copy() %>% 
  merge(.,
        stay_lookup,
        by = c("subject_id",
               "hadm_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>% 
  .[starttime >= icu_intime &
      starttime <= icu_outtime]

## Check number of subject IDs
all_prescriptions_icu[, uniqueN(subject_id)]

## Check number of hadm IDs
all_prescriptions_icu[, uniqueN(hadm_id)]


## 10. EMAR (Electronic Medication Administration Record) data ----------
## Corticosteroids patterns
corticosteroids_patterns <- paste(c("hydrocortisone",
                              "hydrocostrisone na succ.",
                              "methylprednisolone",
                              "dexamethasone",
                              "prednisolone",
                              "prednisone"),
                              collapse = "|")

# sql_emarevents <- glue::glue(
#     "SELECT *
#     FROM {hosp_db}.emar
#     WHERE subject_id IN ({subject_ids_sql_list})")
#   
# # all_emarevents <- run_query(sql_emarevents)
# # saveRDS(all_emarevents, here::here("Data/all_emarevents.rds"))
# all_emarevents <- readRDS(here::here("Data/all_emarevents.rds")) %>% 
#     setDT() %>% 
#     .[grepl(corticosteroids_patterns,
#             medication,
#             ignore.case = TRUE)]
  
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

## Check number of subject IDs
corticosteroid_emar_details[, uniqueN(subject_id)]

## Check number of hadm IDs
corticosteroid_emar_details[, uniqueN(hadm_id)]

corticosteroid_emar_icu <- corticosteroid_emar_details %>% 
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

## Check number of subject IDs
corticosteroid_emar_icu[, uniqueN(subject_id)]

## Check number of hadm IDs
corticosteroid_emar_icu[, uniqueN(hadm_id)]
  

## 11. INGREDIENTS ----------------------------------------------------
# Pull ingredients
sql_ingredients <- glue::glue(
  "SELECT 
  subject_id, 
  hadm_id, 
  stay_id,
  starttime, 
  endtime, 
  itemid, 
  amount, 
  rateuom
  FROM {icu_db}.ingredientevents
  WHERE hadm_id IN ({hadm_ids_sql_list})")

# all_ingredients <- run_query(sql_ingredients)
# saveRDS(all_ingredients, here::here("Data/all_ingredients.rds"))
all_ingredients <- readRDS(here::here("Data/all_ingredients.rds")) %>% 
  setDT()

## Check number of subject IDs
all_ingredients[, uniqueN(subject_id)]

## Check number of hadm IDs
all_ingredients[, uniqueN(hadm_id)]

## Check number of stay IDs
all_ingredients[, uniqueN(stay_id)]


## 12. OUTPUTEVENTS ---------------------------------------------------
# Pull outputevents
sql_outputevents <- glue::glue(
  "SELECT 
  subject_id,
  hadm_id, 
  stay_id, 
  charttime, 
  itemid, 
  value, 
  valueuom
  FROM {icu_db}.outputevents
  WHERE hadm_id IN ({hadm_ids_sql_list})")

# all_outputevents <- run_query(sql_outputevents)
# saveRDS(all_outputevents, here::here("Data/all_outputevents.rds"))
all_outputevents <- readRDS(here::here("Data/all_outputevents.rds")) %>% 
  setDT()

## Check number of subject IDs
all_outputevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_outputevents[, uniqueN(hadm_id)]

## Check number of stay IDs
all_outputevents[, uniqueN(stay_id)]


## 13. SEPTIC SHOCK AND SEPSIS-INDUCED HYPOPERFUSION --------------------
## Define septic shock
## sepsis + vasopressor administration (vasporessor = 1) + lactate > 2
## Septic shock is a subset of sepsis in which profound circulatory,
## cellular, and metabolic abnormalities are associated with a
## greater risk of mortality

## Early septic shock proxy within 24h of sepsis onset

## Define sepsis onset
sepsis_onset <- base_cohort %>% 
  copy() %>% 
  .[, sepsis_onset := suspected_infection_time] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        sepsis_onset,
        icu_intime,
        icu_outtime)]

## Ensure only one row per stay ID
stopifnot(sepsis_onset[, .N, by = stay_id] %>% .[N > 1, .N] == 0)
## Item IDs corresponding to blood lactate measurement
lactate_itemids <- lab_itemids %>% 
  copy() %>% 
  .[label == "Lactate" &
      grepl("blood",
            fluid,
            ignore.case = TRUE)]

## All Lactate Events
all_lactateevents <- all_labevents %>%
  copy() %>% 
  ## Blood lactate item IDs
  .[itemid %in% lactate_itemids$itemid,
    .(subject_id,
      hadm_id,
      # stay_id,
      itemid,
      charttime,
      valuenum,
      valueuom)] %>% 
  unique() %>% 
  merge(.,
        lactate_itemids[, .(itemid,
                            label,
                            category)],
        by = "itemid")

## Check for outliers
summary(all_lactateevents$valuenum)

## Analzye distribution
quantile(all_lactateevents$valuenum,
         probs = c(0.01,
                   0.1,
                   0.5,
                   0.75,
                   0.9,
                   0.95,
                   0.99),
         na.rm = TRUE)

## Remove obvious outliers
all_lactateevents <- all_lactateevents %>% 
  copy() %>% 
  .[!is.na(charttime) & 
      !is.na(valuenum) &
      valueuom == "mmol/L" &
      valuenum >= 0 &
      valuenum <= 30]

# ## 6 hour window after sepsis onset
# ## Allow 1 hour before sepsis onset to account for
# ## documentation timing (grace period)
# lac_window <- lactate_events %>% 
#   copy() %>% 
#   merge(.,
#         sepsis_onset,
#         by = c("subject_id",
#                "hadm_id",
#                "stay_id"),
#         all.x = TRUE,
#         all.y = FALSE,
#         allow.cartesian = TRUE) %>% 
#   .[
#     # charttime >= sepsis_onset - hours(1) &
#     #   charttime <= sepsis_onset + hours(24) &
#       !is.na(valuenum) &
#       valuenum > 2 &
#       valueuom == "mmol/L"] %>% 
#   ## Select first vasopressor administration
#   # .[order(charttime),
#   #   .SD[1], 
#   #   by = .(subject_id,
#   #          hadm_id,
#   #          stay_id)] %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         lactate_over2 = TRUE,
#         # first_lactate_over2 = valuenum,
#         lactate_value = valuenum,
#         # first_lactate_over2_time = charttime
#         lactate_time = charttime)]

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

## Determine Vasopressor item IDs
vasopressor_itemids <- d_items %>% 
  copy() %>% 
  .[grepl(paste(vasopressors_patterns,
                collapse = "|"),
          label,
          ignore.case = TRUE) &
      !grepl("intubation",
             label,
             ignore.case = TRUE)]

## Vasopressor therapy was identified from the MIMIC-IV inputevents table using
## continuous administration records for norepinephrine, epinephrine,
## dopamine, phenylephrine, vasopressin, and angiotensin II. The chartevents item
## "Phenylephrine (Intubation)" was excluded because it represents
## phenylephrine administered during endotracheal intubation rather than
## therapeutic vasopressor support for septic shock. Including this item
## could incorrectly classify patients who received a brief peri-intubation 
## vasopressor bolus without requiring ongoing treatment for circulatory failure.

## Vasopressor administration
all_vasopressorevents <- all_inputevents %>% 
  copy() %>% 
  ## Vasopressor item IDs
  .[itemid %in% vasopressor_itemids$itemid] %>% 
  ## add vasopressor labels
  merge(.,
        vasopressor_itemids[, .(itemid,
                                label)],
        by = "itemid") %>% 
  .[, .(stay_id,
        vaso_start = starttime,
        vaso_end = endtime,
        vaso_label = label,
        vaso_amount = amount,
        vaso_unit = amountuom,
        vaso_rate = rate,
        vaso_rate_unit = rateuom)]


## Lactate measurements associated with septic shock criteria
lactate_events_shock <- all_lactateevents %>% 
  copy() %>% 
  .[valuenum > 2] %>% 
  merge(.,
        sepsis_onset,
        by = c("subject_id",
               "hadm_id"),
        all = FALSE,
        allow.cartesian = TRUE) %>% 
  ## Broad episode restriction before pairing with vasopressors
  .[charttime >= sepsis_onset - hours(6) &
      charttime <= sepsis_onset + hours(24)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        lactate_time = charttime,
        lactate_value = valuenum,
        lactate_unit = valueuom,
        lactate_label = label,
        sepsis_onset)]

## Qualifying vasopressor administration for the septic shock proxy
vasopressor_events_shock <- all_vasopressorevents %>% 
  copy() %>% 
  .[vaso_label %in% c("Norepinephrine",
                 "Epinephrine",
                 "Dopamine",
                 "Angiotensin II",
                 "Vasopressin",
                 "Phenylephrine")] %>% 
  .[!is.na(vaso_start) &
      vaso_rate > 0] %>% 
  .[, .(stay_id,
        vaso_label,
        vaso_start,
        vaso_end,
        vaso_rate,
        vaso_rate_unit,
        vaso_amount,
        vaso_unit)] %>% 
  merge(.,
        sepsis_onset,
        by = "stay_id",
        all = FALSE) %>% 
  ## restrict vasopressors to after sepsis onset (account for delay)
  ## the 6h allowance before derived sepsis onset acknowledges that the 
  ## antibiotic/culture/SOFA-based onset is not an excact biological
  ## onset. This is an operational choice and should be reported as such.
  .[vaso_start >= sepsis_onset - hours(6) &
      ## restrict to period assessed in reference study
      ## Later vasoporessor episode may be due to surgery,
      ## hemorrhage, cardiogenic shock, sedation or intubation,
      ## a second infection or another acute deterioration
      vaso_start <= sepsis_onset + hours(24)]

## Check for missing or invalid vasopressor end times
stopifnot(vasopressor_events_shock[is.na(vaso_end) | vaso_end <= vaso_start, .N] == 0)

## Candidates for septic shock
septic_shock_candidates <- vasopressor_events_shock %>% 
  copy() %>% 
  merge(.,
        lactate_events_shock,
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "sepsis_onset"),
        all = FALSE,
        allow.cartesian = TRUE)


## Define septic shock proxy
first_septic_shock <- septic_shock_candidates %>% 
  copy() %>% 
  ## Lactate measured up to 6 h before vasopressor initiation
  ## or during the qualifying vasopressor episode,
  ## with both criteria completed within 24 h of sepsis onset
  .[lactate_time >= vaso_start - hours(6) &
      lactate_time <= pmin(vaso_end,
                           vaso_start + hours(24),
                           sepsis_onset + hours(24))] %>% 
  .[, time_diff := 
      abs(lubridate::time_length(vaso_start -
                             lactate_time,
                             "minutes"))] %>% 
  .[, septic_shock_onset_time := pmax(
    vaso_start,
    lactate_time)] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          septic_shock_onset_time,
          time_diff)] %>% 
    .[, .SD[1],
      by = .(subject_id,
             hadm_id,
             stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_onset_time,
        shock_lactate_time = lactate_time,
        shock_lactate_value = lactate_value,
        shock_vaso = vaso_label,
        shock_vaso_start = vaso_start,
        shock_vaso_end = vaso_end)]

## Septic shock was operationalized among patients meeting the derived Sepsis-3 
## criteria as temporally concurrent vasopressor administration and serum lactate
## greater than 2 mmol/L. A lactate measurement was considered concurrent if it 
## occurred within six hours before vasopressor initiation or during the vasopressor 
## infusion. Septic shock onset was defined as the later of vasopressor initiation 
## and the qualifying lactate measurement. Because adequacy of volume resuscitation 
## could not be established directly and was not operationally described in the 
## reference study, preceding crystalloid administration was examined in a secondary 
## fluid-supported definition. Receipt of 30 mL/kg crystalloid was not required 
## in the primary definition to avoid circularity when assessing compliance with 
## the fluid-resuscitation recommendation.
  
# ## Define septic shock proxy
# septic_shock <- sepsis_onset %>% 
#   copy() %>% 
#   merge(.,
#         lac_window,
#         by = c("subject_id",
#                "hadm_id",
#                "stay_id"),
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   merge(.,
#         vaso_window,
#         by = c("subject_id",
#                "hadm_id",
#                "stay_id"),
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[, any_vasopressor := fifelse(is.na(any_vasopressor),
#                                 FALSE,
#                                 any_vasopressor)] %>% 
#   .[, septic_shock_proxy := lactate_over2 & any_vasopressor] %>% 
#   ## Sepsis-induced hypoperfusion
#   ## Since there is no universally accepted precise clinical definition of sepsis-induced 
#   ## hypoperfusion, rationale for recommenation 10 e.g. comes from studies of early 
#   ## fluid resuscitation with hypotension and/or elevated lactate
#   ## We use serum lactate > 2mmol/L within +6/-1  hours of suspected infection time
#   ## as a proxy for sepsis-induced hypoperfusion
#   .[, hypoperfusion_proxy := lactate_over2] %>% 
#   .[, septic_shock_onset_time := fifelse(
#     septic_shock_proxy,
#     pmax(first_lactate_over2_time,
#          first_vaso_time,
#          na.rm = TRUE),
#     as.POSIXct(NA)
#   )]

## Add septic shock and hypoperfusion variables to ssc variable data

sepsis_base_cohort_data <- base_cohort %>% 
  copy() %>% 
  merge(.,
        first_septic_shock,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, septic_shock_proxy := fcase(
    
    sepsis3 == TRUE &
      !is.na(septic_shock_onset_time),
    TRUE,
    
    sepsis3 == TRUE,
    FALSE,
    
    default = NA)]

saveRDS(sepsis_base_cohort_data,
        here::here("Data/sepsis_base_cohort_data.rds"))

## 14. RADIOLOGY EVENTS FROM MIMICIV-NOTE -----------------------------
sql_radiology <- glue::glue(
  "SELECT
    *
  FROM {note_db}.radiology
  WHERE hadm_id IN ({hadm_ids_sql_list})")

# all_radiologyevents <- run_query(sql_radiology)
# saveRDS(all_radiology_events, here::here("Data/all_radiologyevents.rds"))
all_radiologyevents <- readRDS(here::here("Data/all_radiologyevents.rds")) %>% 
  setDT()

## Check number of subject IDs
all_radiologyevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_radiologyevents[, uniqueN(hadm_id)]


## 15. DISCHARGE NOTES FROM MIMICIV-NOTE ------------------------------
sql_discharge <- glue::glue(
  "SELECT
  *
  FROM {note_db}.discharge
  WHERE hadm_id IN ({hadm_ids_sql_list})")

# all_dischargeevents <- run_query(sql_discharge)
# saveRDS(all_dischargeevents, here::here("Data/all_dischargeevents.rds"))
all_dischargeevents <- readRDS(here::here("Data/all_dischargeevents.rds")) %>% 
  setDT()

## Check number of subject IDs
all_dischargeevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_dischargeevents[, uniqueN(hadm_id)]
