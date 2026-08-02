source(here::here("Code/global.R"))

## 1. SET UP BIG QUERY CONNECTION ----------------------------------------------
project_id <- "master-thesis-492309"

## Define MIMIC-IV dataset names
hosp_db <- "`physionet-data.mimiciv_3_1_hosp`"
icu_db  <- "`physionet-data.mimiciv_3_1_icu`"
derived_db <- "`physionet-data.mimiciv_3_1_derived`"
note_db <- "`physionet-data.mimiciv_note`"
ed_db <- "`physionet-data.mimiciv_ed`"

## Helper function to execute and download queries
run_query <- function(sql) {
  cat("Executing query...\n")
  bq_table_download(bq_project_query(project_id, sql))
}

## 2. BASE SEPSIS PATIENT COHORT -----------------------------------------------
## MIMIV-IV Patient table
if (!file.exists(here::here("Data/patient_data.rds"))) {
sql_patient <- glue::glue(
  "SELECT
  *
  FROM {hosp_db}.patients")

patient_data <- run_query(sql_patient)
saveRDS(patient_data, here::here("Data/patient_data.rds"))
}

patient_data <- readRDS(here::here("Data/patient_data.rds")) %>% 
  setDT()

## Check number of subject IDs
patient_data[, uniqueN(subject_id)]

## MIMIC-IV ICU table
if (!file.exists(here::here("Data/icu_data.rds"))) {
sql_icu <- glue::glue(
  "SELECT
  
  subject_id,
  hadm_id,
  stay_id,
  intime as icu_intime,
  outtime as icu_outtime,
  los as icu_los
  FROM {icu_db}.icustays")

icu_data <- run_query(sql_icu)
saveRDS(icu_data, here::here("Data/icu_data.rds"))
}

icu_data <- readRDS(here::here("Data/icu_data.rds")) %>% 
  setDT()

## Check number of subject IDs
icu_data[, uniqueN(subject_id)]

## Check number of hadm IDs
icu_data[, uniqueN(hadm_id)]

## Check number of stay IDs
icu_data[, uniqueN(stay_id)]

## MIMIC-IV hospital admission table
if (!file.exists(here::here("Data/hadm_data.rds"))) {
sql_hadm <- glue::glue(
  "SELECT
  *
  FROM {hosp_db}.admissions")

hadm_data <- run_query(sql_hadm)
saveRDS(hadm_data, here::here("Data/hadm_data.rds"))
}

hadm_data <- readRDS(here::here("Data/hadm_data.rds")) %>% 
  setDT() %>% 
  .[order(subject_id,
          hadm_id)]

## Check number of subject IDs
hadm_data[, uniqueN(subject_id)]

## Check number of hadm IDs
hadm_data[, uniqueN(hadm_id)]

## MIMIC-IV Sepsis 3 table
if (!file.exists(here::here("Data/sepsis3_data.rds"))) {
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
  
sepsis3_data <- run_query(sql_sepsis3)
saveRDS(sepsis3_data, here::here("Data/sepsis3_data.rds"))
}

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
if (!file.exists(here::here("Data/d_items.rds"))) {
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

d_items <- run_query(sql_d_items)
saveRDS(d_items, here::here("Data/d_items.rds"))
}

d_items <- readRDS(here::here("Data/d_items.rds")) %>% 
  setDT()


## Categories of labels in d_items lookup table
d_items_categories <- d_items %>% 
  copy() %>% 
  .[, unique(category)]

## 3. HOSPITAL-WIDE DATA -------------------------------------------------------

### MICROBIOLOGY EVENTS --------------------------------------------------------
## This contains cultures, organisms, antibiotic sensitivity results, specimen
## type, and interpretation. It is hospital-level rather than ICU-only.

## The query filters on subject_id rather than hadm_id. Filtering on
## `hadm_id IN (...)` silently drops every specimen with a missing hadm_id,
## because NULL never satisfies an IN predicate in BigQuery. In MIMIC-IV this
## affects emergency department specimens, collected before the hospital
## admission record is created. Diagnostics showed 95% of such blood cultures
## were drawn in the ~4h BEFORE admittime, whereas captured cultures had a median
## of +14.6h after admittime -- i.e. the initial diagnostic draw was
## systematically excluded while later repeat cultures were retained. Restricting
## to hadm_id understated adherence to SSC Recommendation 7 (blood cultures
## before antimicrobials) from 54.5% to 23.3%. The hadm_id is repaired below via
## an encounter-level time window.

if (!file.exists(here::here("Data/all_microbioevents.rds"))) {
  
  sql_microbio <- glue::glue(
    "SELECT
    subject_id,
    hadm_id,
    micro_specimen_id,
    charttime,
    spec_type_desc,
    org_name,
    ab_name,
    interpretation
  FROM {hosp_db}.microbiologyevents
  WHERE subject_id IN ({subject_ids_sql_list})")
  
  all_microbioevents_raw <- run_query(sql_microbio)
  saveRDS(all_microbioevents_raw, here::here("Data/all_microbioevents_raw.rds"))
  all_microbioevents_raw <- readRDS(here::here("Data/all_microbioevents_raw.rds")) %>% 
    setDT()
  
  ## Encounter-level linkage window: a specimen belongs to a hospitalisation if it
  ## was collected from 24 hours before admission (emergency
  ## department workup) through to discharge. The same window is reused to repair
  ## missing hadm_id in the labevents section below.
  admission_window_microbio <- base_cohort %>%
    copy() %>%
    .[, .(subject_id,
          hadm_id,
          window_start = admittime - lubridate::hours(24),
          window_end   = dischtime)] %>%
    unique()
  
  ## Specimens that already carry a cohort hadm_id keep it unchanged.
  microbio_already_linked <- all_microbioevents_raw %>% 
    copy() %>% 
    .[!is.na(hadm_id)] %>% 
    .[hadm_id %in% base_cohort$hadm_id]
  
  ## Specimens with a missing hadm_id are matched to an admission by time.
  microbio_repaired <- all_microbioevents_raw %>% 
    copy() %>% 
    .[is.na(hadm_id)] %>% 
    .[, hadm_id := NULL] %>% 
    merge(.,
          admission_window_microbio,
          by = "subject_id",
          all.x = FALSE,
          all.y = FALSE,
          allow.cartesian = TRUE) %>% 
    .[charttime >= window_start &
        charttime <= window_end] %>% 
    .[, `:=` (window_start = NULL,
              window_end   = NULL)]
  
  ## Check that each specimen is matched to exactly one hospital admission
  stopifnot(microbio_repaired[, .(n_hadm = uniqueN(hadm_id)), by = micro_specimen_id] %>% 
              .[n_hadm > 1, .N] == 0)
  
  ## Final table -- same columns and meaning as before the fix.
  all_microbioevents <- rbind(microbio_already_linked,
                              microbio_repaired,
                              use.names = TRUE,
                              fill = FALSE)
  
  saveRDS(all_microbioevents, here::here("Data/all_microbioevents.rds"))
  
  ## Check that there are no remaining unmatched hospital admission IDs
  stopifnot(all_microbioevents[, sum(is.na(hadm_id))] == 0)
  
  ## Check that only hadm_ids from the base cohort are included
  stopifnot(all_microbioevents[, all(hadm_id %in% base_cohort$hadm_id)])
  
  ## Check that rows have only been added, not removed by the repair process
  stopifnot(nrow(all_microbioevents) >= nrow(microbio_already_linked))
  
  ## Check number of subject IDs
  all_microbioevents[, uniqueN(subject_id)]
  
  ## Check number of hadm IDs
  all_microbioevents[, uniqueN(hadm_id)]
  
}

# saveRDS(all_microbioevents, here::here("Data/all_microbioevents_repaired.rds"))
all_microbioevents <- readRDS(here::here("Data/all_microbioevents.rds")) %>% 
  setDT()


### LAB EVENTS -----------------------------------------------------------------
## Pull lab event look-up table to select relevant item IDs

if (!file.exists(here::here("Data/lab_itemids.rds"))) {
  
  sql_lab_itemids <- glue::glue(
    "SELECT
      itemid,
      label,
      fluid,
      category
  FROM {hosp_db}.d_labitems"
  )
  
  lab_itemids <- run_query(sql_lab_itemids)
  saveRDS(lab_itemids, here::here("Data/lab_itemids.rds"))
  
}

labitem_patterns <- c(
  "lactate",
  "wbc",
  "creatinine",
  "hemoglobin",
  "platelets",
  "inr",
  "troponin",
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


if (!file.exists(here::here("Data/all_labevents.rds"))) {
  
  sql_labevents <- glue::glue(
    "SELECT
    labevent_id,
    subject_id,
    hadm_id,
    charttime,
    itemid,
    valuenum,
    valueuom
  FROM {hosp_db}.labevents
  WHERE subject_id IN ({subject_ids_sql_list})
    AND itemid IN ({sql_lab_itemids})")
  
  all_labevents_raw <- run_query(sql_labevents)
  saveRDS(all_labevents_raw, here::here("Data/all_labevents_raw.rds"))
  all_labevents_raw <- readRDS(here::here("Data/all_labevents_raw.rds")) %>% 
    setDT()
  
  admission_window_lab <- base_cohort %>%
    copy() %>%
    .[, .(subject_id,
          hadm_id,
          window_start = admittime - lubridate::hours(24),
          window_end   = dischtime)] %>%
    unique()
  
  ## Lab rows that already carry a cohort hadm_id keep it unchanged.
  labevents_already_linked <- all_labevents_raw %>% 
    copy() %>% 
    .[!is.na(hadm_id)] %>% 
    .[hadm_id %in% base_cohort$hadm_id]
  
  ## Rows with a missing hadm_id are matched to an admission by time.
  labevents_repaired <- all_labevents_raw %>%
    copy() %>%
    .[is.na(hadm_id)] %>%
    .[, hadm_id := NULL] %>%
    merge(.,
          admission_window_lab,
          by = "subject_id",
          all.x = FALSE,
          all.y = FALSE,
          allow.cartesian = TRUE) %>%
    .[charttime >= window_start &
        charttime <= window_end] %>%
    ## resolve rows matching more than one admission window (readmission within
    ## 24h): prefer the admission the draw falls INSIDE, else the admission it
    ## most immediately precedes.
    .[, admittime := window_start + lubridate::hours(24)] %>%
    .[, is_within_admission := charttime >= admittime] %>%
    .[, gap_to_admit := lubridate::time_length(admittime - charttime, "hours")] %>%
    .[order(labevent_id, -is_within_admission, gap_to_admit)] %>%
    .[, .SD[1], by = labevent_id] %>%
    .[, `:=` (window_start = NULL,
              window_end   = NULL,
              admittime    = NULL,
              is_within_admission = NULL,
              gap_to_admit = NULL)]
  
  ## Check that each lab result maps to exactly one hospitalisation
  stopifnot(
    labevents_repaired[, .(n_hadm = uniqueN(hadm_id)),
                       by = labevent_id][n_hadm > 1, .N] == 0)
  
  ## Combine results
  all_labevents <- rbind(labevents_already_linked,
                         labevents_repaired,
                         use.names = TRUE,
                         fill = FALSE)
  
  saveRDS(all_labevents, here::here("Data/all_labevents.rds"))
  
}

all_labevents <- readRDS(here::here("Data/all_labevents.rds")) %>% setDT()

## Check number of subject IDs
all_labevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_labevents[, uniqueN(hadm_id)]

## Restrict to ICU window
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



### PRESCRIPTIONS --------------------------------------------------------------
# Pull prescriptions
if (!file.exists(here::here("Data/all_prescriptions.rds"))) {
  sql_prescriptions <- glue::glue(
    "SELECT 
subject_id, 
hadm_id, 
starttime, 
stoptime,
drug_type,
drug, 
formulary_drug_cd,
route, 
dose_val_rx, 
dose_unit_rx
  FROM {hosp_db}.prescriptions
  WHERE hadm_id IN ({hadm_ids_sql_list}) AND drug IS NOT NULL")
  
  all_prescriptions <- run_query(sql_prescriptions)
  saveRDS(all_prescriptions, here::here("Data/all_prescriptions.rds"))
}

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


#### ANTIMICROBIALS ------------------------------------------------------------
if (!file.exists(here::here("Data/all_antimicrobialevents.rds"))) {
  ## Systemic administration routes (whitelist). Anything else -- eyes/ears,
  ## topical, inhaled/nebulized, catheter lock, dwell/dialysis, intraperitoneal,
  ## irrigation, intravitreal -- is not systemic antimicrobial therapy.
  systemic_routes <- c("IV",
                       "IV DRIP",
                       "IV BOLUS",
                       "PB",
                       "IVPCA",
                       "PO",
                       "PO/NG",
                       "ORAL",
                       "NG",
                       "G TUBE",
                       "J TUBE",
                       "IM",
                       "IJ",
                       "SL",
                       "SC",
                       "SUBCUT",
                       "IP")
  
  antimicrobial_include <- paste(
    "vancomycin","cefepime","metronidazol","piperacillin","tazobactam","meropenem",
    "ceftriaxone","ceftazidime","cefazolin","cefotaxime","cefuroxime","cefpodoxime",
    "cephalexin","cefoxitin","ceftaroline","ceftolozane","cefdinir","avibactam","vaborbactam",
    "imipenem","cilastatin","ertapenem","doripenem","aztreonam",
    "ciprofloxacin","levofloxacin","moxifloxacin","ofloxacin","gatifloxacin",
    "azithromycin","erythromycin","clarithromycin","clindamycin",
    "ampicillin","sulbactam","amoxicillin","clavulan","penicillin","nafcillin","oxacillin","dicloxacillin",
    "linezolid","tedizolid","daptomycin","tigecycline","doxycycline","minocycline","tetracycline",
    "gentamicin","tobramycin","amikacin","colistin",
    "sulfameth","trimethoprim","nitrofurantoin","fosfomycin","rifampin","chloramphenicol",
    "fluconazole","micafungin","caspofungin","anidulafungin","voriconazole","posaconazole",
    "isavucon","itraconazole","amphotericin",
    "acyclovir","valacyclovir","ganciclovir","valganciclovir","oseltamivir","zanamivir",
    "foscarnet","cidofovir","remdesivir",
    sep = "|")
  
  ## Allergy-protocol entries pass the route filter (IV/PO), so exclude by name.
  antimicrobial_exclude <- "desensiti|graded challenge|\\bchallenge\\b|test dose"
  
  antimicrobial_rx <- all_prescriptions %>% 
    copy() %>% 
    .[grepl(antimicrobial_include, 
            drug, 
            ignore.case = TRUE) &
        !grepl(antimicrobial_exclude,
               drug, 
               ignore.case = TRUE) &
        route %in% systemic_routes &
        ## oral vancomycin (non-absorbed, C. diff) is not systemic therapy:
        !(grepl("vancomycin", 
                drug, 
                ignore.case = TRUE) &
            route %in% c("PO",
                         "PO/NG",
                         "ORAL",
                         "NG",
                         "G TUBE",
                         "J TUBE"))]
  
  saveRDS(antimicrobial_rx, here::here("Data/all_antimicrobialevents.rds"))
}

all_antimicrobialevents <- readRDS(here::here("Data/all_antimicrobialevents.rds")) %>% 
  setDT()

## Check number of subject IDs
all_antimicrobialevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_antimicrobialevents[, uniqueN(hadm_id)]



### EMAR (Electronic Medication Administration Record) data --------------------
if (!file.exists(here::here("Data/all_emar.rds"))) {
  
  sql_emar <- glue::glue(
    "SELECT
    e.subject_id,
    e.hadm_id,
    e.emar_id,
    e.charttime,
    e.medication,
    e.event_txt,
    d.route,
    d.dose_given,
    d.dose_given_unit
  FROM {hosp_db}.emar AS e
  LEFT JOIN {hosp_db}.emar_detail AS d
    ON e.emar_id = d.emar_id AND e.emar_seq = d.emar_seq
  WHERE e.subject_id IN ({subject_ids_sql_list})
    AND e.medication IS NOT NULL")
  
  all_emar_raw <- run_query(sql_emar)
  saveRDS(all_emar_raw, here::here("Data/all_emar_raw.rds"))
  
  all_emar_raw <- readRDS(here::here("Data/all_emar_raw.rds")) %>%
    setDT()
  
  ## Encounter-level linkage window: an administration belongs to a hospitalisation
  ## if charttime falls from `microbio_link_lead_h` hours before admission through
  ## discharge. Reuses the constant defined in the microbiology section.
  admission_window_emar <- base_cohort %>%
    copy() %>%
    .[, .(subject_id,
          hadm_id,
          window_start = admittime - lubridate::hours(24),
          window_end   = dischtime)] %>%
    unique()
  
  ## Rows that already carry a cohort hadm_id keep it unchanged.
  emar_already_linked <- all_emar_raw %>%
    copy() %>%
    .[!is.na(hadm_id)] %>%
    .[hadm_id %in% base_cohort$hadm_id]
  
  ## Rows with a missing hadm_id are matched to an admission by charttime.
  emar_repaired <- all_emar_raw %>%
    copy() %>%
    .[is.na(hadm_id)] %>%
    .[, hadm_id := NULL] %>%
    merge(.,
          admission_window_emar,
          by = "subject_id",
          all.x = FALSE,
          all.y = FALSE,
          allow.cartesian = TRUE) %>%
    .[charttime >= window_start &
        charttime <= window_end] %>%
    ## resolve rows matching more than one admission window (readmission within
    ## the lead window): prefer the admission the row falls INSIDE, else the one
    ## it most immediately precedes.
    .[, admittime := window_start + lubridate::hours(24)] %>%
    .[, is_within_admission := charttime >= admittime] %>%
    .[, gap_to_admit := lubridate::time_length(admittime - charttime, "hours")] %>%
    .[order(emar_id, -is_within_admission, gap_to_admit)] %>%
    .[, .SD[1], by = emar_id] %>%
    .[, `:=` (window_start = NULL,
              window_end   = NULL,
              admittime    = NULL,
              is_within_admission = NULL,
              gap_to_admit = NULL)]
  
  ## Each administration event (emar_id) must map to exactly one hospitalisation
  stopifnot(
    emar_repaired[, .(n_hadm = uniqueN(hadm_id)),
                  by = emar_id][n_hadm > 1, .N] == 0)
  
  ## Combine -- same columns and meaning as before the fix
  all_emar <- rbind(emar_already_linked,
                    emar_repaired,
                    use.names = TRUE,
                    fill = FALSE)
  
  ## Repair only adds rows and never leaves a missing/out-of-cohort hadm_id
  stopifnot(all_emar[, sum(is.na(hadm_id))] == 0)
  stopifnot(all_emar[, all(hadm_id %in% base_cohort$hadm_id)])
  stopifnot(nrow(all_emar) >= nrow(emar_already_linked))
  
  saveRDS(all_emar, here::here("Data/all_emar.rds"))
}


if(!file.exists(here::here("Data/corticosteroid_emar_details.rds"))) {
  ## Corticosteroids patterns
  corticosteroids_patterns <- paste(c("hydrocortisone",
                                      "hydrocostrisone na succ.",
                                      "methylprednisolone",
                                      "dexamethasone",
                                      "prednisolone",
                                      "prednisone"),
                                    collapse = "|")
  
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
  
  corticosteroid_emar_details <- run_query(sql_corticosteroid_emar_details)
  saveRDS(corticosteroid_emar_details, here::here("Data/corticosteroid_emar_details.rds"))
}

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



## 4. ICU-SPECIFIC DATA --------------------------------------------------------

### INPUT EVENTS ---------------------------------------------------------------
## This contains ICU-administered inputs: fluids, vasopressors, medications, infusion 
## rates, volumes. It is needed for variables like IV fluid administration, vasopressor 
## use, medication dose/rate, etc.
if (!file.exists(here::here("Data/all_inputevents.rds"))) {
sql_inputevents <- glue::glue(
"SELECT 
 *
 FROM {icu_db}.inputevents
WHERE subject_id IN ({subject_ids_sql_list})")

all_inputevents <- run_query(sql_inputevents)
saveRDS(all_inputevents, here::here("Data/all_inputevents.rds"))
}

all_inputevents <- readRDS(here::here("Data/all_inputevents.rds")) %>% 
  setDT()

## Check number of stay IDs
all_inputevents[, uniqueN(stay_id)]



### CHART EVENTS --------------------------------------------------------------- 
## Pull chart event look-up table to select relevant item IDs
if (!file.exists(here::here("Data/chartevents_itemids.rds"))) {
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

chartevents_itemids <- run_query(sql_chartevents_itemids)
saveRDS(chartevents_itemids, here::here("Data/chartevents_itemids.rds"))
}

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

if (!file.exists(here::here("Data/all_chartevents.rds"))) {

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

all_chartevents <- run_query(sql_chartevents)
saveRDS(all_chartevents, here::here("Data/all_chartevents.rds"))
}

all_chartevents <- readRDS(here::here("Data/all_chartevents.rds")) %>% 
  setDT()

## Check number of stay IDs
all_chartevents[, uniqueN(stay_id)]
  
#### HEIGHT --------------------------------------------------------------------
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


### PROCEDURE EVENTS -----------------------------------------------------------
if (!file.exists(here::here("Data/all_procedureevents.rds"))) {

sql_procs <- glue::glue(
"SELECT 
  *
  FROM {icu_db}.procedureevents
  WHERE stay_id IN ({stay_ids_sql_list})")

all_procedureevents <- run_query(sql_procs)
saveRDS(all_procedureevents, here::here("Data/all_procedureevents.rds"))
}

all_procedureevents <- readRDS(here::here("Data/all_procedureevents.rds")) %>% 
  setDT()

## Check number of stay IDs
all_procedureevents[, uniqueN(stay_id)]








  

### INGREDIENTS ----------------------------------------------------------------
if (!file.exists(here::here("Data/all_ingredients.rds"))) {
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

all_ingredients <- run_query(sql_ingredients)
saveRDS(all_ingredients, here::here("Data/all_ingredients.rds"))
}

all_ingredients <- readRDS(here::here("Data/all_ingredients.rds")) %>% 
  setDT()

## Check number of subject IDs
all_ingredients[, uniqueN(subject_id)]

## Check number of hadm IDs
all_ingredients[, uniqueN(hadm_id)]

## Check number of stay IDs
all_ingredients[, uniqueN(stay_id)]


### OUTPUTEVENTS ---------------------------------------------------------------
if (!file.exists(here::here("Data/all_outputevents.rds"))) {
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

all_outputevents <- run_query(sql_outputevents)
saveRDS(all_outputevents, here::here("Data/all_outputevents.rds"))
}

all_outputevents <- readRDS(here::here("Data/all_outputevents.rds")) %>% 
  setDT()

## Check number of subject IDs
all_outputevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_outputevents[, uniqueN(hadm_id)]

## Check number of stay IDs
all_outputevents[, uniqueN(stay_id)]


## 5. SEPTIC SHOCK -------------------------------------------------------------
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
      valueuom == "mmol/L" &
      valuenum >= 0 &
      valuenum <= 30]

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
  .[vaso_label == "Epinephrine.",
    vaso_label := "Epinephrine"] %>% 
  # .[vaso_label %in% c("Norepinephrine",
  #                     "Epinephrine",
  #                     "Dopamine",
  #                     "Angiotensin II (Giapreza)",
  #                     "Vasopressin",
  #                     "Phenylephrine",
  #                     "Phenylephrine (50/250)",
  #                     "Phenylephrine (200/250)")] %>% 
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
#   .[, hypoperfusion_proxy := lactate_ov  er2] %>% 
#   .[, septic_shock_onset_time := fifelse(
#     septic_shock_proxy,
#     pmax(first_lactate_over2_time,
#          first_vaso_time,
#          na.rm = TRUE),
#     as.POSIXct(NA)
#   )]

## 6. SEPSIS-INDUCED HYPOPERFUSION ---------------------------------------------
## Recommendation 10 (2026 SSC) targets "sepsis-induced hypoperfusion or septic
## shock". Per the guideline text, sepsis-induced hypoperfusion is identified by
## elevated lactate. The 2026 guideline adopts the Sepsis-3 lactate threshold of
## > 2 mmol/L, demoting the historical >= 4 mmol/L threshold. Relative
## hypotension (SBP below the patient's own pre-morbid baseline) is not
## operationalizable in MIMIC-IV and is omitted (documented as a limitation).
## Septic shock is a subset of hypoperfusion, so shock onset is unioned in as a
## third qualifying event. Hypoperfusion onset is the earliest qualifying event
## within the sepsis episode; this generalizes septic_shock_onset_time.

## Lactate > 2 mmol/L within the sepsis episode
lactate_hypoperfusion <- all_lactateevents %>%
  copy() %>%
  .[valuenum > 2] %>%
  merge(.,
        sepsis_onset,
        by = c("subject_id",
               "hadm_id"),
        all = FALSE,
        allow.cartesian = TRUE) %>%
  .[charttime >= sepsis_onset - hours(6) &
      charttime <= sepsis_onset + hours(24)] %>%
  .[, .(hp_lactate_time = min(charttime)),
    by = .(subject_id,
           hadm_id,
           stay_id)]

## Blood-pressure item IDs for hypotension (mean + systolic, arterial +
## non-invasive). chartevents is ICU-only, so pre-ICU hypotension is not
## observable -- a coverage limitation shared with the shock proxy.
# bp_hypoperfusion_itemids <- chart_itemids %>%
#   copy() %>%
#   .[label %in% c("Arterial Blood Pressure mean",
#                  "Non Invasive Blood Pressure mean",
#                  "Arterial Blood Pressure systolic",
#                  "Non Invasive Blood Pressure systolic",
#                  ## manual cuff readings feed the SBP < 90 arm only
#                  ## (there is no manual mean, so MAP is unaffected)
#                  "Manual Blood Pressure Systolic Left",
#                  "Manual Blood Pressure Systolic Right")]

## Hypotension: MAP < 65 or SBP < 90 within the sepsis episode.
## At each timestamp, arterial is preferred over non-invasive (as in rec 13),
## resolved separately for mean and systolic pressure.
# blood_pressure_events <- all_chartevents %>%
#   copy() %>%
#   ## only patients in sepsis base cohort
#   .[stay_id %in% base_cohort$stay_id] %>%
#   ## Blood pressure item IDs associated with hypoperfusion
#   .[itemid %in% bp_hypoperfusion_itemids$itemid] %>%
#   .[!is.na(valuenum)] %>%
#   merge(.,
#         bp_hypoperfusion_itemids[, .(itemid,
#                                      bp_label = label)],
#         by = "itemid") %>%
#   .[, bp_kind := fifelse(grepl("mean",
#                                bp_label),
#                          "map",
#                          "sbp")] %>%
#   ## measurement-source preference: arterial > non-invasive > manual cuff
#   .[, bp_source_rank := fcase(
#     grepl("Arterial", bp_label), 1L,
#     grepl("Non Invasive", bp_label), 2L,
#     grepl("Manual", bp_label), 3L,
#     default = NA_integer_)] %>%
#   ## plausibility bounds (MAP as in rec 13; SBP physiologic range)
#   .[(bp_kind == "map" &
#        valuenum >= 20 &
#        valuenum <= 200) |
#       (bp_kind == "sbp" &
#          valuenum >= 40 &
#          valuenum <= 300)]

# hypotension_hypoperfusion <- blood_pressure_events %>% 
#   copy() %>% 
#   ## prefer arterial over non-invasive over manual at each timestamp, per kind
#   .[order(stay_id,
#           charttime,
#           bp_kind,
#           bp_source_rank)] %>%
#   .[, .SD[1],
#     by = .(stay_id,
#            charttime,
#            bp_kind)] %>%
#   ## hypotension threshold, after source de-duplication
#   .[(bp_kind == "map" &
#        valuenum < 65) |
#       (bp_kind == "sbp" &
#          valuenum < 90)] %>% 
#   merge(.,
#         sepsis_onset[, .(subject_id,
#                          hadm_id,
#                          stay_id,
#                          sepsis_onset)],
#         by = "stay_id",
#         all = FALSE) %>% 
#   .[charttime >= sepsis_onset - hours(6) &
#       charttime <= sepsis_onset + hours(24)] %>%
#   .[, .(hp_hypotension_time = min(charttime)),
#     by = .(subject_id,
#            hadm_id,
#            stay_id)]

## Septic shock onset as a qualifying hypoperfusion event
shock_hypoperfusion <- first_septic_shock %>%
  copy() %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        hp_shock_time = septic_shock_onset_time)]

## Hypoperfusion onset = earliest qualifying event (lactate > 2,
## or septic shock onset), within the sepsis episode. The three event tables are
## combined with full outer joins so a stay is kept if it meets any one of them,
## then the onset is the earliest of the (possibly missing) event times.
hypoperfusion_onset <- lactate_hypoperfusion %>%
  copy() %>%
  # merge(.,
  #       hypotension_hypoperfusion,
  #       by = c("subject_id",
  #              "hadm_id",
  #              "stay_id"),
  #       all = TRUE) %>%
  merge(.,
        shock_hypoperfusion,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = TRUE) %>%
  .[, hypoperfusion_onset_time := fifelse(
    !is.na(hp_shock_time), 
    hp_shock_time, 
    hp_lactate_time)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        hypoperfusion_onset_time,
        hp_lactate_time,
        # hp_hypotension_time,
        hp_shock_time)]


## Add septic shock and hypoperfusion variables to ssc variable data
sepsis_base_cohort_data <- sepsis_base_cohort_data %>%
  copy() %>%
  merge(.,
        hypoperfusion_onset,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, hypoperfusion_proxy := fcase(

    sepsis3 == TRUE &
      !is.na(hypoperfusion_onset_time),
    TRUE,

    sepsis3 == TRUE,
    FALSE,

    default = NA)]

saveRDS(sepsis_base_cohort_data,
        here::here("Data/sepsis_base_cohort_data.rds"))

## Number of patients with septic shock
sepsis_base_cohort_data[septic_shock_proxy == TRUE, uniqueN(stay_id)]

## Number of patients with septic shock onset before sepsis onset
sepsis_base_cohort_data[septic_shock_onset_time < suspected_infection_time, uniqueN(stay_id)]

## Number of patients with septic shock onset at or after sepsis onset
sepsis_base_cohort_data[septic_shock_onset_time >= suspected_infection_time, uniqueN(stay_id)]

## Number of patients with sepsis-induced hypoperfusion (incl. septic shock)
sepsis_base_cohort_data[hypoperfusion_proxy == TRUE, uniqueN(stay_id)]

## Qualifying-event breakdown for hypoperfusion (patients can meet >1)
sepsis_base_cohort_data[hypoperfusion_proxy == TRUE,
                        .(lactate = sum(!is.na(hp_lactate_time)),
                          # hypotension = sum(!is.na(hp_hypotension_time)),
                          shock = sum(!is.na(hp_shock_time)))]


## 7. NOTE-MIMICIV -------------------------------------------------------------
###  RADIOLOGY EVENTS ----------------------------------------------------------
if (!file.exists(here::here("Data/all_radiologyevents.rds"))) {
sql_radiology <- glue::glue(
  "SELECT
    *
  FROM {note_db}.radiology
  WHERE hadm_id IN ({hadm_ids_sql_list})")

all_radiologyevents <- run_query(sql_radiology)
saveRDS(all_radiology_events, here::here("Data/all_radiologyevents.rds"))
}

all_radiologyevents <- readRDS(here::here("Data/all_radiologyevents.rds")) %>% 
  setDT()

## Check number of subject IDs
all_radiologyevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_radiologyevents[, uniqueN(hadm_id)]


### DISCHARGE NOTES FROM MIMICIV-NOTE ------------------------------------------
if (!file.exists(here::here("Data/all_dischargeevents.rds"))) {
sql_discharge <- glue::glue(
  "SELECT
  *
  FROM {note_db}.discharge
  WHERE hadm_id IN ({hadm_ids_sql_list})")

all_dischargeevents <- run_query(sql_discharge)
saveRDS(all_dischargeevents, here::here("Data/all_dischargeevents.rds"))
}

all_dischargeevents <- readRDS(here::here("Data/all_dischargeevents.rds")) %>% 
  setDT()

## Check number of subject IDs
all_dischargeevents[, uniqueN(subject_id)]

## Check number of hadm IDs
all_dischargeevents[, uniqueN(hadm_id)]

## 8. EMERGENCY DEPARTMENT DATA ------------------------------------------------
### Stays --------------------------------------------------------------------
if (!file.exists(here::here("Data/ed_stays.rds"))) {
  
  sql_edstays <- glue::glue(
    "SELECT subject_id, hadm_id, stay_id,
            intime  AS ed_intime,
            outtime AS ed_outtime
     FROM {ed_db}.edstays
     WHERE subject_id IN ({subject_ids_sql_list})")
  
  ed_stays <- run_query(sql_edstays)
  saveRDS(ed_stays, here::here("Data/ed_stays.rds"))
}

all_edstays <- readRDS(here::here("Data/ed_stays.rds")) %>%
  setDT()

### Diagnosis ------------------------------------------------------------------
if (!file.exists(here::here("Data/all_ed_diagnosisevents.rds"))) {
  
  sql_ed_diagnosis <- glue::glue(
  "SELECT
  *
  FROM {ed_db}.diagnosis
  WHERE subject_id IN ({subject_ids_sql_list})")

all_ed_diagnosisevents <- run_query(sql_ed_diagnosis)
saveRDS(all_ed_diagnosisevents, here::here("Data/all_ed_diagnosisevents.rds"))
}

all_ed_diagnosisevents <- readRDS(here::here("Data/all_ed_diagnosisevents.rds")) %>% 
  setDT() %>% 
  .[, icd_title := tolower(trimws(icd_title))]

## ICD codes sepsis
sepsis_icd <- all_ed_diagnosisevents %>% 
  copy() %>% 
  .[icd_title %in% c("septicemia nos", ## older term for sepsis (bloodstream infection)
                     "sepsis, unspecified organism",
                     "sepsis",
                     "severe sepsis",
                     "gram-negative sepsis, unspecified",
                     "other specified sepsis",
                     "severe sepsis without septic shock",
                     "sepsis following a procedure, initial encounter"),
    .(icd_code,
      icd_version,
      icd_title)] %>% 
  unique()

## ICD codes septic shock
septic_shock_icd <- all_ed_diagnosisevents %>% 
  copy() %>% 
  .[icd_title %in% c("septic shock",
                     "severe sepsis with septic shock",
                     "postprocedural septic shock, initial encounter"),
    .(icd_code,
      icd_version,
      icd_title)] %>% 
  unique()


### Medicine Reconciliation ----------------------------------------------------
## Medications that patients are currently on 
## upon emergency department administration
if(!file.exists(here::here("Data/all_medreconevents.rds"))) {
  
  sql_medrecon <- glue::glue(
    "SELECT
  *
  FROM {ed_db}.medrecon
  WHERE subject_id IN ({subject_ids_sql_list})")
  
all_medreconevents <- run_query(sql_medrecon)
saveRDS(all_medreconevents, here::here("Data/all_medreconevents.rds"))
}

all_medreconevents <- readRDS(here::here("Data/all_medreconevents.rds")) %>% 
  setDT()


### Medicine Dispensations -----------------------------------------------------
## Medicine dispensations made via the Pyxis system
if(!file.exists(here::here("Data/all_dispenseevents.rds"))) {
  
  sql_dispense <- glue::glue(
    "SELECT
  *
  FROM {ed_db}.pyxis
  WHERE subject_id IN ({subject_ids_sql_list})")
  
  all_dispenseevents <- run_query(sql_dispense)
  saveRDS(all_dispenseevents, here::here("Data/all_dispenseevents.rds"))
}

all_dispenseevents <- readRDS(here::here("Data/all_dispenseevents.rds")) %>% 
  setDT() %>% 
  .[, name := tolower(trimws(name))]


### Triage ---------------------------------------------------------------------
if (!file.exists(here::here("Data/all_edtriage.rds"))) {
  
  sql_edtriage <- glue::glue(
    "SELECT
    *
    FROM {ed_db}.triage
    WHERE subject_id IN ({subject_ids_sql_list})")
  
  all_edtriage <- run_query(sql_edtriage)
  saveRDS(all_edtriage, here::here("Data/all_edtriage.rds"))
}
 
 all_edtriage <- readRDS(here::here("Data/all_edtriage.rds")) %>% 
   setDT()

 
 ### Vital Signs ---------------------------------------------------------------
 if (!file.exists(here::here("Data/all_edvitalsign.rds"))) {
   
   sql_edvital <- glue::glue(
     "SELECT
    *
    FROM {ed_db}.vitalsign
    WHERE subject_id IN ({subject_ids_sql_list})")
   
   all_edvitalsign <- run_query(sql_edvital)
   saveRDS(all_edvitalsign, here::here("Data/all_edvitalsign.rds"))
 }
 
all_edvitalsign <- readRDS(here::here("Data/all_edvitalsign.rds")) %>% 
  setDT()

