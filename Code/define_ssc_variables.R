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

## single source of truth for onset anchoring, reused by recs 7, 16 and 17
onset_anchor_lookup <- unique(
  sepsis_base_cohort_data[, .(subject_id,
                              hadm_id,
                              stay_id,
                              suspected_infection_time,
                              culture_time,
                              antibiotic_time)]) %>%
  .[, onset_anchor := fcase(
    
    !is.na(culture_time) &
      suspected_infection_time == culture_time,
    "culture-anchored",
    
    !is.na(antibiotic_time) &
      suspected_infection_time == antibiotic_time,
    "antibiotic-anchored",
    
    default = "other/unresolved")] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        onset_anchor)]

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
  
    default = NA)] %>% 
  ## onset anchor: the same culture/antibiotic circularity as recs 16 and 17.
  ## Whether the culture precedes the antimicrobial is largely fixed by which
  ## timestamp anchors Sepsis-3 onset, so adherence is near-tautological by anchor.
  merge(.,
        onset_anchor_lookup,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, rec_7_culture_before_antimicrobial_causal := fifelse(
    onset_anchor == "culture-anchored",
    rec_7_culture_before_antimicrobial,
    NA)]

stopifnot(nrow(first_sepsis_antimicrobial) == nrow(unique(sepsis_base_cohort_data, 
                                                          by = c("subject_id",
                                                                 "hadm_id",
                                                                 "stay_id"))))

stopifnot(first_sepsis_antimicrobial[, sum(is.na(antimicrobial_time))] == 0)

stopifnot(nrow(rec_7) == nrow(first_sepsis_antimicrobial))

## Adherence Rate
proportions(table(rec_7$rec_7_culture_before_antimicrobial))

## Antimicrobial source rate
proportions(table(first_sepsis_antimicrobial$antimicrobial_source))
table(first_sepsis_antimicrobial$antimicrobial_source)

## adherence by onset anchor (isolates the culture -> antibiotic circularity)
rec_7[, .(adherence = mean(rec_7_culture_before_antimicrobial, na.rm = TRUE),
          n         = .N),
      by = onset_anchor]


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
  .[, lactate_lower_bound := suspected_infection_time - lubridate::hours(1)] %>%
  ## lactate is drawn to assess THIS suspected-sepsis episode (to detect
  ## hypoperfusion / shock), so the window is anchored tightly to onset; a
  ## lactate drawn well before onset predates the suspicion and does not serve
  ## that purpose. The 1h buffer absorbs documentation lag, since onset and the
  ## triggering labs are often near-simultaneous.
  .[lactate_time >= lactate_lower_bound &
      lactate_time <= suspected_infection_time + lubridate::hours(24)] %>%
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
  .[, rec_8_lactate_measured_within_1h_causal := fcase(
    
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
        rec_8_lactate_measured_within_1h_causal,
        rec_8_lactate_measured_within_3h,
        rec_8_lactate_measured_within_6h,
        time_to_sepsis)] %>% 
  unique() %>% 
  .[, rec_8_eligibility := fcase(
    
    !is.na(rec_8_lactate_measured),
    "eligible",
    
    default = NA_character_)]


## Check resulting adherence rates
proportions(table(rec_8$rec_8_lactate_measured))
proportions(table(rec_8$rec_8_lactate_measured_within_1h_causal))
proportions(table(rec_8$rec_8_lactate_measured_within_3h))
proportions(table(rec_8$rec_8_lactate_measured_within_6h))
summary(first_lactate$time_to_sepsis)

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

## Only crystalloid given as fluid counts: ordercategorydescription = "Bolus"
## and "Continuous IV". Crystalloid recorded under "Continuous Med" (drip carrier
## bags) and "Drug Push" (medication diluents) is excluded -- it is incidental to
## drug delivery, infused at drug rather than resuscitation rates, and would
## confound adherence with illness severity (sicker patients on more drips accrue
## more carrier volume). Primary = Bolus + Continuous IV; boluses-only added as a
## sensitivity. Target uses the validated weight_kg_for_fluids, not the
## inputevents patientweight field (which contains gross data-entry errors).
# crystalloid_itemids <- input_fluid_classification %>%
#   .[fluid_type %in% c("Unbalanced crystalloid (normal saline)",
#                       "Balanced crystalloid") |
#       (fluid_type == "Aggregate intake (non-ICU)" &
#          grepl("crystalloid",
#                label,
#                ignore.case = TRUE))]

# ### Aggregated crystalloid intake
# ## Inspect aggregated crystalloid intake
# crystalloid_events_aggregated <- all_inputevents %>% 
#   ## only patients in sepsis base cohort
#   .[stay_id %in% sepsis_base_cohort_data$stay_id] %>%
#   ## aggregated crystalloid intake item IDs
#   .[itemid %in% crystalloid_itemids[
#     resuscitation_role == "aggregated crystalloid"]$itemid] %>% 
#   merge(.,
#         crystalloid_itemids[, .(itemid,
#                                 label)],
#         by = "itemid",
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   ## unify units (mL and L)
#   .[, amount_ml := fcase(
#     
#     amountuom == "mL",
#     amount,
#     
#     amountuom == "L",
#     amount * 1000,
#     
#     default = NA_real_)] %>% 
#   .[, .(subject_id,
#         hadm_id,
#         stay_id,
#         starttime,
#         endtime,
#         storetime,
#         itemid,
#         label,
#         amount_ml,
#         ordercategoryname,
#         ordercategorydescription,
#         patientweight)] %>% 
#   unique() %>% 
#   merge(.,
#         sepsis_base_cohort_data[, .(subject_id,
#                                     hadm_id,
#                                     stay_id,
#                                     suspected_infection_time,
#                                     icu_intime,
#                                     icu_outtime,
#                                     fluid_resuscitation_start,
#                                     fluid_resuscitation_end)],
#         by = c("subject_id",
#                "hadm_id",
#                "stay_id"),
#         all.x = TRUE,
#         all.y = FALSE) %>% 
#   .[, fluid_duration := 
#       lubridate::time_length(
#         endtime-
#           starttime,
#         "mins")] %>% 
#   .[, time_to_sepsis :=
#       lubridate::time_length(
#         starttime -
#           suspected_infection_time,
#         "hours")]
# 
# ## Confirm that all crystalloid stoptimes are within 1 min of crystalloid starttime
# ## Entries display bolus fluid administration over a short time span
# stopifnot(nrow(crystalloid_events_aggregated[fluid_duration > 1]) == 0)
# 
# ## Analyze distribution of time difference between 
# ## crystalloid administration and sepsis onset
# summary(crystalloid_events_aggregated$time_to_sepsis)
# 
# quantile(crystalloid_events_aggregated$time_to_sepsis,
#          probs = c(0.01, 
#                    0.1, 
#                    0.5, 
#                    0.75,
#                    0.8,
#                    0.85,
#                    0.9, 
#                    0.95, 
#                    0.99))
# 
# ## Restrict to relevant sepsis episode
# crystalloid_resuscitation_aggregated <- 
#   crystalloid_events_aggregated %>% 
#   .[starttime >= suspected_infection_time - hours(24) &
#       starttime <= suspected_infection_time + hours(24)] %>% 
#   .[, crystalloid_target := patientweight * 30] %>% 
#   .[, total_crystalloid_reveived_over_30 := amount_ml >= crystalloid_target]


## Non-aggregated Crystalloid intake
## Verify crystalloid bolus definition
all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## filter to crystalloid administrations
  .[itemid %in% input_fluid_classification[resuscitation_role == "crystalloid"]$itemid] %>% 
  ## Unify fluid amount units
  .[, amount_ml := fcase(
    
    amountuom == "mL",
    amount,
    
    amountuom == "L",
    amount * 1000,
    
    default = NA)] %>% 
  .[, .(.N, total_ml = sum(amount_ml, 
                           na.rm = TRUE)),
    by = .(ordercategoryname,
           ordercomponenttypedescription, 
           ordercategorydescription)] %>% 
  .[order(-total_ml)]


## Crystalloids administered within fist 3h
crystalloid_events <- all_inputevents %>% 
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## filter to crystalloid administrations
  .[itemid %in% input_fluid_classification[resuscitation_role == "crystalloid"]$itemid] %>% 
  ## Unify fluid amount units
  .[, amount_ml := fcase(
    
    amountuom == "mL",
    amount,
    
    amountuom == "L",
    amount * 1000,
    
    default = NA)] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    hypoperfusion_proxy,
                                    hypoperfusion_onset_time,
                                    suspected_infection_time)],
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
  .[, counted_as_resuscitation := ordercategoryname %in% c("02-Fluids (Crystalloids)",
                                                           "03-IV Fluid Bolus") &
      ordercategorydescription %in% c("Continuous IV",
                                      "Bolus")]


## Total crystalloid intake in first 3h
total_crystalloid_3h <- crystalloid_events %>% 
  copy() %>% 
  .[, total_crystalloid_3h := 
      sum(fluid_ml_3h,
          na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        total_crystalloid_3h)] %>% 
  unique()


## Resuscitation crystalloid intake first 3h
resus_crystalloid_3h <- crystalloid_events %>% 
  copy() %>% 
  .[counted_as_resuscitation == TRUE] %>% 
  .[, resus_crystalloid_3h := 
      sum(fluid_ml_3h,
          na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        resus_crystalloid_3h)] %>% 
  unique()


## Concurrent crystalloid intake first 3h
concurrent_crystalloid_3h <- crystalloid_events %>% 
  copy() %>% 
  .[counted_as_resuscitation == FALSE] %>% 
  .[, concurrent_crystalloid_3h := 
      sum(fluid_ml_3h,
          na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        concurrent_crystalloid_3h)] %>% 
  unique()


## Strictly bolus crystalloid intake first 3h
bolus_crystalloid_3h <- crystalloid_events %>% 
  copy() %>% 
  .[ordercategorydescription %in% c("Continuous IV",
                                    "Bolus") &
      ordercategoryname == "03-IV Fluid Bolus"] %>% 
  .[, bolus_crystalloid_3h := 
      sum(fluid_ml_3h,
          na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        bolus_crystalloid_3h)] %>% 
  unique()


## Combine into one data table
crystalloid_3h <- merge(
  total_crystalloid_3h,
  resus_crystalloid_3h,
  by = c("subject_id",
         "hadm_id",
         "stay_id"),
  all = TRUE) %>% 
  merge(.,
        concurrent_crystalloid_3h,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = TRUE) %>% 
  merge(.,
        bolus_crystalloid_3h,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = TRUE)


## Sensitivity volume table: identical crystalloid accounting as the primary,
## but the [-1h, +3h] window is anchored to SEPSIS onset (suspected_infection_time)
## rather than hypoperfusion/shock onset. This matches the anchor used by the
## guideline's cited evidence ("30 mL/kg within 3 h of sepsis onset") and, for the
## shock stratum, moves the window earlier (often pre-ICU), quantifying how much
## the anchor choice alone drives the measured adherence.
crystalloid_3h_sepsisonset <- crystalloid_events %>%
  .[hypoperfusion_proxy == TRUE] %>% 
  ## resuscitation window anchored on sepsis onset
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
        crystalloid_3h,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## coalesce missing crystalloid entries
  .[, crystalloid_ml_3h := fifelse(
    !is.na(resus_crystalloid_3h),
    resus_crystalloid_3h,
    fifelse(
      is.na(resus_crystalloid_3h) &
        hypoperfusion_proxy == TRUE,
      0,
      NA_real_))] %>%
  .[, crystalloid_ml_3h_bolus := fifelse(
    !is.na(bolus_crystalloid_3h),
    bolus_crystalloid_3h,
    fifelse(
      is.na(bolus_crystalloid_3h) &
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
        crystalloid_3h_sepsisonset,
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

    default = NA_character_)] %>% 
  merge(.,
        unique(sepsis_base_cohort_data[, .(subject_id,
                                           hadm_id,
                                           stay_id,
                                           septic_shock_proxy)]),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  ## causal treatment: ICU-observable (septic-shock) stratum only. The lactate-only
  ## stratum has its resuscitation window largely before ICU admission, where
  ## inputevents records no fluid, so its non-adherence is a one-directional
  ## observability artefact. Those stays are set NA and excluded from the IRM.
  .[, rec_10_crystalloid_within_3h_causal := fifelse(
    septic_shock_proxy == TRUE,
    rec_10_crystalloid_within_3h,
    NA)]



# Check resulting adherence rates
## PRIMARY: >= 30 mL/kg crystalloid (Bolus + Continuous IV), actual body weight
proportions(table(rec_10$rec_10_crystalloid_within_3h))
table(rec_10$rec_10_crystalloid_within_3h)

## adjusted body weight target (BMI > 30, guideline Table 4)
proportions(table(rec_10$rec_10_crystalloid_within_3h_adjbw))

## resuscitation boluses only
proportions(table(rec_10$rec_10_crystalloid_within_3h_bolus))

## resuscitation boluses only, adjusted body weight target
proportions(table(rec_10$rec_10_crystalloid_within_3h_bolus_adjbw))

## window anchored to sepsis onset instead of hypoperfusion/shock onset
proportions(table(rec_10$rec_10_crystalloid_within_3h_sepsisonset))

## eligibility breakdown
proportions(table(rec_10$rec_10_eligibility))

## causal ml adherence rate
proportions(table(rec_10$rec_10_crystalloid_within_3h_causal))

## Fluid volumes among eligible (sepsis-induced hypoperfusion) patients
## resuscitation crystalloid delivered in the window (mL)
summary(rec_10[hypoperfusion_proxy == TRUE]$crystalloid_ml_3h)

## resuscitation boluses only (mL)
summary(rec_10[hypoperfusion_proxy == TRUE]$crystalloid_ml_3h_bolus)

## total crystalloid in the window, any order category (mL)
summary(rec_10[hypoperfusion_proxy == TRUE]$total_crystalloid_3h)

## concurrent (non-resuscitation) crystalloid in the window (mL)
summary(rec_10[hypoperfusion_proxy == TRUE]$concurrent_crystalloid_3h)



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
  merge(.,
        map_events,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  ## one row per stay: median MAP over each candidate window
  .[, .(septic_shock_proxy      = septic_shock_proxy[1],
        septic_shock_onset_time = septic_shock_onset_time[1],
        
        ## PRIMARY window [-1h, +3h]
        median_map =
          median(valuenum[charttime >= septic_shock_onset_time - lubridate::hours(1) &
                            charttime <= septic_shock_onset_time + lubridate::hours(3)],
                 na.rm = TRUE),
        
        ## SENSITIVITY window [0h, +6h]
        median_map_0_6h =
          median(valuenum[charttime >= septic_shock_onset_time &
                            charttime <= septic_shock_onset_time + lubridate::hours(6)],
                 na.rm = TRUE),
        
        ## SENSITIVITY window [0h, +24h]
        median_map_0_24h =
          median(valuenum[charttime >= septic_shock_onset_time &
                            charttime <= septic_shock_onset_time + lubridate::hours(24)],
                 na.rm = TRUE)),
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  ## PRIMARY adherence: median MAP in the 60-70 mmHg band
  .[septic_shock_proxy == TRUE,
    rec_13_map_target_65 :=
      !is.na(median_map) &
      median_map >= 60 &
      median_map <= 70] %>%
  ## SENSITIVITY adherence: [0h, +6h] window
  .[septic_shock_proxy == TRUE,
    rec_13_map_target_65_0_6h :=
      !is.na(median_map_0_6h) &
      median_map_0_6h >= 60 &
      median_map_0_6h <= 70] %>%
  ## SENSITIVITY adherence: [0h, +24h] window
  .[septic_shock_proxy == TRUE,
    rec_13_map_target_65_0_24h :=
      !is.na(median_map_0_24h) &
      median_map_0_24h >= 60 &
      median_map_0_24h <= 70] %>%
  .[, rec_13_eligibility := fcase(
    
    !is.na(rec_13_map_target_65),
    "eligible",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_)] %>% 
  ## direction of MAP relative to the target band (eligible shock patients)
  .[septic_shock_proxy == TRUE &
      !is.na(median_map),
    rec_13_map_band := fcase(
      
      median_map < 60,
      "below 60 (under target)",
      
      median_map <= 70,
      "60-70 (at target)",
      
      default = "above 70 (higher target)")]

## Check resulting adherence rates
## PRIMARY: median MAP in 60-70 mmHg over [-1h, +3h] of shock onset
proportions(table(rec_13$rec_13_map_target_65))

## window sensitivity
proportions(table(rec_13$rec_13_map_target_65_0_6h))
proportions(table(rec_13$rec_13_map_target_65_0_24h))

## eligibility breakdown
proportions(table(rec_13$rec_13_eligibility))

proportions(table(rec_13$rec_13_map_band))
table(rec_13$rec_13_map_band)


## 14 RECOMMENDATION -----------------------------------------------------------
rec14_label <- ssc_recommendations %>%
  .[rec_num == 14, rec_label]

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
rec_14 <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        anchor_age,
        septic_shock_proxy,
        septic_shock_onset_time)] %>%
  unique() %>%
  merge(.,
        map_events,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  ## one row per stay: median MAP over each candidate window
  .[, .(septic_shock_proxy      = septic_shock_proxy[1],
        septic_shock_onset_time = septic_shock_onset_time[1],
        
        ## PRIMARY window [-1h, +3h]
        median_map =
          median(valuenum[charttime >= septic_shock_onset_time - lubridate::hours(1) &
                            charttime <= septic_shock_onset_time + lubridate::hours(3)],
                 na.rm = TRUE),
        
        ## SENSITIVITY window [0h, +6h]
        median_map_0_6h =
          median(valuenum[charttime >= septic_shock_onset_time &
                            charttime <= septic_shock_onset_time + lubridate::hours(6)],
                 na.rm = TRUE),
        
        ## SENSITIVITY window [0h, +24h]
        median_map_0_24h =
          median(valuenum[charttime >= septic_shock_onset_time &
                            charttime <= septic_shock_onset_time + lubridate::hours(24)],
                 na.rm = TRUE)),
    by = .(subject_id,
           hadm_id,
           stay_id,
           anchor_age)] %>%
  ## PRIMARY adherence: median MAP in the 55-70 mmHg band
  .[septic_shock_proxy == TRUE &
      anchor_age >= 65,
    rec_14_map_target_60_65 :=
      !is.na(median_map) &
      median_map >= 55 &
      median_map <= 70] %>%
  ## SENSITIVITY adherence: [0h, +6h] window
  .[septic_shock_proxy == TRUE &
      anchor_age >= 65,
    rec_14_map_target_60_65_0_6h :=
      !is.na(median_map_0_6h) &
      median_map_0_6h >= 55 &
      median_map_0_6h <= 70] %>%
  ## SENSITIVITY adherence: [0h, +24h] window
  .[septic_shock_proxy == TRUE &
      anchor_age >= 65,
    rec_14_map_target_60_65_0_24h :=
      !is.na(median_map_0_24h) &
      median_map_0_24h >= 55 &
      median_map_0_24h <= 70] %>%
  .[, rec_14_eligibility := fcase(
    
    !is.na(rec_14_map_target_60_65),
    "eligible",
    
    anchor_age < 65,
    "< 65 years old",
    
    anchor_age <= 65 &
      septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_)] %>% 
  ## direction of MAP relative to the target band (eligible shock patients)
  .[septic_shock_proxy == TRUE &
      anchor_age >= 65 &
      !is.na(median_map),
    rec_14_map_band := fcase(
      
      median_map < 55,
      "below 55 (under target)",
      
      median_map <= 70,
      "55-70 (at target)",
      
      default = "above 70 (higher target)")]

## Check resulting adherence rates
## PRIMARY: median MAP in 55-70 mmHg over [-1h, +3h] of shock onset
proportions(table(rec_14$rec_14_map_target_60_65))

## window sensitivity
proportions(table(rec_14$rec_14_map_target_60_65_0_6h))
proportions(table(rec_14$rec_14_map_target_60_65_0_24h))

## eligibility breakdown
proportions(table(rec_14$rec_14_eligibility))
table(rec_14$rec_14_eligibility)

proportions(table(rec_14$rec_14_map_band))
table(rec_14$rec_14_map_band)


## 15 RECOMMENDATION -----------------------------------------------------------
rec15_label <- ssc_recommendations %>% 
  .[rec_num == 15, rec_label]

rec_15 <- sepsis_base_cohort_data %>%
  copy() %>%
  .[, hours_to_icu :=
      lubridate::time_length(icu_intime - suspected_infection_time,
                             unit = "hours")] %>%
  ## adherent if ICU admission follows an onset at or before admission by <= 6h;
  ## in-ICU onset (negative interval) -> recommendation not applicable -> NA
  .[, rec_15_icu_6h_causal := fcase(
    
    is.na(hours_to_icu),
    NA,
    
    hours_to_icu < 0,
    NA,
    
    hours_to_icu <= 6,
    TRUE,
    
    default = FALSE)] %>%
  .[, rec_15_eligibility := fcase(
    
    is.na(hours_to_icu),
    NA_character_,
    
    hours_to_icu < 0,
    "sepsis onset after ICU admission (not applicable)",
    
    default = "eligible")]

## Check resulting adherence rate
proportions(table(rec_15$rec_15_icu_6h_causal))

## eligibility breakdown (denominator = "eligible")
proportions(table(rec_15$rec_15_eligibility))
table(rec_15$rec_15_eligibility)

## hours from onset to ICU admission, among applicable patients
summary(rec_15[rec_15_eligibility == "eligible"]$hours_to_icu)


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
  ## SENSITIVITY: 3h, one-sided
  .[septic_shock_proxy == TRUE,
    rec_16_antimicrobial_within_3h := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial <= 3] %>%
  ## SENSITIVITY: strict two-sided [-1h, +1h]
  .[septic_shock_proxy == TRUE,
    rec_16_antimicrobial_within_1h_strict := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial >= -1 &
      time_to_antimicrobial <= 1] %>%
  ## causal-analysis treatment: culture-anchored stays only. Antibiotic-anchored
  ## recognition is circular (adherence ~1 by construction), so it is set NA and
  ## excluded from the IRM to preserve a genuine exposure and overlap.
  .[septic_shock_proxy == TRUE,
    rec_16_antimicrobial_within_1h_causal := fifelse(
      onset_anchor == "culture-anchored",
      rec_16_antimicrobial_within_1h,
      NA)] %>% 
  .[, rec_16_eligibility := fcase(

    !is.na(rec_16_antimicrobial_within_1h),
    "eligible",

    septic_shock_proxy == FALSE,
    "sepsis without shock (rec 17)",

    default = NA_character_)]


proportions(table(rec_16$rec_16_antimicrobial_within_1h))

proportions(table(rec_16$rec_16_antimicrobial_within_3h))

proportions(table(rec_16$rec_16_antimicrobial_within_1h_strict))

## adherence by onset anchor (isolates the culture -> antibiotic circularity)
rec_16[septic_shock_proxy == TRUE,
       .(within_1h = mean(rec_16_antimicrobial_within_1h, na.rm = TRUE),
         n        = .N),
       by = onset_anchor]

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
  ## SENSITIVITY: 3h, one-sided (NOT rec 18)
  .[septic_shock_proxy == FALSE,
    rec_17_antimicrobial_within_3h := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial <= 3] %>%
  ## SENSITIVITY: strict two-sided [-1h, +1h]
  .[septic_shock_proxy == FALSE,
    rec_17_antimicrobial_within_1h_strict := !is.na(time_to_antimicrobial) &
      time_to_antimicrobial >= -1 &
      time_to_antimicrobial <= 1] %>%
  ## causal-analysis treatment: culture-anchored stays only. Antibiotic-anchored
  ## recognition is circular (adherence ~1 by construction), so it is set NA and
  ## excluded from the IRM to preserve a genuine exposure and overlap.
  .[septic_shock_proxy == FALSE,
    rec_17_antimicrobial_within_1h_causal := fifelse(
      onset_anchor == "culture-anchored",
      rec_17_antimicrobial_within_1h,
      NA)] %>% 
  .[, rec_17_eligibility := fcase(

    !is.na(rec_17_antimicrobial_within_1h),
    "eligible",

    septic_shock_proxy == TRUE,
    "septic shock (rec 16)",
    
    default = NA_character_)]


proportions(table(rec_17$rec_17_antimicrobial_within_1h))
table(rec_17$rec_17_antimicrobial_within_1h)

proportions(table(rec_17$rec_17_antimicrobial_within_1h_causal))

proportions(table(rec_17$rec_17_antimicrobial_within_3h))

proportions(table(rec_17$rec_17_antimicrobial_within_1h_strict))

## adherence by onset anchor (isolates the culture -> antibiotic circularity)
rec_17[septic_shock_proxy == FALSE,
       .(within_1h = mean(rec_17_antimicrobial_within_1h, na.rm = TRUE),
         n        = .N),
       by = onset_anchor]


## Some figures for the thesis
## culture-anchored median recognition-to-antimicrobial interval (hours)
antimicrobial_timing[onset_anchor == "culture-anchored" &
                       !is.na(time_to_antimicrobial),
                     median(time_to_antimicrobial)]

antimicrobial_timing[!is.na(time_to_antimicrobial),
                     .(median_h = median(time_to_antimicrobial),
                       n        = .N),
                     by = .(onset_anchor,
                            septic_shock_proxy)]

## share of evaluable stays with first antimicrobial > 8h BEFORE recognition
antimicrobial_timing[!is.na(time_to_antimicrobial),
                     mean(time_to_antimicrobial < -8)]

## same as a count, for the text
antimicrobial_timing[!is.na(time_to_antimicrobial),
                     .(n_early = sum(time_to_antimicrobial < -8),
                       n_total = .N,
                       share   = mean(time_to_antimicrobial < -8))]


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


proportions(table(rec_42$rec_42_bp_monitoring))
  

## 43 RECOMMENDATION -----------------------------------------------------------
## What is the comprehensive list of IV fluids used for resuscitation in sepsis patients?
rec43_label <- ssc_recommendations %>% 
  .[rec_num == 43, rec_label]

## IV fluid resuscitation events
resuscitation_fluid_events <- all_inputevents %>% 
  ## patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>% 
  ## IV resuscitation fluids item IDs
  # .[itemid %in% resuscitation_fluid_itemids$itemid] %>% 
  .[itemid %in% input_fluid_classification[resuscitation_role %in% c("crystalloid",
                                                                     "colloid")]$itemid] %>% 
  ## keep only fluids given for volume resuscitation (main fluid orders / boluses)
  .[ordercategoryname %in% c("02-Fluids (Crystalloids)",
                             "03-IV Fluid Bolus",
                             "04-Fluids (Colloids)") &
      ordercategorydescription %in% c("Continuous IV",
                                      "Bolus")] %>%
  merge(.,
        input_fluid_classification[, .(itemid,
                                       fluid_label = label,
                                       resuscitation_role)],
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, fluid_type := fcase(
    
    resuscitation_role == "crystalloid",
    "Crystalloid",
    
    resuscitation_role == "colloid",
    "Colloid",
    
    default = NA_character_)] %>% 
  .[, .(stay_id,
       fluid_start = starttime,
       fluid_end = endtime,
       fluid_rate = rate,
       rate_unit = rateuom,
       fluid_amount = amount,
       fluid_unit = amountuom,
       fluid_label,
       fluid_type,
       ordercategoryname,
       ordercategorydescription)]

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
  ## initial resuscitation window [onset - 1h, onset + 3h], consistent with rec 44/45
  ## and the SSC "first 3 hours"; the wider +12h window is retained below as a sensitivity
  .[fluid_start >= fluid_resuscitation_start &
      fluid_start <= fluid_resuscitation_end] %>%
  ## sensitivity (wider window, onset + 15h):
  # .[fluid_start >= fluid_resuscitation_start &
  #     fluid_start <= fluid_resuscitation_end + hours(12)] %>%
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


proportions(table(rec_43$rec_43_crystalloid_first_line))
table(rec_43$rec_43_eligibility)
   

## 44 & 45 RECCOMENDATION ------------------------------------------------------
rec44_label <- ssc_recommendations %>% 
  .[rec_num == 44, rec_label]

rec_45_label <- ssc_recommendations %>% 
  .[rec_num == 45, rec_label]

## Resuscitation IV Fluid Intake 
balanced_saline_fluid_events <- all_inputevents %>%
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>%
  ## Resuscitation IV fluid item IDs
  .[itemid %in% input_fluid_classification[resuscitation_role == "crystalloid" |
                                             fluid_type == "Albumin (Colloid)"]$itemid] %>%
  ## keep only fluids given for volume resuscitation, not as drug carriers etc.
  .[ordercategoryname %in% c("02-Fluids (Crystalloids)",
                             "03-IV Fluid Bolus",
                             "04-Fluids (Colloids)") &
      ordercategorydescription %in% c("Continuous IV",
                                      "Bolus")] %>%
  merge(.,
        input_fluid_classification[, .(itemid,
                                       classification = fluid_type)],
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, fluid_type := fcase(
    
    classification == "Balanced crystalloid",
    "Balanced",
    
    classification == "Unbalanced crystalloid (normal saline)",
    "0.9% Saline",
    
    classification == "Albumin (Colloid)",
    "Albumin",
    
    default = NA_character_)] %>% 
  .[, classification := NULL]
                               

## Total IV resuscitation fluids administered within initital resuscitation window
iv_fluids_initial_window <- sepsis_base_cohort_data %>% 
  merge(.,
        balanced_saline_fluid_events,
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
  .[, fluid_ml_initial_window := fcase(
    ## continuous infusion: prorate by overlap with the resuscitation window
    rateuom %in% c("mL/hour", "mL/min"),
    overlap_time * rate,
    ## bolus (amount-based, no rate): count the full amount if given in the window
    is.na(rate) &
      starttime >= fluid_resuscitation_start &
      starttime <= fluid_resuscitation_end,
    amount,
    default = NA_real_)] %>%
  ## bolus-only volume (resuscitation-specific sensitivity for rec 44):
  ## boluses carry the "Bolus" order description; continuous (mostly maintenance) excluded
  .[, fluid_ml_initial_window_bolus := fifelse(ordercategorydescription == "Bolus",
                                               fluid_ml_initial_window,
                                               NA_real_)] %>%
  .[, total_fluid_initial_window := sum(fluid_ml_initial_window,
                               na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id,
           fluid_type)] %>%
  .[, total_fluid_initial_window_bolus := sum(fluid_ml_initial_window_bolus,
                                              na.rm = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id,
           fluid_type)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        fluid_type,
        total_fluid_initial_window,
        total_fluid_initial_window_bolus)] %>%
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

## Sensitivity (rec 44): balanced vs saline using resuscitation BOLUSES only,
## since continuous crystalloid in MIMIC is predominantly maintenance-rate
rec_44_bolus <- iv_fluids_initial_window %>%
  copy() %>%
  .[is.na(fluid_type),
    fluid_type := "no crystalloid"] %>%
  dcast(subject_id +
          hadm_id +
          stay_id  ~ fluid_type,
        value.var = "total_fluid_initial_window_bolus",
        fill = 0) %>%
  .[, rec_44_balanced_over_saline_bolus_causal := fifelse(
    Balanced > 0 &
      Balanced > `0.9% Saline`,
    TRUE,
    fifelse(
      `0.9% Saline` > 0 &
        Balanced <= `0.9% Saline`,
      FALSE,
      NA))] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        rec_44_balanced_over_saline_bolus_causal)]

rec_44 <- merge(rec_44,
                rec_44_bolus,
                by = c("subject_id",
                       "hadm_id",
                       "stay_id"),
                all.x = TRUE)


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


proportions(table(rec_44$rec_44_balanced_over_saline))
proportions(table(rec_44$rec_44_balanced_over_saline_bolus_causal))
proportions(table(rec_45$rec_45_crystalloid_no_albumin))

table(rec_44$rec_44_balanced_over_saline)
table(rec_44$rec_44_balanced_over_saline_bolus_causal)
table(rec_45$rec_45_crystalloid_no_albumin)
 
table(rec_44$rec_44_eligibility)
table(rec_45$rec_45_eligibility)

## 53 & 55 RECOMMENDATION ------------------------------------------------------
### 53 RECOMMENDATION ----------------------------------------------------------
rec53_label <- ssc_recommendations %>% 
  .[rec_num == 53, rec_label]

## adherence to "norepinehprine first among a given comparator set"
## if multiple agents were given first at the same time,
## Norepinephrine is preferred
first_vaso_among <- function(agents_regex) {
  
  sepsis_base_cohort_data %>% 
    .[, .(subject_id,
          hadm_id,
          stay_id,
          septic_shock_proxy,
          shock_vaso_start)] %>% 
    unique() %>% 
    merge(.,
          all_vasopressorevents[grepl(agents_regex,
                                      vaso_label,
                                      ignore.case = TRUE),
                                .(stay_id,
                                  vaso_start,
                                  vaso_label)],
          by = "stay_id",
          all = FALSE) %>% 
    ## shock vaso start is already linked to the septic shock vasopresor
    ## allow 15 minutes delay to capture any remaining documnentation lag
    .[vaso_start >= shock_vaso_start - minutes(15) &
        vaso_start <= shock_vaso_start + hours(12)] %>% 
    .[, is_first := vaso_start == min(vaso_start,
                                         na.rm = TRUE),
      by = .(subject_id,
             hadm_id,
             stay_id)] %>% 
    .[is_first == TRUE,
      .(norepi_first = any(grepl("Norepinephrine",
                                 vaso_label,
                                 ignore.case = TRUE))),
      by = .(subject_id,
             hadm_id,
             stay_id)]
}

## Recommendation 53 requires Norepinephrine over Epinephrine, Dopamine or Selepressin
rec_53_first_vaso <- first_vaso_among("Norepinephrine|Epinephrine|Dopamine|Selepressin")

## Encode recommendation 53
rec_53 <- sepsis_base_cohort_data %>% 
  copy %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time)] %>% 
  unique() %>% 
  merge(.,
        rec_53_first_vaso,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[septic_shock_proxy == TRUE,
    rec_53_first_vaso_norepi := norepi_first] %>% 
  .[, rec_53_eligibility := fcase(
    
    !is.na(rec_53_first_vaso_norepi),
    "eligible",
    
    septic_shock_proxy == TRUE &
      is.na(norepi_first),
    "first-line agent outside comparators",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_)]


## Adherence rates
proportions(table(rec_53$rec_53_eligibility))
table(rec_53$rec_53_eligibility)
proportions(table(rec_53$rec_53_first_vaso_norepi))


### 55 RECOMMENDATION ----------------------------------------------------------
rec55_label <- ssc_recommendations %>% 
  .[rec_num == 55, rec_label]


## Recommendation 55 requires Norepinephrine over Vasopressin and Angiotensin
rec_55_first_vaso <- first_vaso_among("Norepinephrine|Vasopressin|Angiotensin")

## Encode recommendation 55
rec_55 <- sepsis_base_cohort_data %>% 
  copy %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time)] %>% 
  unique() %>% 
  merge(.,
        rec_55_first_vaso,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[septic_shock_proxy == TRUE,
    rec_55_first_vaso_norepi := norepi_first] %>% 
  .[, rec_55_eligibility := fcase(
    
    !is.na(rec_55_first_vaso_norepi),
    "eligible",
    
    septic_shock_proxy == TRUE &
      is.na(norepi_first),
    "first-line agent outside comparators",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_)]


## Adherence rates
proportions(table(rec_55$rec_55_eligibility))
table(rec_55$rec_55_eligibility)
proportions(table(rec_55$rec_55_first_vaso_norepi))


### 53 & 55 CAUSAL -------------------------------------------------------------
## Combine into one recommendation for causal analysis
combined_first <- first_vaso_among(paste("Norepinephrine",
                                         "Epinephrine",
                                         "Dopamine",
                                         "Vasopressin",
                                         "Angiotensin",
                                         "Selepressin",
                                         "Terlipressin",
                                         "Phenylephrine",
                                         sep = "|"))

rec_53_55_causal <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time)] %>% 
  unique() %>% 
  merge(.,
        combined_first,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[septic_shock_proxy == TRUE,
    rec_53_55_norepi_first_line_causal := norepi_first] %>% 
  .[, rec_53_55_eligibility := fcase(
    
    !is.na(rec_53_55_norepi_first_line_causal),
    "eligible",
    
    septic_shock_proxy == TRUE &
      is.na(rec_53_55_norepi_first_line_causal),
    "no vasopressor in window",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown")]

## Adherence rates
proportions(table(rec_53_55_causal$rec_53_55_eligibility))
table(rec_53_55_causal$rec_53_55_eligibility)
proportions(table(rec_53_55_causal$rec_53_55_norepi_first_line_causal))

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
  .[itemid %in% norepi_itemid$itemid] %>%
  .[, norepi_dose_mcg_kg_min := fcase(
    rateuom == "mcg/kg/min", rate,
    rateuom == "mg/kg/min", rate * 1000,
    default = NA_real_
  )] %>%
  ## physiologic ceiling: norepinephrine essentially never exceeds 5 mcg/kg/min,
  ## so higher values are charting errors and are set NA rather than triggering a
  ## false escalation (matches the ceiling used in vasopressor_hourly for rec 89)
  .[norepi_dose_mcg_kg_min > 5,
    norepi_dose_mcg_kg_min := NA_real_] %>%
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
  ## prefer an in-window vasopressin administration over an earlier out-of-window
  ## one, so that any qualifying vasopressin marks the stay adherent
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.integer(vasopressin_after_escalating_norepi),
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
  ## adherence is defined only within the eligible subpopulation (septic shock
  ## on escalating norepinephrine); non-shock stays are set to NA so they are
  ## excluded from the treated/untreated contrast
  .[, rec_56_vasopressin_after_escalating_norepi_causal :=
      fifelse(septic_shock_proxy == TRUE &
                vasopressin_after_escalating_norepi == TRUE,
             TRUE,
             fifelse(
               septic_shock_proxy == TRUE &
                 vasopressin_after_escalating_norepi == FALSE,
               FALSE,
               NA
             ))] %>%
  .[, rec_56_eligibility := fcase(
    septic_shock_proxy == TRUE &
    !is.na(rec_56_vasopressin_after_escalating_norepi_causal),
    "eligible",
    
    septic_shock_proxy == TRUE &
      (is.na(norepi_escalating_dose) |
      norepi_escalating_dose == FALSE),
    "no escalated dose of Norepinephrine",
    
    septic_shock_proxy == FALSE | is.na(septic_shock_proxy),
    "no septic shock or unknown"
  )] %>% 
  ## Decision-point confounder: the norepinephrine infusion rate at the escalation
  ## that triggers the vasopressin decision, measured before that decision. The
  ## baseline covariates capture severity at sepsis onset but not how high the
  ## norepinephrine requirement had risen by the escalation point.
  .[, cf_rec56_norepi_rate_escalation := norepi_dose_mcg_kg_min]

## Additional decision-point confounders for rec 56: lactate and MAP at the
## escalation point, the hemodynamic depth of shock driving the vasopressin
## decision, taken as the most recent value in the 24h before escalation.
rec56_anchor <- rec_56[!is.na(norepi_first_escalated),
                       .(subject_id,
                         hadm_id,
                         stay_id,
                         norepi_first_escalated)]

rec56_pre_lactate <- rec56_anchor %>%
  merge(.,
        lactate_events[, .(subject_id,
                           hadm_id,
                           lac_time = charttime,
                           lactate = valuenum)],
        by = c("subject_id",
               "hadm_id"),
        allow.cartesian = TRUE) %>%
  .[lac_time <= norepi_first_escalated &
      lac_time >= norepi_first_escalated - hours(24)] %>%
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.numeric(lac_time)),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        cf_rec56_lactate = lactate)]

rec56_pre_map <- rec56_anchor %>%
  merge(.,
        map_events[, .(stay_id,
                       map_time = charttime,
                       map = valuenum)],
        by = "stay_id",
        allow.cartesian = TRUE) %>%
  .[map_time <= norepi_first_escalated &
      map_time >= norepi_first_escalated - hours(24)] %>%
  .[order(stay_id,
          -as.numeric(map_time)),
    .SD[1],
    by = stay_id] %>%
  .[, .(stay_id,
        cf_rec56_map = map)]

rec_56 <- rec_56 %>%
  merge(.,
        rec56_pre_lactate,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  merge(.,
        rec56_pre_map,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE)


## Adherence rates
proportions(table(rec_56$rec_56_eligibility))
table(rec_56$rec_56_eligibility)
proportions(table(rec_56$rec_56_vasopressin_after_escalating_norepi_causal))

summary(rec_56$norepi_dose_mcg_kg_min)
summary(rec_56$cf_rec56_norepi_rate_escalation)
summary(rec_56$cf_rec56_lactate)
summary(rec_56$cf_rec56_map)

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
  .[itemid %in% norepi_itemid$itemid] %>%
  .[, norepi_dose_mcg_kg_min := fcase(
    rateuom == "mcg/kg/min", rate,
    rateuom == "mg/kg/min", rate * 1000,
    default = NA_real_
  )] %>%
  ## physiologic ceiling: norepinephrine essentially never exceeds 5 mcg/kg/min,
  ## so higher values are charting errors and are set NA rather than triggering a
  ## false escalation (matches the ceiling used in vasopressor_hourly for rec 89)
  .[norepi_dose_mcg_kg_min > 5,
    norepi_dose_mcg_kg_min := NA_real_] %>%
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
  ## a missing trigger time means no vasopressin was on board, so the patient
  ## never met the eligibility ladder; the flag is left NA (ineligible) rather
  ## than FALSE (eligible but untreated)
  .[, epi_after_escalating_norepi_vaso := fcase(
    is.na(rec_57_trigger_time),
    NA,

    !is.na(epi_start) &
      ## account for delayed documentation, at most 6h after the trigger
      epi_start >= rec_57_trigger_time - hours(1) &
      epi_start <= rec_57_trigger_time + hours(6),
    TRUE,

    default = FALSE)] %>%
  ## prefer an in-window epinephrine administration over an out-of-window one,
  ## so that any qualifying epinephrine marks the stay adherent
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.integer(epi_after_escalating_norepi_vaso),
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
  ## adherence is defined only within the eligible subpopulation (septic shock
  ## on escalating norepinephrine with vasopressin on board); other stays are NA
  .[, rec_57_epi_after_escalating_norepi_vaso :=
      fifelse(septic_shock_proxy == TRUE &
                epi_after_escalating_norepi_vaso == TRUE,
              TRUE,
              fifelse(
                septic_shock_proxy == TRUE &
                  epi_after_escalating_norepi_vaso == FALSE,
                FALSE,
                NA
              ))] %>%
  .[, rec_57_eligibility := fcase(
    septic_shock_proxy == TRUE &
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


proportions(table(rec_57$rec_57_epi_after_escalating_norepi_vaso))
table(rec_57$rec_57_eligibility)
proportions(table(rec_57$rec_57_eligibility))

summary(rec_57$norepi_dose_mcg_kg_min)

## 67, 68, 69 RECOMMENDATIONS --------------------------------------------------
## Define acute hypoxemic respiratory failure (AHRF)

### Respiratory Support --------------------------------------------------------
## Patients on AHRF receive supplementary oxygen
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


### FiO2 -----------------------------------------------------------------------
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
  .[itemid %in% fio2_itemid$itemid] %>% 
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

## Remove obvious outliers based on distribution
fio2_events <- fio2_events %>% 
  .[fio2 >= 20 &
      fio2 <= 150]

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


### PaO2 -----------------------------------------------------------------------
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
  .[itemid %in% pao2_itemid$itemid] %>% 
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


### SpO2 -----------------------------------------------------------------------
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


### P/F & S/F ratios ------------------------------------------------------------------
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


### AHRF Proxy -----------------------------------------------------------------
## Combine results into AHRF proxy definition
ahrf_proxy <- resp_support_events %>% 
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
      (pf_ratio < 200 |
         (spo2 <= 97 &
            sf_ratio < 235)) &
      oxygen_therapy == TRUE,
    TRUE,
    
    default = FALSE)]
  # ## ARDS timing criteria: within 7 days after sepsis onset
  # .[, ards_timing :=
  #     ## acount for documentation delay
  #     fio2_time >= suspected_infection_time - hours(1) &
  #     fio2_time <= suspected_infection_time + days(7)] %>% 
  # ## Global ARDS hypoxemia criterion
  # .[, ards_hypoxemia := 
  #     fcase(
  #       
  #       !is.na(pf_ratio) & pf_ratio <= 300,
  #       TRUE,
  #       
  #       is.na(pf_ratio) &
  #         !is.na(sf_ratio) &
  #         spo2 <= 97 &
  #         sf_ratio <= 315,
  #       TRUE,
  #       
  #       !is.na(pf_ratio) & pf_ratio > 300,
  #       FALSE,
  #       
  #       is.na(pf_ratio) &
  #         !is.na(sf_ratio) &
  #         (spo2 > 97 |
  #            sf_ratio > 315),
  #       FALSE,
  #       
  #       default = NA)] %>% 
  # ## strict ARDS respiratory support criterion
  # .[, ards_resp_support_strict := fcase(
  #   
  #   resp_support_type %in% c("NIV",
  #                            "Invasive ventilation"),
  #   TRUE,
  #   
  #   resp_support_type == "HFNC" &
  #     flow_label == "O2 Flow" &
  #     flow_rate >= 30 &
  #     flow_time_diff_min <= 30,
  #   TRUE,
  #   
  #   !is.na(resp_support_type),
  #   FALSE,
  #   
  #   default = NA)] %>% 
  # ## ARDS respiratory support criterion
  # .[, ards_resp_support_proxy := resp_support_type %in% c(
  #   "HFNC",
  #   "NIV",
  #   "Invasive ventilation"
  # )] %>% 
  # ## Structured ARDS proxy without imaging
  # .[, ards_global_structured_proxy := fcase(
  #   
  #   ards_timing == TRUE &
  #     ards_hypoxemia == TRUE &
  #     ards_resp_support_strict == TRUE,
  #   TRUE,
  #   
  #   !is.na(ards_timing) &
  #     !is.na(ards_hypoxemia) &
  #     !is.na(ards_resp_support_strict) &
  #     (ards_timing == FALSE |
  #        ards_hypoxemia == FALSE |
  #        ards_resp_support_strict == FALSE),
  #   FALSE,
  #   
  #   default = NA)]


## First AHRF event per stay ID
ahrf_stay <- ahrf_proxy %>% 
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
        ahrf_first_resp_support_type = resp_support_type)] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = TRUE) %>% 
  .[is.na(ahrf_event), ahrf_event := FALSE]


## Proportion of AHRF events in patients
table(ahrf_stay$ahrf_event)
proportions(table(ahrf_stay$ahrf_event))


### 67 RECOMMENDATION ----------------------------------------------------------
rec67_label <- ssc_recommendations %>% 
  .[rec_num == 67, rec_label]

## AHRF-positive stays with their non-invasive 
## respiratory support over a longer window
ahrf_resp <- ahrf_stay %>% 
  copy() %>% 
  .[ahrf_event == TRUE, .(subject_id,
                          hadm_id,
                          stay_id,
                          ahrf_onset_time)] %>% 
  merge(.,
        resp_support_events[, .(stay_id,
                                resp_support_time,
                                resp_support_type)] %>% 
          unique(),
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>% 
  ## same tolerance as FiO2 pairing
  .[resp_support_time >= ahrf_onset_time - hours(2) &
      resp_support_time <= ahrf_onset_time + hours(72)]

## Check for any duplicate first oxygen support devices
ahrf_resp[order(stay_id, resp_support_time),
               .(n_types_at_first =
                   uniqueN(resp_support_type[resp_support_time == min(resp_support_time)])),
               by = stay_id][n_types_at_first > 1, .N]

ahrf_resp[order(stay_id, resp_support_time),
               .(combo = paste(sort(unique(resp_support_type[resp_support_time == min(resp_support_time)])),
                               collapse = " + ")),
               by = stay_id][grepl("\\+", combo), .N, by = combo][order(-N)]

## clinical-intensity ranking to break ties when several device types are
## charted at the same first timestamp (most intensive support wins)
oxygen_intensity <- c("Invasive ventilation",
                      "NIV",
                      "HFNC",
                      "Conventional oxygen",
                      "Other oxygen",
                      "None",
                      "Other")

ahrf_resp <- ahrf_resp %>%
  .[, type_rank := match(resp_support_type,
                         oxygen_intensity)]


oxygen_therapy_summary <- ahrf_resp %>% 
  copy() %>% 
  ## tie-break on intensity so the first device is the most intensive support
  ## charted at the earliest in-window time, not an arbitrary row
  .[order(subject_id,
          hadm_id,
          stay_id,
          resp_support_time,
          type_rank),
    .(first_resp_type = resp_support_type[1],
      first_resp_time = resp_support_time[1],
      used_hfnc = any(resp_support_type == "HFNC"),
      used_niv = any(resp_support_type == "NIV"),
      used_conventional = any(resp_support_type == "Conventional oxygen")),
    by = .(subject_id,
           hadm_id,
           stay_id)]


rec_67 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id)] %>% 
  merge(.,
        oxygen_therapy_summary[, .(subject_id,
                                   hadm_id,
                                   stay_id,
                                   first_resp_type,
                                   used_hfnc,
                                   used_conventional)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, rec_67_hfnc_over_conventional := fcase(
    
    used_hfnc == TRUE,
    TRUE,
    
    used_hfnc == FALSE &
      used_conventional == TRUE,
    FALSE,
    
    default = NA)] %>% 
  .[, rec_67_eligibility := fcase(
    
    !is.na(rec_67_hfnc_over_conventional),
    "eligible",
    
    !is.na(first_resp_type),
    "AHRF, neither HFNC nor conventional",
    
    default = "no AHRF event")]


proportions(table(rec_67$rec_67_hfnc_over_conventional))
table(rec_67$rec_67_eligibility)

### 68 Recommendation ----------------------------------------------------------
re68_label <- ssc_recommendations %>% 
  .[rec_num == 68, rec_label]


rec_68 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id)] %>% 
  merge(.,
        oxygen_therapy_summary[, .(subject_id,
                                   hadm_id,
                                   stay_id,
                                   first_resp_type)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, rec_68_first_oxygen_hfnc_causal := fcase(
    
    first_resp_type == "HFNC",
    TRUE,
    
    first_resp_type == "NIV",
    FALSE,
    
    default = NA)] %>% 
  .[, rec_68_eligibility := fcase(
    
    !is.na(rec_68_first_oxygen_hfnc_causal),
    "eligible",
    
    first_resp_type %in% c("Invasive ventilation",
                           "Conventional oxygen",
                           "Other oxygen",
                           "None",
                           "Other"),
    "AHRF, first device not HFNC or NIV",
    
    default = "no AHRF event")]  
                       

proportions(table(rec_68$rec_68_first_oxygen_hfnc_causal))
table(rec_68$rec_68_eligibility)


### 69 Recommendation ----------------------------------------------------------
rec69_label <- ssc_recommendations %>% 
  .[rec_num == 69, rec_label]


rec_69 <- sepsis_base_cohort_data %>% 
  copy() %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id)] %>% 
  merge(.,
        oxygen_therapy_summary[, .(subject_id,
                                   hadm_id,
                                   stay_id,
                                   first_resp_type,
                                   used_hfnc,
                                   used_niv)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, rec_69_hfnc_over_alternating := fcase(
    
    used_hfnc == TRUE &
      used_niv == FALSE,
    TRUE,
    
    used_hfnc == TRUE &
      used_niv == TRUE,
    FALSE,
    
    default = NA)] %>% 
  .[, rec_69_eligibility := fcase(
    
    !is.na(rec_69_hfnc_over_alternating),
    "eligible",
    
    used_hfnc == FALSE &
      !is.na(first_resp_type),
    "AHRF, HFNC not used",
    
    default = "no AHRF event")]   


proportions(table(rec_69$rec_69_eligibility))
table(rec_69$rec_69_hfnc_over_alternating)


## 71 RECOMMENDATION -----------------------------------------------------------
rec71_label <- ssc_recommendations %>% 
  .[rec_num == 71, rec_label]

## Invasive ventilation events
invasive_vent_events <- resp_support_events %>% 
  copy() %>% 
  .[resp_support_type == "Invasive ventilation",
    .(stay_id,
      vent_time = resp_support_time)] %>% 
  unique()

invasive_vent_start <- invasive_vent_events %>% 
  copy() %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = "stay_id",
        all = FALSE) %>% 
  .[vent_time >= suspected_infection_time - hours(1) &
      vent_time <= suspected_infection_time + days(7)] %>% 
  .[order(stay_id,
          vent_time),
    .(vent_start = vent_time[1]),
    by = .(subject_id,
           hadm_id,
           stay_id)]

## ARDS proxy by oxygenation only: invasively ventilated with PaO2/FiO2 <= 300 in
## the acute window. Bilateral opacities and the cardiac exclusion of the Berlin
## definition cannot be assessed in MIMIC-IV, so this over-captures and is used as
## a documented proxy for ARDS.
ards_oxygenation <- pf_sf_ratios %>% 
  copy() %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)] %>% 
          unique(),
        by = "stay_id",
        all = FALSE) %>% 
  .[stay_id %in% invasive_vent_events$stay_id] %>% 
  .[fio2_time >= suspected_infection_time -  hours(1) &
      fio2_time <= suspected_infection_time + days(7) &
      !is.na(pf_ratio) &
      pf_ratio <= 300] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          fio2_time),
    .(ards_onset_time = fio2_time[1],
      ards_first_pf_ratio = pf_ratio[1]),
    by = .(subject_id,
           hadm_id,
           stay_id)]


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
  .[itemid %in% tidal_volume_itemids$itemid] %>% 
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
    tidal_vol_per_kg <= 8, "Low tidal volume",
    tidal_vol_per_kg > 10, "High tidal volume",
    tidal_vol_per_kg > 8 & tidal_vol_per_kg <= 10, "Intermediate",
    default = NA_character_
  )]

## First set tidal volume within 24h of the ARDS proxy onset,
## preferring an in-window reading
first_tidal_volume <- tidal_volume_events %>% 
  copy() %>% 
  merge(.,
        invasive_vent_start,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>% 
  .[, in_window := chart_hour >= vent_start - hours(1) &
      chart_hour <= vent_start + hours(24)] %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.numeric(in_window),
          chart_hour),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[in_window == TRUE,
    .(subject_id,
      hadm_id,
      stay_id,
      first_tidal_volume_time = chart_hour,
      tidal_volume_ml,
      pbw_kg,
      tidal_vol_per_kg,
      tidal_volume_strategy)]


## Recommendation 71: low tidal volume strategy (<= 8 mL/kg IBW, targeting 6 per
## the ARDSnet protocol) over a high tidal volume strategy (> 10 mL/kg IBW).
## The intermediate band (8 to 10) is neither arm of the guideline contrast and is
## left out of the binary adherence measure.
rec_71 <- ards_oxygenation %>% 
  copy() %>% 
  merge(.,
        first_tidal_volume,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>% 
  .[, rec_71_low_tidal_volume := fcase(
    
    tidal_vol_per_kg <= 8,
    TRUE,
    
    tidal_vol_per_kg > 10,
    FALSE,
    
    default = NA)] %>% 
  .[, rec_71_eligibility := fcase(
    
    !is.na(rec_71_low_tidal_volume),
    "eligible",
    
    !is.na(tidal_vol_per_kg),
    "ARDS proxy, intermediate tidal volume 8 to 10",
    
    default = "ARDS proxy, no tidal volume in window")]


table(rec_71$rec_71_eligibility)
proportions(table(rec_71$rec_71_low_tidal_volume))



## 73 RECOMMENDATION -----------------------------------------------------------
rec73_label <- ssc_recommendations %>%
  .[rec_num == 73, rec_label]

## Item ID for plateau pressure
plateau_pressure_itemids <- chart_itemids %>%
  .[grepl("plateau",
          label,
          ignore.case = TRUE)]

## Extract plateau pressure (cm H2O), averaged per hour
plateau_pressure_events <- all_chartevents %>%
  .[itemid %in% plateau_pressure_itemids$itemid] %>%
  .[!is.na(valuenum) &
      valuenum > 0 &
      ## conservative upper bound that removes obvious outliers
      valuenum <= 70] %>%
  .[, chart_hour := lubridate::floor_date(charttime,
                                          "hour")] %>%
  .[, .(plateau_pressure = mean(valuenum,
                                na.rm = TRUE)),
    by = .(stay_id,
           chart_hour)]

## Maximum plateau pressure within 24h of the invasive ventilation start.
## The recommendation is an upper limit goal, so a patient is counted adherent
## only if the plateau pressure never exceeds 30 cm H2O across the window, rather
## than at a single reading.
window_plateau_pressure <- plateau_pressure_events %>%
  copy() %>%
  merge(.,
        invasive_vent_start,
        by = "stay_id",
        all = FALSE) %>%
  .[chart_hour >= vent_start - hours(1) &
      chart_hour <= vent_start + hours(24)] %>%
  .[, .(max_plateau_pressure = max(plateau_pressure),
        first_plateau_pressure_time = min(chart_hour)),
    by = .(subject_id,
           hadm_id,
           stay_id)]


## Recommendation 73: upper limit goal for plateau pressure of 30 cm H2O over
## higher plateau pressures. Plateau pressure is measurable only under invasive
## ventilation, so the eligible set is the ARDS proxy, as for recommendation 71.
rec_73 <- ards_oxygenation %>%
  copy() %>%
  merge(.,
        window_plateau_pressure,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, rec_73_plateau_pressure_limited := fcase(

    max_plateau_pressure <= 30,
    TRUE,

    max_plateau_pressure > 30,
    FALSE,

    default = NA)] %>%
  .[, rec_73_eligibility := fcase(

    !is.na(rec_73_plateau_pressure_limited),
    "eligible",

    default = "ARDS proxy, no plateau pressure in window")]


table(rec_73$rec_73_eligibility)
proportions(table(rec_73$rec_73_plateau_pressure_limited))



# ## 74 RECOMMENDATION -----------------------------------------------------------
# rec74_label <- ssc_recommendations %>%
#   .[rec_num == 74, rec_label]
# 
# ## Item IDs for the set PEEP and the inspired oxygen fraction
# peep_itemids <- chart_itemids %>%
#   .[grepl("peep",
#           label,
#           ignore.case = TRUE) &
#       grepl("set",
#             label,
#             ignore.case = TRUE)]
# 
# fio2_itemids <- chart_itemids %>%
#   .[grepl("inspired o2 fraction",
#           label,
#           ignore.case = TRUE)]
# 
# ## Set PEEP (cm H2O), averaged per hour
# peep_events <- all_chartevents %>%
#   .[itemid %in% peep_itemids$itemid] %>%
#   .[!is.na(valuenum) &
#       valuenum >= 0 &
#       ## conservative upper bound that removes obvious outliers
#       valuenum <= 40] %>%
#   .[, chart_hour := lubridate::floor_date(charttime,
#                                           "hour")] %>%
#   .[, .(peep = mean(valuenum,
#                     na.rm = TRUE)),
#     by = .(stay_id,
#            chart_hour)]
# 
# ## FiO2 as a fraction, averaged per hour. The source records a percentage, so it
# ## is divided by 100 and restricted to the physiological 21 to 100 range
# fio2_events <- all_chartevents %>%
#   .[itemid %in% fio2_itemids$itemid] %>%
#   .[!is.na(valuenum) &
#       valuenum >= 21 &
#       valuenum <= 100] %>%
#   .[, chart_hour := lubridate::floor_date(charttime,
#                                           "hour")] %>%
#   .[, .(fio2 = mean(valuenum,
#                     na.rm = TRUE) / 100),
#     by = .(stay_id,
#            chart_hour)]
# 
# ## PEEP and FiO2 paired within the same hour, with the minimum PEEP of the
# ## ARDSnet higher-PEEP strategy for that FiO2. The step function uses the lower
# ## bounds of the ALVEOLI higher-PEEP, lower-FiO2 table as a reproducible proxy for
# ## what counts as "higher PEEP", since the recommendation gives no threshold.
# peep_fio2_events <- merge(peep_events,
#                           fio2_events,
#                           by = c("stay_id",
#                                  "chart_hour"),
#                           all = FALSE) %>%
#   .[, required_high_peep := fcase(
#     fio2 <= 0.30, 12,
#     fio2 <= 0.40, 14,
#     fio2 <= 0.50, 16,
#     fio2 <= 0.70, 18,
#     fio2 <= 0.80, 20,
#     fio2 <= 1.00, 22,
#     default = NA_real_)] %>%
#   .[, higher_peep := peep >= required_high_peep]
# 
# ## First paired PEEP and FiO2 reading within 24h of invasive ventilation start,
# ## preferring an in-window reading
# first_peep_strategy <- peep_fio2_events %>%
#   copy() %>%
#   merge(.,
#         invasive_vent_start,
#         by = "stay_id",
#         all = FALSE) %>%
#   .[, in_window := chart_hour >= vent_start - hours(1) &
#       chart_hour <= vent_start + hours(24)] %>%
#   .[order(subject_id,
#           hadm_id,
#           stay_id,
#           -as.numeric(in_window),
#           chart_hour),
#     .SD[1],
#     by = .(subject_id,
#            hadm_id,
#            stay_id)] %>%
#   .[in_window == TRUE,
#     .(subject_id,
#       hadm_id,
#       stay_id,
#       first_peep_time = chart_hour,
#       peep,
#       fio2,
#       required_high_peep,
#       higher_peep)]
# 
# ## Recommendation 74: higher PEEP over lower PEEP for moderate to severe ARDS
# ## (PaO2/FiO2 <= 200). Descriptive only: "higher versus lower" has no guideline
# ## threshold, so adherence is scored against an imposed higher-PEEP table and the
# ## recommendation is not carried into the causal analysis.
# rec_74 <- ards_oxygenation %>%
#   copy() %>%
#   .[ards_first_pf_ratio <= 200] %>%
#   merge(.,
#         first_peep_strategy,
#         by = c("subject_id",
#                "hadm_id",
#                "stay_id"),
#         all.x = TRUE,
#         all.y = FALSE) %>%
#   .[, rec_74_higher_peep := higher_peep] %>%
#   .[, rec_74_eligibility := fcase(
# 
#     !is.na(rec_74_higher_peep),
#     "eligible",
# 
#     default = "moderate-severe ARDS, no PEEP/FiO2 pair in window")]
# 
# 
# table(rec_74$rec_74_eligibility)
# proportions(table(rec_74$rec_74_higher_peep))


## 79 RECOMMENDATION -----------------------------------------------------------
rec79_label <- ssc_recommendations %>% 
  .[rec_num == 79, rec_label]

## Corticosteroid administration
## Prednisone & prednisolone are oral corticosteroids --> not IV
## Acetate steroids are not administered intravenously
corticosteroid_events <- corticosteroid_emar_icu %>% 
  .[grepl("Administered|Started|Confirmed", 
         event_txt,
         ignore.case = TRUE) & 
     !grepl("Not Given|Hold Dose|Stopped", 
            event_txt,
            ignore.case = TRUE)] %>% 
  ## drop confirmed enteral routes
  ## IV records carry NA route in this source
  .[is.na(route) |
      route %notin% c("PO", "NG")] %>%
  ## IV confirmed by formulation: sodium succinate salts injectable only
  .[grepl("succ",
          medication,
          ignore.case = TRUE) |
      administration_type == "IV Infusion"] %>% 
  .[, medication := trimws(tolower(medication))] %>% 
  .[, corticosteroid_type := fcase(
    
    grepl("hydrocortisone na succ|hydrocortisone sodium succ",
          medication),
    "Hydrocortisone sodium succinate",
    
    grepl("methylprednisolone sodium succ",
          medication),
          "Methylprednisolone sodium succinate",
    
    default = medication)] %>% 
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
  ## IV corticosteroid given at or after septic shock onset (one-sided lower
  ## bound with a 1h tolerance for charting delay; no upper bound, since in
  ## refractory shock steroids may be started well beyond the first 24h)
  .[, received_iv_corticosteroid :=
      !is.na(steroid_time) &
      !is.na(septic_shock_onset_time) &
      steroid_time >= septic_shock_onset_time - hours(1)] %>%
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.integer(received_iv_corticosteroid),
          steroid_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, rec_79_iv_corticosteroids := fcase(
    septic_shock_proxy == TRUE &
      received_iv_corticosteroid == TRUE,
    TRUE,

    septic_shock_proxy == TRUE &
      received_iv_corticosteroid == FALSE,
    FALSE,

    default = NA)] %>%
  .[, rec_79_eligibility := fcase(
    
    !is.na(rec_79_iv_corticosteroids),
    "eligible",
    
    septic_shock_proxy == FALSE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",
    
    default = NA_character_)]


## Adherence rates
table(rec_79$rec_79_eligibility)
proportions(table(rec_79$rec_79_iv_corticosteroids))
rec_79[rec_79_iv_corticosteroids == TRUE, .N, by = corticosteroid_type][order(-N)]


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
  ## keep genuine boluses/volume expansion (>= 250 mL) and drop gross per-record
  ## errors (a single administration above 10 L is a data-entry error, as with the
  ## 20,000 kg weights), before summing per hour
  .[amount >= 250 & amount <= 10000] %>%
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
  ## normalize mislabeled epinephrine so it is not dropped by the fcase below
  .[vaso_label == "Epinephrine.", vaso_label := "Epinephrine"] %>%
  ## potency factors applied only to weight-based rates (mcg/kg/min, or mg/kg/min
  ## scaled to mcg). Non-weight-based rows (mcg/min, missing unit) are left NA and
  ## dropped below rather than mis-scaled; they are a negligible handful.
  .[, norepi_equiv_rate := fcase(
    vaso_label == "Norepinephrine" & vaso_rate_unit == "mcg/kg/min",
    vaso_rate,
    vaso_label == "Norepinephrine" & vaso_rate_unit == "mg/kg/min",
    vaso_rate * 1000,

    vaso_label == "Epinephrine" & vaso_rate_unit == "mcg/kg/min",
    vaso_rate,

    grepl("Phenylephrine",
          vaso_label,
          ignore.case = TRUE) &
      vaso_rate_unit == "mcg/kg/min",
    vaso_rate / 10,

    vaso_label == "Dopamine" & vaso_rate_unit == "mcg/kg/min",
    vaso_rate / 100,

    ## Goradia et al. conversion: vasopressin in units/min contributes
    ## 2.5x to the norepinephrine equivalent (0.04 units/min ~= 0.1 mcg/kg/min),
    ## so units/hour is first divided by 60 and then scaled by 2.5
    vaso_label == "Vasopressin" & vaso_rate_unit == "units/hour",
    (vaso_rate / 60) * 2.5,

    ## Angiotensin is not reliably convertible to NE-equivalent
    default = NA_real_
  )] %>%
  ## physiologic ceiling: a single agent's norepinephrine-equivalent essentially
  ## never exceeds 5 mcg/kg/min, so higher values are charting errors (misplaced
  ## decimal or mislabeled unit) and are dropped before the hourly aggregation
  .[!is.na(norepi_equiv_rate) & norepi_equiv_rate > 0 & norepi_equiv_rate <= 5] %>%
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
  ## collapse multiple titration rows of the same agent within an hour to one rate
  ## (the max for that hour) before combining agents, so a single infusion's many
  ## charted rows are not summed with themselves
  .[, .(agent_rate = max(norepi_equiv_rate, na.rm = TRUE)),
    by = .(stay_id, chart_hour, vaso_label)] %>%
  ## sum concurrent agents' norepinephrine-equivalents per hour (combined burden,
  ## not the single highest agent) so combination vasopressors count as high dose
  .[, .(
    vasopressor_rate = sum(agent_rate, na.rm = TRUE)
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
  .[, end_hour := pmin(start_hour + lubridate::hours(7 * 24),
                       lubridate::floor_date(icu_outtime, "hour"),
                       na.rm = TRUE)] %>% 
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

## Decision-point confounders for recommendation 89, summarizing the acute
## resuscitation course up to the stabilization point, before the fluid-removal
## decision. The baseline covariates describe severity at sepsis onset but not how
## intense and prolonged the resuscitation had been by the time of stabilization,
## which drives both whether fluid is then removed and the outcome.
rec89_decision_confounders <- acute_resuscitation_hours %>%
  merge(.,
        post_resuscitation_time,
        by = "stay_id",
        all = FALSE) %>%
  .[chart_hour <= post_resuscitation_time] %>%
  .[, .(cf_rec89_peak_vasopressor = max(vasopressor_rate,
                                        na.rm = TRUE),
        cf_rec89_cum_fluid_bolus = sum(fluid_bolus_ml,
                                       na.rm = TRUE),
        cf_rec89_hours_to_stabilization =
          as.numeric(difftime(post_resuscitation_time[1],
                              septic_shock_onset_time[1],
                              units = "hours"))),
    by = stay_id]

## Recommendation 89
## First active fluid removal in [post_resuscitation, +48h]. Built from
## post_resuscitation_time, so only stays that reached the post-resuscitation
## phase can appear; a stay with removal only outside the window or before
## stabilization does not.
fluid_removal_in_window <- post_resuscitation_time %>%
  merge(.,
        active_fluid_removal_events,
        by = "stay_id",
        all.x = FALSE,
        all.y = FALSE) %>%
  .[fluid_removal_time >= post_resuscitation_time &
      fluid_removal_time <= post_resuscitation_time + hours(48)] %>%
  .[order(stay_id,
          fluid_removal_time),
    .SD[1],
    by = stay_id] %>%
  .[, .(stay_id,
        first_fluid_removal_time = fluid_removal_time,
        first_fluid_removal_detail = fluid_removal_detail)]

## SENSITIVITY: wider window of 7 days after post-resuscitation (vs the 48h primary),
## matching the 7-day acute-resuscitation horizon, to test robustness to the window
fluid_removal_in_window_7d <- post_resuscitation_time %>%
  merge(.,
        active_fluid_removal_events,
        by = "stay_id",
        all.x = FALSE,
        all.y = FALSE) %>%
  .[fluid_removal_time >= post_resuscitation_time &
      fluid_removal_time <= post_resuscitation_time + hours(7 * 24)] %>%
  .[order(stay_id,
          fluid_removal_time),
    .SD[1],
    by = stay_id] %>%
  .[, .(stay_id,
        first_fluid_removal_time_7d = fluid_removal_time)]

rec_89 <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_proxy,
        septic_shock_onset_time)] %>%
  unique() %>%
  merge(.,
        post_resuscitation_time,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>%
  merge(.,
        fluid_removal_in_window,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>%
  merge(.,
        fluid_removal_in_window_7d,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>%
  merge(.,
        rec89_decision_confounders,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>%
  ## adherence is defined only for shock patients who reached the
  ## post-resuscitation phase; never-stabilized patients are non-evaluable (NA)
  .[septic_shock_proxy == TRUE &
      !is.na(post_resuscitation_time),
    rec_89_active_fluid_removal := !is.na(first_fluid_removal_time)] %>%
  ## SENSITIVITY: active fluid removal within 7 days of post-resuscitation
  .[septic_shock_proxy == TRUE &
      !is.na(post_resuscitation_time),
    rec_89_active_fluid_removal_7d_causal := !is.na(first_fluid_removal_time_7d)] %>%
  .[, rec_89_eligibility := fcase(

    septic_shock_proxy != TRUE |
      is.na(septic_shock_proxy),
    "no septic shock or unknown",

    is.na(post_resuscitation_time),
    "did not reach post-resuscitation phase",

    !is.na(rec_89_active_fluid_removal_7d_causal),
    "eligible",

    default = NA_character_)]


## Adherence rates
proportions(table(rec_89$rec_89_eligibility))
table(rec_89$rec_89_eligibility)
proportions(table(rec_89$rec_89_active_fluid_removal))
proportions(table(rec_89$rec_89_active_fluid_removal_7d_causal))

summary(rec_89$cf_rec89_cum_fluid_bolus)
summary(rec_89$cf_rec89_peak_vasopressor)
summary(rec_89$cf_rec89_hours_to_stabilization)

## 90 RECOMMENDATION -----------------------------------------------------------
rec90_label <- ssc_recommendations %>% 
  .[rec_num == 90, rec_label]

## Red blood cell transfusion item IDs
rbc_transfusion_itemids <- d_items %>% 
  .[grepl("packed red blood cells|packed red cells",
          label,
          ignore.case = TRUE)]

## RBC transfusions (keep subject/hadm for labevents linkage)
rbc_transfusion_events <- all_inputevents %>%
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>%
  ## RBC transfusion item IDs
  .[itemid %in% rbc_transfusion_itemids$itemid] %>%
  .[!is.na(amount) & amount > 0] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        transfusion_time = starttime,
        transfused_ml = amount)]

## Hemoglobin from labevents (the laboratory value that triggers transfusion;
## hospital-wide, so it also covers draws before ICU admission)
hemoglobin_itemid <- lab_itemids %>%
  .[label == "Hemoglobin" &
      grepl("blood",
            fluid,
            ignore.case = TRUE)]

## Hemoglobin measurements
hemoglobin_events <- all_labevents %>%
  ## hemoglobin lab item IDs
  .[itemid %in% hemoglobin_itemid$itemid] %>%
  .[!is.na(valuenum) &
      valuenum >= 3 &
      valuenum <= 20] %>%
  .[, .(subject_id,
        hadm_id,
        hb_time = charttime,
        hemoglobin = valuenum)]

## Most recent hemoglobin in [transfusion - 24h, transfusion] per transfusion
pre_hb <- rbc_transfusion_events %>%
  merge(.,
        hemoglobin_events,
        by = c("subject_id",
               "hadm_id"),
        allow.cartesian = TRUE) %>%
  .[hb_time <= transfusion_time &
      hb_time >= transfusion_time - hours(24)] %>%
  .[order(subject_id,
          hadm_id,
          stay_id,
          transfusion_time,
          -as.numeric(hb_time))] %>%
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id,
           transfusion_time)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        transfusion_time,
        hemoglobin)]

## Attach the pre-transfusion hemoglobin back to ALL transfusions, so a transfusion
## with no qualifying pre-transfusion hemoglobin is retained (trigger unknown)
## rather than dropped and the patient mislabeled as never transfused
pre_transfusion_hb <- rbc_transfusion_events %>%
  merge(.,
        pre_hb,
        by = c("subject_id",
               "hadm_id",
               "stay_id",
               "transfusion_time"),
        all.x = TRUE,
        all.y = FALSE)

## Classify transfusion threshold
rec_90 <- sepsis_base_cohort_data %>%
  merge(.,
        pre_transfusion_hb,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
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
  .[, rec_90_restrictive_transfusion_causal := fcase(
    transfusion_after_sepsis == TRUE &
      transfusion_strategy == "Restrictive",
    TRUE,

    transfusion_after_sepsis == TRUE &
      transfusion_strategy %in% c("Liberal",
                                  "Intermediate"),
    FALSE,

    default = NA)] %>%
  ## Sensitivity: restrictive threshold of <8 g/dL (vs <7 primary), the looser
  ## cut some guidelines allow for cardiac or elderly patients
  .[, rec_90_restrictive_transfusion_8 := fcase(
    transfusion_after_sepsis == TRUE &
      !is.na(hemoglobin) &
      hemoglobin < 8,
    TRUE,

    transfusion_after_sepsis == TRUE &
      !is.na(hemoglobin) &
      hemoglobin >= 8,
    FALSE,

    default = NA)] %>%
  .[, rec_90_eligibility := fcase(

    !is.na(rec_90_restrictive_transfusion_causal),
    "eligible",

    transfusion_after_sepsis == TRUE &
      is.na(transfusion_strategy),
    "transfused, pre-transfusion hemoglobin unavailable",

    transfusion_after_sepsis == FALSE |
      is.na(transfusion_after_sepsis),
    "no blood transfusion after sepsis onset",

    default = NA_character_
  )]

## Decision-point confounders for recommendation 90: the hemodynamic severity at
## the transfusion, which pushes a clinician toward a liberal transfusion at a
## given hemoglobin and also predicts outcome. The trigger hemoglobin itself
## defines the treatment arm (restrictive = Hb < 7) and is deliberately excluded.
## Anchored to the first transfusion after sepsis onset, taking the most recent
## value in the preceding 24h.
rec90_transfusion_anchor <- rec_90 %>%
  .[!is.na(transfusion_time) &
      transfusion_after_sepsis == TRUE,
    .(subject_id,
      hadm_id,
      stay_id,
      transfusion_time)]

pre_lactate_transfusion <- rec90_transfusion_anchor %>%
  merge(.,
        lactate_events[, .(subject_id,
                           hadm_id,
                           lac_time = charttime,
                           lactate = valuenum)],
        by = c("subject_id",
               "hadm_id"),
        allow.cartesian = TRUE) %>%
  .[lac_time <= transfusion_time &
      lac_time >= transfusion_time - hours(24)] %>%
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.numeric(lac_time)),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        cf_rec90_lactate = lactate)]

pre_map_transfusion <- rec90_transfusion_anchor %>%
  merge(.,
        map_events[, .(stay_id,
                       map_time = charttime,
                       map = valuenum)],
        by = "stay_id",
        allow.cartesian = TRUE) %>%
  .[map_time <= transfusion_time &
      map_time >= transfusion_time - hours(24)] %>%
  .[order(stay_id,
          -as.numeric(map_time)),
    .SD[1],
    by = stay_id] %>%
  .[, .(stay_id,
        cf_rec90_map = map)]

rec_90 <- rec_90 %>%
  merge(.,
        pre_lactate_transfusion,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  merge(.,
        pre_map_transfusion,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE)


## Adherence rates
proportions(table(rec_90$rec_90_eligibility))
table(rec_90$rec_90_eligibility)
proportions(table(rec_90$rec_90_restrictive_transfusion_causal))

proportions(table(rec_90$rec_90_restrictive_transfusion_8))

proportions(table(rec_90[rec_90_eligibility == "eligible"]$transfusion_strategy))
rec_90[rec_90_eligibility == "eligible", .N, by = transfusion_strategy][order(-N)]

summary(rec_90$cf_rec90_lactate)
summary(rec_90$cf_rec90_map)

## 91 RECOMMENDATION -----------------------------------------------------------
## cannot be assessed within the first 24h
## was excluded in previous paper, due to this reason
rec91_label <- ssc_recommendations %>% 
  .[rec_num == 91, rec_label]

## Item IDs for enteral nutrition
enteral_formula_itemids <- d_items %>% 
  .[category == "Nutrition - Enteral" &
      linksto == "inputevents"]


## Enteral nutrition administration (from inputevents, all enteral formulas)
enteral_nutrition_events <- all_inputevents %>%
  ## only patients in sepsis base cohort
  .[stay_id %in% sepsis_base_cohort_data$stay_id] %>%
  ## Enteral nutrition item IDs
  .[itemid %in% enteral_formula_itemids$itemid] %>%
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

## Patients who need enteral nutrition: invasive mechanical ventilation > 48h.
## Brief intubations (e.g. peri-procedural) do not require tube feeding and would
## otherwise dilute the denominator, so they are excluded.
## Invasive ventilation items are selected by label rather than a hardcoded itemid;
## non-invasive ventilation is excluded because those patients are often not NPO.
vent_itemids <- all_procedureevents %>%
  .[ordercategoryname == "Ventilation"] %>% 
  .[, .(itemid,
        ordercategoryname,
        ordercategorydescription)] %>% 
  unique() %>% 
  merge(.,
        d_items[, .(itemid,
                    label)],
        by = "itemid",
        all.x = TRUE,
        all.y = FALSE)

invasive_vent_itemids <- vent_itemids %>% 
  copy() %>% 
  .[!grepl("non-invasive",
           label,
           ignore.case = TRUE)]


## Prolonged ventilation > 48 hours
## this is when enteral nutrition is necessary
prolonged_ventilation <- all_procedureevents %>%
  .[stay_id %in% sepsis_base_cohort_data$stay_id &
      itemid %in% invasive_vent_itemids$itemid] %>% 
  .[, .(vent_hours = sum(as.numeric(difftime(endtime,
                                             starttime,
                                             units = "hours")),
                         na.rm = TRUE)),
    by = stay_id] %>%
  .[vent_hours > 48,
    .(stay_id,
      prolonged_ventilation = TRUE)]

## Enteral nutrition within 72 hours of sepsis onset, among prolonged-ventilation patients
rec_91 <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        suspected_infection_time,
        septic_shock_proxy,
        septic_shock_onset_time)] %>%
  unique() %>%
  merge(.,
        enteral_nutrition_events,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  ## enteral feed within the early window [onset - 1h, onset + 72h]
  .[, enteral_within_72h := !is.na(enteral_time) &
      enteral_time >= suspected_infection_time - hours(1) &
      enteral_time <= suspected_infection_time + hours(72)] %>%
  ## first enteral feed, preferring one inside the early window
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.numeric(enteral_within_72h),
          enteral_time)] %>%
  .[, .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  ## restrict the denominator to patients who need enteral nutrition
  merge(.,
        prolonged_ventilation,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, prolonged_ventilation := fcoalesce(prolonged_ventilation, FALSE)] %>%
  ## adherence defined only for the prolonged-ventilation population
  .[prolonged_ventilation == TRUE,
    rec_91_early_enteral_nutrition_causal := enteral_within_72h] %>%
  .[, rec_91_eligibility := fcase(

    !is.na(rec_91_early_enteral_nutrition_causal),
    "eligible",

    prolonged_ventilation == FALSE,
    "not mechanically ventilated > 48h (not applicable)",

    default = NA_character_)] %>% 
  ## Decision-point confounder: septic shock within the first 48h of sepsis onset,
  ## measured before the feeding window. Early hemodynamic instability is the main
  ## reason enteral feeding is delayed or withheld and it also predicts outcome.
  ## Total ventilation duration is deliberately not used, since it is largely
  ## determined after the feeding decision and would be a consequence of treatment.
  .[, cf_rec91_early_shock := as.integer(
    septic_shock_proxy == TRUE &
      !is.na(septic_shock_onset_time) &
      septic_shock_onset_time <= suspected_infection_time + hours(48))]


## Adherence rates
proportions(table(rec_91$rec_91_eligibility))
table(rec_91$rec_91_eligibility)
proportions(table(rec_91$rec_91_early_enteral_nutrition_causal))

table(rec_91$cf_rec91_early_shock)

rec_91[prolonged_ventilation == TRUE,
       .(early     = mean(rec_91_early_enteral_nutrition_causal),
         never_fed = mean(is.na(enteral_time)),
         late_fed  = mean(!is.na(enteral_time) & rec_91_early_enteral_nutrition_causal == FALSE))]


## invasive ventilation hours per stay
vent_hours_by_stay <- all_procedureevents %>%
  .[stay_id %in% sepsis_base_cohort_data$stay_id &
      itemid %in% invasive_vent_itemids$itemid] %>%
  .[, .(vent_hours = sum(as.numeric(difftime(endtime, starttime, units = "hours")),
                         na.rm = TRUE)),
    by = stay_id]

## early-enteral adherence under each ventilation-duration threshold
rbindlist(lapply(c(24, 48, 72), function(thr) {
  ids <- vent_hours_by_stay[vent_hours > thr, stay_id]
  data.table(threshold_h = thr,
             n_eligible  = rec_91[stay_id %in% ids, .N],
             early_rate  = rec_91[stay_id %in% ids,
                                  mean(enteral_within_72h, na.rm = TRUE)])
}))

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

## Insulin timeliness across windows: did any insulin fall in
## [first glucose >= 180 - 1h, + Xh], for the 3h and 12h sensitivities around the
## 6h primary. Computed as an any-insulin aggregate per stay (not from the single
## 6h-selected row) so each window is evaluated independently.
insulin_timeliness <- sepsis_base_cohort_data %>%
  .[, .(subject_id,
        hadm_id,
        stay_id)] %>%
  unique() %>%
  merge(.,
        first_glucose_180[, .(subject_id,
                              hadm_id,
                              stay_id,
                              first_glucose_180_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>%
  merge(.,
        insulin_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>%
  .[, .(
    insulin_within_3h = any(!is.na(insulin_time) &
                              insulin_time >= first_glucose_180_time - hours(1) &
                              insulin_time <= first_glucose_180_time + hours(3)),
    insulin_within_12h = any(!is.na(insulin_time) &
                               insulin_time >= first_glucose_180_time - hours(1) &
                               insulin_time <= first_glucose_180_time + hours(12))),
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
  .[, rec_92_insulin_started_at_glucose_180_causal := fcase(
    
    !is.na(first_glucose_180_time) &
      insulin_after_glucose_180 == TRUE,
    TRUE,
    
    !is.na(first_glucose_180_time) &
      insulin_after_glucose_180 == FALSE,
    FALSE,
    
    default = NA)] %>% 
  .[, rec_92_eligibility := fcase(
    
    !is.na(rec_92_insulin_started_at_glucose_180_causal),
    "eligible",
    
    is.na(first_glucose_180_time),
    "no glucose value >= 180",

    default = NA_character_)] %>%
  ## sensitivity analysis: tighter (3h) and wider (12h) windows around the 6h
  ## primary, evaluated independently via the insulin_timeliness aggregate
  merge(.,
        insulin_timeliness,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, rec_92_insulin_started_at_glucose_180_3h := fifelse(
    !is.na(first_glucose_180_time),
    insulin_within_3h,
    NA)] %>%
  .[, rec_92_insulin_started_at_glucose_180_12h := fifelse(
    !is.na(first_glucose_180_time),
    insulin_within_12h,
    NA)] %>% 
  ## Decision-point confounder: the qualifying glucose value itself. Among eligible
  ## stays (all above 180) the exact level still varies and drives both whether
  ## insulin is started promptly and the outcome; it is the eligibility gate, not
  ## the adherence rule, so it is a legitimate covariate.
  .[, cf_rec92_qualifying_glucose := first_glucose_180_value]




proportions(table(rec_92$rec_92_eligibility))
table(rec_92$rec_92_eligibility)
proportions(table(rec_92$rec_92_insulin_started_at_glucose_180_causal))
proportions(table(rec_92$rec_92_insulin_started_at_glucose_180_3h))
proportions(table(rec_92$rec_92_insulin_started_at_glucose_180_12h))

summary(rec_92$cf_rec92_qualifying_glucose)

## 96 RECOMMENDATION -----------------------------------------------------------
rec96_label <- ssc_recommendations %>% 
  .[rec_num == 96, rec_label]

## Arterial blood gas
ph_itemids <- chart_itemids %>% 
  .[, label := trimws(tolower(label))] %>% 
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
  .[itemid %in% ph_itemids$itemid] %>%
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


## AKI stage >= 2 (AKIN 2/3 equivalent) from derived KDIGO staging
## firs time stage >= 2 is reached after sepsis onset, per stay
aki_severe_onset <- kdigo_stages %>%
  copy() %>% 
.[!is.na(aki_stage)] %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>% 
  .[aki_stage >= 2 &
      charttime >= suspected_infection_time - hours(1) &
      charttime <= suspected_infection_time + days(7)] %>% 
  .[order(charttime),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        aki_stage_time = charttime,
        aki_stage)]


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
## Eligible subpopulation: septic shock together with severe metabolic acidemia
## (pH <= 7.2) and KDIGO AKI stage >= 2 (AKIN 2/3 equivalent), the acidemia and
## AKI criteria occurring within 24h of each other. Septic shock is required to
## stay faithful to the guideline text.
rec_96_eligible <- sepsis_base_cohort_data %>%
  .[septic_shock_proxy == TRUE] %>%
  merge(.,
        aki_severe_onset,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE) %>%
  merge(.,
        severe_acidemia_events[severe_acidemia == TRUE,
                               .(subject_id,
                                 hadm_id,
                                 stay_id,
                                 acidemia_time,
                                 ph,
                                 severe_acidemia)],
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all = FALSE,
        allow.cartesian = TRUE) %>%
  ## require acidemia and AKI reasonably close together
  .[abs(as.numeric(difftime(acidemia_time,
                            aki_stage_time,
                            units = "hours"))) <= 24] %>%
  .[, eligibility_time := pmax(acidemia_time,
                               aki_stage_time)] %>%
  .[order(eligibility_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)]

## First sodium bicarbonate administration after all criteria are met. Kept in a
## side table so eligible patients who never received bicarbonate are retained in
## the denominator rather than dropped by the NA comparison.
first_bicarb_after_eligibility <- rec_96_eligible %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        eligibility_time)] %>%
  merge(.,
        sodium_bicarb_events,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = TRUE) %>%
  .[!is.na(bicarbonate_time) &
      bicarbonate_time >= eligibility_time] %>%
  .[order(bicarbonate_time),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_bicarbonate_time = bicarbonate_time,
        bicarbonate_type,
        bicarbonate_amount,
        bicarbonate_unit)]

rec_96 <- rec_96_eligible %>%
  merge(.,
        first_bicarb_after_eligibility,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        septic_shock_onset_time,
        acidemia_time,
        ph,
        aki_stage_time,
        aki_stage,
        eligibility_time,
        first_bicarbonate_time,
        bicarbonate_type,
        bicarbonate_amount,
        bicarbonate_unit)] %>%
  .[, rec_96_sodium_bicarb_causal := !is.na(first_bicarbonate_time)] %>%
  ## restore the full cohort so every stay carries a row, as for the
  ## recommendations built on sepsis_base_cohort_data. Stays outside the eligible
  ## set are marked non-eligible and keep an NA adherence value.
  merge(sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id)],
        .,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  .[, rec_96_eligibility := fcase(

    !is.na(eligibility_time),
    "eligible",

    default = "no severe acidemia with AKI 2/3 in shock")] %>% 
  ## Decision-point confounders: the pH value and KDIGO stage at the qualifying
  ## time. Both are eligibility gates (pH <= 7.2, stage >= 2), but among eligible
  ## stays they still vary and drive the bicarbonate decision without defining it,
  ## so they are legitimate covariates rather than the adherence rule.
  .[, `:=` (cf_rec96_ph = ph,
            cf_rec96_aki_stage = as.numeric(aki_stage))]

## Additional decision-point confounders for rec 96: lactate and the peak
## norepinephrine-equivalent vasopressor burden in the 24h before the qualifying
## time, capturing the depth of shock that drives last-ditch bicarbonate use and
## predicts outcome. This is the "how close to dying" signal the pH and stage do
## not fully carry.
rec96_anchor <- rec_96[!is.na(eligibility_time),
                       .(subject_id,
                         hadm_id,
                         stay_id,
                         eligibility_time)]

rec96_pre_lactate <- rec96_anchor %>%
  merge(.,
        lactate_events[, .(subject_id,
                           hadm_id,
                           lac_time = charttime,
                           lactate = valuenum)],
        by = c("subject_id",
               "hadm_id"),
        allow.cartesian = TRUE) %>%
  .[lac_time <= eligibility_time &
      lac_time >= eligibility_time - hours(24)] %>%
  .[order(subject_id,
          hadm_id,
          stay_id,
          -as.numeric(lac_time)),
    .SD[1],
    by = .(subject_id,
           hadm_id,
           stay_id)] %>%
  .[, .(subject_id,
        hadm_id,
        stay_id,
        cf_rec96_lactate = lactate)]

rec96_pre_vaso <- rec96_anchor %>%
  merge(.,
        vasopressor_hourly[, .(stay_id,
                               chart_hour,
                               vasopressor_rate)],
        by = "stay_id",
        allow.cartesian = TRUE) %>%
  .[chart_hour <= eligibility_time &
      chart_hour >= eligibility_time - hours(24)] %>%
  .[, .(cf_rec96_vaso_burden = max(vasopressor_rate,
                                   na.rm = TRUE)),
    by = stay_id]

rec_96 <- rec_96 %>%
  merge(.,
        rec96_pre_lactate,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE) %>%
  merge(.,
        rec96_pre_vaso,
        by = "stay_id",
        all.x = TRUE,
        all.y = FALSE)


## eligible subpopulation size and adherence
proportions(table(rec_96$rec_96_sodium_bicarb_causal))
table(rec_96$aki_stage)

summary(rec_96$cf_rec96_ph)
summary(rec_96$cf_rec96_aki_stage)
table(rec_96$cf_rec96_aki_stage)
summary(rec_96$cf_rec96_lactate)
summary(rec_96$cf_rec96_vaso_burden)


## 98 Recommendation -----------------------------------------------------------
rec98_label <- ssc_recommendations %>% 
  .[rec_num == 98, rec_label]


vte_prophylaxis <- all_prescriptions %>% 
  copy() %>% 
  .[grepl("heparin|enoxaparin|lovenox|dalteparin|fragmin|tinzaparin",
          drug,
          ignore.case = TRUE) &
      !grepl("flush|lock",
             drug,
             ignore.case = TRUE)] %>% 
  ## single numeric dose
  ## IV weight-based ranges become NA
  .[, dose_num := suppressWarnings(as.numeric(gsub(",", "", dose_val_rx)))] %>%  
  .[, agent := fcase(
    
    grepl("enoxaparin|lovenox|dalteparin|fragmin|tinzaparin",
          drug,
          ignore.case = TRUE),
    "LMWH",
    
    grepl("heparin",
          drug,
          ignore.case = TRUE),
    "UFH",
    
    default = NA_character_)] %>% 
  .[route %chin% c("SC",
                   "SUBCUT",
                   "SQ")] %>% 
  .[(agent == "UFH"  & 
       dose_num %in% c(5000, 
                       7500)) |
      (agent == "LMWH" &
         !grepl("treatment", 
                drug, 
                ignore.case = TRUE) &
         (grepl("prophylaxis", 
                drug, 
                ignore.case = TRUE) |
            (dose_unit_rx == "mg" & 
               dose_num <= 40)))] %>% 
  .[, .(subject_id,
        hadm_id,
        starttime,
        agent)]

## first prophylactic agent per stay
first_prophylaxis <- vte_prophylaxis %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id)],
        by = c("subject_id",
               "hadm_id"),
        all = FALSE) %>% 
  .[order(subject_id,
          hadm_id,
          stay_id,
          starttime),
    .SD[1],
    by = c("subject_id",
           "hadm_id",
           "stay_id")] %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id,
        first_vte_agent = agent)]

## ever-LMWH sensitivity
ever_lmwh <- vte_prophylaxis %>% 
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id)],
        by = c("subject_id",
               "hadm_id"),
        all = FALSE) %>% 
  .[, .(used_lmwh = any(agent == "LMWH"),
        used_ufh = any(agent == "UFH")),
    by = .(subject_id,
           hadm_id,
           stay_id)]

rec_98 <- sepsis_base_cohort_data %>% 
  .[, .(subject_id,
        hadm_id,
        stay_id)] %>% 
  merge(.,
        first_prophylaxis,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE) %>% 
  merge(.,
        ever_lmwh,
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE) %>% 
  .[, rec_98_lmwh_over_ufh := fcase(
    
    first_vte_agent == "LMWH", 
    TRUE,
    
    first_vte_agent == "UFH",  
    FALSE,
    
    default = NA)] %>% 
  .[, rec_98_lmwh_ever := fcase(
    
    used_lmwh == TRUE,                         
    TRUE,
    
    used_lmwh == FALSE & used_ufh == TRUE,     
    FALSE,
    
    default = NA)] %>% 
  .[, rec_98_eligibility := fcase(
    
    !is.na(rec_98_lmwh_over_ufh), 
    "eligible",
    
    default = "no heparin-class VTE prophylaxis")]

proportions(table(rec_98$rec_98_lmwh_over_ufh))
proportions(table(rec_98$rec_98_lmwh_ever))
table(rec_98$rec_98_eligibility)


## Combine all recommendations into one dataset --------------------------------
DT_ssc_encoded <- sepsis_base_cohort_data %>% 
  merge(.,
        rec_7[, .(subject_id,
                  hadm_id,
                  stay_id,
                  rec_7_culture_before_antimicrobial_causal)],
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
                rec_8_lactate_measured_within_1h_causal)] %>% 
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
                   rec_10_crystalloid_within_3h_causal,
                   rec_10_eligibility)] %>% 
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
                   rec_15_icu_6h_causal,
                   rec_15_eligibility)] %>%
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
                   rec_16_antimicrobial_within_1h_causal,
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
                   rec_17_antimicrobial_within_1h_causal,
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
                   rec_44_balanced_over_saline_bolus_causal,
                   rec_44_eligibility)] %>% 
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
                   rec_45_crystalloid_no_albumin,
                   rec_45_eligibility)] %>%
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
                   rec_53_first_vaso_norepi,
                   rec_53_eligibility)] %>% 
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
                   rec_55_first_vaso_norepi,
                   rec_55_eligibility)] %>% 
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>% 
  merge(.,
        rec_53_55_causal[, .(subject_id,
                             hadm_id,
                             stay_id,
                             rec_53_55_norepi_first_line_causal)]) %>% 
  merge(.,
        rec_56[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_56_vasopressin_after_escalating_norepi_causal,
                   rec_56_eligibility,
                   cf_rec56_norepi_rate_escalation,
                   cf_rec56_lactate,
                   cf_rec56_map)] %>%
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
                   rec_57_epi_after_escalating_norepi_vaso,
                   rec_57_eligibility)] %>% 
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
                   rec_67_hfnc_over_conventional,
                   rec_67_eligibility)] %>% 
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
                   rec_68_first_oxygen_hfnc_causal,
                   rec_68_eligibility)] %>% 
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
                   rec_69_hfnc_over_alternating,
                   rec_69_eligibility)] %>% 
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
                   rec_71_low_tidal_volume,
                   rec_71_eligibility)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_73[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_73_plateau_pressure_limited,
                   rec_73_eligibility)] %>%
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
                   rec_79_iv_corticosteroids,
                   rec_79_eligibility)] %>%
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
                   rec_89_active_fluid_removal_7d_causal,
                   rec_89_eligibility,
                   cf_rec89_peak_vasopressor,
                   cf_rec89_cum_fluid_bolus,
                   cf_rec89_hours_to_stabilization)] %>%
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
                   rec_90_restrictive_transfusion_causal,
                   rec_90_eligibility,
                   cf_rec90_lactate,
                   cf_rec90_map)] %>%
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
                   rec_91_early_enteral_nutrition_causal,
                   rec_91_eligibility,
                   cf_rec91_early_shock)] %>%
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
                   rec_92_insulin_started_at_glucose_180_causal,
                   rec_92_eligibility,
                   cf_rec92_qualifying_glucose)] %>%
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
                   rec_96_sodium_bicarb_causal,
                   rec_96_eligibility,
                   cf_rec96_ph,
                   cf_rec96_aki_stage,
                   cf_rec96_lactate,
                   cf_rec96_vaso_burden)] %>%
          unique(),
        by = c("subject_id",
               "hadm_id",
               "stay_id"),
        all.x = TRUE,
        all.y = FALSE,
        allow.cartesian = FALSE) %>%
  merge(.,
        rec_98[, .(subject_id,
                   hadm_id,
                   stay_id,
                   rec_98_lmwh_ever,
                   rec_98_eligibility)] %>%
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


## Adherence summary for Table 6.2 -------------------------------------------
## For each recommendation the eligible set is the stays with a non-missing
## adherence value, and the reported rate is the mean of that indicator. The
## descriptive adherence column is used, which for recommendations 7, 10, 16, 17
## and 44 is broader than the restricted _causal variant used in the causal
## analysis, so that the figures match the ones reported in the encoding chapter.
adh_spec <- list(
  list("7",  rec_7,  "rec_7_culture_before_antimicrobial"),
  list("8",  rec_8,  "rec_8_lactate_measured_within_1h_causal"),
  list("10", rec_10, "rec_10_crystalloid_within_3h"),
  list("13", rec_13, "rec_13_map_target_65"),
  list("14", rec_14, "rec_14_map_target_60_65"),
  list("15", rec_15, "rec_15_icu_6h_causal"),
  list("16", rec_16, "rec_16_antimicrobial_within_1h"),
  list("17", rec_17, "rec_17_antimicrobial_within_1h"),
  list("42", rec_42, "rec_42_bp_monitoring"),
  list("43", rec_43, "rec_43_crystalloid_first_line"),
  list("44", rec_44, "rec_44_balanced_over_saline"),
  list("45", rec_45, "rec_45_crystalloid_no_albumin"),
  list("53", rec_53, "rec_53_first_vaso_norepi"),
  list("55", rec_55, "rec_55_first_vaso_norepi"),
  list("56", rec_56, "rec_56_vasopressin_after_escalating_norepi_causal"),
  list("57", rec_57, "rec_57_epi_after_escalating_norepi_vaso"),
  list("67", rec_67, "rec_67_hfnc_over_conventional"),
  list("68", rec_68, "rec_68_first_oxygen_hfnc_causal"),
  list("69", rec_69, "rec_69_hfnc_over_alternating"),
  list("71", rec_71, "rec_71_low_tidal_volume"),
  list("73", rec_73, "rec_73_plateau_pressure_limited"),
  list("79", rec_79, "rec_79_iv_corticosteroids"),
  list("89", rec_89, "rec_89_active_fluid_removal_7d_causal"),
  list("90", rec_90, "rec_90_restrictive_transfusion_causal"),
  list("91", rec_91, "rec_91_early_enteral_nutrition_causal"),
  list("92", rec_92, "rec_92_insulin_started_at_glucose_180_causal"),
  list("96", rec_96, "rec_96_sodium_bicarb_causal"),
  list("98", rec_98, "rec_98_lmwh_ever"))

adherence_summary <- rbindlist(lapply(adh_spec, function(s) {
  v <- s[[2]][[s[[3]]]]
  data.table(rec        = s[[1]],
             eligible_n = sum(!is.na(v)),
             adherence  = round(100 * mean(v, na.rm = TRUE), 1))
}))

fwrite(adherence_summary,
       here::here("Outputs/adherence_summary.csv"))


ssc_recommendations_encoded <- ssc_recommendations %>%
  .[rec_num %in% c(7,
                   8,
                   10,
                   12,
                   13,
                   14,
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
                   73,
                   79,
                   89,
                   90,
                   91,
                   96,
                   98)]



## Export analysis frame for the Python sensitivity pipeline -------------------
id_cols <- c("subject_id",
             "hadm_id",
             "stay_id")

outcomes_dt <- readRDS(here::here("Data/sepsis_base_cohort_data.rds")) %>% 
  setDT()

baseline_covariates <- readRDS(here::here("Data/baseline_covariates.rds")) %>% 
  setDT()

## Every encoded recommendation except 13 and 14 enters the causal screen, at the
## treatment definition it would use in the analysis: the _causal variant where a
## subgroup restriction applies, and the plain adherence indicator otherwise.
## Recommendations 13 and 14 are excluded a priori because a mean arterial pressure
## target is titrated to the patient's state, so its adherence is a consequence of
## the haemodynamics rather than an assignable action, which no overlap test can
## repair. Inclusion in the causal analysis is then decided uniformly by the
## propensity-score overlap of each candidate, not by its adherence rate.
candidate_cols <- c("rec_7_culture_before_antimicrobial_causal",
                    "rec_8_lactate_measured_within_1h_causal",
                    "rec_10_crystalloid_within_3h_causal",
                    "rec_15_icu_6h_causal",
                    "rec_16_antimicrobial_within_1h_causal",
                    "rec_17_antimicrobial_within_1h_causal",
                    "rec_42_bp_monitoring",
                    "rec_43_crystalloid_first_line",
                    "rec_44_balanced_over_saline_bolus_causal",
                    "rec_45_crystalloid_no_albumin",
                    "rec_53_55_norepi_first_line_causal",
                    "rec_56_vasopressin_after_escalating_norepi_causal",
                    "rec_57_epi_after_escalating_norepi_vaso",
                    "rec_67_hfnc_over_conventional",
                    "rec_68_first_oxygen_hfnc_causal",
                    "rec_69_hfnc_over_alternating",
                    "rec_71_low_tidal_volume",
                    "rec_73_plateau_pressure_limited",
                    "rec_79_iv_corticosteroids",
                    "rec_89_active_fluid_removal_7d_causal",
                    "rec_90_restrictive_transfusion_causal",
                    "rec_91_early_enteral_nutrition_causal",
                    "rec_92_insulin_started_at_glucose_180_causal",
                    "rec_96_sodium_bicarb_causal",
                    "rec_98_lmwh_ever")

W_export <- c("anchor_age", "charlson_comorbidity_index", "sofa_score", "sapsii",
              "creatinine", "bun", "wbc", "platelets", "hemoglobin", "bilirubin",
              "albumin", "inr", "lactate", "ph_blood", "bicarbonate",
              "aki_baseline_stage")

## Decision-point confounders, added to the baseline block only for the model of
## the recommendation they belong to (see the extra_covariates map in the Python
## notebook). They are continuous and are left numeric, not integer-coerced.
confounder_cols <- c("cf_rec56_norepi_rate_escalation",
                     "cf_rec56_lactate",
                     "cf_rec56_map",
                     "cf_rec89_peak_vasopressor",
                     "cf_rec89_cum_fluid_bolus",
                     "cf_rec89_hours_to_stabilization",
                     "cf_rec92_qualifying_glucose",
                     "cf_rec96_ph",
                     "cf_rec96_aki_stage",
                     "cf_rec96_lactate",
                     "cf_rec96_vaso_burden",
                     "cf_rec91_early_shock",
                     "cf_rec90_lactate",
                     "cf_rec90_map")

dml_export <- DT_ssc_encoded[, c(id_cols, candidate_cols, confounder_cols),
                             with = FALSE] %>%
  merge(outcomes_dt[, c(id_cols, "mort_28d", "icu_mortality", "aki_new"),
                        with = FALSE],
        by = id_cols, all.x = TRUE) %>%
  merge(baseline_covariates[, c(id_cols, "gender", W_export,
                                "myocardial_infarct", "congestive_heart_failure",
                                "well_observed", "well_observed_strict"),
                            with = FALSE],
        by = id_cols, all.x = TRUE)

## numeric typing for Python
dml_export[, gender_male := as.integer(gender == "M")]
dml_export[, gender := NULL]
dml_export[, (candidate_cols) := lapply(.SD, as.integer),
           .SDcols = candidate_cols]
dml_export[, aki_baseline_stage := as.numeric(aki_baseline_stage)]

## rec 90 decision-point confounder: a dedicated cardiac-risk flag (myocardial
## infarction or congestive heart failure), which lowers the hemoglobin threshold
## a clinician accepts. The aggregate Charlson index in W only partially captures
## this, so the specific cardiac mechanism is isolated here.
dml_export[, cf_rec90_cardiac := as.integer(myocardial_infarct == 1 |
                                              congestive_heart_failure == 1)]
dml_export[, c("myocardial_infarct", "congestive_heart_failure") := NULL]

arrow::write_parquet(dml_export, here::here("Outputs/dml_analysis_frame.parquet"))
  
