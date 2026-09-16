
source(here::here("Code/define_ssc_variables.R"))



# thesis_figures.R

# Recomputes every numeric figure quoted in the thesis text so the written
# values can be cross-checked and updated after code changes.
#
# USAGE:  source this AFTER running define_ssc_variables.R, so that the cohort
#         (sepsis_base_cohort_data) and all rec_* / antimicrobial_timing objects
#         exist in the environment.
#
#   source("define_ssc_variables.R")
#   source("thesis_figures.R")
#
# OUTPUT: prints a table (section | description | thesis | computed | match)
#         and writes it to ../outputs/thesis_figures_output.csv
#
# The "thesis" column holds the value CURRENTLY written in the thesis PDF.
# The "computed" column is recomputed live from the data. "match" flags rows
# where the two disagree (CHECK) so you can see at a glance what has drifted.
#
# Rows marked "manual" in the note column are one-off diagnostics computed
# during development; they are listed for completeness but not auto-recomputed.


## COLLECTOR FUNCTIONS ---------------------------------------------------------
## initial empty list to which each data.table row is added sequentially
.figrows <- list()

### Adding Row Function --------------------------------------------------------
## adds an additional row for each computed value (figure from the thesis)
## a uniform, order-preserving way to register every figure (its label, 
## the value written in the thesis, the value recomputed from data, and an optional note)
add <- function(section, 
                description,
                thesis, 
                computed = NA_character_,
                note = "") {
  
  ## superassignment ensures the list is updated in the global environment so that 
  ## the new row is not just added locally and doesn't vanish in the function output
  .figrows[[length(.figrows) + 1L]] <<- data.table(section = section,
                                                   description = description,
                                                   thesis = as.character(thesis),
                                                   computed = as.character(computed),
                                                   note = note)
}


### Error Isolation Function ---------------------------------------------------
## evaluates the input expression and returns its value. If this throws an error, 
## it catches it and returns it instead of letting the error propagate. This is 
## necessary to prevent a singular error from causing the whole comparison to fail
safe <- function(expr) {
  
  tryCatch(expr, 
           error = function(e) {
             
             paste0("ERR: ", conditionMessage(e))
             
             }
  )
}

### Cohort Adherence Functions -------------------------------------------------
## adherence rate over the evaluable stays as denominator (non-NA entries)
## takes a logical vector (a recommendation adherence flag), drops the NA values 
## and returns a formatted string containing the adherance rate (TRUE / TRUE + FALSE)
rate <- function(flag) {
  
  flag <- flag[!is.na(flag)]
  n <- length(flag)
  k <- sum(flag)
  
  if (n == 0) {
    
    return("NA (no evaluable stays)")
    
    } else {
      sprintf("%.1f%% (%d/%d)", 
              100 * k / n, 
              k, 
              n)
    }
}

## proportion of adherance over all stays (not just evaluable)
## takes a logical vector (e.g. a cohort subgroup), drops NA values
## and retruns a formatted string containing the proportion of adherance (TRUE / TRUE + FALSE + NA)
## NA entries are effectively counted as "FALSE" in this function
prop <- function(cond) {
  
  n <- length(cond) 
  k <- sum(cond, na.rm = TRUE)
  
  if (n == 0) {
    
    return("NA")
    
  } else {
    
    sprintf("%.1f%% (%d/%d)", 100 * k / n, k, n)
  }
}


## formats an integer with thousands separator, so e.g. 21189 prints as 21,189
## matches how counts are written in the thesis --> purely cosmetic and for easy
## visual comparability between the computed columns
cnt <- function(x) {
  
  format(x, 
         big.mark = ",")
}


## COHORT SIZES ----------------------------------------------------------------
add("Cohort", "Sepsis base cohort (n)",
    "21,189", safe(cnt(nrow(sepsis_base_cohort_data))))

add("Cohort", "Septic shock proxy (n)",
    "6,487", safe(cnt(sepsis_base_cohort_data[, sum(septic_shock_proxy == TRUE, 
                                                    na.rm = TRUE)])))

add("Cohort", "Sepsis without shock (n)",
    "14,702", safe(cnt(sepsis_base_cohort_data[, sum(septic_shock_proxy != TRUE | 
                                                       is.na(septic_shock_proxy))])))

add("Cohort", "Hypoperfusion-eligible (rec 10) (n)",
    "11,455", safe(cnt(sepsis_base_cohort_data[, sum(hypoperfusion_proxy == TRUE, 
                                                     na.rm = TRUE)])))

add("Cohort", "Lactate-only stratum (hypoperfusion & not shock) (n)",
    "4,968", safe(cnt(sepsis_base_cohort_data[, sum(hypoperfusion_proxy == TRUE & 
                                                      septic_shock_proxy != TRUE, na.rm = TRUE)])))


## 7 RECOMMENDATION - blood cultures before antimicrobial therapy --------------
add("Rec 7", "Adherence (culture before antimicrobial)",
    "50.2% (10,629/21,189)", safe(rate(rec_7$rec_7_culture_before_antimicrobial)))

## onset-anchor structure
add("Rec 7", "Recognition time coincides with a culture timestamp",
    "85.1%", safe(prop(sepsis_base_cohort_data[, suspected_infection_time == culture_time])))

## blood-culture-anchored vs non-blood strata.
## A stay is "blood-anchored" if a qualifying blood culture was drawn exactly at
## sepsis onset (the anchoring culture is itself a blood culture). Rebuilt from
## all_microbioevents because this stratifier is not persisted, and using ALL
## qualifying blood cultures (not just the earliest, which may precede onset).
blood_at_onset <- all_microbioevents %>%
  .[hadm_id %in% sepsis_base_cohort_data$hadm_id] %>%
  .[spec_type_desc %in% c("BLOOD CULTURE",
                          "BLOOD CULTURE ( MYCO/F LYTIC BOTTLE)")] %>%
  .[, .(subject_id,
        hadm_id,
        blood_culture_time = charttime)] %>%
  unique() %>%
  merge(.,
        sepsis_base_cohort_data[, .(subject_id,
                                    hadm_id,
                                    stay_id,
                                    suspected_infection_time)],
        by = c("subject_id",
               "hadm_id"),
        all = FALSE) %>%
  .[blood_culture_time == suspected_infection_time] %>%
  .[, .(anchored_blood = TRUE),
    by = .(subject_id,
           hadm_id,
           stay_id)]

d7 <- merge(rec_7[, .(subject_id,
                      hadm_id,
                      stay_id,
                      rec_7_culture_before_antimicrobial)],
            blood_at_onset,
            by = c("subject_id", 
                   "hadm_id", 
                   "stay_id"),
            all.x = TRUE) %>%
  .[, anchored_blood := fcoalesce(anchored_blood, FALSE)]

add("Rec 7", "Onset anchored on a BLOOD culture",
    "43.4% (9,198)", safe(prop(d7$anchored_blood)))

add("Rec 7", "Adherence | blood-anchored stratum",
    "91.7%", safe(rate(d7[anchored_blood == TRUE,
                          rec_7_culture_before_antimicrobial])))

add("Rec 7", "Adherence | non-blood-anchored stratum",
    "18.3% (2,196/11,991)", safe(rate(d7[anchored_blood == FALSE,
                                         rec_7_culture_before_antimicrobial])))

add("Rec 7", 
    "Antimicrobial time available from prescriptions",
    "99.7% (21,122/21,189)", 
    NA, 
    note = "manual: run coverage diagnostic on first_sepsis_antimicrobial")

add("Rec 7",
    "hadm_id repair raised adherence 23.3% -> 54.5%",
    "23.3% -> 54.5%",
    NA, 
    note = "manual: pre/post hadm_id-repair diagnostic (historical)")

add("Rec 7", 
    "Date-rounded onset (midnight) stays",
    "2.2% (467)", 
    NA,
    note = "manual: onset-imprecision diagnostic")


## 8 RECOMMENDATION - measurement of blood lactate -----------------------------
add("Rec 8", 
    "Lactate within 1h of onset (primary)",
    "52.0% (11,023)", 
    safe(rate(rec_8$rec_8_lactate_measured_within_1h)))

add("Rec 8", 
    "Lactate within 3h",
    "61.9% (13,107)",
    safe(rate(rec_8$rec_8_lactate_measured_within_3h)))

add("Rec 8", 
    "Lactate within 6h",
    "67.9% (14,387)", 
    safe(rate(rec_8$rec_8_lactate_measured_within_6h)))

add("Rec 8", 
    "Lactate measured at all in window (~24h)",
    "83.9% (17,783)", 
    safe(rate(rec_8$rec_8_lactate_measured)),
    note = "24h figure = any lactate in the extraction window; confirm window is [-1h,+24h]")


## 10 RECOMMENDATION - 30 mL/kg crystalloid within resuscitation window --------
## rec_10 does not carry septic_shock_proxy; merge it in for the strata
r10 <- merge(rec_10,
             sepsis_base_cohort_data[, .(subject_id,
                                         hadm_id,
                                         stay_id,
                                         septic_shock_proxy)],
             by = c("subject_id", 
                    "hadm_id", 
                    "stay_id"),
             all.x = TRUE)

add("Rec 10",
    "Adherence | septic-shock stratum (primary)",
    "12.4%", 
    safe(rate(r10[septic_shock_proxy == TRUE,
                     rec_10_crystalloid_within_3h])))

add("Rec 10", 
    "Adherence | septic-shock stratum, boluses only",
    "10.0%", 
    safe(rate(r10[septic_shock_proxy == TRUE, 
                     rec_10_crystalloid_within_3h_bolus])))

add("Rec 10", 
    "Adherence | lactate-only stratum",
    "0.8%", 
    safe(rate(r10[septic_shock_proxy != TRUE & 
                       hypoperfusion_proxy == TRUE, 
                     rec_10_crystalloid_within_3h])))

add("Rec 10", 
    "Adherence | pooled (all hypoperfusion-eligible)",
    "~7.4%", 
    safe(rate(r10$rec_10_crystalloid_within_3h)))

add("Rec 10", 
    "Evaluable septic-shock stays (weight available)",
    "6,476",
    safe(cnt(r10[septic_shock_proxy == TRUE &
                      !is.na(rec_10_crystalloid_within_3h), .N])))

## AdjBW sensitivity
add("Rec 10", 
    "AdjBW sensitivity | shock stratum",
    "14.5%", 
    safe(rate(r10[septic_shock_proxy == TRUE, 
                     rec_10_crystalloid_within_3h_adjbw])))

add("Rec 10", 
    "AdjBW sensitivity | lactate-only stratum",
    "1.1%", 
    safe(rate(r10[septic_shock_proxy != TRUE & 
                       hypoperfusion_proxy == TRUE, 
                     rec_10_crystalloid_within_3h_adjbw])))

add("Rec 10", 
    "AdjBW sensitivity | pooled",
    "8.7%", 
    safe(rate(r10$rec_10_crystalloid_within_3h_adjbw)))

## sepsis-onset anchor sensitivity
add("Rec 10",
    "Sepsis-onset anchor | shock stratum",
    "5.6%", 
    safe(rate(r10[septic_shock_proxy == TRUE, 
                     rec_10_crystalloid_within_3h_sepsisonset])))

add("Rec 10", 
    "Sepsis-onset anchor | lactate-only stratum",
    "1.5%",
    safe(rate(r10[septic_shock_proxy != TRUE & 
                       hypoperfusion_proxy == TRUE, 
                     rec_10_crystalloid_within_3h_sepsisonset])))

add("Rec 10", 
    "AdjBW cohort stays (validated height & BMI>30)",
    "4,672", 
    NA, 
    note = "manual: count of BMI>30 stays used for AdjBW")

add("Rec 10",
    "Received >=30 mL/kg at any point in ICU stay",
    "63.%", 
    NA, 
    note = "manual: whole-stay volume diagnostic")

add("Rec 10", 
    "Pre-ICU aggregate: eligible with record / median vol / >=30mL/kg",
    "22% (1,427); 3,000 mL (38.0 mL/kg); 61.8%", 
    NA,
    note = "manual: Pre-Admission/Non-ICU Intake diagnostic")

add("Rec 10", 
    "Lactate-only stratum with pre-ICU onset",
    "33.4%", 
    NA, 
    note = "manual: pre-ICU onset diagnostic")

add("Rec 10",
    "Carrier crystalloid share of events / volume",
    "~68% / ~18%",
    NA, 
    note = "manual: MetaVision order-category diagnostic")


## 12 RECOMMENDATION - peripheral vasopressor ----------------------------------
add("Rec 12", 
    "Adherence (vasopressor without central delay)",
    "70.2% (4,556/6,487)", 
    safe(rate(rec_12$rec_12_vaso_without_central_delay)))

add("Rec 12", "Decomposition: peripheral-first / pre-existing central",
    "3,433 (75%) / 1,123 (25%)", 
    NA, 
    note = "manual: decomposition of adherent group")

add("Rec 12", 
    "Lines in place at shock onset placed before episode",
    "29% (rest 71% acute)",
    NA, 
    note = "manual: line-timing diagnostic")

add("Rec 12", 
    "Median documented line duration",
    "50.4 h", 
    NA, 
    note = "manual: procedureevents duration diagnostic")


## 13 RECOMMENDATION - initial MAP target of 65 mmHg ------------------------
add("Rec 13", 
    "Evaluable septic-shock stays",
    "98.5% (6,393/6,487)", 
    safe({
      
      elig <- rec_13[septic_shock_proxy == TRUE, .N]
      ev   <- rec_13[septic_shock_proxy == TRUE &
                       !is.na(median_map), .N]

      sprintf("%.1f%% (%d/%d)", 100 * ev / elig, ev, elig)
    }))

## rec_13_map_target_65 is NA for ineligible non-shock stays, but for eligible 
## shock stays a missing MAP yields FALSE, not NA —-> so rate()'s NA-based denominator
## would wrongly include the 94 no-MAP shock stays. The explicit !is.na(median_map) 
## filter restricts to the actually evaluable 6,393 stays.
add("Rec 13",
    "Adherence (median MAP in 60-70 band)",
    "32.6% (2,081)",
    safe(rate(rec_13[septic_shock_proxy == TRUE & !is.na(median_map),
                     rec_13_map_target_65])))

add("Rec 13", 
    "Non-adherent ABOVE target (median MAP > 70)",
    "60.8% (3,885)", 
    safe({
      
      d <- rec_13[septic_shock_proxy == TRUE
                  & !is.na(median_map)]

      prop(d$median_map > 70)
    }))

add("Rec 13",
    "Non-adherent BELOW target (median MAP < 60)",
    "6.7% (427)", 
    safe({
      
      d <- rec_13[septic_shock_proxy == TRUE &
                    !is.na(median_map)]

      prop(d$median_map < 60)
    }))


## 15 RECOMMENDATION - ICU admission within six hours --------------------------
add("Rec 15", 
    "Adherence (pooled, in-ICU onset counted adherent)",
    "78.2% (16,572/21,189)", 
    safe(rate(rec_15$rec_15_icu_6h)))

add("Rec 15", 
    "Sepsis onset AFTER ICU admission (moot group)",
    "44.6% (9,445)",
    safe(prop(rec_15$hours_to_icu < 0)))

add("Rec 15", 
    "Admitted within 6h of a PRE-ICU onset",
    "33.6% (7,127)", 
    safe(prop(rec_15$hours_to_icu >= 0 & 
                rec_15$hours_to_icu <= 6)))

add("Rec 15", 
    "Admitted >6h after a PRE-ICU onset",
    "21.8% (4,617)", 
    safe(prop(rec_15$hours_to_icu > 6)))

add("Rec 15", 
    "Intent-faithful (pre-ICU-onset only) adherence",
    "60.7% (7,127/11,744)", 
    safe(rate(rec_15$rec_15_icu_6h_preicu)))


## 16 & 17 RECOMMENDATIONS - antimicrobial therapy within one hour -------------
add("Rec 16", 
    "Within 1h of recognition (shock, primary)",
    "37.7%", 
    safe(rate(rec_16[septic_shock_proxy == TRUE, 
                     rec_16_antimicrobial_within_1h])))

add("Rec 16", 
    "Strict two-sided [-1h,+1h] (shock)",
    "28.7%", 
    safe(rate(rec_16[septic_shock_proxy == TRUE, 
                     rec_16_antimicrobial_within_1h_strict])))

add("Rec 16", 
    "Within 3h (shock)",
    "45.8%", 
    safe(rate(rec_16[septic_shock_proxy == TRUE, 
                     rec_16_antimicrobial_within_3h])))

add("Rec 17", 
    "Within 1h of recognition (no shock, primary)",
    "28.8%", 
    safe(rate(rec_17[septic_shock_proxy == FALSE, 
                     rec_17_antimicrobial_within_1h])))

add("Rec 17", 
    "Strict two-sided [-1h,+1h] (no shock)",
    "18.9%", 
    safe(rate(rec_17[septic_shock_proxy == FALSE, 
                     rec_17_antimicrobial_within_1h_strict])))

add("Rec 17", 
    "Within 3h (no shock)",
    "37.3%", 
    safe(rate(rec_17[septic_shock_proxy == FALSE, 
                     rec_17_antimicrobial_within_3h])))

## onset-anchor stratification (isolates the tautological stratum)
add("Rec 16/17", 
    "Cohort culture-anchored / antibiotic-anchored",
    "85% / 15%", 
    safe({
      p <- antimicrobial_timing[, .N, 
                                by = onset_anchor] %>% 
        .[, .(onset_anchor, pct = 100 * N / sum(N))]
      
      paste(sprintf("%s %.1f%%", 
                    p$onset_anchor, 
                    p$pct), 
            collapse = "; ")
    }))

add("Rec 16", 
    "Adherence | antibiotic-anchored stratum (tautological)",
    "98.6%", 
    safe(rate(antimicrobial_timing[septic_shock_proxy == TRUE & 
                                     onset_anchor == "antibiotic-anchored",
                                   !is.na(time_to_antimicrobial) & 
                                     time_to_antimicrobial <= 1])))

add("Rec 17", "Adherence | antibiotic-anchored stratum (tautological)",
    "98.9%", 
    safe(rate(antimicrobial_timing[septic_shock_proxy == FALSE & 
                                     onset_anchor == "antibiotic-anchored",
                                   !is.na(time_to_antimicrobial) & 
                                     time_to_antimicrobial <= 1])))

add("Rec 16", 
    "Adherence | culture-anchored stratum (informative)",
    "20.7%", 
    safe(rate(antimicrobial_timing[septic_shock_proxy == TRUE & 
                                     onset_anchor == "culture-anchored",
                                   !is.na(time_to_antimicrobial) & 
                                     time_to_antimicrobial <= 1])))

add("Rec 17", 
    "Adherence | culture-anchored stratum (informative)",
    "19.4%", 
    safe(rate(antimicrobial_timing[septic_shock_proxy == FALSE & 
                                     onset_anchor == "culture-anchored",
                                   !is.na(time_to_antimicrobial) & 
                                     time_to_antimicrobial <= 1])))

add("Rec 16/17",
    "Shock vs no-shock antibiotic-anchored share",
    "21.9% vs 11.8%", 
    safe({
      
      a <- prop(antimicrobial_timing[septic_shock_proxy == TRUE, 
                                     onset_anchor == "antibiotic-anchored"])
      
      b <- prop(antimicrobial_timing[septic_shock_proxy == FALSE, 
                                     onset_anchor == "antibiotic-anchored"])
      
      paste(a, 
            "vs", 
            b)
    }))

add("Rec 16/17", 
    "Culture-anchored median recognition->antimicrobial interval",
    "+6.6 h", 
    safe({
      
      m <- antimicrobial_timing[onset_anchor == "culture-anchored", 
                                median(time_to_antimicrobial, 
                                       na.rm = TRUE)]
      
      sprintf("%.1f h", 
              m)
    }))


## 42 RECOMMENDATION - invasive or non-invasive BP monitoring ------------------
add("Rec 42",
    "Adherence (any BP measurement in window)",
    "98.6% (6,397/6,487)", 
    safe(rate(rec_42$rec_42_bp_monitoring)))

add("Rec 42", 
    "Sepsis-onset anchor (mis-anchored, for contrast)",
    "52.5%",
    NA, 
    note = "manual: historical sepsis-onset-anchored value")


## ASSEMBLE, FLAG DRIFT, PRINT, WRITE ------------------------------------------
figures <- rbindlist(.figrows)

## crude drift flag: compare the first numeric token of thesis vs computed
firstnum <- function(x) {
  
  as.numeric(sub(".*?(-?[0-9]+\\.?[0-9]*).*", 
                 "\\1", 
                 x))
}

figures[, match := {
  tv <- suppressWarnings(firstnum(thesis))
  cv <- suppressWarnings(firstnum(computed))
  fcase(
    is.na(computed) |
      computed == "",
    "-",
    grepl("^ERR", 
          computed),                        
    "ERR",
    is.na(tv) | 
      is.na(cv),                          
    "?",
    abs(tv - cv) <= 0.15,                           
    "OK",
    default = "CHECK"
  )
}]

print(figures[, .(section, 
                  description, 
                  thesis, 
                  computed, 
                  match)], 
      nrow = 200)

cat("\nDrift summary:\n")

print(figures[, .N, 
              by = match])


if (!dir.exists(here::here("Outputs"))) {
  
  dir.create(here::here("Outputs"), 
             recursive = TRUE)
  
}

## Save output as csv
fwrite(figures, 
       here::here("Outputs/thesis_figures_output.csv"))




sepsis_fc <- sepsis_base_cohort_data %>% 
  as_fc()

sepsis_fc %>% 
  fc_draw()

