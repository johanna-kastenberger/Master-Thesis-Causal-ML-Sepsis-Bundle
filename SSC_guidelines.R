## Source global
source(here::here("global.R"))

rec_num <- c(1:107)
rec_label <- rep(NA_character_, 107)
rec_type <- rep(NA_character_, 107)
rec_use <- rep(NA_character_, 107)

ssc_guidelines <- data.table(rec_num,
                             rec_label,
                             rec_use) %>% 
  .[rec_num %in% 1:15, rec_cat := "screening and early managament"] %>% 
  .[rec_num %in% 16:41, rec_cat := "infection"] %>% 
  .[rec_num %in% 42:64, rec_cat := "hemodynamic management"] %>% 
  .[rec_num %in% 65:78, rec_cat := "respiratory support"] %>% 
  .[rec_num %in% 79:99, rec_cat := "additional and adjunctive therapies"] %>% 
  .[rec_num %in% 100:107, rec_cat := "goals of care"]

ssc_guidelines[rec_num == 1,
               `:=` (rec_label = "For hospitals and health systems, we recommend using 
               a performance improvement program for sepsis, including sepsis screening 
               for acutely ill, high-risk patients; standard operating procedures 
               for treatment; and implementation of spesis quality improvement strategies.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = "no")] 

ssc_guidelines[rec_num == 2,
               `:=` (rec_label = "For hospitals and health systems, we suggest using a 
               'code sepsis' or 'sepsis huddle' protocol over not using such a protocol.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 3,
               `:=` (rec_label = "In acutely ill adults en route to hospital by ambulance 
               or flight, we suggest using a standard sepsis screening tool over 
               not using a screening tool.",
                     
                     rec_type = "conditional recommendation")]

ssc_guidelines[rec_num == 4,
               `:=` (rec_label = "For acutely ill patients in hospital, we recommend 
               using NEWS, NEWS2, MEWS, or SIRS over qSOFA as a single tool to 
               screen for sepsis.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 5,
               `:=` (rec_label = "Sepsis is a clinical diagnosis and should not 
                     be ruled in or ruled out using a single biomarker or diagnostic test.",
                     
                     rec_type = "good practice statement",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 6,
               `:=` (rec_label = "There is insufficient evidence to make a recommendation 
                     regarding use of novel rapid host response diagnostics.",
                     
                     rec_type = "insufficient evidence",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 7,
               `:=` (rec_label = "For adults with possible, probable, or definite 
                     sepsis or septic shock, we recommend collecting blood cultures 
                     as soon as possible and ideally before the administration of 
                     antimicrobial therapy.",
                     
                     rec_type = "strong recommendation")]

ssc_guidelines[rec_num == 8,
               `:=` (rec_label = "For adults with possible or probable sepsis or 
                     septic shock, we suggest measuring blood lactate.",
                     
                     rec_type = "conditional recommendation")]

ssc_guidelines[rec_num == 9,
               `:=` (rec_label = "Sepsis and septic shock are medical emergencies; 
                     treatment and resuscitation should begin immediately.",
                     
                     rec_type = "good practice statement",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 10,
               `:=` (rec_label = "For adults with sepsis-induced hypoperfusion or 
                     septic shock, we suggest administering at least 30ml/kg of 
                     intravenous crystalloid in the first 3 hours.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = "yes")]

ssc_guidelines[rec_num == 11,
               `:=` (rec_label = "For adults with sepsis-induced hypotension, we 
                     suggest initial intravenous crystalloid fluid bolus resuscitation 
                     followed by vasopressor support if hypotension persists.",
                     
                     rec_type = "conditional recommendation")]

ssc_guidelines[rec_num == 12,
               `:=` (rec_label = "In adults with septic shock, we suggest starting 
                     vasopressors peripherally to restore mean arterial pressure 
                     rather than delaying initiation until central venous access is secured.",
                     
                     rec_type = "conditional recommendation")]

ssc_guidelines[rec_num == 13,
               `:=` (rec_label = "For adults with septic shock, we recommend an 
                     initial MAP target of 65 mm Hg over higher MAP targets.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = "yes")]

ssc_guidelines[rec_num == 14,
               `:=` (rec_label = "For adults with septic shock aged 65 years or 
                     older, we suggest an initial MAP range of 60-65 mm Hg over 
                     higher ranges.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = "yes")]

ssc_guidelines[rec_num == 15,
               `:=` (rec_label = "For adults with sepsis or septic shock who require 
                     ICU admission, we suggest admitting the patients to the ICU 
                     within 6 hours.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = "yes")]

ssc_guidelines[rec_num == 16,
               `:=` (rec_label = "For adults with possible, or definite septic shock, 
                     we recommend administering antimicrobial therapy immediately, 
                     ideally within 1 hour of recognition",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = "yes")]

ssc_guidelines[rec_num == 17,
               `:=` (rec_label = "For adults with probable or definite sepsis, 
                     we recommend administering antimicrobial therapy immediately, 
                     ideally within 1 hour of recognition.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = "yes")]

ssc_guidelines[rec_num == 18,
               `:=` (rec_label = "For adults with possible sespsi without shock, 
                     we suggest a time-limited course of rapid investigation and 
                     if concern for infection persists, the administration of 
                     antimicrobial therapy within 3 hours from the time when sepsis 
                     was first suspected.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 19,
               `:=` (rec_label = "Clinicians should perform a rapid assessment of 
                     the likelihood of infectious versus noninfectious causes of 
                     acute illness in adults with possible sepsis without shock.",
                     
                     rec_type = "good practice statement",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 20,
               `:=` (rec_label = "For adults with a low likelihood of infection 
                     and without shock, we suggest deferring antimicrobial therapy 
                     while continuing to closely monitor the patient.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 21,
               `:=` (rec_label = "For adults with definite or probable sepsis and 
                     hypotension (i.e., septic shock) and who have an anticipated 
                     time to in-hospital medical evaluation of over 60 minutes, 
                     we suggest administering antimicrobial therapy in ambulance 
                     or flight.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 22,
               `:=` (rec_label = "For adults with possible or probable sepsis or septic shock, we suggest using clinical evaluation alone over procalcitonin plus clinical evaluation to decide whether to start antimicrobial therapy.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 23,
                    `:=` (rec_label = "Adults with sepsis or septic shock should be 
                     rapidly evaluated for specific anatomical diagnoses or sources 
                     of infection that require emergent source control.",
                     
                     rec_type = "good practice statement",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 24,
               `:=` (rec_label = "For adults with sepsis or septic shock and a specific 
                     anatomical diagnosis or source of infection that requires source 
                     control, we suggest early source control over late source control, 
                     ideally within 6 hours.",
                     
                     rec_type = "conditional recommendation",
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 25,
               `:=` (rec_label = "For adults with sepsis or septic shock at high 
                     risk of infection with a specific multidrug resistant (MDR) 
                     pathogen, we suggest using empirical antimicrobial therapy 
                     with coverage for this MDR pathogen.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 26,
               `:=` (rec_label = "For adults with sepsis or septic shock at low 
                     risk of infeciton with a specific multidrug resistant (MDR) 
                     pathogen, we suggest against using empirical antimicrobial 
                     therapy with coverage for this MDR pathogen.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 27,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     against empirical antifungal therapy.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 28,
               `:=` (rec_label = "For adults with sepsis or septic shock without 
                     risk for anaerobic infection, we suggest using an empiric 
                     antibiotic regimen without anaerobic coverage.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 29,
               `:=` (rec_label = "For adults with sepssi or septic shock with 
                     specific risk factors for anaerobic infection, we suggest an 
                     empiric antibiotic regimen that includes anaerobic coverage.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 30,
               `:=` (rec_label = "There is insufficient evidence to make a recommendation 
                     regarding use of departmental (i.e., ICU-wide) microbiological 
                     surveillance samples on the upper respiratory tract to guide 
                     empirical antimicrobial therapy.",
                     
                     rec_type = "insufficient evidence",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 31,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     using pathogen-specific rapid diagnostic tests on a case-by-case 
                     basis in selected patients based on clinical features, local 
                     pathogen- and resistance patterns, seasonality, and availability 
                     of tests and antibiotic stewardship of guidance.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 32,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     against use of Candida fungal biomarkers to guide initiation of 
                     empiric antifungal therapy.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 33,
               `:=` (rec_label = "For adults with sepsis or septic shock, we recommend 
                     using prolonged infusion of beta-lactams for maintenance (after 
                     an initial loading dose) over bolus administration.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 34,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     using antimicrobial therapeutic drug monitoring (TDM) on a 
                     case-by-case basisin selected patients, based on clinical features, 
                     local pathogen- and resistance patterns, drug class, and availability 
                     of TDM.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 35,
               `:=` (rec_label = "Clinicians should continuously reevaluate patients, 
                     search for alternative diagnoses, and discontinue empiric 
                     antimicrobial therapy if an alternative cause of illness is 
                     demonstrated or strongly suspected in adults with suspected 
                     sepsis or septic shock but unconfirmed infection.",
                     
                     rec_type = "good practice statement",
                     
                     rec_use = "no")]

ssc_guidelines[rec_num == 36,
               `:=` (rec_label = "For adults with sepsis or septic shock, we 
                     recommend de-escalation of antimicrobial therapy over no 
                     de-escalation when a confirmed microbiological diagnosis 
                     and susceptibility profile is available.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 37,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     de-escalation of antimicrobial therapy over no de-escalation 
                     when no pathogens are identified on final culture results.",
                     
                     rec_type = "conditional reccomendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 38,
               `:=` (rec_label = "For adults with sepsis or septic shock who are 
                     receiving empiric antifungal therapy, we suggest against use 
                     of Candida fungal biomarkers to guide discontinuation of empiric 
                     antifungal therapy.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 39,
               `:=` (rec_label = "For adults with an initial diagnosis of sepsis 
                     or septic shock and adequate source control, we suggest using 
                     shorter over longer duration of antimicrobial therapy.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 40,
               `:=` (rec_label = "For adults with an initial diagnosis of sepsis 
                     or septic shock and adequate source control where optimal 
                     duration of therapy is unclear, we suggest using procalcitonin 
                     AND clinical evaluation to decide when to discontinue antimicrobial 
                     therapy over clinical evaluation alone.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 41,
               `:=` (rec_label = "In mechanically ventilated adults with sepsis or 
                     septic shock in units with a low prevalence of antimicrobial 
                     resistance, we suggest using selective decontamination of the 
                     digestive tract (SDD).",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]



