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

## Hemodynamic Management

ssc_guidelines[rec_num == 42,
               `:=` (rec_label = "For adults with septic shock, we suggest using 
                     either invasive or non-invasive blood pressure monitoring.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]


ssc_guidelines[rec_num == 43,
               `:=` (rec_label = "For adults with sepsis or septic shock, we recommend 
                     using crystalloids as first-line fluid for resuscitation.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = "yes")]

ssc_guidelines[rec_num == 44,
               `:=` (rec_label = "For adults with sepsis or septic shock undergoing 
                     initial resuscitation, we suggest using balanced crystalloids 
                     over 0.9% saline.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 45,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     using crystalloids alone over crystalloids with supplemental 
                     albumin for fluid resuscitation.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 46,
               `:=` (rec_label = "For adults with sepsis or septic shock, we recommend 
                     against using starches for resuscitation.",
                     
                     rec_type = "strong recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 47,
               `:=` (rec_label = "For adults with sepsis and septic shock, we suggest 
                     against using gelatin for resuscitation.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 48,
               `:=` (rec_label = "For adults with sepsis or septic shock who have 
                     already received fluid resuscitation with 30 ml/kg and have 
                     persistent hypoperfusion, we suggest using either a liberal 
                     or a restrictive fluid resuscitation strategy based on individual 
                     patient and health system factors.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 49,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     using dynamic measures to guide initial fluid resuscitation over 
                     physical examination or static measures alone.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 50,
               `:=` (rec_label = "For adults with septic shock, there is insufficient 
                     evidence to make a recommendation on using minimally invasive or 
                     non-invasive cardiac output monitoring in addition to usual care.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 51,
               `:=` (rec_label = "For adults with sepsis and elevated lactate or
                     septic shock, we suggest using serial lactate measrements to 
                     guide resuscitation.",
                     
                     rec_use = "conditional recommendation",
                     
                     rec_type = NA_character_)]

ssc_guidelines[rec_num == 52,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     using capillary refill time to guide resuscitation as an adjunct
                     to other measures or perfusion.",
                     
                     rec_use = "conditional recommendation",
                     
                     rec_type = NA_character_)]

ssc_guidelines[rec_num == 53,
               `:=` (rec_label = "For adults with septic shock, we recommend using 
                     norepinephrine as the first-line agent over dopamine, epinephrine, 
                     or selepressin.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 54,
               `:=` (rec_label = "For adults with septic shock, we suggest against 
                     using terlipressin.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 55,
               `:=` (rec_label = "For adults with septic shock, we suggest using 
                     norepinephrine as the first-line agent over vasopressin or 
                     angiotensin II.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 56,
               `:=` (rec_label = "For adults with septic shock on escalating doses 
                     or norepinephrine, we suggest adding vasopressin.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 57,
               `:=` (rec_label = "For adults with septic shock and inadequate MAP 
                     levels despite norepinephrine and vasopressin, we suggest adding 
                     epinephrine.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 58,
               `:=` (rec_label = "For adults with septic shock with concomitant
                     cardiac dysfunction, we suggest using either norepinephrine 
                     or epinephrine as first line vasopressor.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 59,
               `:=` (rec_label = "For adults with refractory septic shock and escalating 
                     vasopressor requirements, there is insufficient evidence to make 
                     a recommendation on IV methylene blue.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 60,
               `:=` (rec_label = "For adults with septic shock and cardiac dysfunction
                     with persistent hypoperfusion despite adequate fluid status and 
                     arterial blood pressure, we suggest using inotropes over no 
                     inotropes.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 61,
               `:=` (rec_label = "For adults with septic shock with persistent
                     hypoperfusion and cardiac dysfunction despite adequate fluid 
                     resuscitation and arterial blood pressure, we suggest adding 
                     dobutamine or norepinephrine or using epinephrine alone.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 62,
               `:=` (rec_label = "For adults with shock and cardiac dysfunction 
                     with persistent hypoperfusion despite adequate volume status 
                     and arterial blood pressure, we suggest against using levosimendan.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 63,
               `:=` (rec_label = "For adults with septic shock and ongoing requirement 
                     for vasopressors, there is insufficient evidence to make a 
                     recommendation on use of oral midodrine.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 64,
               `:=` (rec_label = "For adults with septic shock, we suggest against 
                     using beta-blockers as a treatment for septic shock.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

## Respiratory Support

ssc_guidelines[rec_num == 65,
               `:=` (rec_label = "For adults with sepsis, we suggest measuring 
                     oxygenation by either pulse oximeter or arterial bloods gas 
                     in conjunction with physical examination and clinical acumen.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 66,
               `:=` (rec_label = "For adults with sepsis and acute hypoxemic respiratory 
                     failure, we suggest titrating FiO2 to target either higher, 
                     more liberal oxygen levels or lower, conservative oxygen levels 
                     depending on patient factors and resource limitations.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 67,
               `:=` (rec_label = "For adults with sepsis and acute hypoxemic respiratory 
                     failure, we suggest using high flow nasal cannula (HFNC) therapy 
                     over conventional oxygen therapy.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 68,
               `:=` (rec_label = "For adults with sepsis and acute hypoxemic respiratory 
                     failure, we suggest using HFNC as the initial therapy over 
                     non-invasive positive pressure ventilation.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 69,
               `:=` (rec_label = "For adults with sepsis and acute hypoxemic respiratory 
                     failure, we suggest using HFNC over high flow alternating with
                     non-invasive positive pressure ventilation.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 70,
               `:=` (rec_label = "For adults with sepsis and acute hypoxemic respiratory 
                     failure who are not intubated, we suggest a trial of awake proning.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 71,
               `:=` (rec_label = "For adults with sepsis and ARDS, we recommend 
                     using a low tidal volume ventilation strategy (6 ml/kg) over 
                     a high tidal volumn strategy (> 10 ml/kg).",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = "yes")]

ssc_guidelines[rec_num == 72,
               `:=` (rec_label = "For adults with sepsis acute hypoxemic respiratory 
                     failure without ARDS, we suggest using a tidal volume of 6-8 ml/kg 
                     IBW over a lower (4 to <6 ml/kg IBW) tidal volume.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 73,
               `:=` (rec_label = "For adults with sepsis and ARDS, we recommend 
                     using an upper limited goal for plateau pressure of 30 cm H20 
                     over higher plateau pressures.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 74,
               `:=` (rec_label = "For adults with sepsis and moderate-severe ARDS, 
                     we suggest using higher PEEP over lower PEEP.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 75,
               `:=` (rec_label = "For adults with sepsis and moderate-severe ARDS, 
                     we recommend against using an incremental PEEP titration strategy.",
                     
                     rec_type = "strong recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 76,
                `:=` (rec_label = "For adults with sepsis and moderate-severe ARDS, 
                      we suggest using prone ventilation for greater than 12 hours 
                      daily.",
                      
                      rec_type = "conditional recommendation",
                      
                      rec_use = NA_character_)]

ssc_guidelines[rec_num == 77,
               `:=` (rec_label = "For adults with sepsis and moderate-severe ARDS, 
                     we suggest using intermittent NMBA boluses over continuous NMBA 
                     infusion.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 78,
               `:=` (rec_label = "For adults with severe ARDS due to sepsis, we 
                     suggest using veno-venous ECMO when conventional mechanical 
                     ventilation fails in experienced centers with infrastructure 
                     to support its use.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

## Additional and Adjunctive Therapies

ssc_guidelines[rec_num == 79,
               `:=` (rec_label = "For adults with septic shock, we suggest using 
                     intravenous corticosteroids.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 80,
               `:=` (rec_label = "For adults with sepsis or septic shock and fever, 
                     we suggest against the use of antipyretic therapy, either 
                     pharmacologic or surface cooling, for the purpose of improving
                     clinical outcomes.",
                     
                     rec_type = "conditonal negative recommendation",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 81,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     against using IV vitamin C.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 82,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                      against using intravenous immunoglobulins.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 83,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest
                     against using blood purification therapies, including hemoperfusion, 
                     high-dose hemofiltration, or plasma exchange.",
                     
                     rec_type = "conditional reccomendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 84,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest
                     against using polymyxin B hemoperfusion.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 85,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     against the use of Vitamin D therapy for sepsis treatment.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative reccomendation")]

ssc_guidelines[rec_num == 86,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     against using XueBiJing injection outside of jurisdiction where 
                     it has regulatory approval.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 87,
               `:=` (rec_label = "For adults with sepsis or septic shock, and who 
                     have risk factors for GI bleeding, we suggest the use of stress
                     ulcer prophylaxis with the use of proton-pump inhibitors versus 
                     not using stress ulcer prophylaxis.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 88,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest
                     against using probiotics.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 89,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     against using probiotics.",
                     
                     rec_type = "conditonal recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 90,
               `:=` (rec_label = "For adults with sepsis or septic shock, we recommend
                     using a restrictive transfusion strategy over a liberal  transfusion 
                     strategy.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 91,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     early (within 72 hours) initiation of enteral nutrition.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 92,
               `:=` (rec_label = "For adults with sepsis or septic shock, we recommend 
                     initiating insulin therapy at a glucose level of >= 180 mg/dL 
                     (10 mmol/L).",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 93,
               `:=` (rec_label = "For adults with sepsis or septic shock and acute 
                     kidney injury, with no definitive indication for renal replacement 
                     therapy, we suggest against using renal replacement therapy.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 94,
               `:=` (rec_label = "For adults with sepsis or septic shock and acute 
                     kidney injury warranting renal replacement therapy, we suggest
                     either continuous or intermittent renal replacement therapy.",
                     
                     rec_type = "none",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 95,
               `:=` (rec_label = "For adults with septic shock and hypoperfusion-induced
                     lactic acidemia, we suggest against using sodium bicarbonate therapy 
                     to improve hemodynamics or to reduce vasopressor requirements.",
                     
                     rec_type = "conditional recommendation against",
                     
                     rec_use = "no, negative recommendation")]

ssc_guidelines[rec_num == 96,
               `:=` (rec_label = "For adults with septic shock, severe metabolic 
                     acidemia (pH <= 7.2), and acute kidney injury (AKIN score 2 or 3), 
                     we suggest using sodium bicarbonate therapy.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 97,
               `:=` (rec_label = "For adults with sepsis or septic shock, we recommend
                     using pharmacologic venous thromboembolism (VTE) prohylaxis unless 
                     a contraindication exists.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 98,
               `:=` (rec_label = "For adults with sepsis or septic shock, we recommend 
                     using low molecular weight heparin over unfractionated heparin 
                     for VTE prophylaxis.",
                     
                     rec_type = "strong recommendation",
                     
                     rec_use = NA_character_)]

ssc_guidelines[rec_num == 99,
               `:=` (rec_label = "For adults with sepsis or septic shock, we suggest 
                     using pharmacological VTE prophylaxis alone over pharmacological
                     VTE prophylaxis plus mechanical VTE prophylaxis.",
                     
                     rec_type = "conditional recommendation",
                     
                     rec_use = NA_character_)]






