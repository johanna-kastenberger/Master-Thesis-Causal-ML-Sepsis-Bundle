## Source global file
source(here::here("Code/global.R"))

DT_ssc_encoded <- readRDS(here::here("Data/DT_ssc_encoded.rds")) %>% 
  setDT()
  

## Recommendation 7
rec7_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 7, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_7_culture_before_antimicrobial,
             fill = rec_7_culture_before_antimicrobial)) +
  geom_bar() +
  theme_minimal()

## Recommendation 8
rec8_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 8, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_8_lactate_measured_at_suspicion,
             fill = rec_8_lactate_measured_at_suspicion)) +
  geom_bar() +
  theme_minimal()

## Recommendation 10
## septic shock OR sepsis-induced hypoperfusion
rec10_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 10, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_10_crystalloid_within_3h,
             fill = rec_10_crystalloid_within_3h)) +
  geom_bar() +
  theme_minimal()

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_10_eligibility,
             fill = rec_10_eligibility)) +
  geom_bar() +
  theme_minimal()

## Recommenation 12
## septic shock
rec12_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 12, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_12_vaso_before_cvc,
             fill = rec_12_vaso_before_cvc)) +
  geom_bar() +
  theme_minimal()

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_12_eligibility,
             fill = rec_12_eligibility)) +
  geom_bar() +
  theme_minimal()

## Recommendation 13
rec_13_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 13, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_13_map_target_65,
             fill = rec_13_map_target_65)) +
  geom_bar() +
  theme_minimal()

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_13_eligibility,
             fill = rec_13_eligibility)) +
  geom_bar() +
  theme_minimal()

## Recommendation 15
rec_15_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 15, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_15_icu_6h,
             fill = rec_15_icu_6h)) +
  geom_bar() +
  theme_minimal()

## Recommendation 16
rec_16_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 16, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_16_shock_antib_within_1h,
             fill = rec_16_shock_antib_within_1h)) +
  geom_bar() +
  theme_minimal()

## Recommendation 17
rec_17_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 17, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_17_sepsis_antib_within_1h,
             fill = rec_17_sepsis_antib_within_1h)) +
  geom_bar() +
  theme_minimal()

## Recommendation 43
rec_43_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 43, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_43_crystalloid_first_line,
             fill = rec_43_crystalloid_first_line)) +
  geom_bar() +
  theme_minimal()

## Recommendation 44
rec_44_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 44, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_44_balanced_over_saline,
             fill = rec_44_balanced_over_saline)) +
  geom_bar() +
  theme_minimal()

## Recommendation 45
rec_45_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 45, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_45_crystalloid_without_albumin,
             fill = rec_45_crystalloid_without_albumin)) +
  geom_bar() +
  theme_minimal()

## Recommendation 53
rec_53_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 53, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_53_first_vaso_norepi,
             fill = rec_53_first_vaso_norepi)) +
  geom_bar() +
  theme_minimal()

## Recommendation 55
rec_55_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 55, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_55_first_vaso_norepi,
             fill = rec_55_first_vaso_norepi)) +
  geom_bar() +
  theme_minimal()

## Recommendation 56
rec_56_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 56, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_56_vasopressin_after_norepi,
             fill = rec_56_vasopressin_after_norepi)) +
  geom_bar() +
  theme_minimal()

## Recommendation 57
rec_57_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 57, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_57_epinephrine_added,
             fill = rec_57_epinephrine_added)) +
  geom_bar() +
  theme_minimal()

## Recommendation 67
rec_67_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 67, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_67_first_oxygen_hfnc,
             fill = rec_67_first_oxygen_hfnc)) +
  geom_bar() +
  theme_minimal()

## Recommendation 68
rec_68_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 68, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_68_first_oxygen_hfnc_over_niv,
             fill = rec_68_first_oxygen_hfnc_over_niv)) +
  geom_bar() +
  theme_minimal()

## Recommendation 69
rec_69_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 69, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_69_hfnc_over_hfnc_niv_alternating,
             fill = rec_69_hfnc_over_hfnc_niv_alternating)) +
  geom_bar() +
  theme_minimal()

## Recommendation 71
rec_71_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 71, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_71_high_tidal_volume,
             fill = rec_71_high_tidal_volume)) +
  geom_bar() +
  theme_minimal()

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_71_low_tidal_volume,
             fill = rec_71_low_tidal_volume)) +
  geom_bar() +
  theme_minimal()

## Recommendation 72
rec_72_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 72, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_72_tidal_volume_6_8,
             fill = rec_72_tidal_volume_6_8)) +
  geom_bar() +
  theme_minimal()

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_72_tower_tidal_volume_4_to_less_6,
             fill = rec_72_tower_tidal_volume_4_to_less_6)) +
  geom_bar() +
  theme_minimal()

## Recommendation 79
rec_79_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 79, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_79_iv_corticosteroids,
             fill = rec_79_iv_corticosteroids)) +
  geom_bar() +
  theme_minimal()

## Recommendation 89
rec_89_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 89, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_89_active_fluid_removal,
             fill = rec_89_active_fluid_removal)) +
  geom_bar() +
  theme_minimal()

## Recommendation 90
rec_90_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 90, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_90_restrictive_transfusion,
             fill = rec_90_restrictive_transfusion)) +
  geom_bar() +
  theme_minimal()

## Recommendation 91
rec_91_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 91, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_91_early_enteral_nutrition,
             fill = rec_91_early_enteral_nutrition)) +
  geom_bar() +
  theme_minimal()

## Recommendation 92
rec_92_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 92, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_92_insulin_at_glucose_180,
             fill = rec_92_insulin_at_glucose_180)) +
  geom_bar() +
  theme_minimal()

## Recommendation 96
rec_96_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 96, rec_label]

DT_ssc_encoded %>% 
  copy() %>% 
  ggplot(aes(x = rec_96_sodium_bicarb,
             fill = rec_96_sodium_bicarb)) +
  geom_bar() +
  theme_minimal()


## Sepsis and/or septic shock recs  --------------------------------------------
rec7_label
rec8_label
rec12_label
rec_15_label
rec17_label
rec43_label
rec44_label
rec45_label
rec72_label
rec90_label
rec91_label
rec92_label


## Specifically Septic Shock
rec10_label
rec12_label
rec13_label
rec16_label
rec53_label
rec55_label
rec56_label
rec57_label
rec79_label
rec89_label


## Sepsis + other conditional statement
rec67_label
rec68_label
rec69_label
rec71_label



