## Source global file
source(here::here("Code/global.R"))

DT_ssc_encoded <- readRDS(here::here("Data/DT_ssc_encoded.rds")) %>% 
  setDT()

## Adherence columns
adh_cols <- names(DT_ssc_encoded) %>% 
  .[grepl("^rec_", 
          names(DT_ssc_encoded)) &
      vapply(DT_ssc_encoded, 
             is.logical,
             logical(1))]


## lonog format, eligible patients only
long <- melt(DT_ssc_encoded,
             id.vars = c("subject_id",
                         "hadm_id",
                         "stay_id"),
             measure.vars = adh_cols,
             variable.name = "recommendation",
             value.name = "adherent") %>% 
  .[!is.na(adherent)]


counts <- long %>% 
  copy() %>% 
  .[, .N, by = .(recommendation,
                 adherent)] %>% 
  .[, status := factor(adherent,
                       levels = c(TRUE,
                                  FALSE),
                       labels = c("Adherent",
                                  "Not adherent"))]

counts %>% 
  copy() %>% 
  ggplot(aes(status,
             N,
             fill = status)) +
  geom_col() +
  facet_wrap(~ recommendation,
             scales = "free_y") +
  labs(x = NULL,
       y = "Eligible patients") +
  theme_minimal() +
  theme(legend.position = "none")


## Functions to create adherance & eligbility barplots ---------------------------------------
create_barplot_adherance <- function(.data,
                           .rec,
                           .title) {
  
  .data %>% 
    copy() %>% 
    .[, plot_var := get(.rec)] %>% 
    .[, plot_var := factor(plot_var,
                      levels = c(TRUE,
                                 FALSE,
                                 as.logical(NA)))] %>%
    .[order(plot_var)] %>%
    ggplot(aes(x = plot_var,
               fill = plot_var,
               color = plot_var)) +
    geom_bar() +
    geom_text(
      color = "black",
      group = 1,
      stat = "count",
      aes(label = scales::percent(after_stat(count / sum(count)),
                                  accuracy = 0.1)),
      vjust = -0.3
    ) +
    theme_minimal() +
    scale_fill_manual(
      name = "",
      values = c("TRUE" = pastel.blue,
                 "FALSE" = pastel.orange),
      na.value = "lightgrey") +
    scale_color_manual(
      values = c("TRUE" = blue,
                 "FALSE" = orange),
      na.value = "grey",
      guide = "none"
    ) +
    labs(x = "",
         y = "N",
         title = .title,
         caption = paste("Number of stays: ", .data[, uniqueN(stay_id)])) +
    theme_minimal()
}

create_barplot_eligibility <- function(.data,
                                     .rec,
                                     .title,
                                     .fill_values,
                                     .color_values) {
  
  .data %>% 
    copy() %>% 
    .[, plot_var := get(.rec)] %>% 
    ggplot(aes(x = plot_var,
               fill = plot_var,
               color = plot_var)) +
    geom_bar() +
    geom_text(
      color = "black",
      group = 1,
      stat = "count",
      aes(label = scales::percent(after_stat(count / sum(count)),
                                  accuracy = 0.1)),
      vjust = -0.3
    ) +
    theme_minimal() +
    scale_fill_manual(
      name = "",
      values = .fill_values) +
    scale_color_manual(
      values = .color_values,
      guide = "none"
    ) +
    labs(x = "",
         y = "N",
         title = .title,
         caption = paste("Number of stays: ", .data[, uniqueN(stay_id)])) +
    theme_minimal()
}


## Adherance barplots ----------------------------------------------------------
### 7 Recommendation -----------------------------------------------------------
rec7_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 7, rec_label]

create_barplot_adherance(.data = rec_7,
               .rec = "blood_culture_collected",
               .title = "Recommendation 7 - Blood culture collected")

create_barplot_adherance(.data = DT_ssc_encoded,
               .rec = "rec_7_culture_before_antimicrobial",
               .title = "Recommendation 7 - Blood culture collected before antimicrobial therapy")


### 8 Recommendation 8 ---------------------------------------------------------
rec8_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 8, rec_label]

create_barplot_adherance(.data = DT_ssc_encoded,
               .rec = "rec_8_lactate_measured",
               .title = "Recommendation 8 - Lactate measured")

create_barplot_adherance(.data = DT_ssc_encoded,
               .rec = "rec_8_lactate_measured_within_1h",
               .title = "Recommendation 8 - Lactate measured within 1h of sepsis onset")

create_barplot_adherance(.data = DT_ssc_encoded,
                         .rec = "rec_8_lactate_measured_within_3h",
                         .title = "Recommendation 8 - Lactate measured within 3h of sepsis onset")

create_barplot_adherance(.data = DT_ssc_encoded,
                         .rec = "rec_8_lactate_measured_within_6h",
                         .title = "Recommendation 8 - Lactate measured within 6h of sepsis onset")

rec_8 %>% 
  copy() %>% 
  ggplot(aes(x = time_to_sepsis)) +
  geom_histogram(color = "mediumblue",
                 fill = "lightblue") +
  scale_x_continuous(limits = c(-1, 6),
                     breaks = seq(-1, 6, 2)) +
  theme_minimal() +
  labs(x = "Time from sepsis onset to lactate measurement",
       y = "N")

### 10 Recommendation ----------------------------------------------------------
## septic shock OR sepsis-induced hypoperfusion
## currently only septic shock --> revisit
rec10_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 10, rec_label]

create_barplot_eligibility(.data = DT_ssc_encoded,
               .rec = "rec_10_eligibility",
               .title = "Recommendation 10 elibility - 30mL/kg crystalloid within first 3h",
               .fill_values = c("eligible" = pastel.blue,
                                "missing weight" = pastel.orange,
                                "no sepsis-induced hypoperfusion or unknown" = "lightgrey"),
               .color_values = c("eligible" = blue,
                                 "missing weight" = orange,
                                 "no sepsis-induced hypoperfusion or unknown" = "grey"))

create_barplot_adherance(.data = DT_ssc_encoded[hypoperfusion_proxy == TRUE],
                         .rec = "rec_10_crystalloid_within_3h",
                         .title = "Recommendation 10 - 30mL/kg crystalloid within first 3h")

create_barplot_adherance(.data = DT_ssc_encoded[hypoperfusion_proxy == TRUE],
                         .rec = "rec_10_crystalloid_within_3h_bolus",
                         .title = "Recommendation 10 - 30mL/kg crystalloid within first 3h (bolus fluid only)")


### 12 Recommenation -----------------------------------------------------------
## septic shock
rec12_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 12, rec_label]

create_barplot_eligibility(.data = DT_ssc_encoded,
                         .rec = "rec_12_eligibility",
                         .title = "Recommendation 12 eligibility - Peripheral vasopressors before central venous access",
                         .fill_values = c("eligible" = pastel.blue,
                                          "no septic shock or unknown" = pastel.orange),
                         .color_values = c("eligible" = blue,
                                           "no septic shock or unknown" = orange))

create_barplot_adherance(.data = DT_ssc_encoded[septic_shock_proxy == TRUE],
                         .rec = "rec_12_vaso_without_central_delay",
                         .title = "Recommendation 12 - Peripheral vasopressors before central venous access")


### 13 Recommendation ----------------------------------------------------------
rec_13_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 13, rec_label]

create_barplot_eligibility(.data = DT_ssc_encoded,
                           .rec = "rec_13_eligibility",
                           .title = "Recommendation 13 eligibility - initial map target of 65mmHG",
                           .fill_values = c("eligible" = pastel.blue,
                                            "no septic shock or unknown" = pastel.orange),
                           .color_values = c("eligible" = blue,
                                             "no septic shock or unknown" = orange))

create_barplot_adherance(.data = DT_ssc_encoded[septic_shock_proxy == TRUE],
                         .rec = "rec_13_map_target_65",
                         .title = "Recommendation 13 - initial map target of 65mmHG")


### 15 Recommendation ----------------------------------------------------------
rec_15_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 15, rec_label]

create_barplot_adherance(.data = DT_ssc_encoded,
                         .rec = "rec_15_icu_6h",
                         .title = "Recommendation 15 - ICU admission within 6h")


### 16 & 17 Recommendation ----------------------------------------------------------
rec_16_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 16, rec_label]

create_barplot_adherance(.data = DT_ssc_encoded[septic_shock_proxy == TRUE],
                         .rec = "rec_16_antimicrobial_within_3h",
                         .title = "Recommendation 16 - antimicrobial therapy within 3h for septic shock patients")

create_barplot_adherance(.data = DT_ssc_encoded[septic_shock_proxy == TRUE],
                         .rec = "rec_16_antimicrobial_within_1h",
                         .title = "Recommendation 16 - antimicrobial therapy within 1h for septic shock patients")

create_barplot_adherance(.data = DT_ssc_encoded[septic_shock_proxy == FALSE],
                         .rec = "rec_17_antimicrobial_within_3h",
                         .title = "Recommendation 17 - antimicrobial therapy within 3h for sepsis patients without shock")

create_barplot_adherance(.data = DT_ssc_encoded[septic_shock_proxy == FALSE],
                         .rec = "rec_17_antimicrobial_within_1h",
                         .title = "Recommendation 17 - antimicrobial therapy within 1h for sepsis patients without shock")

### 42 Recommendation ----------------------------------------------------------
rec_42_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 42, rec_label]

create_barplot_eligibility(.data = DT_ssc_encoded,
                           .rec = "rec_42_eligibility",
                           .title = "Recommendation 42 eligibility - Invasive or non-invasive blood pressure monitoring for septic shock patients",
                           .fill_values = c("eligible" = pastel.blue,
                                            "no septic shock or unknown" = pastel.orange),
                           .color_values = c("eligible" = blue,
                                             "no septic shock or unknown" = orange))

create_barplot_adherance(.data = DT_ssc_encoded[septic_shock_proxy == TRUE],
                         .rec = "rec_42_bp_monitoring",
                         .title = "Recommendation 42 - Invasive or non-invasive blood pressure monitoring for septic shock patients")


### 43 Recommendation ----------------------------------------------------------
rec_43_label <- ssc_recommendations %>% 
  copy() %>% 
  .[rec_num == 43, rec_label]

create_barplot_eligibility(.data = DT_ssc_encoded,
                           .rec = "rec_43_eligibility",
                           .title = "Recommendation 43 eligibility - Crystalloids as first-line fluid for resuscitation",
                           .fill_values = c("eligible" = pastel.blue,
                                            "no initial fluid resuscitation within 3h" = pastel.orange),
                           .color_values = c("eligible" = blue,
                                             "no initial fluid resuscitation within 3h" = orange))

create_barplot_adherance(.data = DT_ssc_encoded,
                         .rec = "rec_43_crystalloid_first_line",
                         .title = "Recommendation 43 - Crystalloids as first-line fluid for resuscitation")

### 44 Recommendation ----------------------------------------------------------
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


## -----------------------------------------------------------------------------
round(prop.table(table(DT_ssc_encoded$rec_7_culture_before_antimicrobial)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_8_lactate_measured_at_suspicion)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_10_crystalloid_within_3h)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_12_vaso_before_cvc)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_13_map_target_65)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_15_icu_6h)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_16_17_antib_within_3h)) * 100, 2)
round(prop.table(table(DT_ssc_encoded$rec_16_17_antib_within_1h)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_42_bp_monitoring)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_43_crystalloid_first_line)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_44_balanced_over_saline)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_45_crystalloid_no_albumin)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_53_first_vaso_norepi)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_55_first_vaso_norepi)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_56_vasopressin_after_escalating_norepi)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_57_epi_after_escalating_norepi_vaso)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_67_first_oxygen_hfnc)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_68_first_oxygen_hfnc_over_niv)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_69_hfnc_over_hfnc_niv_alternating)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_79_iv_corticosteroids)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_89_active_fluid_removal)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_90_restrictive_transfusion)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_91_early_enteral_nutrition)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_92_insulin_started_at_glucose_180)) * 100, 2)

round(prop.table(table(DT_ssc_encoded$rec_96_sodium_bicarb)) * 100, 2)
