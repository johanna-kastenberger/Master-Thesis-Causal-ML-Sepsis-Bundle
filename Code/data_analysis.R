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
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Adherent" = blue,
                               "Not adherent" = orange))


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
