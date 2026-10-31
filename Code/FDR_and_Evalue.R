
source(here::here("Code/global.R"))

## FDR and E-value sensitivity -------------------------------------------------

res <- fread(here::here("Outputs/dml_irm_results_py.csv")) %>% 
  setDT() %>% 
  ## Benjamini-Hochberg FDR across the full-tier tests
  .[tier == "full", q_bh := p.adjust(pval,
                                       method = "BH")]

## E-value from the risk difference, via the approximate risk ratio
## RR = (p0 + ate) / p0, then VanderWeele-Ding
evalue <- function(p0, 
                   est) {
  
  p1 <- p0 + est
  if(is.na(p0) || p0 <= 0 || p1 <= 0) return(NA_real_)
  rr <- p1 / p0
  if (rr < 1) rr <- 1 / rr
  rr + sqrt(rr * (rr - 1))
}

res <- res %>% 
  copy() %>% 
  .[tier == "full", E_point := mapply(evalue,
                                      p0,
                                      ate)] %>% 
  ## E-value for the confidence bound nearest the null (1 if the CI crosses it)
  .[tier == "full", E_ci := mapply(function(p0,
                                            lo,
                                            hi,
                                            a) {
    if (lo <= 0 &
        hi >= 0) return(1)
    evalue(p0, if(a < 0) hi else lo)
  }, p0,
  ci_lo,
  ci_hi,
  ate)]

fwrite(res, here::here("Outputs/dml_irm_results_expanded.csv"))


## Results tables --------------------------------------------------------------
## Labels
rec_levels <- c("7",
                "8",
                "10",
                "15",
                "16",
                "17",
                "42",
                "43",
                "44",
                "45",
                "53/55",
                "56",
                "57",
                "67",
                "68",
                "69",
                "71",
                "73",
                "79",
                "89",
                "90",
                "91",
                "92",
                "96",
                "98")

### SSC Adherence Table --------------------------------------------------------
rectext <- c(
  "7"="Blood culture before antimicrobials",
  "8"="Lactate measured within 1\\,h",
  "10"="Crystalloid 30\\,mL/kg within 3\\,h",
  "13"="Initial MAP target of 65\\,mmHg",
  "14"="Initial MAP target of 60--65\\,mmHg (for patients aged 65 and older)",
  "15"="ICU admission within 6\\,h",
  "16"="Antimicrobial within 1\\,h (for septic shock)",
  "17"="Antimicrobial within 1\\,h (for sepsis without shock)",
  "42"="Blood pressure monitoring (invasive or non-invasive)",
  "43"="Crystalloid as first-line resuscitation fluid",
  "44"="Resuscitation fluid volume (balanced crystalloid over 0.9\\% saline)",
  "45"="Resuscitation fluid volume (crystalloid over albumin)",
  "53"="Norepinephrine first-line (over epinephrine, dopamine)",
  "55"="Norepinephrine first-line (over vasopressin, angiotensin)",
  "56"="Vasopressin added to escalating norepinephrine",
  "57"="Epinephrine added to norepinephrine and vasopressin",
  "67"="HFNC over conventional oxygen",
  "68"="HFNC over non-invasive ventilation initially",
  "69"="HFNC over alternating HFNC and NIV",
  "71"="Lower tidal volume ventilation strategy",
  "73"="Lower plateau pressure in ARDS",
  "79"="Intravenous corticosteroids",
  "89"="Active fluid removal",
  "90"="Restrictive transfusion threshold",
  "91"="Early enteral nutrition",
  "92"="Insulin at glucose $\\geq$ 180\\,mg/dL",
  "96"="Sodium bicarbonate for severe acidemia and AKI 2/3",
  "98"="Venous thromboembolism prophylaxis (LMWH preferred over UFH)")

adherence <- fread(here::here("Outputs/adherence_summary.csv")) %>% 
  setDT() %>% 
  .[, recommendation := rectext[as.character(rec)]] %>% 
  .[, rec_order := match(as.character(rec),
                         rec_levels)] %>% 
  .[order(rec_order)] %>% 
  .[, row := sprintf("%s & %s & %s & %.1f \\\\",
                     rec, recommendation, formatC(eligible_n, big.mark=",", format="d"),
                     adherence)]


tab_adherence <- c(
  "\\begin{table}[p]", 
  "  \\centering",
  "  \\caption{Adherence rates of encoded recommendations.}", 
  "  \\label{tab:ssc-adherence}",
  "  \\begin{tabular}{@{}l p{8.5cm} c c@{}}", 
  "    \\toprule",
  "    Rec. & Recommendation & Eligible $n$ & Adherence (\\%) \\\\",
  "    \\midrule",
  paste0("    ", 
         adherence$row),
  "    \\bottomrule",
  "  \\end{tabular}",
  "\\end{table}")

writeLines(tab_adherence, here::here("Outputs/adherence_table.tex"))

### Propensity screening table -------------------------------------------------
max.frac.outside <- 0.10
min.arm <- 100

labels <- c(
  "7"="Culture before antimicrobial", 
  "8"="Lactate within 1\\,h",
  "10"="Crystalloid 30\\,mL/kg within 3\\,h", 
  "15"="ICU admission within 6\\,h",
  "16"="Antimicrobial within 1\\,h (shock)", 
  "17"="Antimicrobial within 1\\,h (sepsis)",
  "42"="Blood pressure monitoring", 
  "43"="Crystalloid first-line",
  "44"="Balanced over saline", 
  "45"="Crystalloid over albumin",
  "53/55"="Norepinephrine first-line", 
  "56"="Vasopressin added",
  "57"="Epinephrine added",
  "67"="HFNC over conventional oxygen", 
  "68"="HFNC over NIV initially",
  "69"="HFNC over alternating", 
  "71"="Low tidal volume", 
  "73"="Plateau pressure limit",
  "79"="Intravenous corticosteroids", 
  "89"="Active fluid removal",
  "90"="Restrictive transfusion", 
  "91"="Early enteral nutrition",
  "92"="Insulin at glucose $\\geq$ 180",
  "96"="Sodium bicarbonate", 
  "98"="LMWH over UFH")

ps <- fread(here::here("Outputs/propensity_overlap_py.csv")) %>% 
  .[tier == "full"] %>% 
  .[, rec := fifelse(grepl("^rec_53_55", 
                           treat_col),
                     "53/55",
                     sub("^rec_([0-9]+)_.*$", 
                         "\\1", 
                         treat_col))] %>% 
  .[, frac_outside := frac_below_05 + frac_above_95] %>% 
  .[, min_arm := pmin(n_treated,
                      n - n_treated)] %>% 
  .[, adequate := frac_outside <= max.frac.outside &
      min_arm >= min.arm] %>% 
  .[, size_only := frac_outside <= max.frac.outside &
      min_arm < min.arm] %>% 
  .[, label := labels[rec]] %>% 
  .[, rec_ord := match(rec,
                       rec_levels)] %>% 
  .[order(rec_ord)] %>% 
  .[, causal := fifelse(adequate,
                        "Yes",
                        fifelse(
                          size_only,
                          "No\\tnote{a}",
                          "No"))] %>% 
  .[, row := sprintf("%s & %s & %s & %s & %.2f & %.2f & %.1f & %s \\\\",
                      rec, label,
                      formatC(n,         big.mark = ",", format = "d"),
                      formatC(n_treated, big.mark = ",", format = "d"),
                      ps_p05, ps_p95, 100 * frac_outside, causal)]

size_n <- as.integer(ps[size_only == TRUE, 
                        n_treated])[1]

tab_propensity <- c(
  "\\begin{table}[p]",
  "  \\centering", 
  paste0("  \\caption{Propensity-score $\\hat e(x)$ overlap screen. The columns ",
         "$\\hat e_{5}$ and $\\hat e_{95}$ are the 5th and 95th percentiles of the ",
         "propensity score, and \\textit{Outside} is the share of patients whose ",
         "score falls outside the $[0.05, 0.95]$ interval.}"),
  "  \\label{tab:propensity-screen}",
  "  \\small",
  "  \\begin{threeparttable}",
  "  \\begin{tabular}{llcccccc}", 
  "    \\toprule",
  "    Rec. & Treatment indicator & $n$ & Treated & $\\hat e_{5}$ & $\\hat e_{95}$ & Outside (\\%) & Causal \\\\",
  "    \\midrule",
  paste0("    ", 
         ps$row),
  "    \\bottomrule",
  "  \\end{tabular}",
  "  \\begin{tablenotes}\\footnotesize",
  sprintf("    \\item[a] Excluded by the group-size condition rather than overlap, with only %d patients receiving the adherent treatment.", size_n),
  "  \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}")

writeLines(tab_propensity, here::here("Outputs/propensity_table.tex"))


### Causal ML results table ----------------------------------------------------
res_expanded <- fread(here::here("Outputs/dml_irm_results_expanded.csv")) %>% 
  setDT() %>% 
  .[, rec := fifelse(grepl("^rec_53_55", 
                         treat_col), 
                   "53/55",
                     sub("^rec_([0-9]+)_.*$",
                         "\\1", 
                         treat_col))] %>% 
  .[, outcome_lab := fcase(
    
    outcome == "mort_28d",
    "28-day mortality",
    
    outcome == "icu_mortality",
    "ICU mortality",
    
    outcome == "aki_new",
    "new AKI")] %>% 
  .[, rec_ord := match(rec,
                       rec_levels)] %>% 
  .[, out_ord := match(outcome,
                       c("mort_28d",
                         "icu_mortality",
                         "aki_new"))]


## Formatters
clean_e <- function(x) gsub("e([+-])0(\\d)", 
                            "e\\1\\2",
                            x)

fmt_est <- function(x) sprintf("%+.3f", 
                               x)

fmt_ci  <- function(lo, 
                    hi) sprintf("[%+.3f, %+.3f]", 
                                lo, 
                                hi)

fmt_rv  <- function(x) sprintf("%.3f", 
                               x)

fmt_q   <- function(q) clean_e(fcase(
  
  q <  0.01, 
  formatC(q, 
          format = "e", 
          digits = 1),
  
  q <  0.1,  formatC(q, 
                     format = "f", 
                     digits = 3),
  
  default    = formatC(q, 
                       format = "f", 
                       digits = 2)))

## emit rows with \addlinespace between recommendations
emit_grouped <- function(dt) {
  
  out <- character(0); prev <- ""
  
  for (i in seq_len(nrow(dt))) {
    
    if (i > 1 && dt$rec[i] != prev) out <- c(out, "\\addlinespace")
    out <- c(out, dt$row[i]); prev <- dt$rec[i]
  }
  
  out
}

## Full results table
main <- res_expanded %>% 
  copy() %>% 
  .[tier == "full"] %>% 
  .[order(rec_ord,
          out_ord)] %>% 
  .[, star := fifelse(q_bh < 0.05 &
                        rv >= 0.05,
                      "*",
                      "")] %>% 
  .[, row := sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
                     rec, 
                     outcome_lab, 
                     fmt_est(ate),
                     fmt_ci(ci_lo, 
                            ci_hi), 
                     fmt_q(q_bh), 
                     fmt_rv(rv), 
                     star)]

tab_results <- c(
  "\\begin{table}[p]", 
  " \\small",
  "  \\centering", 
  paste0("  \\caption{Full-tier causal estimates for all fifteen included ",
         "recommendations against the three outcomes. ATE is the average ",
         "treatment effect, $q_{\\text{BH}}$ is the Benjamini-Hochberg adjusted ",
         "$p$-value, and rv is the robustness value. A star marks an effect that ",
         "is significant after correction ($q < 0.05$) and has a robustness ",
         "value of at least $0.05$.}"),
  "  \\label{tab:causal-results}", 
  "  \\begin{tabular}{llrrrrc}", 
  "    \\toprule",
  "    Rec. & Outcome & ATE & 95\\% CI & $q_{\\text{BH}}$ & rv & \\\\",
  "    \\midrule",
  paste0("    ", 
         emit_grouped(main)),
  "    \\bottomrule",
  "  \\end{tabular}",
  "\\end{table}")

writeLines(tab_results, here::here("Outputs/results_table.tex"))


### Tier-stability table -------------------------------------------------------
sig   <- res[tier == "full" & 
               q_bh < 0.05,
             .(treat_col,
               outcome)]

tiers <- merge(res, 
               sig,
               by = c("treat_col", 
                      "outcome"))

fmt_t <- function(x) fifelse(is.na(x),
                             "n/a", 
                             sprintf("%+.3f",
                                     x))

wide  <- dcast(tiers, 
               treat_col + outcome ~ tier, 
               value.var = "ate") %>% 
  .[, rec := fifelse(grepl("^rec_53_55", 
                            treat_col), 
                      "53/55",
                      sub("^rec_([0-9]+)_.*$", 
                          "\\1", 
                          treat_col))] %>% 
  .[, outcome_lab := fcase(
  
  outcome == "mort_28d",      
  "28-day mortality",
  
  outcome == "icu_mortality", 
  "ICU mortality",
  
  outcome == "aki_new",      
  "new AKI")] %>% 
  .[, rec_ord := match(rec, rec_levels)] %>% 
  .[, out_ord := match(outcome, c("mort_28d", "icu_mortality", "aki_new"))] %>% 
  .[order(rec_ord, 
         out_ord)] %>% 
  .[, row := sprintf("%s & %s & %s & %s & %s \\\\",
                      rec, outcome_lab,
                      fmt_t(full), fmt_t(well_observed), fmt_t(well_observed_strict))]


tab_tiers <- c(
  "\\begin{table}[p]", 
  "  \\centering", 
  paste0("  \\caption{Average treatment effect for every significant effect at ",
         "the three observability tiers. The estimates are stable in sign and ",
         "magnitude across tiers. Recommendation 15 cannot be evaluated at the ",
         "strictest tier, since its eligible population and the in-unit-onset ",
         "criterion of that tier are mutually exclusive.}"),
  "  \\label{tab:tiers}",
  "  \\begin{tabular}{llrrr}", 
  "    \\toprule",
  "    Rec. & Outcome & Full & Well-observed & Strict \\\\",
  "    \\midrule",
  paste0("    ", 
         emit_grouped(wide)),
  "    \\bottomrule", "  \\end{tabular}",
  "\\end{table}")

writeLines(tab_tiers, here::here("Outputs/tier_table.tex"))


### Benchmarking Stability Table -----------------------------------------------
exp <- fread(here::here("Outputs/dml_irm_results_expanded.csv")) %>% 
  setDT() %>% 
  .[tier == "full"]

ben <- fread(here::here("Outputs/dml_irm_benchmark_py.csv")) %>% 
  setDT()

## recommendation labels (every rec that appears in the benchmark file)
rec_lab <- c(
  rec_7_culture_before_antimicrobial_causal = "7",
  rec_15_icu_6h_causal = "15",
  rec_44_balanced_over_saline_bolus_causal = "44",
  rec_53_55_norepi_first_line_causal = "53/55",
  rec_69_hfnc_over_alternating = "69",
  rec_89_active_fluid_removal_7d_causal = "89",
  rec_90_restrictive_transfusion_causal = "90",
  rec_91_early_enteral_nutrition_causal = "91",
  rec_92_insulin_started_at_glucose_180_causal = "92",
  rec_96_sodium_bicarb_causal = "96")

out_lab <- c(mort_28d = "28-day mortality",
             icu_mortality = "ICU mortality",
             aki_new = "new AKI")

m <- merge(
  ben[, .(treat_col,
          outcome,
          cf_d_sofa_score,
          cf_d_sapsii)],
  exp[, .(treat_col,
          outcome,
          rv,
          E_point,
          E_ci,
          q_bh)],
  by = c("treat_col",
         "outcome"),
  all.x = TRUE) %>% 
  .[, rec := rec_lab[treat_col]] %>% 
  .[, out := out_lab[outcome]] %>% 
  .[!is.na(rec)] %>% 
  ## order to match Table 7.3: by recommendation, then outcome
  .[, rec_ord := match(treat_col, names(rec_lab))] %>% 
  .[, out_ord := match(outcome, names(out_lab))] %>% 
  .[order(rec_ord,
          out_ord)] %>% 
  ## blank-line separator between recommendations, as in the results table
  .[, first_of_rec := rec_ord != shift(rec_ord, type = "lag")] %>% 
  .[1, first_of_rec := FALSE]

body <- m %>% 
  copy() %>% 
  .[, {
  sep <- fifelse(first_of_rec, "    \\addlinespace\n", "")
  paste0(sep,
         sprintf("    %s & %s & %.3f & %.2f (%.2f) & %.3f & %.3f \\\\",
                 rec, 
                 out, 
                 rv, 
                 E_point, 
                 E_ci, 
                 cf_d_sofa_score, 
                 cf_d_sapsii))}]

tab_benchmark <- c(
  "\\begin{table}[ht]",
  "  \\centering",
  paste0("  \\caption{Sensitivity analysis for the full-tier effects that are ",
         "significant at $q<0.05$. The robustness value rv and the $E$-value ",
         "(with the bound for its 95\\% confidence limit in parentheses) measure ",
         "how strong unmeasured confounding would need to be to overturn the ",
         "estimate. $cf_{d}$ is the partial $R^2$ that the SOFA and SAPS-II ",
         "severity scores explain of treatment assignment, the observed ",
         "benchmark against which rv is read. An effect whose $cf_{d}$ exceeds ",
         "its rv could be overturned by severity confounding alone.}"),
  "  \\label{tab:sensitivity}",
  "  \\begin{tabular}{llrrrr}",
  "    \\toprule",
  "    Rec. & Outcome & rv & $E$-value & $cf_{d}$ SOFA & $cf_{d}$ SAPS-II \\\\",
  "    \\midrule",
  body,
  "    \\bottomrule",
  "  \\end{tabular}",
  "\\end{table}")

writeLines(tab_benchmark, here::here("Outputs/sensitivity_benchmark.tex"))
cat("wrote Outputs/sensitivity_benchmark_table.tex (", nrow(m), "rows)\n")
