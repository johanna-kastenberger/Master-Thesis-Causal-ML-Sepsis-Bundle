# Deriving a Sepsis Bundle of Care with Causal Machine Learning

Code for the master's thesis *Deriving a Sepsis Bundle of Care with Causal Machine
Learning: Encoding and Evaluating Adherence to the 2026 Surviving Sepsis Campaign
Guidelines in MIMIC-IV.*

The pipeline encodes the 2026 Surviving Sepsis Campaign (SSC) recommendations as
computable phenotypes in the MIMIC-IV critical-care database, measures adherence to
each recommendation, estimates the causal effect of adherence on patient outcomes with
Double Machine Learning (an Interactive Regression Model), and derives a four-component
bundle of care from the recommendations that show a robust benefit.

---

## 1. Requirements

### Software

| Component | Version used | Notes |
|-----------|-------------|-------|
| R         | 4.6.0       | Data extraction, encoding, descriptive analysis, E-values, tables |
| Python    | 3.12.15     | Double ML grid with the native sensitivity analysis, SHAP diagnostics |

R packages (loaded in `Code/global.R`): `data.table`, `magrittr`, `dplyr`,
`tidyverse`, `lubridate`, `here`, `glue`, `stringr`, `flowchart`, `arrow`,
`kableExtra`, `gtsummary`, `gt`, `bigrquery`, `DoubleML`, `mlr3`, `mlr3learners`,
`ranger`, `xgboost`, `dagitty`, `EValue`, `DiagrammeR`, `DiagrammeRsvg`, `rsvg`.

Python packages: `numpy`, `pandas`, `scikit-learn`, `doubleml`, `shap`, `matplotlib`,
`tqdm`, `pyarrow`.

### Data access

The analysis runs on **MIMIC-IV v3.1**, which is not redistributed with this
repository. Patient-level data must be obtained directly from PhysioNet under its data
use agreement, and the extracted `.rds`/`.parquet` files are git-ignored. The next
section explains how to obtain access and point the code at your own query project.

---

## 2. Getting access to MIMIC-IV and connecting to BigQuery

The extraction script `Code/data_preparation.R` reads MIMIC-IV directly from the
PhysioNet-hosted copy on Google BigQuery. Running it on another machine requires a
credentialed PhysioNet account, BigQuery access to the MIMIC-IV project, and your own
Google Cloud billing project. The MIMIC tables themselves are free to query; the
billing project only exists so Google can meter the (small) query cost, which stays
within the free tier for this pipeline.

### Step 1 - Become a credentialed PhysioNet user

1. Create an account at <https://physionet.org>.
2. Complete the required **CITI "Data or Specimens Only Research"** training course and
   upload the completion report to your PhysioNet profile.
3. Submit the credentialing application and wait for approval.

### Step 2 - Sign the MIMIC-IV data use agreement

1. Open the MIMIC-IV v3.1 project page: <https://physionet.org/content/mimiciv/3.1/>.
2. Read and sign the data use agreement at the bottom of the page. Access is granted to
   your credentialed account once it is signed.

### Step 3 - Request BigQuery access to MIMIC-IV

1. On the MIMIC-IV project page, open the **"Files"** section and follow the link to
   *"Request access to MIMIC-IV on Google BigQuery"* (the "Google Cloud" access link).
2. Enter the Google account (Gmail or Google Workspace address) you will use with
   BigQuery. PhysioNet then grants that Google identity read access to the
   `physionet-data` BigQuery project, where the MIMIC-IV datasets live
   (`physionet-data.mimiciv_3_1_hosp`, `mimiciv_3_1_icu`, `mimiciv_3_1_derived`,
   `mimiciv_3_1_ed`, `mimiciv_note`).
3. Access propagation can take up to a few hours.

### Step 4 - Create your own Google Cloud billing project

You cannot run queries *inside* the read-only `physionet-data` project, so you supply
your own project to execute them from.

1. Go to the Google Cloud Console: <https://console.cloud.google.com>.
2. Create a new project (any name). Note its **Project ID**, for example
   `my-mimic-analysis`, shown on the project dashboard. This is the ID the code needs.
3. Enable the **BigQuery API** for that project (APIs & Services -> Enable APIs ->
   BigQuery API). A billing account must be attached to the project, but the MIMIC
   queries in this pipeline stay within BigQuery's monthly free query allowance.

### Step 5 - Point the code at your project

Open `Code/data_preparation.R` and change the single line near the top:

    ## 1. SET UP BIG QUERY CONNECTION ----------------------------------------------
    project_id <- "master-thesis-492309"   # <-- replace with YOUR Google Cloud project ID

Set `project_id` to the Project ID from Step 4. Nothing else needs to change: the
`physionet-data.mimiciv_3_1_*` dataset names are the shared public references and stay
the same for everyone.

### Step 6 - Authenticate on first run

The first query triggers `bigrquery`'s OAuth flow in the browser. Sign in with the
**same Google account** you registered with PhysioNet in Step 3, and grant the
requested BigQuery scope. The token is cached, so later runs are non-interactive. To
switch accounts, run `bigrquery::bq_auth(email = "you@example.com")` before sourcing the
script, or clear the cache with `bigrquery::bq_deauth()`.

A quick way to confirm everything is wired up correctly:

    library(bigrquery)
    project_id <- "my-mimic-analysis"   # your project ID
    bq_table_download(
      bq_project_query(
        project_id,
        "SELECT COUNT(*) AS n FROM `physionet-data.mimiciv_3_1_hosp.patients`"))

A returned row count means credentialing, BigQuery access, and billing are all working.

---

## 3. Repository layout

    Code/                 all analysis scripts (see Section 4)
    Data/                 extracted patient-level .rds / .parquet  (git-ignored, created locally)
    Outputs/              CSV results, LaTeX tables, figures, SHAP plots

`Data/` is populated by `data_preparation.R` and `define_ssc_variables.R`; it is not
tracked in version control because it contains patient-level data.

---

## 4. The scripts and the order to run them

The pipeline is deliberately split: R prepares the cohort and the analysis frame, Python fits the Double ML grid and runs the sensitivity analysis that the R DoubleML package does not provide, and R reads the results back to compute E-values and build the thesis tables. Run the scripts in the order below.

### `Code/global.R`
Loads every R package and defines the plotting color palette. Every other R script sources this first, so it is not run on its own.

### `Code/SSC_guidelines.R`
Builds the catalog of all 129 SSC recommendations, grouped into the guideline's eight thematic categories, and records for each whether it is encodable in MIMIC-IV and why. Writes `Data/ssc_recommendations.rds` and the inclusion/exclusion tables (`Outputs/ssc_included_table.tex`, `Outputs/ssc_excluded_table.tex`, and the `ssc_mimic_encodability_*` files). Run this once before the encoding step.

### `Code/data_preparation.R`
Sets up the BigQuery connection (Section 2), then extracts and assembles the base sepsis cohort and all supporting tables from MIMIC-IV: the Sepsis-3 cohort, baseline covariates, antimicrobial administrations, vasopressors, fluids, laboratory values, and the derived severity scores. Results are cached as `.rds` files in `Data/` so the expensive queries run only once. **This is the only script that needs BigQuery access and a user-specific `project_id`.**

### `Code/define_ssc_variables.R`
The core encoding step. For each analyzed recommendation it derives the eligibility, the adherence, and the causal-analysis treatment variables, applying the clinical windows and definitions described in the thesis. Writes the fully encoded cohort `Data/DT_ssc_encoded.rds` and exports the modeling matrix `Data/dml_analysis_frame.parquet`, which is the hand-off file read by the Python Double ML scripts.

### `Code/data_analysis.R`
Descriptive analysis of adherence across the encoded cohort: per-recommendation
adherence rates, eligible counts, and the summary tables
(`Outputs/adherence_table.tex`, `Outputs/adherence_summary.csv`).

### `Code/baseline_table.R`
Builds the baseline-characteristics table describing the cohort. Reads `Data/sepsis_base_cohort_data.rds` and `Data/baseline_covariates.rds` and reports demographics, comorbidities, admission laboratory values, severity scores, and outcomes in three columns (whole cohort, sepsis without shock, septic shock) with a shock-vs-no-shock comparison, using `gtsummary`. Writes `Outputs/baseline_table.tex`. It sources `data_preparation.R`, so the cached `.rds` frames must already exist (the two cohort files are read directly rather than re-extracted).

### `Code/double_ML_IRM_all_recs.R`
The R Double ML run across all analyzed recommendations, outcomes, and observability tiers, using `DoubleML`/`mlr3` with random-forest nuisance learners. Sources `define_ssc_variables.R`, so it can be run directly once the cohort is built. This is the R counterpart that the Python sensitivity script mirrors.

### `Code/double_ML_IRM_singular_model.R`
A single-recommendation version of the IRM, useful for inspecting or debugging one model in isolation rather than the full grid.

### `Code/double_ML_IRM_sensitivity.ipynb`  *(Python)*
Re-fits the Interactive Regression Model grid in Python and adds the native DoubleML **sensitivity analysis** that the R package lacks: the robustness values (`rv`, `rva`) and the benchmarking of those values against observed confounders (SOFA, SAPS II). Reads `dml_analysis_frame.parquet` and writes `Outputs/dml_irm_results_py.csv` and `Outputs/dml_irm_diagnostics_py.csv`. A `.py` paired script (`double_ML_IRM_sensitivity.py`) holds the same code as a plain module.

> Note on paths: the notebook expects to run from the project root, so the parquet and output CSVs resolve under `Outputs/`. If your Jupyter kernel starts in `Code/`, `os.chdir` to the project root in the first cell.

### `Code/FDR_and_Evalue.R`
Reads the Python results CSVs back into R and produces the inferential layer: the Benjamini-Hochberg false-discovery-rate correction across the full-tier tests, the E-values (via the `EValue` package), the propensity-overlap inclusion screen, and the robustness-value benchmark. Writes the main thesis tables (`Outputs/results_table.tex`, `tier_table.tex`, `sensitivity_benchmark.tex`, `propensity_table.tex`, and `dml_irm_results_expanded.csv`).

### `Code/association_relative_bundle_adherence.R`
The bundle-completeness analysis. Fits the adjusted logistic regression relating how much of the derived bundle a patient received to 28-day mortality, and writes the odds-ratio and mortality-gradient tables (`Outputs/completeness_or_table.tex`, `completeness_gradient_table.tex`).

### `Code/propensity_shap.ipynb`  *(Python)*
SHAP diagnostics for the nuisance models. Fits diagnostic random forests and produces SHAP summary plots for the propensity (treatment-assignment) and outcome models, used to read which baseline variables drive assignment and outcome and to flag confounding. Writes the plots under `Outputs/shap/` and `Outputs/propensity_shap_rec*.pdf`.

---

## 5. End-to-end run

    # R, from the project root, after completing Section 2
    source("Code/SSC_guidelines.R")          # recommendation catalog
    source("Code/data_preparation.R")        # BigQuery extraction  (needs your project_id)
    source("Code/define_ssc_variables.R")    # encoding -> DT_ssc_encoded.rds + parquet
    source("Code/baseline_table.R")          # Table 1 -> Outputs/baseline_table.tex

    # Python, from the project root
    jupyter nbconvert --to notebook --execute Code/double_ML_IRM_sensitivity.ipynb
    jupyter nbconvert --to notebook --execute Code/propensity_shap.ipynb

    # R again, to build the inferential tables from the Python output
    source("Code/FDR_and_Evalue.R")
    source("Code/association_relative_bundle_adherence.R")

---

## 6. Data and reproducibility

- Patient-level data (`Data/`) is never committed, only aggregate results and LaTeX tables in `Outputs/` are tracked.
- All stochastic steps use a fixed seed (`123`) for reproducibility.
- MIMIC-IV access is credentialed and governed by the PhysioNet data use agreement. Redistributing the extracted data is NOT allowed.

## 7. Citing the data

MIMIC-IV should be cited as the PhysioNet dataset (Johnson et al., 2024, <https://doi.org/10.13026/kpb9-mt58>) together with the data descriptor (Johnson et al., 2023, *Scientific Data*). The ICU data originate from the MetaVision clinical information system (iMDsoft), as documented in the descriptor.
