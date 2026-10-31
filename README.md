# Deriving a Sepsis Bundle of Care with Causal Machine Learning

Code for the master's thesis *Deriving a Sepsis Bundle of Care with Causal Machine Learning: Encoding and Evaluating Adherence to the 2026 Surviving Sepsis Campaign Guidelines in MIMIC-IV.*

The pipeline encodes the 2026 Surviving Sepsis Campaign (SSC) recommendations as computable phenotypes in the MIMIC-IV critical-care database, measures adherence to each recommendation, estimates the causal effect of adherence on patient outcomes with Double Machine Learning (an Interactive Regression Model), and derives a four-component bundle of care from the recommendations that show a robust benefit.

---

## 1. Requirements

### Software

| Component | Version used | Notes |
|-----------|-------------|-------|
| R         | 4.6.0       | Data extraction, encoding, descriptive analysis, E-values, tables |
| Python    | 3.12.15     | Double ML grid with the native sensitivity analysis, SHAP diagnostics |

R packages (loaded in `Code/global.R`): `data.table`, `magrittr`, `dplyr`, `tidyverse`, `lubridate`, `here`, `glue`, `stringr`, `flowchart`, `arrow`, `kableExtra`, `bigrquery`, `DoubleML`, `mlr3`, `mlr3learners`, `ranger`, `xgboost`, `dagitty`, `EValue`, `DiagrammeR`, `DiagrammeRsvg`, `rsvg`.

Python packages: `numpy`, `pandas`, `scikit-learn`, `doubleml`, `shap`, `matplotlib`, `tqdm`, `pyarrow`.

