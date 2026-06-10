# Longitudinal Modelling of Animal Behaviour and Health Status Using Accelerometer Data

This repository contains the reproducible analysis pipeline and Quarto report for the STAT4026 final project on accelerometer-derived sheep behaviour and facial eczema-related liver-enzyme outcomes.

The analysis investigates whether post-exposure behavioural changes in grazing, lying and rumination align with day 21 GGT-defined facial eczema severity in the WL study cohort.

## Repository structure

```text
.
├── report.qmd
├── references.bib
├── styles/
│   └── custom.css
├── R/
│   ├── 00_paths.R
│   ├── 01_libraries_constants_theme.R
│   ├── 02_helpers.R
│   ├── 10_build_analysis_index.R
│   ├── 20_clean_accelerometer_files.R
│   ├── 30_coverage_and_signal_eda.R
│   ├── 40_health_eda.R
│   ├── 50_validation_model_comparison.R
│   ├── 60_deploy_temporal_xgb.R
│   ├── 70_behaviour_summaries.R
│   ├── 80_health_association_models.R
│   ├── 90_save_report_bundle.R
│   └── run_pipeline.R
├── data/
│   ├── raw/
│   └── validation/
└── outputs/
```

## Main files 
`report.qmd` is the final Quarto report 

`R/run_pipeline.R` runs the full analysis pipeline from raw inputs through to the report-ready object bundle.

`outputs/report_bundle.rds` is the main generated file used by the Quarto report. The report reads this object so that the report can be rendered without rerunning the full raw-data processing pipeline every time.

## Data 

The raw client data are not committed to this repository because they may contain client-owned study data and large accelerometer files.

To reproduce the analysis, place the supplied data into the expected folder structure under `data/raw/`. The required structure is documented in data/raw/README.md.

Validation data used for behaviour-classifier comparison should be placed under `data/validation/`. The expected structure is documented in `data/validation/README.md`.

## Reproducing the analysis

Open the project in RStudio or another R environment from the repository root.

If using `renv`, restore the package environment first:
```{r}
renv::restore()
```

Then run the full pipline:
```{r}
source("R/run_pipeline.R")
```

This creates the report-ready output bundle:

`outputs/report_bundle.rds`

After the pipeline has completed, render the report:

```{r}
quarto::quarto_render("report.qmd")
```

Alternatively, the report can be rendered from the terminal:

```{bash}
quarto render report.qmd
```

## Expected workflow

The intended workflow is:

```{text}
raw data
→ linkage and cleaning
→ behaviour classification
→ behaviour summaries
→ health association analysis
→ report_bundle.rds
→ report.qmd
```

The full pipeline is reproducible from the raw data, but the report itself is designed to read from `outputs/report_bundle.rds` for faster and more stable rendering.

## Software requirements

This project uses R, Quarto and the following main R packages:

- tidyverse
- lubridate
- readxl
- janitor
- fs
- data.table
- xgboost
- pROC
- flextable
- scales
- stringr
- htmltools
- glue
- patchwork

Package versions should be restored from `renv.lock` where available.

## Notes on reproducibility

All file paths are relative to the project root through `R/00_paths.R`.

The analysis assumes the WL study day 0 date is 31 March 2026.

Generated files are written to `outputs/`.

Large raw data and generated outputs are excluded from GitHub using `.gitignore`.
