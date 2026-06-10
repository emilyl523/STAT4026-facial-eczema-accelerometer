# Validation data

Place the labelled accelerometer validation data and any supplied classifier inputs in this folder before running the behaviour-classifier comparison scripts.

These files are not committed to GitHub because they may be client-provided or derived from external labelled datasets.

## Expected contents

This folder should contain the labelled behaviour data used to compare:

```text
Random Forest benchmark
XGBoost using original movement features
Temporal-context XGBoost
```

The exact filenames should match the paths used in:

```text
R/50_validation_model_comparison.R
R/60_deploy_temporal_xgb.R
```

## Notes

The validation stage is used to select the behaviour classifier carried forward into the WL deployment.

The final report relies on the generated validation summary file:
```{text}
outputs/validation_loco_metrics_overall_model_comparison.csv
```

and on the selected Temporal-context XGBoost deployment outputs included in:
```text
outputs/report_bundle.rds
```
If validation files are stored outside the repository, set:
```r
Sys.setenv(FE_VALIDATION_DATA_DIR = "/path/to/validation/data")
source("R/run_pipeline.R")
```
