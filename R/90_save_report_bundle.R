# R/90_save_report_bundle.R

message("Saving report-ready objects.")

```{r}
#| label: save-report-ready-bundle
#| echo: false
#| message: true
#| warning: false

report_ready_dir <- file.path(output_dir, "report_ready")
fs::dir_create(report_ready_dir)

report_objects_to_save <- c(
  "accelerometer_inventory",
  "accelerometer_inventory_summary",
  "WL_accelerometer",
  "WL_clean_linkage_check",
  "WL_severity_summary",
  "WL_signal_quality_summary",
  "WL_daily_cleaned_signal_coverage_summary",
  "WL_daily_cleaned_signal_coverage",
  "WL_accelerometer_coverage",
  "WL_analysis_index",
  "WL_health_data_clean",
  "WL_ggt_day21_severity",
  "WL_health_eda",
  "WL_day21_target",
  "WL_health_long_eda",
  "WL_hourly_signal_eda",
  "validation_metrics_overall",
  "validation_metrics_by_collar",
  "validation_predictions",
  "WL_predictions_temporal_xgb",
  "WL_hourly_behaviour_temporal_xgb",
  "WL_daily_behaviour_temporal_xgb",
  "WL_circadian_behaviour_summary",
  "WL_circadian_animal_hour",
  "WL_conditional_support_summary",
  "WL_behaviour_animal_day",
  "WL_behaviour_animal_day_long",
  "WL_behaviour_baseline",
  "WL_behaviour_change",
  "WL_baseline_support_summary",
  "daily_contribution_support_summary",
  "WL_behaviour_phase_change",
  "phase_control_reference",
  "WL_behaviour_phase_control_referenced",
  "WL_phase_behaviour_summary",
  "WL_early_warning_features_long",
  "WL_health_model_features",
  "WL_health_feature_associations"
)

missing_objects <- report_objects_to_save[
  !purrr::map_lgl(report_objects_to_save, exists, envir = .GlobalEnv)
]

if (length(missing_objects) > 0) {
  stop(
    "These report objects do not exist yet. Run the earlier pipeline chunks that create them before saving the bundle:\n",
    paste(missing_objects, collapse = "\n")
  )
}

save_report_csv <- function(object_name) {
  object_value <- get(object_name, envir = .GlobalEnv)

  readr::write_csv(
    object_value,
    file.path(report_ready_dir, paste0(object_name, ".csv"))
  )

  invisible(object_name)
}

purrr::walk(report_objects_to_save, save_report_csv)

read_report_csv <- function(object_name) {
  path <- file.path(report_ready_dir, paste0(object_name, ".csv"))

  if (!file.exists(path)) {
    stop("Missing report-ready CSV: ", path)
  }

  readr::read_csv(path, show_col_types = FALSE)
}

report_bundle <- purrr::set_names(report_objects_to_save) |>
  purrr::map(read_report_csv)

saveRDS(
  report_bundle,
  file.path(output_dir, "report_bundle.rds")
)
```
