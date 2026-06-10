# R/run_pipeline.R

source("R/00_paths.R")
source("R/01_libraries_constants_theme.R")
source("R/02_helpers.R")

source("R/10_build_analysis_index.R")
source("R/20_clean_accelerometer_files.R")
source("R/30_coverage_and_signal_eda.R")
source("R/40_health_eda.R")
source("R/50_validation_model_comparison.R")
source("R/60_deploy_temporal_xgb.R")
source("R/70_behaviour_summaries.R")
source("R/80_health_association_models.R")
source("R/90_save_report_bundle.R")

message("Pipeline complete. Outputs saved to: ", output_dir)
