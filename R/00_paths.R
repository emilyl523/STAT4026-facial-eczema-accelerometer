# R/00_paths.R
library(here)
library(fs)
library(tibble)
library(dplyr)

project_dir <- here::here()

raw_data_dir <- Sys.getenv(
  "FE_RAW_DATA_DIR",
  unset = here::here("data", "raw")
)

validation_data_dir <- Sys.getenv(
  "FE_VALIDATION_DATA_DIR",
  unset = here::here("data", "validation")
)

output_dir <- here::here("outputs")
report_ready_dir <- file.path(output_dir, "report_ready")
cleaned_dir <- file.path(output_dir, "WL_cleaned_files")

fs::dir_create(output_dir)
fs::dir_create(report_ready_dir)
fs::dir_create(cleaned_dir)

analysis_tz <- "Pacific/Auckland"

farms <- c("FG", "IM", "OS", "WL")
primary_program <- "WL"
wl_day0_date <- as.Date("2026-03-31")

data_roots <- tibble::tibble(
  program = farms,
  data_root = file.path(raw_data_dir, paste0("Accelerometer data - ", farms))
) |>
  dplyr::filter(fs::dir_exists(data_root))

WL_data <- data_roots |>
  dplyr::filter(program == primary_program) |>
  dplyr::pull(data_root)

if (length(WL_data) == 0) {
  WL_data <- NA_character_
}

WL_GGT_GLDH <- file.path(WL_data, "WL results_GGT and GLDH.xlsx")
WL_set1 <- file.path(WL_data, "Set 1", "Set 1 - collar numbers.xlsx")
WL_set2 <- file.path(WL_data, "Set 2", "Set 2 - collar numbers.xlsx")
