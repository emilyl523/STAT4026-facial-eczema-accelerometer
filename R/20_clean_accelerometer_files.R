# R/20_clean_accelerometer_files.R

message("Cleaning raw accelerometer files and calculating coverage.")

```{r}
#| label: clean-raw-accelerometer-files
#| echo: true
#| message: true
#| warning: false

required_raw_accelerometer_cols <- c(
  "date",
  "time",
  "accel_x",
  "accel_y",
  "accel_z"
)

read_accelerometer_file <- function(file_path) {
  
  accelerometer_data <- readr::read_csv(
    file_path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "latin1")
  ) |>
    janitor::clean_names()
  
  assert_required_columns(
    data = accelerometer_data,
    required_cols = required_raw_accelerometer_cols,
    object_name = file_path
  )
  
  accelerometer_data |>
    mutate(
      file_path = file_path,
      date_text = safe_text(date),
      time_text = safe_text(time),
      timestamp = parse_accelerometer_timestamp(date_text, time_text),
      accel_x = as.numeric(accel_x),
      accel_y = as.numeric(accel_y),
      accel_z = as.numeric(accel_z),
      acceleration_magnitude = sqrt(accel_x^2 + accel_y^2 + accel_z^2)
    )
}

audit_accelerometer_file <- function(file_path) {
  
  read_accelerometer_file(file_path) |>
    arrange(timestamp) |>
    mutate(
      time_gap_seconds = as.numeric(
        timestamp - lag(timestamp),
        units = "secs"
      ),
      is_zero_signal = accel_x == 0 & accel_y == 0 & accel_z == 0,
      is_extreme_acceleration = acceleration_magnitude > 100
    ) |>
    summarise(
      number_of_rows = n(),
      number_of_valid_timestamps = sum(!is.na(timestamp)),
      first_timestamp = min(timestamp, na.rm = TRUE),
      last_timestamp = max(timestamp, na.rm = TRUE),
      recording_duration_hours = as.numeric(
        last_timestamp - first_timestamp,
        units = "hours"
      ),
      median_time_gap_seconds = median(time_gap_seconds, na.rm = TRUE),
      maximum_time_gap_seconds = max(time_gap_seconds, na.rm = TRUE),
      number_of_duplicate_timestamps = n() - n_distinct(timestamp, na.rm = TRUE),
      proportion_zero_acceleration = mean(is_zero_signal, na.rm = TRUE),
      maximum_acceleration_magnitude = max(acceleration_magnitude, na.rm = TRUE),
      .groups = "drop"
    )
}

empty_signal_quality_row <- tibble(
  number_of_rows = NA_integer_,
  number_of_valid_timestamps = NA_integer_,
  first_timestamp = as.POSIXct(NA, tz = analysis_tz),
  last_timestamp = as.POSIXct(NA, tz = analysis_tz),
  recording_duration_hours = NA_real_,
  median_time_gap_seconds = NA_real_,
  maximum_time_gap_seconds = NA_real_,
  number_of_duplicate_timestamps = NA_integer_,
  proportion_zero_acceleration = NA_real_,
  maximum_acceleration_magnitude = NA_real_
)

safe_audit_accelerometer_file <- purrr::possibly(
  audit_accelerometer_file,
  otherwise = empty_signal_quality_row
)

WL_signal_quality_all <- WL_accelerometer_idx_clean |>
  mutate(
    signal_quality = purrr::map(file_path, safe_audit_accelerometer_file)
  ) |>
  select(
    recording_period,
    sd_card_id,
    collar_id,
    animal_id,
    recording_date,
    recording_study_day,
    file_path,
    signal_quality
  ) |>
  tidyr::unnest(signal_quality, names_repair = "unique") |>
  mutate(
    is_read_error = is.na(number_of_rows),
    has_invalid_timestamp_issue = number_of_valid_timestamps == 0,
    is_partial_day = recording_duration_hours < 20,
    has_long_gap = maximum_time_gap_seconds > 5,
    has_duplicate_timestamps = number_of_duplicate_timestamps > 0,
    has_zero_signal_issue = proportion_zero_acceleration > 0.05,
    has_extreme_acceleration = maximum_acceleration_magnitude > 100,
    signal_quality_flag = case_when(
      is_read_error ~ "Read error",
      has_invalid_timestamp_issue ~ "No valid timestamps",
      has_zero_signal_issue & proportion_zero_acceleration >= 0.95 ~ "Sensor failure",
      is_partial_day |
        has_long_gap |
        has_duplicate_timestamps |
        has_zero_signal_issue |
        has_extreme_acceleration ~ "Review",
      TRUE ~ "Pass"
    )
  )

WL_signal_quality_summary <- WL_signal_quality_all |>
  count(signal_quality_flag, name = "number_of_files") |>
  mutate(
    proportion_of_files = number_of_files / sum(number_of_files)
  )

WL_files_for_processing <- WL_signal_quality_all |>
  filter(
    !signal_quality_flag %in% c(
      "Read error",
      "No valid timestamps",
      "Sensor failure"
    )
  ) |>
  select(
    animal_id,
    recording_period,
    sd_card_id,
    collar_id,
    recording_date,
    recording_study_day,
    file_path,
    is_partial_day,
    has_long_gap,
    has_duplicate_timestamps,
    has_zero_signal_issue,
    has_extreme_acceleration
  )

read_clean_accelerometer_file <- function(file_path) {
  
  accelerometer_data <- read_accelerometer_file(file_path) |>
    filter(!is.na(timestamp)) |>
    mutate(
      is_zero_signal = accel_x == 0 & accel_y == 0 & accel_z == 0,
      is_extreme_acceleration = acceleration_magnitude > 100
    ) |>
    filter(!is_zero_signal)
  
  if (nrow(accelerometer_data) == 0) {
    return(tibble())
  }
  
  acceleration_cap <- quantile(
    accelerometer_data$acceleration_magnitude,
    probs = 0.999,
    na.rm = TRUE
  )
  
  accelerometer_data |>
    mutate(
      acceleration_magnitude_winsorised = pmin(
        acceleration_magnitude,
        acceleration_cap
      )
    ) |>
    group_by(file_path, timestamp) |>
    summarise(
      accel_x = mean(accel_x, na.rm = TRUE),
      accel_y = mean(accel_y, na.rm = TRUE),
      accel_z = mean(accel_z, na.rm = TRUE),
      acceleration_magnitude = mean(acceleration_magnitude, na.rm = TRUE),
      acceleration_magnitude_winsorised = mean(
        acceleration_magnitude_winsorised,
        na.rm = TRUE
      ),
      extreme_acceleration_present = any(is_extreme_acceleration, na.rm = TRUE),
      number_of_rows_collapsed = n(),
      .groups = "drop"
    )
}

cleaned_dir <- file.path(output_dir, "WL_cleaned_files")
fs::dir_create(cleaned_dir)

process_and_save_clean_file <- function(row_index) {
  
  row <- WL_files_for_processing[row_index, ]
  
  cleaned <- read_clean_accelerometer_file(row$file_path) |>
    mutate(
      animal_id = row$animal_id,
      recording_period = row$recording_period,
      sd_card_id = row$sd_card_id,
      collar_id = row$collar_id,
      recording_date = row$recording_date,
      recording_study_day = row$recording_study_day,
      is_partial_day = row$is_partial_day,
      has_long_gap = row$has_long_gap,
      has_duplicate_timestamps = row$has_duplicate_timestamps,
      has_zero_signal_issue = row$has_zero_signal_issue,
      has_extreme_acceleration = row$has_extreme_acceleration,
      .before = 1
    )
  
  output_path <- file.path(
    cleaned_dir,
    paste0(
      stringr::str_pad(row_index, width = 4, pad = "0"),
      "_animal_",
      row$animal_id,
      "_day_",
      row$recording_study_day,
      "_",
      format(row$recording_date, "%Y%m%d"),
      "_cleaned.csv"
    )
  )
  
  readr::write_csv(cleaned, output_path)
  
  tibble(
    row_index = row_index,
    animal_id = row$animal_id,
    recording_date = row$recording_date,
    recording_study_day = row$recording_study_day,
    source_file_path = row$file_path,
    output_path = output_path,
    n_cleaned_rows = nrow(cleaned),
    status = "success"
  )
}

safe_process_and_save_clean_file <- purrr::possibly(
  process_and_save_clean_file,
  otherwise = tibble(
    row_index = NA_integer_,
    animal_id = NA_character_,
    recording_date = as.Date(NA),
    recording_study_day = NA_integer_,
    source_file_path = NA_character_,
    output_path = NA_character_,
    n_cleaned_rows = NA_integer_,
    status = "failed"
  )
)

cleaning_log <- purrr::map_dfr(
  seq_len(nrow(WL_files_for_processing)),
  safe_process_and_save_clean_file
)

readr::write_csv(
  cleaning_log,
  file.path(output_dir, "WL_cleaning_log.csv")
)
```

```{r}
#| label: cleaned-signal-coverage
#| echo: true
#| message: true
#| warning: false

summarise_cleaned_file_coverage <- function(output_path) {
  
  cleaned <- readr::read_csv(
    output_path,
    show_col_types = FALSE
  ) |>
    mutate(
      timestamp = as.POSIXct(timestamp, tz = analysis_tz),
      recording_date = as.Date(recording_date)
    )
  
  if (nrow(cleaned) == 0) {
    return(
      tibble(
        output_path = output_path,
        animal_id = NA_character_,
        recording_date = as.Date(NA),
        recording_study_day = NA_integer_,
        first_timestamp = as.POSIXct(NA, tz = analysis_tz),
        last_timestamp = as.POSIXct(NA, tz = analysis_tz),
        n_cleaned_rows = 0L,
        n_distinct_timestamps = 0L,
        recorded_hours = NA_real_,
        timestamp_completeness = NA_real_
      )
    )
  }
  
  cleaned |>
    summarise(
      output_path = output_path,
      animal_id = first(animal_id),
      recording_date = first(recording_date),
      recording_study_day = first(recording_study_day),
      first_timestamp = min(timestamp, na.rm = TRUE),
      last_timestamp = max(timestamp, na.rm = TRUE),
      n_cleaned_rows = n(),
      n_distinct_timestamps = n_distinct(timestamp),
      recorded_hours = as.numeric(
        last_timestamp - first_timestamp,
        units = "hours"
      ),
      expected_seconds_in_recorded_span = as.numeric(
        last_timestamp - first_timestamp,
        units = "secs"
      ) + 1,
      timestamp_completeness = safe_divide(
        n_distinct_timestamps,
        expected_seconds_in_recorded_span
      ),
      .groups = "drop"
    )
}

safe_summarise_cleaned_file_coverage <- purrr::possibly(
  summarise_cleaned_file_coverage,
  otherwise = tibble(
    output_path = NA_character_,
    animal_id = NA_character_,
    recording_date = as.Date(NA),
    recording_study_day = NA_integer_,
    first_timestamp = as.POSIXct(NA, tz = analysis_tz),
    last_timestamp = as.POSIXct(NA, tz = analysis_tz),
    n_cleaned_rows = NA_integer_,
    n_distinct_timestamps = NA_integer_,
    recorded_hours = NA_real_,
    expected_seconds_in_recorded_span = NA_real_,
    timestamp_completeness = NA_real_
  )
)

WL_daily_cleaned_signal_coverage <- cleaning_log |>
  filter(status == "success", !is.na(output_path)) |>
  mutate(
    coverage = purrr::map(output_path, safe_summarise_cleaned_file_coverage)
  ) |>
  select(coverage) |>
  tidyr::unnest(coverage) |>
  mutate(
    coverage_category = case_when(
      is.na(recorded_hours) | n_cleaned_rows == 0 ~ "No coverage",
      recorded_hours >= 20 & timestamp_completeness >= 0.80 ~ "High coverage",
      recorded_hours >= 8 ~ "Partial coverage",
      recorded_hours > 0 ~ "Low coverage",
      TRUE ~ "No coverage"
    ),
    coverage_category = factor(
      coverage_category,
      levels = coverage_levels
    )
  )

WL_daily_cleaned_signal_coverage_summary <- WL_daily_cleaned_signal_coverage |>
  count(coverage_category, name = "number_of_animal_days") |>
  mutate(
    proportion_of_animal_days = number_of_animal_days / sum(number_of_animal_days)
  )

readr::write_csv(
  WL_daily_cleaned_signal_coverage,
  file.path(output_dir, "WL_daily_cleaned_signal_coverage.csv")
)
```
