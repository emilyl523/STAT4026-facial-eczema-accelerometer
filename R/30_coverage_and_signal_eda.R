# R/30_coverage_and_signal_eda.R

message("Generating coverage summaries and hourly signal EDA.")


```{r}
#| label: generate-wl-hourly-signal-eda
#| echo: true
#| message: true
#| warning: false

hourly_signal_path <- file.path(
  output_dir,
  "WL_hourly_signal_eda.csv"
)

cleaning_log_path <- file.path(
  output_dir,
  "WL_cleaning_log.csv"
)

if (!exists("cleaning_log")) {
  cleaning_log <- readr::read_csv(
    cleaning_log_path,
    show_col_types = FALSE
  )
}

cleaned_file_index <- cleaning_log |>
  mutate(
    row_index = as.integer(row_index),
    animal_id = as.character(animal_id),
    recording_date = as.Date(recording_date),
    recording_study_day = as.integer(recording_study_day),
    output_path = as.character(output_path),
    status = as.character(status)
  ) |>
  filter(
    status == "success",
    !is.na(output_path),
    file.exists(output_path)
  ) |>
  arrange(row_index)

if (nrow(cleaned_file_index) == 0) {
  stop("No cleaned files were found from WL_cleaning_log.csv.")
}

message("Generating hourly signal EDA from ", nrow(cleaned_file_index), " cleaned files.")

parse_cleaned_timestamp <- function(x) {
  lubridate::parse_date_time(
    as.character(x),
    orders = c(
      "ymd HMS z",
      "ymd HMS",
      "ymd HM",
      "dmy HMS",
      "dmy HM"
    ),
    tz = analysis_tz,
    quiet = TRUE
  )
}

summarise_cleaned_file_hourly_signal <- function(output_path) {
  
  dt <- data.table::fread(
    file = output_path,
    select = c(
      "animal_id",
      "recording_date",
      "recording_study_day",
      "timestamp",
      "acceleration_magnitude",
      "acceleration_magnitude_winsorised",
      "extreme_acceleration_present"
    )
  )
  
  dt[, animal_id := as.character(animal_id)]
  dt[, recording_date := as.Date(recording_date)]
  dt[, recording_study_day := as.integer(recording_study_day)]
  dt[, timestamp := parse_cleaned_timestamp(timestamp)]
  dt <- dt[!is.na(timestamp)]
  
  if (nrow(dt) == 0) {
    return(
      tibble(
        animal_id = NA_character_,
        recording_date = as.Date(NA),
        recording_study_day = NA_integer_,
        hour_of_day = NA_integer_,
        n_cleaned_rows = 0L,
        median_acceleration_magnitude = NA_real_,
        mean_acceleration_magnitude = NA_real_,
        median_acceleration_magnitude_winsorised = NA_real_,
        mean_acceleration_magnitude_winsorised = NA_real_,
        p90_acceleration_magnitude = NA_real_,
        any_extreme_acceleration_present = NA
      )
    )
  }
  
  dt[, hour_of_day := lubridate::hour(timestamp)]
  
  dt[
    ,
    .(
      n_cleaned_rows = .N,
      median_acceleration_magnitude = median(
        acceleration_magnitude,
        na.rm = TRUE
      ),
      mean_acceleration_magnitude = mean(
        acceleration_magnitude,
        na.rm = TRUE
      ),
      median_acceleration_magnitude_winsorised = median(
        acceleration_magnitude_winsorised,
        na.rm = TRUE
      ),
      mean_acceleration_magnitude_winsorised = mean(
        acceleration_magnitude_winsorised,
        na.rm = TRUE
      ),
      p90_acceleration_magnitude = as.numeric(
        quantile(acceleration_magnitude, 0.90, na.rm = TRUE)
      ),
      any_extreme_acceleration_present = any(
        extreme_acceleration_present,
        na.rm = TRUE
      )
    ),
    by = .(
      animal_id,
      recording_date,
      recording_study_day,
      hour_of_day
    )
  ] |>
    as_tibble()
}

safe_summarise_cleaned_file_hourly_signal <- purrr::possibly(
  summarise_cleaned_file_hourly_signal,
  otherwise = tibble(
    animal_id = NA_character_,
    recording_date = as.Date(NA),
    recording_study_day = NA_integer_,
    hour_of_day = NA_integer_,
    n_cleaned_rows = NA_integer_,
    median_acceleration_magnitude = NA_real_,
    mean_acceleration_magnitude = NA_real_,
    median_acceleration_magnitude_winsorised = NA_real_,
    mean_acceleration_magnitude_winsorised = NA_real_,
    p90_acceleration_magnitude = NA_real_,
    any_extreme_acceleration_present = NA
  )
)

WL_hourly_signal_eda <- purrr::map_dfr(
  cleaned_file_index$output_path,
  safe_summarise_cleaned_file_hourly_signal
) |>
  filter(
    !is.na(animal_id),
    !is.na(hour_of_day)
  )

data.table::fwrite(
  as.data.table(WL_hourly_signal_eda),
  hourly_signal_path
)

WL_hourly_signal_eda |>
  summarise(
    number_of_animals = n_distinct(animal_id),
    number_of_animal_date_hour_rows = n(),
    first_recording_date = min(recording_date, na.rm = TRUE),
    last_recording_date = max(recording_date, na.rm = TRUE),
    first_study_day = min(recording_study_day, na.rm = TRUE),
    last_study_day = max(recording_study_day, na.rm = TRUE)
  )
```

```{r}
WL_accelerometer_coverage <- readr::read_csv(
  file.path(output_dir, "WL_daily_cleaned_signal_coverage.csv"),
  show_col_types = FALSE
) |>
  mutate(
    animal_id = as.character(animal_id),
    recording_date = as.Date(recording_date),
    recording_study_day = as.integer(recording_study_day),
    coverage_category = factor(
      coverage_category,
      levels = coverage_levels
    )
  )

WL_hourly_signal_eda <- readr::read_csv(
  file.path(output_dir, "WL_hourly_signal_eda.csv"),
  show_col_types = FALSE
) |>
  mutate(
    animal_id = as.character(animal_id),
    recording_date = as.Date(recording_date),
    recording_study_day = as.integer(recording_study_day),
    hour_of_day = as.integer(hour_of_day)
  )
```

