# R/02_helpers.R

safe_text <- function(x) {
  iconv(as.character(x), from = "latin1", to = "UTF-8", sub = "")
}

safe_divide <- function(numerator, denominator) {
  ifelse(denominator == 0, NA_real_, numerator / denominator)
}

clean_sd_card_id <- function(sd_card_id) {
  stringr::str_extract(as.character(sd_card_id), "\\d+")
}

normalise_path_for_regex <- function(file_path) {
  stringr::str_replace_all(file_path, "\\\\", "/")
}

extract_recording_period <- function(file_path) {
  file_path <- normalise_path_for_regex(file_path)
  stringr::str_extract(file_path, "Set [12]|Round [12]")
}

extract_sd_card_id <- function(file_path) {
  file_path <- normalise_path_for_regex(file_path)
  stringr::str_match(file_path, "(Set [12]|Round [12])/([^/]+)")[, 3]
}

parse_accelerometer_timestamp <- function(date_text, time_text) {
  lubridate::parse_date_time(
    paste(date_text, time_text),
    orders = c(
      "dmy HMS",
      "dmy HM",
      "dmY HMS",
      "dmY HM",
      "ymd HMS",
      "ymd HM"
    ),
    tz = analysis_tz
  )
}

assert_required_columns <- function(data, required_cols, object_name) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      object_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
}

repair_report_types <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  data |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c("animal_id", "collar_id", "sd_card_id", "sd_card_id_raw")),
        as.character
      ),
      dplyr::across(
        tidyselect::any_of(c("recording_study_day", "study_day", "hour_of_day")),
        as.integer
      ),
      dplyr::across(
        tidyselect::any_of(c("recording_date", "first_recording_date", "last_recording_date")),
        as.Date
      )
    )
}
