# R/10_build_analysis_index.R

message("Building file inventory, collar linkage and health endpoint objects.")

```{r}
#| label: preprocessing-constants
#| include: false

# Programme subfolders — user must match this structure
farms <- c("FG", "IM", "OS", "WL")
primary_program <- "WL"

analysis_tz <- "Pacific/Auckland"
wl_day0_date <- as.Date("2026-03-31")

# GGT thresholds
ggt_threshold_mild     <- 70
ggt_threshold_moderate <- 300
ggt_threshold_severe   <- 700

severity_levels <- c(
  "Control",
  "Dosed: below clinical threshold",
  "Dosed: mild GGT elevation",
  "Dosed: moderate GGT elevation",
  "Dosed: severe GGT elevation"
)

coverage_levels <- c(
  "High coverage",
  "Partial coverage",
  "Low coverage",
  "No coverage"
)

fs::dir_create(output_dir)

data_roots <- tibble(
  program = farms,
  data_root = file.path(project, paste0("Accelerometer data - ", farms))
) |>
  filter(fs::dir_exists(data_root))

WL_data <- data_roots |>
  filter(program == primary_program) |>
  pull(data_root)

WL_GGT_GLDH <- file.path(WL_data, "WL results_GGT and GLDH.xlsx")
WL_set1 <- file.path(WL_data, "Set 1", "Set 1 - collar numbers.xlsx")
WL_set2 <- file.path(WL_data, "Set 2", "Set 2 - collar numbers.xlsx")
```

```{r}
#| label: preprocessing-helper-functions
#| include: false
#| message: false
#| warning: false

safe_text <- function(x) {
  iconv(as.character(x), from = "latin1", to = "UTF-8", sub = "")
}

safe_divide <- function(numerator, denominator) {
  ifelse(denominator == 0, NA_real_, numerator / denominator)
}

clean_sd_card_id <- function(sd_card_id) {
  str_extract(as.character(sd_card_id), "\\d+")
}

normalise_path_for_regex <- function(file_path) {
  str_replace_all(file_path, "\\\\", "/")
}

extract_recording_period <- function(file_path) {
  file_path <- normalise_path_for_regex(file_path)
  str_extract(file_path, "Set [12]|Round [12]")
}

extract_sd_card_id <- function(file_path) {
  file_path <- normalise_path_for_regex(file_path)
  str_match(file_path, "(Set [12]|Round [12])/([^/]+)")[, 3]
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
```

```{r}
#| label: file-inventory-and-study-day-alignment
#| include: false
#| message: false
#| warning: false

build_accelerometer_inventory <- function(program, data_root) {
  
  fs::dir_ls(data_root, recurse = TRUE, regexp = "\\.csv$") |>
    tibble(file_path = _) |>
    mutate(
      program = program,
      file_name = fs::path_file(file_path),
      recording_period = extract_recording_period(file_path),
      sd_card_id_raw = extract_sd_card_id(file_path),
      sd_card_id = clean_sd_card_id(sd_card_id_raw),
      recording_date_text = str_extract(
        file_name,
        "\\d{2}-\\d{2}-\\d{2}(?=\\.csv$)"
      ),
      recording_date = lubridate::dmy(recording_date_text),
      recording_study_day = as.integer(recording_date - wl_day0_date)
    )
}

accelerometer_inventory <- purrr::pmap_dfr(
  list(data_roots$program, data_roots$data_root),
  build_accelerometer_inventory
)

accelerometer_inventory_summary <- accelerometer_inventory |>
  group_by(program) |>
  summarise(
    number_of_files = n(),
    number_of_sd_cards = n_distinct(sd_card_id, na.rm = TRUE),
    first_recording_date = min(recording_date, na.rm = TRUE),
    last_recording_date = max(recording_date, na.rm = TRUE),
    first_study_day = min(recording_study_day, na.rm = TRUE),
    last_study_day = max(recording_study_day, na.rm = TRUE),
    number_of_files_without_recording_date = sum(is.na(recording_date)),
    number_of_files_without_sd_card = sum(is.na(sd_card_id)),
    .groups = "drop"
  )

WL_accelerometer <- accelerometer_inventory |>
  filter(program == primary_program)
```

```{r}
#| label: collar-linkage-health-linkage-and-cohort
#| include: false
#| message: false
#| warning: false

WL_collar_set1 <- read_excel(WL_set1, sheet = "Sheet1") |>
  clean_names() |>
  transmute(
    recording_period = "Set 1",
    sd_card_id_raw = as.character(sd),
    sd_card_id = clean_sd_card_id(sd),
    collar_id = as.character(collar),
    animal_id = as.character(animal_id),
    expected_number_of_files = parse_number(as.character(files))
  )

WL_collar_set2 <- read_excel(WL_set2, sheet = "Sheet1") |>
  clean_names() |>
  transmute(
    recording_period = "Set 2",
    sd_card_id_raw = as.character(sd),
    sd_card_id = clean_sd_card_id(sd),
    collar_id = as.character(collar),
    animal_id = as.character(animal_id),
    expected_number_of_files = parse_number(as.character(files))
  )

WL_collar_mapping_raw <- bind_rows(WL_collar_set1, WL_collar_set2)

WL_duplicate_sd_card_mappings <- WL_collar_mapping_raw |>
  filter(!is.na(sd_card_id)) |>
  group_by(recording_period, sd_card_id) |>
  summarise(
    number_of_rows = n(),
    number_of_animals = n_distinct(animal_id, na.rm = TRUE),
    animal_ids = paste(sort(unique(na.omit(animal_id))), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(number_of_rows > 1 | number_of_animals > 1)

WL_duplicate_animal_period_mappings <- WL_collar_mapping_raw |>
  filter(!is.na(animal_id)) |>
  group_by(recording_period, animal_id) |>
  summarise(
    number_of_rows = n(),
    number_of_sd_cards = n_distinct(sd_card_id, na.rm = TRUE),
    sd_card_ids = paste(sort(unique(na.omit(sd_card_id))), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(number_of_rows > 1 | number_of_sd_cards > 1)

WL_collar_mapping <- WL_collar_mapping_raw |>
  distinct(recording_period, sd_card_id, animal_id, .keep_all = TRUE)

WL_accelerometer_idx <- WL_accelerometer |>
  left_join(
    WL_collar_mapping,
    by = c("recording_period", "sd_card_id")
  ) |>
  mutate(
    has_animal_link = !is.na(animal_id)
  )

WL_health_data <- read_excel(WL_GGT_GLDH, sheet = "Data") |>
  clean_names() |>
  transmute(
    animal_id = as.character(animal_id),
    exposure_group = case_when(
      str_to_lower(as.character(dosed)) %in% c("yes", "y", "true", "1") ~ "Dosed",
      str_to_lower(as.character(dosed)) %in% c("no", "n", "false", "0") ~ "Control",
      TRUE ~ NA_character_
    ),
    study_day = as.integer(day),
    ggt = as.numeric(ggt),
    gldh = as.numeric(gldh),
    log_ggt = log1p(ggt),
    log_gldh = log1p(gldh)
  )

WL_health_animals <- WL_health_data |>
  filter(!is.na(animal_id)) |>
  distinct(animal_id)

WL_accelerometer_animals <- WL_accelerometer_idx |>
  filter(!is.na(animal_id)) |>
  distinct(animal_id)

WL_health_without_accelerometer <- WL_health_animals |>
  anti_join(WL_accelerometer_animals, by = "animal_id")

WL_accelerometer_without_health <- WL_accelerometer_animals |>
  anti_join(WL_health_animals, by = "animal_id")

WL_eligible_animals <- WL_health_animals |>
  inner_join(WL_accelerometer_animals, by = "animal_id")

WL_health_data_clean <- WL_health_data |>
  semi_join(WL_eligible_animals, by = "animal_id")

WL_accelerometer_idx_clean <- WL_accelerometer_idx |>
  semi_join(WL_eligible_animals, by = "animal_id")

WL_ggt_day21_severity <- WL_health_data_clean |>
  filter(study_day == 21) |>
  transmute(
    animal_id,
    exposure_group,
    ggt_day21 = ggt,
    gldh_day21 = gldh,
    fe_severity_class = case_when(
      exposure_group == "Control" ~ "Control",
      is.na(ggt_day21) ~ NA_character_,
      ggt_day21 < ggt_threshold_mild ~ "Dosed: below clinical threshold",
      ggt_day21 < ggt_threshold_moderate ~ "Dosed: mild GGT elevation",
      ggt_day21 < ggt_threshold_severe ~ "Dosed: moderate GGT elevation",
      ggt_day21 >= ggt_threshold_severe ~ "Dosed: severe GGT elevation",
      TRUE ~ NA_character_
    ),
    clinically_elevated_ggt = case_when(
      exposure_group == "Control" ~ FALSE,
      is.na(ggt_day21) ~ NA,
      ggt_day21 >= ggt_threshold_mild ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  mutate(
    fe_severity_class = factor(fe_severity_class, levels = severity_levels)
  )

WL_health_data_clean <- WL_health_data_clean |>
  left_join(
    WL_ggt_day21_severity |>
      select(
        animal_id,
        ggt_day21,
        gldh_day21,
        fe_severity_class,
        clinically_elevated_ggt
      ),
    by = "animal_id"
  )

WL_analysis_index <- WL_accelerometer_idx_clean |>
  left_join(
    WL_ggt_day21_severity,
    by = "animal_id"
  )

WL_clean_linkage_check <- tibble(
  number_of_eligible_animals = n_distinct(WL_eligible_animals$animal_id),
  number_of_health_rows_retained = nrow(WL_health_data_clean),
  number_of_accelerometer_files_retained = nrow(WL_accelerometer_idx_clean),
  number_of_health_animals_excluded = nrow(WL_health_without_accelerometer),
  number_of_accelerometer_animals_excluded = nrow(WL_accelerometer_without_health),
  number_of_unlinked_accelerometer_files = sum(!WL_accelerometer_idx$has_animal_link)
)

WL_severity_summary <- WL_ggt_day21_severity |>
  count(exposure_group, fe_severity_class, name = "number_of_animals")
```

```{r}
#| label: save-final-analysis-index
#| include: false
#| message: false
#| warning: false

readr::write_csv(
  WL_analysis_index,
  file.path(output_dir, "WL_analysis_index.csv")
)
```



