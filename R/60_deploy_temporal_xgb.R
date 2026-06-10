# R/60_deploy_temporal_xgb.R

message("Fitting selected Temporal-context XGBoost models and deploying to WL.")

```{r}
#| label: wl-temporal-xgb-deployment
#| echo: true
#| message: true
#| warning: false

mbl_dir <- file.path(code_dir, "mbl", "R")
source(file.path(mbl_dir, "mbl_roll.R"))

if (!exists("diff_s")) {
  diff_s <- function(x) {
    c(NA_real_, as.numeric(diff(x), units = "secs"))
  }
}

cleaned_dir <- file.path(output_dir, "WL_cleaned_files")

wl_prediction_path <- file.path(
  output_dir,
  "WL_predictions_temporal_xgb.csv"
)

wl_hourly_behaviour_path <- file.path(
  output_dir,
  "WL_hourly_behaviour_temporal_xgb.csv"
)

wl_daily_behaviour_path <- file.path(
  output_dir,
  "WL_daily_behaviour_temporal_xgb.csv"
)

model_dir <- file.path(
  output_dir,
  "final_temporal_xgb_models"
)

fs::dir_create(model_dir)
```

```{r}
#| label: fit-final-temporal-xgb-models
#| echo: true
#| message: true
#| warning: false

fit_final_temporal_xgb <- function(source_outcome, positive_value) {
  
  training_data <- prepare_validation_data(
    data = data_merge1_temporal,
    source_outcome = source_outcome,
    positive_value = positive_value,
    predictor_cols = temporal_context_predictors
  )
  
  x_train <- training_data |>
    select(all_of(temporal_context_predictors)) |>
    as.matrix()
  
  y_train <- as.numeric(training_data$target) - 1
  
  xgboost::xgb.train(
    params = xgb_params,
    data = xgboost::xgb.DMatrix(
      data = x_train,
      label = y_train,
      missing = NA
    ),
    nrounds = 100,
    verbose = 0
  )
}

final_temporal_xgb_models <- behaviour_specs |>
  mutate(
    model = purrr::map2(
      source_outcome,
      positive_value,
      fit_final_temporal_xgb
    )
  )

purrr::walk2(
  final_temporal_xgb_models$model,
  final_temporal_xgb_models$behaviour,
  ~ xgboost::xgb.save(
    .x,
    fname = file.path(model_dir, paste0("temporal_xgb_", .y, ".model"))
  )
)
```

```{r}
#| label: create-wl-temporal-context-features
#| echo: true
#| message: false
#| warning: false

read_cleaned_wl_file_for_prediction <- function(file_path) {
  
  wl_file <- data.table::fread(file_path) |>
    as_tibble() |>
    mutate(
      animal_id = as.character(animal_id),
      collar_id = as.character(collar_id),
      recording_period = as.character(recording_period),
      sd_card_id = as.character(sd_card_id),
      recording_date = as.Date(recording_date),
      recording_study_day = as.integer(recording_study_day),
      timestamp = parse_cleaned_timestamp(timestamp),
      collar = factor(animal_id),
      collar_time = timestamp,
      ax = as.numeric(accel_x),
      ay = as.numeric(accel_y),
      az = as.numeric(accel_z)
    ) |>
    filter(!is.na(timestamp)) |>
    arrange(collar, collar_time)
  
  if (nrow(wl_file) == 0) {
    return(tibble())
  }
  
  wl_file
}

make_wl_temporal_features <- function(file_path) {
  
  wl_raw <- read_cleaned_wl_file_for_prediction(file_path)
  
  if (nrow(wl_raw) == 0) {
    return(tibble())
  }
  
  wl_rolling <- wl_raw |>
    select(
      animal_id,
      collar_id,
      recording_period,
      sd_card_id,
      recording_date,
      recording_study_day,
      timestamp,
      collar,
      collar_time,
      ax,
      ay,
      az
    ) |>
    add_rolling_features()
  
  missing_original <- setdiff(
    published_rf_predictors,
    names(wl_rolling)
  )
  
  if (length(missing_original) > 0) {
    stop(
      "Original rolling features missing in ",
      basename(file_path),
      ": ",
      paste(missing_original, collapse = ", ")
    )
  }
  
  wl_temporal <- wl_rolling |>
    make_temporal_context_features(
      base_features = published_rf_predictors,
      group_col = "collar"
    )
  
  missing_temporal <- setdiff(
    temporal_context_predictors,
    names(wl_temporal)
  )
  
  if (length(missing_temporal) > 0) {
    stop(
      "Temporal-context features missing in ",
      basename(file_path),
      ": ",
      paste(missing_temporal, collapse = ", ")
    )
  }
  
  wl_temporal
}
```

```{r}
#| label: predict-wl-behaviour-temporal-xgb
#| echo: true
#| message: true
#| warning: false

cleaned_file_paths <- list.files(
  cleaned_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

predict_wl_file_temporal_xgb <- function(file_path) {
  
  message("Predicting behaviours for ", basename(file_path))
  
  wl_features <- make_wl_temporal_features(file_path)
  
  if (nrow(wl_features) == 0) {
    return(tibble())
  }
  
  x_wl <- wl_features |>
    select(all_of(temporal_context_predictors)) |>
    as.matrix()
  
  d_wl <- xgboost::xgb.DMatrix(
    data = x_wl,
    missing = NA
  )
  
  prediction_data <- wl_features |>
    transmute(
      animal_id = as.character(animal_id),
      collar_id = as.character(collar_id),
      recording_period = as.character(recording_period),
      sd_card_id = as.character(sd_card_id),
      recording_date = as.Date(recording_date),
      recording_study_day = as.integer(recording_study_day),
      timestamp = as.POSIXct(timestamp, tz = analysis_tz)
    )
  
  for (i in seq_len(nrow(final_temporal_xgb_models))) {
    
    behaviour_name <- final_temporal_xgb_models$behaviour[i]
    behaviour_model <- final_temporal_xgb_models$model[[i]]
    
    pred_prob <- predict(
      behaviour_model,
      newdata = d_wl
    )
    
    prediction_data[[paste0("prob_", behaviour_name)]] <- as.numeric(pred_prob)
    prediction_data[[paste0("pred_", behaviour_name)]] <- as.integer(pred_prob >= 0.5)
  }
  
  prediction_data
}

WL_predictions_temporal_xgb <- purrr::map_dfr(
  cleaned_file_paths,
  predict_wl_file_temporal_xgb
)

data.table::fwrite(
  data.table::as.data.table(WL_predictions_temporal_xgb),
  wl_prediction_path
)

WL_predictions_temporal_xgb |>
  summarise(
    number_of_predictions = n(),
    number_of_animals = n_distinct(animal_id),
    first_timestamp = min(timestamp, na.rm = TRUE),
    last_timestamp = max(timestamp, na.rm = TRUE),
    first_study_day = min(recording_study_day, na.rm = TRUE),
    last_study_day = max(recording_study_day, na.rm = TRUE)
  )
```

```{r}
#| label: summarise-wl-temporal-xgb-behaviour
#| echo: true
#| message: false
#| warning: false

WL_hourly_behaviour_temporal_xgb <- WL_predictions_temporal_xgb |>
  mutate(
    hour = lubridate::floor_date(timestamp, unit = "hour"),
    hour_of_day = lubridate::hour(timestamp)
  ) |>
  group_by(
    animal_id,
    recording_date,
    recording_study_day,
    hour,
    hour_of_day
  ) |>
  summarise(
    n_predictions = n(),
    grazing_probability = mean(prob_grazing, na.rm = TRUE),
    ruminating_probability = mean(prob_ruminating, na.rm = TRUE),
    lying_probability = mean(prob_lying, na.rm = TRUE),
    grazing_proportion = mean(pred_grazing == 1, na.rm = TRUE),
    ruminating_proportion = mean(pred_ruminating == 1, na.rm = TRUE),
    lying_proportion = mean(pred_lying == 1, na.rm = TRUE),
    .groups = "drop"
  )

WL_daily_behaviour_temporal_xgb <- WL_predictions_temporal_xgb |>
  group_by(
    animal_id,
    recording_date,
    recording_study_day
  ) |>
  summarise(
    n_predictions = n(),
    grazing_probability = mean(prob_grazing, na.rm = TRUE),
    ruminating_probability = mean(prob_ruminating, na.rm = TRUE),
    lying_probability = mean(prob_lying, na.rm = TRUE),
    grazing_proportion = mean(pred_grazing == 1, na.rm = TRUE),
    ruminating_proportion = mean(pred_ruminating == 1, na.rm = TRUE),
    lying_proportion = mean(pred_lying == 1, na.rm = TRUE),
    .groups = "drop"
  )

data.table::fwrite(
  data.table::as.data.table(WL_hourly_behaviour_temporal_xgb),
  wl_hourly_behaviour_path
)

data.table::fwrite(
  data.table::as.data.table(WL_daily_behaviour_temporal_xgb),
  wl_daily_behaviour_path
)

WL_daily_behaviour_temporal_xgb |>
  summarise(
    number_of_animal_days = n(),
    number_of_animals = n_distinct(animal_id),
    first_study_day = min(recording_study_day, na.rm = TRUE),
    last_study_day = max(recording_study_day, na.rm = TRUE),
    mean_grazing = mean(grazing_proportion, na.rm = TRUE),
    mean_ruminating = mean(ruminating_proportion, na.rm = TRUE),
    mean_lying = mean(lying_proportion, na.rm = TRUE)
  )
```

```{r}
#| label: circadian-behaviour-pattern-generation
#| echo: true
#| message: true
#| warning: false

hourly_behaviour_path <- file.path(
  output_dir,
  "WL_hourly_behaviour_temporal_xgb.csv"
)

coverage_path <- file.path(
  output_dir,
  "WL_daily_cleaned_signal_coverage.csv"
)

circadian_summary_path <- file.path(
  output_dir,
  "WL_circadian_behaviour_summary_temporal_xgb.csv"
)

circadian_animal_hour_path <- file.path(
  output_dir,
  "WL_circadian_animal_hour_temporal_xgb.csv"
)

WL_hourly_behaviour_temporal_xgb <- readr::read_csv(
  hourly_behaviour_path,
  show_col_types = FALSE
) |>
  mutate(
    animal_id = as.character(animal_id),
    recording_date = as.Date(recording_date),
    recording_study_day = as.integer(recording_study_day),
    hour = as.POSIXct(hour, tz = analysis_tz),
    hour_of_day = as.integer(hour_of_day)
  )

WL_accelerometer_coverage <- readr::read_csv(
  coverage_path,
  show_col_types = FALSE
) |>
  mutate(
    animal_id = as.character(animal_id),
    recording_date = as.Date(recording_date),
    recording_study_day = as.integer(recording_study_day),
    coverage_category = factor(
      coverage_category,
      levels = c(
        "High coverage",
        "Partial coverage",
        "Low coverage",
        "No coverage"
      )
    )
  )

WL_circadian_behaviour_long <- WL_hourly_behaviour_temporal_xgb |>
  left_join(
    WL_accelerometer_coverage |>
      select(
        animal_id,
        recording_date,
        recording_study_day,
        coverage_category
      ),
    by = c(
      "animal_id",
      "recording_date",
      "recording_study_day"
    )
  ) |>
  filter(
    coverage_category %in% c("High coverage", "Partial coverage")
  ) |>
  pivot_longer(
    cols = c(
      grazing_probability,
      ruminating_probability,
      lying_probability,
      grazing_proportion,
      ruminating_proportion,
      lying_proportion
    ),
    names_to = c("behaviour", "summary_type"),
    names_pattern = "(.+)_(probability|proportion)",
    values_to = "behaviour_value"
  ) |>
  mutate(
    behaviour = recode(
      behaviour,
      grazing = "Grazing",
      ruminating = "Ruminating",
      lying = "Lying"
    ),
    behaviour = factor(
      behaviour,
      levels = c("Grazing", "Ruminating", "Lying")
    ),
    summary_type = recode(
      summary_type,
      probability = "Mean predicted probability",
      proportion = "Predicted proportion"
    )
  )

WL_circadian_animal_hour <- WL_circadian_behaviour_long |>
  group_by(
    animal_id,
    hour_of_day,
    behaviour,
    summary_type
  ) |>
  summarise(
    animal_hour_value = mean(behaviour_value, na.rm = TRUE),
    animal_hour_records = n(),
    .groups = "drop"
  )

WL_circadian_behaviour_summary <- WL_circadian_animal_hour |>
  group_by(
    hour_of_day,
    behaviour,
    summary_type
  ) |>
  summarise(
    median_value = median(animal_hour_value, na.rm = TRUE),
    lower_quartile = quantile(animal_hour_value, 0.25, na.rm = TRUE),
    upper_quartile = quantile(animal_hour_value, 0.75, na.rm = TRUE),
    mean_value = mean(animal_hour_value, na.rm = TRUE),
    n_animals = n_distinct(animal_id),
    .groups = "drop"
  )

WL_circadian_behaviour_summary |>
  filter(summary_type == "Predicted proportion") |>
  arrange(behaviour, hour_of_day)
```

```{r}
#| label: tbl-conditional-probability-support-temporal-xgb
#| echo: true
#| message: false
#| warning: false

prediction_path <- file.path(
  output_dir,
  "WL_predictions_temporal_xgb.csv"
)

coverage_path <- file.path(
  output_dir,
  "WL_daily_cleaned_signal_coverage.csv"
)
WL_coverage_for_support <- data.table::fread(
  coverage_path,
  select = c(
    "animal_id",
    "recording_date",
    "recording_study_day",
    "coverage_category"
  )
)

WL_coverage_for_support[
  ,
  `:=`(
    animal_id = as.character(animal_id),
    recording_date = as.Date(recording_date),
    recording_study_day = as.integer(recording_study_day)
  )
]

coverage_rank <- c(
  "No coverage" = 0L,
  "Low coverage" = 1L,
  "Partial coverage" = 2L,
  "High coverage" = 3L
)

WL_coverage_for_support[
  ,
  coverage_rank_value := coverage_rank[as.character(coverage_category)]
]

WL_coverage_for_support <- WL_coverage_for_support[
  ,
  .(
    coverage_rank_value = max(coverage_rank_value, na.rm = TRUE)
  ),
  by = .(
    animal_id,
    recording_date,
    recording_study_day
  )
][
  ,
  coverage_category := names(coverage_rank)[
    match(coverage_rank_value, coverage_rank)
  ]
][
  coverage_category %in% c("High coverage", "Partial coverage")
][
  ,
  .(
    animal_id,
    recording_date,
    recording_study_day,
    coverage_category
  )
]

data.table::setkey(
  WL_prediction_support_data,
  animal_id,
  recording_date,
  recording_study_day
)

data.table::setkey(
  WL_coverage_for_support,
  animal_id,
  recording_date,
  recording_study_day
)

WL_prediction_support_data <- WL_prediction_support_data[
  WL_coverage_for_support,
  nomatch = 0
]

summarise_conditional_support <- function(data, behaviour_name, prob_col, pred_col) {
  
  animal_level <- data[
    ,
    .(
      n_prediction_records = .N,
      n_positive_predictions = sum(get(pred_col) == 1, na.rm = TRUE),
      positive_prediction_rate = mean(get(pred_col) == 1, na.rm = TRUE),
      median_probability_when_positive = ifelse(
        sum(get(pred_col) == 1, na.rm = TRUE) > 0,
        median(get(prob_col)[get(pred_col) == 1], na.rm = TRUE),
        NA_real_
      ),
      mean_probability_when_positive = ifelse(
        sum(get(pred_col) == 1, na.rm = TRUE) > 0,
        mean(get(prob_col)[get(pred_col) == 1], na.rm = TRUE),
        NA_real_
      )
    ),
    by = animal_id
  ]
  
  animal_level[
    ,
    .(
      behaviour = behaviour_name,
      median_probability_when_positive = median(
        median_probability_when_positive,
        na.rm = TRUE
      ),
      lower_quartile_when_positive = quantile(
        median_probability_when_positive,
        0.25,
        na.rm = TRUE
      ),
      upper_quartile_when_positive = quantile(
        median_probability_when_positive,
        0.75,
        na.rm = TRUE
      ),
      mean_probability_when_positive = mean(
        mean_probability_when_positive,
        na.rm = TRUE
      ),
      median_positive_prediction_rate = median(
        positive_prediction_rate,
        na.rm = TRUE
      ),
      n_animals_total = uniqueN(animal_id),
      n_animals_with_positive_predictions = uniqueN(
        animal_id[n_positive_predictions > 0]
      )
    )
  ]
}

WL_conditional_support_summary <- data.table::rbindlist(
  list(
    summarise_conditional_support(
      WL_prediction_support_data,
      behaviour_name = "Grazing",
      prob_col = "prob_grazing",
      pred_col = "pred_grazing"
    ),
    summarise_conditional_support(
      WL_prediction_support_data,
      behaviour_name = "Ruminating",
      prob_col = "prob_ruminating",
      pred_col = "pred_ruminating"
    ),
    summarise_conditional_support(
      WL_prediction_support_data,
      behaviour_name = "Lying",
      prob_col = "prob_lying",
      pred_col = "pred_lying"
    )
  ),
  use.names = TRUE
) |>
  as_tibble() |>
  mutate(
    behaviour = factor(
      behaviour,
      levels = c("Grazing", "Ruminating", "Lying")
    )
  )

WL_conditional_support_summary
```
