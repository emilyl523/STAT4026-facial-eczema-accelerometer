# R/50_validation_model_comparison.R

message("Running behaviour classifier validation and model comparison.")

```{r}
#| label: validation-model-comparison-generation
#| echo: true
#| message: true
#| warning: false

set.seed(4026)

library(randomForest)
library(xgboost)
library(data.table)
library(tidyverse)

# Use the same five predictors used in the original published RF formulas.
published_rf_predictors <- c(
  "ax_rm",
  "ay_rm",
  "az_rm",
  "ar_rv",
  "ar_rk"
)

behaviour_specs <- tibble::tribble(
  ~behaviour,     ~source_outcome, ~positive_value,
  "grazing",      "grazing",       "1",
  "ruminating",   "ruminating",    "1",
  "lying",        "upright",       "0"
)

data_dir <- file.path(code_dir, "data")
data_merge1 <- readr::read_csv(
  file.path(data_dir, "data_merge1.csv"),
  show_col_types = FALSE
) |>
  mutate(
    collar = factor(collar),
    grazing = as.character(grazing),
    ruminating = as.character(ruminating),
    upright = as.character(upright)
  )

assert_required_columns(
  data = data_merge1,
  required_cols = c(
    "collar",
    "grazing",
    "ruminating",
    "upright",
    published_rf_predictors
  ),
  object_name = "data_merge1"
)
```

```{r}
#| label: validation-metric-functions
#| echo: true
#| message: false
#| warning: false

has_both_classes <- function(x) {
  length(unique(na.omit(as.character(x)))) == 2
}

rank_auc <- function(truth, pred_prob, positive_class = "1") {
  
  truth <- as.character(truth)
  complete_idx <- !is.na(truth) & !is.na(pred_prob)
  
  truth <- truth[complete_idx]
  pred_prob <- pred_prob[complete_idx]
  
  if (length(unique(truth)) < 2) {
    return(NA_real_)
  }
  
  y <- ifelse(truth == positive_class, 1, 0)
  n_pos <- sum(y == 1)
  n_neg <- sum(y == 0)
  
  if (n_pos == 0 || n_neg == 0) {
    return(NA_real_)
  }
  
  ranks <- rank(pred_prob, ties.method = "average")
  
  (sum(ranks[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

calculate_binary_metrics_safe <- function(truth, pred_class, pred_prob, positive_class = "1") {
  
  truth <- factor(as.character(truth), levels = c("0", "1"))
  pred_class <- factor(as.character(pred_class), levels = c("0", "1"))
  
  complete_idx <- !is.na(truth) & !is.na(pred_class)
  
  truth <- truth[complete_idx]
  pred_class <- pred_class[complete_idx]
  pred_prob <- pred_prob[complete_idx]
  
  tp <- sum(truth == positive_class & pred_class == positive_class)
  tn <- sum(truth != positive_class & pred_class != positive_class)
  fp <- sum(truth != positive_class & pred_class == positive_class)
  fn <- sum(truth == positive_class & pred_class != positive_class)
  
  sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn), NA_real_)
  specificity <- ifelse(tn + fp > 0, tn / (tn + fp), NA_real_)
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), NA_real_)
  
  tibble(
    n = length(truth),
    event_rate = mean(truth == positive_class),
    accuracy = mean(truth == pred_class),
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = mean(c(sensitivity, specificity), na.rm = TRUE),
    precision = precision,
    f1 = ifelse(
      !is.na(precision) && !is.na(sensitivity) && precision + sensitivity > 0,
      2 * precision * sensitivity / (precision + sensitivity),
      NA_real_
    ),
    auc = rank_auc(
      truth = truth,
      pred_prob = pred_prob,
      positive_class = positive_class
    )
  )
}
```

```{r}
#| label: validation-feature-functions
#| echo: true
#| message: false
#| warning: false

temporal_order_candidates <- c(
  "collar_time",
  "obs_time",
  "timestamp",
  "date_time",
  "datetime",
  "time",
  "Time",
  "recording_time",
  "seconds",
  "second",
  "row_time"
)

make_temporal_context_features <- function(data, base_features, group_col = "collar") {
  
  temporal_order_cols <- intersect(
    temporal_order_candidates,
    names(data)
  )
  
  data_context <- data |>
    mutate(.row_id_original = row_number())
  
  if (length(temporal_order_cols) > 0) {
    message(
      "Temporal context features ordered by: ",
      paste(temporal_order_cols, collapse = ", ")
    )
    
    data_context <- data_context |>
      arrange(
        .data[[group_col]],
        across(all_of(temporal_order_cols)),
        .row_id_original
      )
  } else {
    message(
      "No explicit temporal ordering column found. Temporal context features will use original row order within collar."
    )
    
    data_context <- data_context |>
      arrange(
        .data[[group_col]],
        .row_id_original
      )
  }
  
  data_context |>
    group_by(.data[[group_col]]) |>
    mutate(
      across(
        all_of(base_features),
        list(
          lag1 = ~ dplyr::lag(.x, 1),
          lag2 = ~ dplyr::lag(.x, 2),
          roll5_mean = ~ data.table::frollmean(
            .x,
            n = 5,
            align = "right",
            fill = NA_real_
          ),
          roll15_mean = ~ data.table::frollmean(
            .x,
            n = 15,
            align = "right",
            fill = NA_real_
          )
        ),
        .names = "{.col}_{.fn}"
      )
    ) |>
    arrange(.row_id_original) |>
    ungroup() |>
    select(-.row_id_original)
}

temporal_context_predictors <- c(
  published_rf_predictors,
  paste0(published_rf_predictors, "_lag1"),
  paste0(published_rf_predictors, "_lag2"),
  paste0(published_rf_predictors, "_roll5_mean"),
  paste0(published_rf_predictors, "_roll15_mean")
)

data_merge1_temporal <- data_merge1 |>
  make_temporal_context_features(
    base_features = published_rf_predictors,
    group_col = "collar"
  )
```

```{r}
#| label: validation-model-functions
#| echo: true
#| message: true
#| warning: false

xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nthread = 2
)

prepare_validation_data <- function(data, source_outcome, positive_value, predictor_cols) {
  
  data |>
    mutate(
      target = case_when(
        as.character(.data[[source_outcome]]) == positive_value ~ "1",
        as.character(.data[[source_outcome]]) %in% c("0", "1") ~ "0",
        TRUE ~ NA_character_
      ),
      target = factor(target, levels = c("0", "1"))
    ) |>
    select(collar, target, all_of(predictor_cols)) |>
    drop_na(target, all_of(predictor_cols))
}

fit_predict_rf <- function(train_data, test_data, predictor_cols) {
  
  rf_fit <- randomForest::randomForest(
    x = train_data |>
      select(all_of(predictor_cols)) |>
      as.data.frame(),
    y = train_data$target,
    ntree = 500
  )
  
  pred_prob <- predict(
    rf_fit,
    newdata = test_data |>
      select(all_of(predictor_cols)) |>
      as.data.frame(),
    type = "prob"
  )[, "1"]
  
  pred_class <- predict(
    rf_fit,
    newdata = test_data |>
      select(all_of(predictor_cols)) |>
      as.data.frame(),
    type = "response"
  ) |>
    as.character()
  
  tibble(
    pred_prob = as.numeric(pred_prob),
    pred_class = pred_class
  )
}

fit_predict_xgb <- function(train_data, test_data, predictor_cols) {
  
  x_train <- train_data |>
    select(all_of(predictor_cols)) |>
    as.matrix()
  
  x_test <- test_data |>
    select(all_of(predictor_cols)) |>
    as.matrix()
  
  xgb_fit <- xgboost::xgb.train(
    params = xgb_params,
    data = xgboost::xgb.DMatrix(
      data = x_train,
      label = as.numeric(train_data$target) - 1,
      missing = NA
    ),
    nrounds = 100,
    verbose = 0
  )
  
  pred_prob <- predict(
    xgb_fit,
    newdata = xgboost::xgb.DMatrix(
      data = x_test,
      missing = NA
    )
  )
  
  tibble(
    pred_prob = as.numeric(pred_prob),
    pred_class = if_else(pred_prob >= 0.5, "1", "0")
  )
}

run_loco_model <- function(data, behaviour, source_outcome, positive_value, predictor_cols, model_label) {
  
  validation_data <- prepare_validation_data(
    data = data,
    source_outcome = source_outcome,
    positive_value = positive_value,
    predictor_cols = predictor_cols
  )
  
  held_out_collars <- sort(unique(validation_data$collar))
  
  purrr::map_dfr(held_out_collars, function(test_collar) {
    
    train_data <- validation_data |>
      filter(collar != test_collar)
    
    test_data <- validation_data |>
      filter(collar == test_collar)
    
    if (
      nrow(train_data) == 0 ||
      nrow(test_data) == 0 ||
      !has_both_classes(train_data$target)
    ) {
      return(tibble())
    }
    
    message(
      model_label,
      " | ",
      behaviour,
      " | held-out collar ",
      as.character(test_collar)
    )
    
    predictions <- if (model_label == "Random Forest") {
      fit_predict_rf(
        train_data = train_data,
        test_data = test_data,
        predictor_cols = predictor_cols
      )
    } else if (model_label == "XGBoost") {
      fit_predict_xgb(
        train_data = train_data,
        test_data = test_data,
        predictor_cols = predictor_cols
      )
    } else if (model_label == "Temporal-context XGBoost") {
      fit_predict_xgb(
        train_data = train_data,
        test_data = test_data,
        predictor_cols = predictor_cols
      )
    } else {
      stop("Unknown model label: ", model_label)
    }
    
    tibble(
      behaviour = behaviour,
      held_out_collar = as.character(test_collar),
      model = model_label,
      truth = as.character(test_data$target),
      pred_prob = predictions$pred_prob,
      pred_class = predictions$pred_class
    )
  })
}
```

```{r}
#| label: validation-run-fair-model-comparison
#| echo: true
#| message: true
#| warning: false

loco_predictions_rf <- purrr::pmap_dfr(
  behaviour_specs,
  ~ run_loco_model(
    data = data_merge1,
    behaviour = ..1,
    source_outcome = ..2,
    positive_value = ..3,
    predictor_cols = published_rf_predictors,
    model_label = "Random Forest"
  )
)

loco_predictions_xgb <- purrr::pmap_dfr(
  behaviour_specs,
  ~ run_loco_model(
    data = data_merge1,
    behaviour = ..1,
    source_outcome = ..2,
    positive_value = ..3,
    predictor_cols = published_rf_predictors,
    model_label = "XGBoost"
  )
)

loco_predictions_temporal_xgb <- purrr::pmap_dfr(
  behaviour_specs,
  ~ run_loco_model(
    data = data_merge1_temporal,
    behaviour = ..1,
    source_outcome = ..2,
    positive_value = ..3,
    predictor_cols = temporal_context_predictors,
    model_label = "Temporal-context XGBoost"
  )
)

validation_predictions <- bind_rows(
  loco_predictions_rf,
  loco_predictions_xgb,
  loco_predictions_temporal_xgb
)

validation_metrics_by_collar <- validation_predictions |>
  group_by(behaviour, model, held_out_collar) |>
  group_modify(
    ~ calculate_binary_metrics_safe(
      truth = .x$truth,
      pred_class = .x$pred_class,
      pred_prob = .x$pred_prob,
      positive_class = "1"
    )
  ) |>
  ungroup()

validation_metrics_overall <- validation_predictions |>
  group_by(behaviour, model) |>
  group_modify(
    ~ calculate_binary_metrics_safe(
      truth = .x$truth,
      pred_class = .x$pred_class,
      pred_prob = .x$pred_prob,
      positive_class = "1"
    )
  ) |>
  ungroup()

readr::write_csv(
  validation_predictions,
  file.path(output_dir, "validation_loco_predictions_model_comparison.csv")
)

readr::write_csv(
  validation_metrics_by_collar,
  file.path(output_dir, "validation_loco_metrics_by_collar_model_comparison.csv")
)

readr::write_csv(
  validation_metrics_overall,
  file.path(output_dir, "validation_loco_metrics_overall_model_comparison.csv")
)
```
