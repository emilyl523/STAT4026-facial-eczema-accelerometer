# R/80_health_association_models.R

message("Running early-warning association models.")

```{r}
early_warning_phases <- c("Days 1–5","Days 6–12")
primary_behaviours <- c("Grazing","Lying")

WL_early_warning_features_long <- WL_behaviour_phase_control_referenced |>
  filter(
    analysis_phase %in% early_warning_phases,
    behaviour %in% primary_behaviours,
    !is.na(severity_short)
  ) |>
  mutate(
    analysis_phase = factor(
      analysis_phase,
      levels = early_warning_phases
    ),
    behaviour = factor(
      behaviour,
      levels = c("Grazing", "Lying")
    )
  ) |>
  select(
    animal_id,
    exposure_group,
    severity_short,
    fe_severity_class,
    clinically_elevated_ggt,
    ggt_day21,
    gldh_day21,
    behaviour,
    analysis_phase,
    control_referenced_phase_change,
    number_of_observed_days,
  )

WL_health_model_features <- WL_early_warning_features_long |>
  mutate(
    feature_name = paste(
      behaviour,
      analysis_phase,
      sep = "_"
    ),
    feature_name = stringr::str_replace_all(feature_name, "[^A-Za-z0-9]+", "_"),
    feature_name = stringr::str_to_lower(feature_name)
  ) |>
  select(
    animal_id,
    exposure_group,
    severity_short,
    fe_severity_class,
    clinically_elevated_ggt,
    ggt_day21,
    gldh_day21,
    feature_name,
    control_referenced_phase_change,
    number_of_observed_days
  ) |>
  tidyr::pivot_wider(
    names_from = feature_name,
    values_from = c(
      control_referenced_phase_change,
      number_of_observed_days
    )
  ) |>
  mutate(
    log_ggt_day21 = log1p(ggt_day21),
    log_gldh_day21 = log1p(gldh_day21),
    clinically_elevated_ggt = as.logical(clinically_elevated_ggt),
    severity_short = factor(
      severity_short,
      levels = observed_severity_levels
    )
  )
WL_health_model_features <- WL_health_model_features |>
  mutate(
    inactivity_shift_days_1_5 = 
      control_referenced_phase_change_lying_days_1_5 -
      control_referenced_phase_change_grazing_days_1_5,
    inactivity_shift_days_6_12 =
      control_referenced_phase_change_lying_days_6_12 -
      control_referenced_phase_change_grazing_days_6_12, 
    inactivity_shift_sustained = inactivity_shift_days_1_5 + inactivity_shift_days_6_12,
inactivity_shift_progression = inactivity_shift_days_6_12 - inactivity_shift_days_1_5
  )
```

```{r}
#| label: early-behaviour-signal-testing
#| message: false
#| warning: false

early_warning_feature_names <- c(
  "control_referenced_phase_change_grazing_days_1_5",
  "control_referenced_phase_change_lying_days_1_5",
  "control_referenced_phase_change_grazing_days_6_12",
  "control_referenced_phase_change_lying_days_6_12",
  "inactivity_shift_days_1_5",
  "inactivity_shift_days_6_12", 
  "inactivity_shift_sustained",
  "inactivity_shift_progression"
)

fit_feature_association <- function(data, outcome, feature) {
  model_data <- data |>
    dplyr::select(
      outcome_value = dplyr::all_of(outcome),
      feature_value = dplyr::all_of(feature)
    ) |>
    tidyr::drop_na()

  if (nrow(model_data) < 5 || dplyr::n_distinct(model_data$feature_value) < 2) {
    return(
      tibble::tibble(
        n_animals = nrow(model_data),
        estimate = NA_real_,
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        p_value = NA_real_,
        r_squared = NA_real_
      )
    )
  }

  fit <- lm(outcome_value ~ feature_value, data = model_data)

  coef_summary <- summary(fit)$coefficients
  ci <- confint(fit)

  tibble::tibble(
    n_animals = nrow(model_data),
    estimate = coef_summary["feature_value", "Estimate"],
    p_value = coef_summary["feature_value", "Pr(>|t|)"],
    r_squared = summary(fit)$r.squared
  )
}

run_feature_association_layer <- function(data, layer_name) {
  tidyr::expand_grid(
    outcome = c("log_ggt_day21", "log_gldh_day21"),
    feature = early_warning_feature_names
  ) |>
    dplyr::mutate(
      model_layer = layer_name,
      result = purrr::map2(
        outcome,
        feature,
        ~ fit_feature_association(
          data = data,
          outcome = .x,
          feature = .y
        )
      )
    ) |>
    tidyr::unnest(result)
}

WL_health_feature_associations <- dplyr::bind_rows(
  run_feature_association_layer(
    data = WL_health_model_features,
    layer_name = "Whole cohort"
  ),
  run_feature_association_layer(
    data = WL_health_model_features |>
      dplyr::filter(exposure_group == "Dosed"),
    layer_name = "Dosed only"
  )
) |>
  dplyr::mutate(
    outcome_label = dplyr::recode(
      outcome,
      log_ggt_day21 = "Day 21 GGT",
      log_gldh_day21 = "Day 21 GLDH"
    ),
    feature_label = dplyr::recode(
      feature,
      control_referenced_phase_change_grazing_days_1_5 = "Grazing change, days 1–5",
      control_referenced_phase_change_lying_days_1_5 = "Lying change, days 1–5",
      inactivity_shift_days_1_5 = "Inactivity shift, days 1-5",
      control_referenced_phase_change_grazing_days_6_12 = "Grazing change, days 6–12",
      control_referenced_phase_change_lying_days_6_12 = "Lying change, days 6–12",
      inactivity_shift_days_6_12 = "Inactivity shift, days 6–12",
      inactivity_shift_sustained = "Inactivity shift sustained",
      inactivity_shift_progression = "Inactivity shift progression"
    )
  ) |>
  dplyr::select(
    model_layer,
    outcome,
    outcome_label,
    feature,
    feature_label,
    n_animals,
    estimate, 
    p_value,
    r_squared
  )

make_signal_summary_table <- function(results, layer_name) {
  results |>
    filter(model_layer == layer_name) |>
    mutate(
      feature_label = factor(
        feature_label,
        levels = c(
          "Grazing change, days 1–5",
          "Lying change, days 1–5",
          "Inactivity shift, days 1-5",
          "Grazing change, days 6–12",
          "Lying change, days 6–12",
          "Inactivity shift, days 6–12",
          "Inactivity shift sustained",
          "Inactivity shift progression"
        )
      ),
      direction = case_when(
        estimate > 0 ~ "↑",
        estimate < 0 ~ "↓",
        TRUE ~ "–"
      ),
      support_text = paste0(
        direction,
        " R² ",
        sprintf("%.2f", r_squared),
        ", p=",
        case_when(
          is.na(p_value) ~ "NA",
          p_value < 0.001 ~ "<0.001",
          TRUE ~ sprintf("%.3f", p_value)
        )
      )
    ) |>
    select(
      Feature = feature_label,
      Outcome = outcome_label,
      support_text,
      n_animals
    ) |>
    pivot_wider(
      names_from = Outcome,
      values_from = support_text
    ) |>
    mutate(
      `Animals` = n_animals,
      `Read-out` = case_when(
        str_sub(`Day 21 GGT`, 1, 1) == stringr::str_sub(`Day 21 GLDH`, 1, 1) ~
          "Consistent direction",
        TRUE ~ "Mixed direction"
      )
    ) |>
    select(
      Feature,
      `Day 21 GGT`,
      `Day 21 GLDH`,
      `Read-out`,
      Animals
    ) |>
    arrange(Feature)
}
```

```{r}
#| label: tbl-whole-cohort-signal-summary
#| tbl-cap: "Whole-cohort association summary for early-warning behaviour features against day 21 liver-enzyme outcomes."
#| echo: false
#| message: false
#| warning: false

whole_cohort_signal_summary <- make_signal_summary_table(
  results = WL_health_feature_associations,
  layer_name = "Whole cohort"
)

cross_layer_consistent <- WL_health_feature_associations |>
  dplyr::mutate(
    direction = dplyr::case_when(
      estimate > 0 ~ "up",
      estimate < 0 ~ "down",
      TRUE ~ "flat"
    )
  ) |>
  dplyr::group_by(feature_label, outcome) |>
  dplyr::summarise(
    cross_layer_consistent = dplyr::n_distinct(
      direction[direction != "flat"]
    ) == 1,
    .groups = "drop"
  ) |>
  dplyr::group_by(feature_label) |>
  dplyr::summarise(
    cross_layer_consistent = all(cross_layer_consistent),
    .groups = "drop"
  )

whole_cohort_signal_rows <- WL_health_feature_associations |>
  dplyr::filter(model_layer == "Whole cohort") |>
  dplyr::mutate(
    direction = dplyr::case_when(
      estimate > 0 ~ "up",
      estimate < 0 ~ "down",
      TRUE ~ "flat"
    )
  ) |>
  dplyr::group_by(feature_label) |>
  dplyr::summarise(
    max_r_squared = max(r_squared, na.rm = TRUE),
    consistent_direction = dplyr::n_distinct(
      direction[direction != "flat"]
    ) == 1,
    .groups = "drop"
  ) |>
  dplyr::left_join(cross_layer_consistent, by = "feature_label") |>
  dplyr::filter(consistent_direction, cross_layer_consistent) |>
  dplyr::slice_max(
    order_by = max_r_squared,
    n = 2,
    with_ties = FALSE
  ) |>
  dplyr::pull(feature_label)

whole_cohort_signal_summary <- whole_cohort_signal_summary |>
  dplyr::mutate(
    Signal = dplyr::if_else(
      as.character(Feature) %in% whole_cohort_signal_rows,
      "●",
      ""
    )
  ) |>
  dplyr::select(
    Signal,
    Feature,
    `Day 21 GGT`,
    `Day 21 GLDH`,
    `Read-out`,
    Animals
  )

whole_cohort_signal_summary |>
  flextable::flextable() |>
  flextable::theme_booktabs() |>
  flextable::bold(part = "header") |>
  flextable::fontsize(size = 10.5, part = "all") |>
  flextable::color(
    j = "Signal",
    color = "#3d5c3a",
    part = "body"
  ) |>
  flextable::bold(
    j = "Signal",
    bold = TRUE,
    part = "body"
  ) |>
  flextable::align(
    j = "Signal",
    align = "center",
    part = "all"
  ) |>
  flextable::padding(
    padding.top = 6,
    padding.bottom = 6,
    padding.left = 8,
    padding.right = 8,
    part = "all"
  ) |>
  flextable::width(j = "Feature", width = 2.6) |>
  flextable::width(j = c("Day 21 GGT", "Day 21 GLDH"), width = 2.0) |>
  flextable::width(j = "Read-out", width = 1.7) |>
  flextable::width(j = "Animals", width = 0.8) |>
  flextable::align(
    j = c("Day 21 GGT", "Day 21 GLDH", "Animals"),
    align = "center",
    part = "all"
  ) |>
  flextable::border_outer(
    border = officer::fp_border(color = "#B8AFA0", width = 1)
  ) |>
  flextable::border_inner_h(
    border = officer::fp_border(color = "#d1d9ce", width = 0.6)
  ) |>
  flextable::border_inner_v(
    border = officer::fp_border(color = "#d1d9ce", width = 0.6)
  ) |>
  flextable::vline(
    j = c("Feature", "Day 21 GGT", "Day 21 GLDH", "Read-out"),
    border = officer::fp_border(color = "#D8D0C4", width = 0.7),
    part = "all"
  ) |>
    flextable::add_footer_lines(
      values = "● marks the strongest rows with consistent direction across GGT and GLDH."
    ) |>
  flextable::fontsize(size = 9, part = "footer") |>
  flextable::color(color = "#6b6559", part = "footer")
```

```{r}
#| label: tbl-dosed-only-signal-summary
#| tbl-cap: "Dosed-only association summary for early-warning behaviour features against day 21 liver-enzyme outcomes."
#| echo: false
#| message: false
#| warning: false

dosed_only_signal_summary <- make_signal_summary_table(
  results = WL_health_feature_associations,
  layer_name = "Dosed only"
)

dosed_only_signal_rows <- WL_health_feature_associations |>
  dplyr::filter(model_layer == "Dosed only") |>
  dplyr::mutate(
    direction = dplyr::case_when(
      estimate > 0 ~ "up",
      estimate < 0 ~ "down",
      TRUE ~ "flat"
    )
  ) |>
  dplyr::group_by(feature_label) |>
  dplyr::summarise(
    max_r_squared = max(r_squared, na.rm = TRUE),
    consistent_direction = dplyr::n_distinct(direction[direction != "flat"]) == 1,
    .groups = "drop"
  ) |>
  dplyr::left_join(cross_layer_consistent, by = "feature_label") |>
  dplyr::filter(consistent_direction) |>
  dplyr::slice_max(
    order_by = max_r_squared,
    n = 2,
    with_ties = FALSE
  ) |>
  dplyr::pull(feature_label)

dosed_only_signal_summary <- dosed_only_signal_summary |>
  dplyr::mutate(
    Signal = dplyr::if_else(
      as.character(Feature) %in% dosed_only_signal_rows,
      "●",
      ""
    )
  ) |>
  dplyr::select(
    Signal,
    Feature,
    `Day 21 GGT`,
    `Day 21 GLDH`,
    `Read-out`,
    Animals
  )

dosed_only_signal_summary |>
  flextable::flextable() |>
  flextable::theme_booktabs() |>
  flextable::bold(part = "header") |>
  flextable::fontsize(size = 10.5, part = "all") |>
  flextable::color(
    j = "Signal",
    color = "#3d5c3a",
    part = "body"
  ) |>
  flextable::bold(
    j = "Signal",
    bold = TRUE,
    part = "body"
  ) |>
  flextable::align(
    j = "Signal",
    align = "center",
    part = "all"
  ) |>
  flextable::padding(
    padding.top = 6,
    padding.bottom = 6,
    padding.left = 8,
    padding.right = 8,
    part = "all"
  ) |>
  flextable::width(j = "Feature", width = 2.6) |>
  flextable::width(j = c("Day 21 GGT", "Day 21 GLDH"), width = 2.0) |>
  flextable::width(j = "Read-out", width = 1.7) |>
  flextable::width(j = "Animals", width = 0.8) |>
  flextable::align(
    j = c("Day 21 GGT", "Day 21 GLDH", "Animals"),
    align = "center",
    part = "all"
  ) |>
  flextable::border_outer(
    border = officer::fp_border(color = "#B8AFA0", width = 1)
  ) |>
  flextable::border_inner_h(
    border = officer::fp_border(color = "#d1d9ce", width = 0.6)
  ) |>
  flextable::border_inner_v(
    border = officer::fp_border(color = "#d1d9ce", width = 0.6)
  ) |>
  flextable::vline(
    j = c("Feature", "Day 21 GGT", "Day 21 GLDH", "Read-out"),
    border = officer::fp_border(color = "#D8D0C4", width = 0.7),
    part = "all"
  ) |>
    flextable::add_footer_lines(
      values = "● marks the strongest rows with consistent direction across GGT and GLDH."
    ) |>
  flextable::fontsize(size = 9, part = "footer") |>
  flextable::color(color = "#6b6559", part = "footer")
```

