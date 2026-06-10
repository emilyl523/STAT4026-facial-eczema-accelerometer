# R/70_behaviour_summaries.R

message("Creating baseline-adjusted and control-referenced behaviour summaries.")

```{r}
#| label: animal-day-behaviour-layer
#| echo: true
#| message: false
#| warning: false

WL_behaviour_animal_day <- WL_daily_behaviour_temporal_xgb |>
  left_join(
    WL_accelerometer_coverage |>
      select(
        animal_id,
        recording_date,
        recording_study_day,
        coverage_category,
        recorded_hours,
        timestamp_completeness
      ),
    by = c(
      "animal_id",
      "recording_date",
      "recording_study_day"
    )
  ) |>
  left_join(
    WL_day21_target |>
      mutate(animal_id = as.character(animal_id)) |>
      select(
        animal_id,
        exposure_group,
        severity_short,
        fe_severity_class,
        clinically_elevated_ggt,
        ggt_day21,
        gldh_day21
      ),
    by = "animal_id"
  ) |>
  mutate(
    severity_short = factor(
      severity_short,
      levels = observed_severity_levels
    ),
    coverage_category = factor(
      coverage_category,
      levels = coverage_levels
    ),
    usable_for_main_trajectory = coverage_category %in% c(
      "High coverage",
      "Partial coverage"
    )
  )

WL_behaviour_animal_day_long <- WL_behaviour_animal_day |>
  filter(usable_for_main_trajectory) |>
  pivot_longer(
    cols = c(
      grazing_proportion,
      ruminating_proportion,
      lying_proportion,
      grazing_probability,
      ruminating_probability,
      lying_probability
    ),
    names_to = c("behaviour", "summary_type"),
    names_pattern = "(.+)_(proportion|probability)",
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
      proportion = "Predicted proportion",
      probability = "Mean predicted probability"
    )
  )
```

```{r}
#| label: tbl-baseline-normalised-behaviour
#| echo: true
#| message: false
#| warning: false

WL_behaviour_baseline <- WL_behaviour_animal_day_long |>
  filter(
    summary_type == "Predicted proportion",
    recording_study_day < 0
  ) |>
  group_by(
    animal_id,
    behaviour
  ) |>
  summarise(
    baseline_behaviour = median(behaviour_value, na.rm = TRUE),
    baseline_days = n_distinct(recording_study_day),
    baseline_recorded_hours = median(recorded_hours, na.rm = TRUE),
    baseline_timestamp_completeness = median(timestamp_completeness, na.rm = TRUE),
    .groups = "drop"
  )

WL_behaviour_change <- WL_behaviour_animal_day_long |>
  filter(summary_type == "Predicted proportion") |>
  left_join(
    WL_behaviour_baseline,
    by = c("animal_id", "behaviour")
  ) |>
  mutate(
    change_from_baseline = behaviour_value - baseline_behaviour,
    relative_change_from_baseline = if_else(
      !is.na(baseline_behaviour) & baseline_behaviour > 0,
      change_from_baseline / baseline_behaviour,
      NA_real_
    ),
    has_baseline = !is.na(baseline_behaviour)
  ) |>
  filter(has_baseline)

# baseline support check 
WL_baseline_support_summary <- WL_behaviour_baseline |>
  left_join(
    WL_day21_target |>
      mutate(animal_id = as.character(animal_id)) |>
      select(
        animal_id,
        exposure_group,
        severity_short
      ),
    by = "animal_id"
  ) |>
  group_by(
    severity_short,
    behaviour
  ) |>
  summarise(
    n_animals = n_distinct(animal_id),
    median_baseline_days = median(baseline_days, na.rm = TRUE),
    min_baseline_days = min(baseline_days, na.rm = TRUE),
    median_baseline_behaviour = median(baseline_behaviour, na.rm = TRUE),
    lower_quartile_baseline = quantile(baseline_behaviour, 0.25, na.rm = TRUE),
    upper_quartile_baseline = quantile(baseline_behaviour, 0.75, na.rm = TRUE),
    median_recorded_hours = median(baseline_recorded_hours, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    behaviour,
    severity_short
  )

WL_baseline_support_summary
```

```{r}
#| label: tbl-daily-contribution-support-check
#| echo: true
#| message: false
#| warning: false

daily_contribution_support <- WL_behaviour_control_referenced |>
  filter(
    summary_type == "Predicted proportion",
    coverage_category %in% c("High coverage", "Partial coverage")
  ) |>
  group_by(
    severity_short,
    behaviour,
    recording_study_day
  ) |>
  summarise(
    n_animals = n_distinct(animal_id),
    .groups = "drop"
  ) |>
  mutate(
    analysis_phase = case_when(
      recording_study_day >= 1 & recording_study_day <= 5 ~ "Days 1–5",
      recording_study_day >= 6 & recording_study_day <= 12 ~ "Days 6–12",
      recording_study_day >= 13 & recording_study_day <= 21 ~ "Days 13–21",
      TRUE ~ NA_character_
    ),
    analysis_phase = factor(
      analysis_phase,
      levels = c("Days 1–5", "Days 6–12", "Days 13–21")
    )
  ) |>
  filter(!is.na(analysis_phase))

daily_contribution_support_summary <- daily_contribution_support |>
  group_by(
    severity_short,
    behaviour,
    analysis_phase
  ) |>
  summarise(
    median_daily_animals = median(n_animals, na.rm = TRUE),
    minimum_daily_animals = min(n_animals, na.rm = TRUE),
    maximum_daily_animals = max(n_animals, na.rm = TRUE),
    number_of_daily_points = n(),
    daily_points_with_one_animal = sum(n_animals == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    behaviour,
    analysis_phase,
    severity_short
  )

daily_contribution_support_summary
```

```{r}
#| label: phase-level-baseline-change
#| echo: true
#| message: false
#| warning: false

WL_behaviour_phase_change <- WL_behaviour_change |>
  filter(
    summary_type == "Predicted proportion",
    coverage_category %in% c("High coverage", "Partial coverage"),
    recording_study_day > 0
  ) |>
  mutate(
    analysis_phase = case_when(
      recording_study_day >= 1 & recording_study_day <= 5 ~ "Days 1–5",
      recording_study_day >= 6 & recording_study_day <= 12 ~ "Days 6–12",
      recording_study_day >= 13 & recording_study_day <= 21 ~ "Days 13–21",
      TRUE ~ NA_character_
    ),
    analysis_phase = factor(
      analysis_phase,
      levels = c("Days 1–5", "Days 6–12", "Days 13–21")
    )
  ) |>
  filter(!is.na(analysis_phase)) |>
  group_by(
    animal_id,
    exposure_group,
    severity_short,
    fe_severity_class,
    clinically_elevated_ggt,
    ggt_day21,
    gldh_day21,
    behaviour,
    analysis_phase
  ) |>
  summarise(
    median_change_from_baseline = median(change_from_baseline, na.rm = TRUE),
    mean_change_from_baseline = mean(change_from_baseline, na.rm = TRUE),
    number_of_observed_days = n_distinct(recording_study_day),
    observed_study_days = paste(sort(unique(recording_study_day)), collapse = ", "),
    median_recorded_hours = median(recorded_hours, na.rm = TRUE),
    median_timestamp_completeness = median(timestamp_completeness, na.rm = TRUE),
    .groups = "drop"
  )
```

```{r}
#| label: phase-level-control-reference
#| echo: true
#| message: false
#| warning: false

phase_control_reference <- WL_behaviour_phase_change |>
  filter(exposure_group == "Control") |>
  group_by(
    behaviour,
    analysis_phase
  ) |>
  summarise(
    control_median_phase_change = median(
      median_change_from_baseline,
      na.rm = TRUE
    ),
    control_n_animals = n_distinct(animal_id),
    .groups = "drop"
  )

WL_behaviour_phase_control_referenced <- WL_behaviour_phase_change |>
  left_join(
    phase_control_reference,
    by = c("behaviour", "analysis_phase")
  ) |>
  mutate(
    control_referenced_phase_change =
      median_change_from_baseline - control_median_phase_change
  )

WL_phase_behaviour_summary <- WL_behaviour_phase_control_referenced |>
  group_by(
    severity_short,
    behaviour,
    analysis_phase
  ) |>
  summarise(
    n_animals = n_distinct(animal_id),
    median_change_from_baseline = median(
      median_change_from_baseline,
      na.rm = TRUE
    ),
    lower_quartile_change = quantile(
      median_change_from_baseline,
      0.25,
      na.rm = TRUE
    ),
    upper_quartile_change = quantile(
      median_change_from_baseline,
      0.75,
      na.rm = TRUE
    ),
    median_control_referenced_change = median(
      control_referenced_phase_change,
      na.rm = TRUE
    ),
    lower_quartile_control_referenced = quantile(
      control_referenced_phase_change,
      0.25,
      na.rm = TRUE
    ),
    upper_quartile_control_referenced = quantile(
      control_referenced_phase_change,
      0.75,
      na.rm = TRUE
    ),
    median_observed_days = median(number_of_observed_days, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    behaviour,
    analysis_phase,
    severity_short
  )
```
