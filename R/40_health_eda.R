# R/40_health_eda.R

message("Preparing health EDA and severity endpoint objects.")

```{r}
#| label: eda-health-target-setup
#| include: false
#| message: false
#| warning: false

WL_health_eda <- WL_health_data_clean |>
  mutate(
    exposure_group = factor(
      exposure_group,
      levels = c("Control", "Dosed")
    ),
    fe_severity_class = factor(
      fe_severity_class,
      levels = severity_levels
    ),
    severity_short = recode(
      as.character(fe_severity_class),
      !!!severity_short_labels
    ),
    severity_short = factor(
      severity_short,
      levels = severity_short_labels[severity_levels]
    )
  )

WL_day21_target <- WL_health_eda |>
  filter(study_day == 21) |>
  distinct(
    animal_id,
    exposure_group,
    ggt_day21,
    gldh_day21,
    fe_severity_class,
    severity_short,
    clinically_elevated_ggt
  )

WL_health_long_eda <- WL_health_eda |>
  select(
    animal_id,
    exposure_group,
    study_day,
    ggt,
    gldh,
    log_ggt,
    log_gldh,
    fe_severity_class,
    severity_short
  ) |>
  pivot_longer(
    cols = c(ggt, gldh),
    names_to = "marker",
    values_to = "raw_value"
  ) |>
  mutate(
    marker = recode(
      marker,
      ggt = "GGT",
      gldh = "GLDH"
    ),
    marker = factor(marker, levels = c("GGT", "GLDH")),
    log_value = log1p(raw_value)
  )

observed_severity_levels <- WL_day21_target |>
  count(severity_short, name = "n_animals") |>
  filter(n_animals > 0) |>
  pull(severity_short) |>
  as.character()

observed_severity_colours <- severity_short_colours[
  names(severity_short_colours) %in% observed_severity_levels
]

WL_day21_target <- WL_day21_target |>
  mutate(
    severity_short = factor(
      severity_short,
      levels = observed_severity_levels
    )
  )

WL_health_eda <- WL_health_eda |>
  mutate(
    severity_short = factor(
      severity_short,
      levels = observed_severity_levels
    )
  )

WL_health_long_eda <- WL_health_long_eda |>
  mutate(
    severity_short = factor(
      severity_short,
      levels = observed_severity_levels
    )
  )
```

# fig-eda-cohort-and-health-target data setup only
```{r}
p_cohort <- WL_health_eda |>
  distinct(animal_id, exposure_group) |>
  count(exposure_group, name = "n_animals") |>
  ggplot(aes(x = exposure_group, y = n_animals, fill = exposure_group)) +
  geom_col(width = 0.5, colour = NA) +
  geom_text(
    aes(label = n_animals),
    vjust = -0.55,
    fontface = "bold",
    size = 4,
    colour = "grey25"
  ) +
  scale_fill_manual(values = group_colours, labels = group_labels) +
  scale_y_continuous(
    breaks = seq(0, 20, by = 5),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "A — Retained cohort",
    x = NULL,
    y = "Animals"
  ) +
  theme_eda(base_size = 10) +
  theme(legend.position = "none")

p_day21_severity <- WL_day21_target |>
  filter(exposure_group == "Dosed") |>
  count(severity_short, name = "n_animals") |>
  mutate(
    severity_short = factor(
      severity_short,
      levels = setdiff(observed_severity_levels, "Control")
    )
  ) |>
  ggplot(aes(x = severity_short, y = n_animals, fill = severity_short)) +
  geom_col(width = 0.55, colour = NA) +
  geom_text(
    aes(label = n_animals),
    vjust = -0.5,
    fontface = "bold",
    size = 3.7,
    colour = "grey25"
  ) +
  scale_fill_manual(values = observed_severity_colours, drop = TRUE) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(
    title = "B — Day 21 GGT severity among dosed animals",
    x = NULL,
    y = "Animals"
  ) +
  theme_eda(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(hjust = 0.5)
  )
```

# fig-eda-primary-support-health-trajectories data setup only
```{r}
health_summary_eda <- WL_health_long_eda |>
  group_by(marker, severity_short, study_day) |>
  summarise(
    median_log_value = median(log_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    marker = factor(marker, levels = c("GGT", "GLDH")),
    severity_short = factor(
      severity_short,
      levels = observed_severity_levels
    )
  )

reference_days <- tibble(
  study_day = c(-7, 0, 1, 7, 21)
)
```

# fig-eda-accelerometer-coverage-readiness data setup only
```{r}
coverage_plot_data <- WL_accelerometer_coverage |>
  mutate(
    animal_id = as.character(animal_id),
    recording_date = as.Date(recording_date)
  ) |>
  left_join(
    WL_day21_target |>
      select(animal_id, exposure_group, severity_short),
    by = "animal_id"
  ) |>
  mutate(
    severity_short = replace_na(as.character(severity_short), "Unlinked"),
    severity_short = factor(
      severity_short,
      levels = c("Control", "Below threshold", "Mild", "Moderate", "Severe", "Unlinked")
    ),
    coverage_category = factor(
      coverage_category,
      levels = names(coverage_colours)
    ),
    time_index = recording_study_day
  )

reference_lines <- tibble(
  time_index = c(-7, 0, 1, 7, 21),
  reference = c("Baseline", "Exposure", "Acute", "Recovery", "Follow-up")
)

animal_order <- coverage_plot_data |>
  distinct(animal_id, severity_short) |>
  arrange(severity_short, animal_id) |>
  pull(animal_id)
```




