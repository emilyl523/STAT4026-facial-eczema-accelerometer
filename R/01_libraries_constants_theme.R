# R/01_libraries_constants_theme.R

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(fs)
library(data.table)
library(gt)
library(scales)
library(stringr)
library(htmltools)
library(glue)
library(patchwork)
library(flextable)
library(xgboost)
library(pROC)

coverage_levels <- c(
  "High coverage",
  "Partial coverage",
  "Low coverage",
  "No coverage"
)

coverage_colours <- c(
  "High coverage"    = "#b9d6b6",
  "Partial coverage" = "#edd691",
  "Low coverage"     = "#d6ad9c",
  "No coverage"      = "#a64b4b"
)

severity_levels <- c(
  "Control",
  "Dosed: below clinical threshold",
  "Dosed: mild GGT elevation",
  "Dosed: moderate GGT elevation",
  "Dosed: severe GGT elevation"
)

severity_short_labels <- c(
  "Control" = "Control",
  "Dosed: below clinical threshold" = "Below threshold",
  "Dosed: mild GGT elevation" = "Mild",
  "Dosed: moderate GGT elevation" = "Moderate",
  "Dosed: severe GGT elevation" = "Severe"
)

severity_colours <- c(
  "Control"                         = "#2F6F9F",
  "Dosed: below clinical threshold"  = "#E2B84C",
  "Dosed: mild GGT elevation"        = "#D9822B",
  "Dosed: moderate GGT elevation"    = "#B83A32",
  "Dosed: severe GGT elevation"      = "#5E1B1B"
)

severity_short_colours <- c(
  "Control"         = "#2F6F9F",
  "Below threshold" = "#E2B84C",
  "Mild"            = "#D9822B",
  "Moderate"        = "#B83A32",
  "Severe"          = "#5E1B1B"
)

group_colours <- c(
  "Control" = "#2F6F9F",
  "Dosed"   = "#B83A32"
)

group_labels <- c(
  "Control" = "Control",
  "Dosed" = "Dosed"
)

behaviour_colours <- c(
  "Grazing"    = "#78b874",
  "Lying"      = "#749dab",
  "Ruminating" = "#b5a282"
)

ggt_threshold_mild <- 70
ggt_threshold_moderate <- 300
ggt_threshold_severe <- 700

fmt_count <- function(x) {
  scales::comma(x, accuracy = 1)
}

fmt_percent <- function(x) {
  scales::percent(x, accuracy = 1)
}

theme_eda <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face   = "bold",
        size   = base_size + 1,
        colour = "#1e2820",
        margin = ggplot2::margin(t = 4, b = 4)
      ),
      plot.subtitle = element_text(
        colour = "#6b6559",
        size   = base_size - 1,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = element_text(
        colour = "#b8afa0",
        size   = base_size - 2,
        margin = ggplot2::margin(t = 8)
      ),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "#cfcbc4", linewidth = 0.38),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(
        colour    = "#d0c8b8",
        fill      = NA,
        linewidth = 0.4
      ),
      axis.title = element_text(colour = "#3a4236", size = base_size - 1),
      axis.text  = element_text(colour = "#6b6559", size = base_size - 1),
      axis.ticks = element_line(colour = "#b8afa0", linewidth = 0.35),
      strip.background = element_rect(
        fill      = "#e4eeee",
        colour    = "#c8c0b0",
        linewidth = 0.4
      ),
      strip.text = element_text(
        face   = "bold",
        colour = "black",
        size   = 10
      ),
      legend.position   = "bottom",
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key        = element_rect(fill = "transparent", colour = NA),
      legend.title = element_text(
        colour = "#3a4236",
        size   = base_size - 1,
        face   = "bold"
      ),
      legend.text = element_text(colour = "#6b6559", size = base_size - 1),
      plot.margin = ggplot2::margin(16, 16, 16, 16)
    )
}
