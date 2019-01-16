## Imports ----
library(tidyverse)
library(ggsci)
library(here)
source(here("code", "mk_nytimes.R"))
library(patchwork)

## Load data ----
jp_results <- readRDS(here("data", "joinpoint_results_public.RDS"))

all_rates <- ggplot() +
    geom_line(
        data = jp_results %>%
            filter(abbrev != "US"),
        aes(
            x = year,
            y = pred_rate,
            group = abbrev,
            color = abbrev == "US"
        ),
        alpha = .5
    ) +
    geom_line(
        data = jp_results %>%
            filter(abbrev == "US"),
        aes(
            x = year,
            y = pred_rate,
            group = abbrev,
            color = abbrev == "US"
        ),
        alpha = 1,
        size = 1.5
    ) +
    facet_grid(~ opioid_cat) +
    mk_nytimes() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous("Modeled Mortality Rate (per 100,000)",
                       expand = c(0, .05)) +
    scale_color_manual(NULL, values = pal_jama()(2), guide = FALSE)

by_types <- ggplot() +
    geom_line(
        data = jp_results %>%
            filter(abbrev != "US",
                   opioid_type != "opioid"),
        aes(
            x = year,
            y = pred_rate,
            group = abbrev,
            color = abbrev == "US"
        ),
        alpha = .5
    ) +
    geom_line(
        data = jp_results %>%
            filter(abbrev == "US",
                   opioid_type != "opioid"),
        aes(
            x = year,
            y = pred_rate,
            group = abbrev,
            color = abbrev == "US"
        ),
        alpha = 1,
        size = 1.5
    ) +
    facet_grid(~ opioid_cat) +
    mk_nytimes() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL,
                       expand = c(0, .05)) +
    scale_color_manual(NULL, values = pal_jama()(2), guide = FALSE)

overall_rate <- ggplot() +
    geom_line(
        data = jp_results %>%
            filter(abbrev != "US",
                   opioid_type == "opioid"),
        aes(
            x = year,
            y = pred_rate,
            group = abbrev,
            color = abbrev == "US"
        ),
        alpha = .5
    ) +
    geom_line(
        data = jp_results %>%
            filter(abbrev == "US",
                   opioid_type == "opioid"),
        aes(
            x = year,
            y = pred_rate,
            group = abbrev,
            color = abbrev == "US"
        ),
        alpha = 1,
        size = 1.5
    ) +
    facet_grid(~ opioid_cat) +
    mk_nytimes() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous("Modeled Mortality Rate (per 100,000)",
                       expand = c(0, .05)) +
    scale_color_manual(NULL, values = pal_jama()(2), guide = FALSE)

overall_rate + by_types + plot_layout(ncol = 2, widths = c(1, 3))


ggsave(
    here("plots", "figs1_modeled_rates.pdf"),
    all_rates,
    width = 7.5,
    height = 3,
    scale = 1,
    device = cairo_pdf
)
