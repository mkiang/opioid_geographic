## Imports ----
library(tidyverse)
library(viridis)
library(usmap)
library(here)
source(here("code", "mk_nytimes.R"))

## Data ----
lel_df <-
    readRDS(here("data", "ex_diff_all_ages_all_areas.RDS")) %>%
    filter(age == 15,
           year == 2016,
           race == "total",
           abbrev != "US") %>%
    select(-race, -age, -age_cat, -year)

## Better category  names
lel_df <- lel_df %>%
    mutate(opioid_cat = factor(
        opioid_type,
        levels = c(
            "opioids",
            "natural",
            "heroin",
            "synth",
            "car_accident",
            "firearms",
            "drugs"
        ),
        labels = c(
            "All opioids",
            "Natural",
            "Heroin",
            "Synthetic",
            "Car Accidents",
            "Firearms",
            "All drugs"
        ),
        ordered = TRUE
    ))

## Subset to just drugs
sub_lel <- lel_df %>%
    filter(opioid_type %in% c("opioids", "heroin", "natural", "synth"))

## Make prettier color breaks and labels
color_bar_range <- c(.05, .5)
color_bar_brks  <- c(.05, .2, .3, .4, .5)
color_bar_lbls  <- c(
    sprintf("%0.2f to %0.2f",
            min(sub_lel$ex_diff),
            color_bar_range[1]),
    color_bar_brks[2:4],
    sprintf("%0.2f to %0.2f",
            color_bar_range[2],
            max(sub_lel$ex_diff))
)

## Truncate to match the pretty breaks
sub_lel <- sub_lel %>%
    mutate(
        ex_diff_trunc =
            case_when(
                ex_diff < color_bar_range[1] ~ color_bar_range[1],
                ex_diff > color_bar_range[2] ~ color_bar_range[2],
                TRUE ~ ex_diff
            )
    )

## Join with the map
sub_lel <- sub_lel %>%
    right_join(usmap::us_map(regions = "states") %>%
                   rename(abbrev = abbr))

## Plot it
p1 <- ggplot() +
    geom_polygon(
        data = sub_lel,
        aes(
            x = long,
            y = lat,
            group = group,
            fill = ex_diff_trunc
        ),
        color = "black",
        size = .15
    ) +
    scale_fill_viridis(
        option = "C",
        name = "Life Expectancy Lost After Age 15 (years)",
        direction = -1,
        breaks = color_bar_brks,
        labels = color_bar_lbls,
        limits = color_bar_range,
        discrete = FALSE,
        na.value = "grey50"
    ) +
    coord_equal() +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    facet_wrap(~ opioid_cat, ncol = 2) +
    mk_nytimes(
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        # panel.border = element_rect(
        #     linetype = "solid",
        #     fill = NA,
        #     color = "grey75"
        # )
    ) +
    guides(
        fill =
            guide_colorbar(
                title.position = "top",
                barwidth = 25,
                barheight = .5,
                label.position = "bottom",
                label.hjust = .5
            )
    )

ggsave(
    here(config::get()$plot_dir, 'fig2_current_lel15.pdf'),
    p1,
    width = 6,
    height = 5.5,
    scale = 1.25,
    device = cairo_pdf
)
