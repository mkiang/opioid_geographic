## Plot statebins of the AAPC by race/ethnicity and opioid type
library(tidyverse)
library(statebins)
library(viridis)
library(here)
source(here('code', 'mk_nytimes.R'))

cfig <-  config::get()
PVAL <- cfig$sig_p_value

## Helpers
import_results_data <- function(filename) {
    df <- readr::read_delim(
        filename,
        delim = ";",
        escape_double =  FALSE,
        trim_ws = TRUE,
        col_types = cols()
    )
    names(df) <- tolower(names(df))
    names(df) <- gsub(" |#|%|", "", names(df))
    names(df) <- gsub(",|-|\\.", "_", names(df))
    
    return(df)
}

## Import AAPC data
aapc_df <- import_results_data(here(
    "joinpoint_analyses",
    "jp_output",
    "opioids_by_type.aapc.txt"
))

sub_aapc <- aapc_df %>%
    rename(sig_aapc = `statisticallysignificant(0=no1=yes)`) %>%
    mutate(
        race_cat = factor(
            race,
            levels = c("total", "nhw", "nhb"),
            labels = c("Total", "Non-Hispanic White",
                       "Non-Hispanic Black"),
            ordered = TRUE
        ),
        opioid_cat = factor(
            opioid_type,
            levels =
                c("opioid", "natural_opioid",
                  "heroin", "synth_opioid"),
            labels = c("All opioids", "Natural",
                       "Heroin", "Synthetic"),
            ordered = TRUE
        )
    ) %>%
    filter(
        opioid_type %in% c("opioid", "heroin",
                           "natural_opioid", "synth_opioid"),
        race == "total",
        abbrev != "US"
    ) %>%
    select(
        opioid_cat,
        race_cat,
        abbrev,
        startobs,
        endobs,
        aapc,
        aapcc_i_low,
        aapcc_i_high,
        p_value
    ) %>%
    arrange(opioid_cat, race_cat, abbrev)

sub_aapc <- sub_aapc %>%
    mutate(aapc_sig = ifelse(p_value < PVAL, aapc, NA))

## Plot it
brks <- c(min(sub_aapc$aapc_sig, na.rm = TRUE),
          10,
          20,
          30,
          max(sub_aapc$aapc_sig, na.rm = TRUE))

lbls <- sprintf("%0.1f", c(
    min(sub_aapc$aapc_sig, na.rm = TRUE),
    10,
    20,
    30,
    max(sub_aapc$aapc, na.rm = TRUE)
))

p <- ggplot(sub_aapc, aes(fill = aapc_sig, state = abbrev)) +
    geom_statebins() +
    scale_fill_viridis(
        "AAPC (%)",
        direction = -1,
        option = "plasma",
        breaks = brks,
        labels = lbls
    ) +
    facet_wrap( ~ opioid_cat, ncol = 2) +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    mk_nytimes(
        legend.position = "bottom",
        panel.border = element_rect(
            linetype = "solid",
            fill = NA,
            color = "grey75"
        ),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(.25, "cm")
    )

ggsave(
    './plots/figs2_aapc_statebins.pdf',
    p,
    width = 6,
    height = 5,
    scale = 1.25,
    device = cairo_pdf
)