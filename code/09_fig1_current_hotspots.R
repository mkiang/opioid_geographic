## 09_fig1_current_hospots.R ----
## 
## Make figure 1 using the shiny helper functions. See the helper functions
## used for details. 

## Imports ----
library(tidyverse)
library(statebins)
library(here)

## Pull in YAML config ----
cfig <- config::get()

## Define YAML parameters
priv_data <- cfig$private_data
save_data <- cfig$working_data
plot_dir <- cfig$plot_dir
mort_mid_bin <- cfig$rate_cuts
apc_mid_bin <- cfig$apc_cuts
sigpvalue <- cfig$sig_p_value

## Load functions ----
source(here("results_viewer", "shiny_helpers", "gen_hotspots_legend.R"))
source(here(
    "results_viewer",
    "shiny_helpers",
    "gen_hotspots_map.R"
))
source(here("results_viewer", "shiny_helpers", "gen_hotspots_text.R"))
source(here("results_viewer", "shiny_helpers", "gen_color_legend.R"))
source(here("code", "mk_nytimes.R"))

## Load data ----
## For the paper, we used the private version of this file. I commented
## it out here because for many years, the identical plot can be generated
## with the public file.
jp_results <-
    readRDS(here(save_data, "joinpoint_results_public.RDS"))
# jp_results <-
#     readRDS(here(priv_data, "joinpoint_results_private.RDS"))

## Plots ----
p1 <- gen_hotspots_map(jp_results, "pval",
                             sigpvalue,
                             mort_mid_bin,
                             apc_mid_bin,
                             2016)
p_legend <- gen_hotspots_legend(mort_mid_bin, apc_mid_bin) +
    labs(subtitle = NULL)

## Save plots ----
ggsave(
    here(plot_dir, "fig1_current_hotspots.pdf"),
    p1,
    width = 6.5,
    height = 5,
    scale = 1.2,
    device = cairo_pdf
)

ggsave(
    here(plot_dir, "fig1_legend.pdf"),
    p_legend,
    width = 2.5,
    height = 2.5,
    scale = 1,
    device = cairo_pdf
)

## Save png version for README file
ggsave(
    here(plot_dir, "fig1_current_hotspots.png"),
    p1,
    width = 6.5,
    height = 5,
    scale = 1.2
)

ggsave(
    here(plot_dir, "fig1_legend.png"),
    p_legend,
    width = 2.5,
    height = 2.5,
    scale = 1
)

## Print the text -- for reference in paper
print(gen_hotspots_text(jp_results,
                        2016,
                        mort_mid_bin,
                        apc_mid_bin,
                        "pval",
                        .05))

# ## Code for making plot for submission
# library(cowplot)
# pp <- plot_grid(p_legend, p1, align = "hv", axis = "b",
#                 rel_widths = c(1, 4.5))
# ggsave(here(plot_dir, "fig1_hotspots_map.pdf"),
#        pp, width = 8, height = 5.5, scale = 1.5, device = cairo_pdf)
