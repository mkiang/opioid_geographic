## Imports ----
library(shiny)
library(geofacet)
library(tidyverse)
library(DT)
library(statebins)
library(viridis)
library(here)

## Load additional code ----
source(here("shiny_helpers", "mk_nytimes.R"))
source(here("shiny_helpers", "gen_aapc_trends_plot.R"))
source(here("shiny_helpers", "gen_aapc_summary_table.R"))
source(here("shiny_helpers", "gen_hotspots_legend.R"))
source(here("shiny_helpers", "gen_hotspots_map.R"))
source(here("shiny_helpers", "gen_hotspots_statebins.R"))
source(here("shiny_helpers", "gen_hotspots_text.R"))
source(here("shiny_helpers", "gen_color_legend.R"))
source(here("shiny_helpers", "gen_state_apc_results.R"))
source(here("shiny_helpers", "state_table_generators.R"))
source(here("shiny_helpers", "gen_apc_rate_table.R"))
source(here("shiny_helpers", "gen_state_lel_plot.R"))
source(here("shiny_helpers", "gen_lel_map.R"))
source(here("shiny_helpers", "gen_lel_statebins.R"))
source(here("shiny_helpers", "gen_lel_table.R"))

## Make a state name:abbrev dictionary ----
st_name_abbrev <- as.list(c(state.abb, "DC"))
names(st_name_abbrev) <- c(state.name, "District of Columbia")
st_name_abbrev <- as.list(sort(unlist(st_name_abbrev)))

## Load data ----
jp_results <-
    readRDS(here("shiny_data", "joinpoint_results_public.RDS"))
lel_df <-
    readRDS(here("shiny_data", "ex_diff_all_ages_all_areas.RDS"))
lel_df <- lel_df %>%
    filter(race == "total") %>%
    select(-race) %>%
    mutate(opioid_cat = factor(
        opioid_type,
        levels = c(
            "opioids",
            "natural",
            "heroin",
            "synth",
            "car_accident",
            "firearms"
        ),
        labels = c(
            "All opioids",
            "Natural",
            "Heroin",
            "Synthetic",
            "Car Accidents",
            "Firearms"
        ),
        ordered = TRUE
    ))

## Helpers
hotspots_picker <-
    function(jp_results,
             h_p_or_q,
             h_sigpvalue,
             mort_mid_bin,
             apc_mid_bin,
             h_year,
             statebins) {
        if (length(statebins) > 0) {
            gen_hotspots_statebins(
                jp_results = jp_results,
                h_p_or_q = h_p_or_q,
                h_sigpvalue = h_sigpvalue,
                mort_mid_bin = mort_mid_bin,
                apc_mid_bin = apc_mid_bin,
                h_year = h_year
            )
        } else {
            gen_hotspots_map(
                jp_results = jp_results,
                h_p_or_q = h_p_or_q,
                h_sigpvalue = h_sigpvalue,
                mort_mid_bin = mort_mid_bin,
                apc_mid_bin = apc_mid_bin,
                h_year = h_year
            )
        }
    }

lel_picker <-
    function( lel_df,
              l_age, 
              l_year, 
              l_comparison, 
              l_statebins) {
        if (length(l_statebins) > 0) {
            gen_lel_statebins(
                lel_df = lel_df,
                l_age = l_age,
                l_year = l_year,
                l_comparison = l_comparison
            )
        } else {
            gen_lel_map(
                lel_df = lel_df,
                l_age = l_age,
                l_year = l_year,
                l_comparison = l_comparison
            )
        }
    }

## Start server code ----
shinyServer(function(input, output) {
    ## Main joinpoint state map
    output$state_map  <- renderPlot({
        gen_aapc_trends_plot(
            jp_results = jp_results,
            opioid_types = input$outcome,
            show_raw = input$show_raw,
            raw_ci = input$raw_ci,
            model_fit = input$model_fit,
            linetype_sig = input$linetype_sig,
            joinpoint = input$joinpoint,
            joinpoint_sig = input$joinpoint_sig,
            ymax = input$ymax,
            legends_on = input$legends_on,
            sig_aapc_only = input$sig_aapc_only,
            sigpvalue = as.numeric(input$sigpvalue),
            p_or_q = input$p_or_q,
            disable_clip = input$disable_clip
        )
    })
    
    ## AAPC summary table
    output$aapc_table <- DT::renderDataTable({
        gen_aapc_summary_table(jp_results = jp_results,
                               opioid_types = input$outcome)
    })
    
    ## Hotspot legend
    output$hotspots_legend <- renderPlot(expr = {
        gen_hotspots_legend(
            mort_mid_bin = input$mort_mid_bin,
            apc_mid_bin = input$apc_mid_bin
        )
    },
    width = 250)
    
    ## Hotspots map
    output$hotspots_map <-
        renderPlot({
            hotspots_picker(
                jp_results = jp_results,
                h_p_or_q = input$h_p_or_q,
                h_sigpvalue = input$h_sigpvalue,
                mort_mid_bin = input$mort_mid_bin,
                apc_mid_bin = input$apc_mid_bin,
                h_year = input$h_year, 
                statebins = input$h_statebins[[1]]
            )
        })

## More Context ("percentiles")
output$percentile_hotspots <- renderUI({
    gen_hotspots_text(
        jp_results = jp_results,
        h_p_or_q = input$h_p_or_q,
        h_sigpvalue = input$h_sigpvalue,
        mort_mid_bin = input$mort_mid_bin,
        apc_mid_bin = input$apc_mid_bin,
        h_year = input$h_year
    )
})

output$apc_rate_table <- DT::renderDataTable({
    gen_apc_rate_table(jp_results = jp_results,
                       h_year = input$h_year)
})

## State-specific APC plot
output$state_specific <- renderPlot({
    gen_state_apc_results(
        jp_results = jp_results,
        s_outcome = input$s_outcome,
        s_state = input$s_state,
        s_p_or_q = input$s_p_or_q,
        s_sigpvalue = input$s_sigpvalue,
        s_show_raw = input$s_show_raw,
        s_raw_ci = input$s_raw_ci,
        s_model_fit = input$s_model_fit,
        s_linetype_sig = input$s_linetype_sig,
        s_joinpoint = input$s_joinpoint,
        s_joinpoint_sig = input$s_joinpoint_sig,
        s_legends_on = input$s_legends_on
    )
})

## APC state-specific model fit
output$state_table_fit <- DT::renderDataTable({
    gen_state_table_fit(
        jp_results = jp_results,
        s_state = input$s_state,
        s_outcome = input$s_outcome
    )
})

## State-specific LEL plot
output$state_specific_lel <- renderPlot({
    gen_state_lel_plot(lel_df,
                       s_outcome = input$s_outcome,
                       
                       s_state = input$s_state)
})

## APC state-specific model estimates
output$state_table_estimates <- DT::renderDataTable({
    gen_state_table_model_estimates(
        jp_results = jp_results,
        s_state = input$s_state,
        s_outcome = input$s_outcome
    )
})

## APC state-specific model estimates
output$state_table_predictions <- DT::renderDataTable({
    gen_state_table_predictions(
        jp_results = jp_results,
        s_state = input$s_state,
        s_outcome = input$s_outcome
    )
})

## LEL plot
output$lel_map <- renderPlot({
    lel_picker(
        lel_df = lel_df,
        l_age = input$l_age,
        l_year = input$l_year,
        l_comparison = input$l_comparison,
        l_statebins = input$l_statebins
    )
})

## LEL plot
output$lel_table <- DT::renderDataTable({
    gen_lel_table(
        lel_df = lel_df,
        l_age = input$l_age,
        l_year = input$l_year
    )
})
})
