## Imports ----
library(tidyverse)
library(here)
library(qvalue)     ## devtools::install_github("jdstorey/qvalue")
source(here("code", "99_helpers.R"))

## Pull in YAML config ----
cfig <- config::get()

## Define YAML parameters
priv_data <- cfig$private_data
save_data <- cfig$working_data

## Get all the year/state/opioid_type observations we need to remove ----
remove_df <- read_csv(here(save_data, "remove_obs.csv"))

## Make a dataframe to sort states ----
state_cat <- tibble(abbrev = c(state.abb, "DC"),
                    name = c(state.name, "D.C.")) %>%
    arrange(name) %>%
    add_row(abbrev = "US",
            name = "Total")

## Import the results dataframe ----
## which has the fitted line and the raw data
jp_results <-
    import_results_data(here(
        "joinpoint_analyses",
        "jp_output",
        "opioids_by_type.data.txt"
    )) %>%
    select(
        year,
        abbrev,
        opioid_type,
        race,
        rate,
        rate_se = standarderror,
        pred_rate = model,
        everything()
    ) %>%
    select(-apc)

## APC bounds
jp_results <- jp_results %>%
    left_join(
        import_results_data(
            here("joinpoint_analyses", "jp_output", "opioids_by_type.apc.txt")
        ) %>%
            select(race, opioid_type, abbrev, year = segmentstart, apc, apc95lcl, apc95ucl)
    ) %>%
    group_by(race, abbrev, opioid_type) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
        apc = zoo::na.locf(apc),
        apc95lcl = zoo::na.locf(apc95lcl),
        apc95ucl = zoo::na.locf(apc95ucl)
    )

## Import some columns from the model estimates dataframe ----
## which has the slope and intercepts of every segment
jp_estimates <-
    import_results_data(here(
        "joinpoint_analyses",
        "jp_output",
        "opioids_by_type.modelestimates.txt"
    )) %>%
    select(
        year = joinpoint,
        abbrev,
        opioid_type,
        race,
        int_est = interceptestimate,
        int_se = interceptstderror,
        int_pval = interceptp_value,
        slope_est = slopeestimate,
        slope_se = slopestderror,
        slope_pval = slopep_value,
        slopechange_est = slopechgestimate,
        slopechange_se = slopechgstderror,
        slopechange_pval = slopechgp_value,
        df,
        sse,
        mse,
        year_lb = joinpoint95lcl,
        year_ub = joinpoint95ucl,
        model,
        segment
    ) %>%
    mutate(j_year = year,
           year = ifelse(is.na(year), 1999, year))

## Import the AAPC dataframe ----
## which has average APC over the entire period
aapc_df <- import_results_data(here(
    "joinpoint_analyses",
    "jp_output",
    "opioids_by_type.aapc.txt"
)) %>%
    rename(
        sig_aapc  = `statisticallysignificant(0=no1=yes)`,
        aapc_pval = p_value,
        aapc_ub   = aapcc_i_high,
        aapc_lb   = aapcc_i_low
    ) %>%
    mutate(
        aapc_sig = ifelse(sig_aapc == 1, aapc, NA),
        aapc_cat = factor(
            sig_aapc,
            levels = 0:1,
            labels = c("Non-significant", "Significant"),
            ordered = TRUE
        )
    ) %>%
    filter(opioid_type %in% c("opioid", "heroin",
                              "natural_opioid", "synth_opioid")) %>%
    select(-joinpointmodel,
           -aapcindex,
           -startobs,
           -endobs,
           -teststatistic)

## Estimate the false discovery rate (FDR) adjusted p-values. ----
##
## Due to multiple comparisons, we expect to get some number of
## "significant" findings by chance. One way to adjust our p-value is to
## use q-values, which adjust for the number of false positives one would
## expect to find. See this for more:
##  http://genomine.org/papers/Storey_FDR_2011.pdf
aapc_qvals <- qvalue(aapc_df$aapc_pval,
                     lambda = seq(0.01, 0.70, 0.01))
aapc_df$aapc_qval <- aapc_qvals$qvalues

jp_int_qvals <- qvalue(jp_estimates$int_pval)
jp_estimates$int_qval <- jp_int_qvals$qvalues

jp_slope_qvals <- qvalue(jp_estimates$slope_pval)
jp_estimates$slope_qval <- jp_slope_qvals$qvalues

jp_change_qvals <- qvalue(jp_estimates$slopechange_pval,
                          lambda = seq(0.01, 0.33, 0.01))
jp_estimates$slopechange_qval <- jp_change_qvals$qvalues


## Join them together ----
## and then use last obs carried forward
jp_results <- jp_results %>%
    left_join(jp_estimates) %>%
    left_join(aapc_df) %>%
    mutate_at(vars(starts_with("int_"), starts_with("slope_"), model, segment),
              zoo::na.locf)

jp_results <- jp_results %>%
    filter(opioid_type %in% c("opioid", "natural_opioid",
                              "heroin", "synth_opioid")) %>%
    mutate(
        opioid_cat = factor(
            opioid_type,
            levels = c("opioid", "natural_opioid",
                       "heroin", "synth_opioid"),
            labels = c("All opioids", "Natural",
                       "Heroin", "Synthetic"),
            ordered = TRUE
        ),
        race_cat = factor(
            race,
            levels = c("nhw", "nhb", "total"),
            labels = c("Non-Hispanic White",
                       "Non-Hispanic Black",
                       "Total"),
            ordered = TRUE
        )
    )

## Indicators for significant slope and changepoints ----
jp_results <- jp_results %>%
    mutate(
        slope_sig    = (slope_pval < .05) + 0,
        slopechg_sig = (slopechange_pval < .05) + 0
    )

## Create a line segment grouping for every regression line ----
jp_results <- jp_results %>%
    group_by(opioid_type,
             abbrev,
             race,
             int_est,
             int_se,
             slope_est,
             slope_pval) %>%
    mutate(line_seg = case_when(year == min(year) ~ 1, TRUE ~ 0)) %>%
    group_by(opioid_type, race, abbrev) %>%
    mutate(line_seg = cumsum(line_seg)) %>%
    ungroup()

## Double observations for sig-non-sig transition ----
## We need to duplicate rows where there's a transition between
## significant and non-significant segments or else the lines get weird
## when plotting.
##
## So we take the dataframe, we find all models with more than one line
## segment, take the first observation from the line segments (after the
## first segment), duplicate that row and assign it the line segment before it.
##
## Then we need to make sure that the duplicate row switches significance
## to match the rest of the line segment when necessary.
##
## Add a `dupe_row` column so it is easier to filter out the rows when we
## don't need them. Always use filter(dupe_row == 0) when doing non-plotting
## tasks.
jp_results <- bind_rows(
    jp_results %>%
        mutate(dupe_row = 0),
    jp_results %>%
        filter(line_seg > 1) %>%
        group_by(abbrev, opioid_type, race, line_seg) %>%
        filter(year == min(year)) %>%
        ungroup() %>%
        mutate(line_seg = line_seg - 1,
               dupe_row = 1)
) %>%
    arrange(abbrev, race, opioid_type, year, line_seg) %>%
    group_by(abbrev, race, opioid_type, line_seg) %>%
    mutate(slope_sig = getmode(slope_sig)) %>%
    ungroup()

## Add in an ordered state variable ----
jp_results <- jp_results %>%
    mutate(name_cat = factor(
        abbrev,
        levels = state_cat$abbrev,
        labels = state_cat$name,
        ordered = TRUE
    ))

## Add a flag to suppress observations ----
jp_results <- jp_results %>%
    left_join(remove_df %>%
                  select(-name, -st_fips))

## Save it ----
## A public version for the Shiny app (removes all N < 10 rates and MSE/SSE)
## and a private version for the paper.
saveRDS(
    jp_results %>%
        mutate(
            mse = ifelse(remove_obs == 1, NA, mse),
            sse = ifelse(remove_obs == 1, NA, sse),
            rate = ifelse(remove_obs == 1, NA, rate),
            rate_se = ifelse(remove_obs == 1, NA, rate_se)
        ),
    file = here(save_data, "joinpoint_results_public.RDS")
)

saveRDS(
    jp_results %>%
        mutate(
            mse = ifelse(remove_obs == 1, NA, mse),
            sse = ifelse(remove_obs == 1, NA, sse),
            rate = ifelse(remove_obs == 1, NA, rate),
            rate_se = ifelse(remove_obs == 1, NA, rate_se)
        ),
    file = here(
        "online_results_viewer",
        "shiny_data",
        "joinpoint_results_public.RDS"
    )
)

saveRDS(jp_results,
        file = here(priv_data, "joinpoint_results_private.RDS"))
