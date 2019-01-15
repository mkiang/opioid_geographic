## 05_calculate_lel.R ----
## Use the working dataframe to estimate life expectancy lost for all age 
## groups above 15 by state, year, and opioid type.

## Imports ----
library(tidyverse)
library(here)
source(here("code", "99_helpers.R"))

## Define parameters ----
cfig <- config::get()
priv_folder <- cfig$private_data
data_folder <- cfig$working_data

## Get data ---- 
state_df <- readRDS(here(priv_folder, "state_working_data.RDS"))

## Just select columns we want in order we want them
working_df <- state_df %>%
    select(year,
           st_fips,
           abbrev,
           race,
           age,
           age_cat,
           pop,
           deaths:car_accident)

## Bind with a national category
working_df <- bind_rows(
    working_df,
    working_df %>%
        group_by(year, race, age, age_cat) %>%
        summarize_if(is.numeric, sum) %>%
        mutate(st_fips = "99",
               abbrev = "US")
)

## Make a grid dataframe so we don't need to do nested for loops
subsets_df <- expand.grid(unique(working_df$year),
                          unique(working_df$race),
                          unique(working_df$abbrev))

## Now just loop through the expanded grid once with all the parms and save
## ONTE: Can change this to be parallel at some point, but it's fairly fast.
holder <- list()
for (i in 1:nrow(subsets_df)) {
    holder[[i]] <- get_ex_diff_all_types(
        working_df,
        year_x = subsets_df$Var1[i],
        race_x = subsets_df$Var2[i],
        st_abbrev = subsets_df$Var3[i]
    )
}

## Reduce it down
holder <- bind_rows(holder)

## Save
saveRDS(holder, here(data_folder, "ex_diff_all_ages_all_areas.RDS"))
