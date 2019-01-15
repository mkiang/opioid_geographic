## 04_create_working_df.R ----
## This creates a super dataframe with state/age/year deaths combined
## with state population, standard population, division, state centroid, etc.
##
## This one dataframe should be able to generate all the other rates we are
## interested in.

## Imports ----
library(tidyverse)
library(narcan)     ## devtools::install_github("mkiang/narcan")
library(here)
source(here("code", "99_helpers.R"))  ## State information

## DELETE THIS ----
Sys.setenv(R_CONFIG_ACTIVE = "dev")

## Define parameters ----
cfig <- config::get()
priv_folder <- cfig$private_data
data_folder <- cfig$working_data
year_0 <- cfig$start_year
year_n <- cfig$end_year

## Load data ----
state_opioid <-
    readRDS(here(priv_folder, "state_opioid_deaths_no_pops.RDS"))

county_pops <-
    readRDS(here(data_folder, "pop_est_collapsed_long.RDS")) %>%
    filter(year %in% year_0:year_n) %>%
    mutate(st_fips = substr(fipsihme, 1, 2)) %>%
    rename(fipschar = fipsihme)

## Remap/filter race_ethnicity column of opioid data ----
## Remap race to one that matches population files and subset to just total
state_opioid <- state_opioid %>%
    mutate(
        race = case_when(race_ethnicity == "all_races_all_hisp" ~ "total")
        ) %>%
    select(-race_ethnicity)

## Munge population data ----
## We want to end up with population estimates for every state from 1999-2016
## by 5-year age group for total population
state_pops <- county_pops %>%
    group_by(st_fips, year, age, race, hispanic) %>%
    summarize(pop_est = sum(pop_est)) %>%
    ungroup()

state_pop_est <- state_pops %>%
    group_by(year, st_fips, age) %>%
    summarize(pop = sum(pop_est)) %>%
    mutate(race = "total") %>%
    ungroup()

## Merge in standard populations and state/division info
state_pop_est <- state_pop_est %>%
    left_join(st_info) %>%
    narcan::add_std_pop(.)

## Save ---- 
saveRDS(state_pop_est,
        sprintf("%s/state_pop_est_working.RDS", data_folder))

## Merge pop and death data ----
## Recategorize age because the join will have NAs, merge the state pops,
## then replace NA counts with 0, and finally add standard population.
## 
## NOTE: We drop 99 opioid cases in the entire data because they do not
## include age.
##
## The `vars(one_of())` is (I think) the most robust way to select only
## the flagged columns. Else, we have to specify by column name, which may
## change as analysis carries on.
all_data <- state_pop_est %>%
    left_join(state_opioid, by = c("year", "st_fips", "age", "race")) %>%
    mutate(age_cat = categorize_age_5(age)) %>%
    mutate_at(vars(one_of(
        state_opioid %>%
            select(-year, -st_fips, -age,
                   -age_cat, -race) %>%
            names()
    )),
    funs(ifelse(is.na(.), 0, .))) %>%
    ungroup()

saveRDS(all_data, sprintf("%s/state_working_data.RDS", priv_folder))

## Create a CSV of every state/year/opioid-type that has < 10 deaths ----
remove_df <- state_opioid %>%
    group_by(year, st_fips) %>%
    summarize_at(
        vars(
            opioid_death,
            heroin_present,
            other_natural_present,
            other_synth_present
        ),
        sum
    ) %>%
    ungroup() %>% 
    rename(
        opioid = opioid_death,
        heroin = heroin_present,
        natural_opioid = other_natural_present,
        synth_opioid = other_synth_present
    ) %>%
    gather(opioid_type, n, opioid:synth_opioid) %>%
    mutate(remove_obs = ifelse(n < 10, 1, 0)) %>%
    select(-n) %>%
    left_join(narcan::st_fips_map,
              by = c("st_fips" = "fips"))

write_csv(remove_df, sprintf("%s/remove_obs.csv", data_folder))
