## 06_calculate_rates.R ----
## Use the working dataset to estimate age-standardized rates for each year,
## state, and opioid type.

## Imports ----
library(tidyverse)
library(narcan)     ## devtools::install_github("mkiang/narcan")
library(here)

## DELETE THIS ----
Sys.setenv(R_CONFIG_ACTIVE = "dev")

## Define parameters ----
cfig <- config::get()
data_folder <- cfig$private_data
save_folder <- cfig$working_data
year_0 <- cfig$start_year
year_n <- cfig$end_year
z_rate <- as.numeric(cfig$zero_rate)

## Load data ----
all_data <-
    readRDS(here(data_folder, "state_working_data.RDS")) %>%
    ungroup() %>%
    select(
        -nonop_drug_death,
        -opium_present,
        -methadone_present,
        -other_op_present,
        -unspecified_op_present,
        -num_opioids,
        -multi_opioids
    )

## Add rows for national rate (by year/age) ----
all_data <- bind_rows(
    all_data,
    all_data %>%
        group_by(year, age, race, age_cat) %>%
        summarize_at(
            vars(
                pop,
                deaths,
                drug_death,
                opioid_death,
                heroin_present,
                other_natural_present,
                other_synth_present,
                firearm,
                car_accident
            ),
            sum,
            na.rm = TRUE
        ) %>%
        add_std_pop() %>%
        mutate(
            st_fips = "99",
            abbrev = "US",
            division = "Entire US",
            st_lat = 0,
            st_lon = 0,
            name = "USA"
        )
)

## Convert counts to age-specific rates ----
age_spec_rates <- all_data %>%
    calc_asrate_var(total, deaths) %>%
    calc_asrate_var(drug, drug_death) %>%
    calc_asrate_var(opioid, opioid_death) %>%
    calc_asrate_var(heroin, heroin_present) %>%
    calc_asrate_var(natural_opioid, other_natural_present) %>%
    calc_asrate_var(synth_opioid, other_synth_present) %>%
    calc_asrate_var(firearm, firearm) %>%
    calc_asrate_var(car_accident, car_accident) %>%
    mutate_at(vars(ends_with("_var")), funs(ifelse(is.nan(.), 0, .))) %>%
    ungroup()

total_deaths <- age_spec_rates %>%
    group_by(year, st_fips, abbrev, name, race) %>%
    summarize_at(vars(
        ends_with("_death"),
        ends_with("_present"),
        firearm,
        car_accident,
        pop
    ),
    sum,
    na.rm = TRUE) %>%
    mutate_all(function(x)
        ifelse(x < 10, NA, x)) %>%
    ungroup()

saveRDS(total_deaths,
        here(save_folder, "total_deaths_suppressed.RDS"))

## Calculate age-standardized rates ----
std_death_rates <- age_spec_rates %>%
    calc_stdrate_var(total_rate, total_var,
                     st_fips, abbrev, name, race, year) %>%
    left_join(
        age_spec_rates %>%
            calc_stdrate_var(opioid_rate, opioid_var,
                             st_fips, abbrev, name, race, year)
    ) %>%
    left_join(
        age_spec_rates %>%
            calc_stdrate_var(drug_rate, drug_var,
                             st_fips, abbrev, name, race, year)
    ) %>%
    left_join(
        age_spec_rates %>%
            calc_stdrate_var(
                natural_opioid_rate,
                natural_opioid_var,
                st_fips,
                abbrev,
                name,
                race,
                year
            )
    ) %>%
    left_join(
        age_spec_rates %>%
            calc_stdrate_var(heroin_rate, heroin_var,
                             st_fips, abbrev, name, race, year)
    ) %>%
    left_join(
        age_spec_rates %>%
            calc_stdrate_var(
                synth_opioid_rate,
                synth_opioid_var,
                st_fips,
                abbrev,
                name,
                race,
                year
            )
    ) %>%
    left_join(
        age_spec_rates %>%
            calc_stdrate_var(firearm_rate, firearm_var,
                             st_fips, abbrev, name, race, year)
    ) %>%
    left_join(
        age_spec_rates %>%
            calc_stdrate_var(
                car_accident_rate,
                car_accident_var,
                st_fips,
                abbrev,
                name,
                race,
                year
            )
    ) %>%
    ungroup()

## Fix NaNs ----
## Note that for some fips/race/year combinations, there are literally zero
## popuations. This results in a NaN rate, but should be a zero rate.
std_death_rates <- std_death_rates %>%
    mutate_at(vars(ends_with("_rate")),
              funs(ifelse(is.nan(.) | . == 0, z_rate, .)))

saveRDS(std_death_rates,
        file = here(data_folder, 'age_std_rates.RDS'))

## Make a public version of this dataset ----
## We remove observations based on fewer than 10 deaths to not violate NCHS
## data use rules.
suppress_cells <- read_csv(here(save_folder, "remove_obs.csv"))

suppress_cells <- suppress_cells %>%
    filter(remove_obs == 1) %>%
    select(year, st_fips, opioid_type, remove_obs) %>%
    mutate(opioid_type = paste0(opioid_type, "_remove")) %>%
    spread(opioid_type, remove_obs)

std_death_rates_public <- std_death_rates %>%
    left_join(suppress_cells) %>%
    mutate(
        opioid_rate = ifelse(opioid_remove == 1, NA, opioid_rate),
        opioid_var = ifelse(opioid_remove == 1, NA, opioid_var),
        natural_opioid_rate = 
            ifelse(natural_opioid_remove == 1, NA, natural_opioid_rate),
        natural_opioid_var = 
            ifelse(natural_opioid_remove == 1, NA, natural_opioid_var),
        heroin_rate = ifelse(heroin_remove == 1, NA, heroin_rate),
        heroin_var = ifelse(heroin_remove == 1, NA, heroin_var),
        synth_opioid_rate = 
            ifelse(synth_opioid_remove == 1, NA, synth_opioid_rate),
        synth_opioid_var = 
            ifelse(synth_opioid_remove == 1, NA, synth_opioid_var)
    )
saveRDS(std_death_rates_public,
        file = here(save_folder, 'age_std_rates_PUBLIC.RDS'))
