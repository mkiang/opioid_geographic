## 07_reshape_for_joinpoint.R ----
## Right now the data are in a wide format, but the joinpoint program needs
## them long We just reshape and re-arrange the data in a way compatible 
## with the joinpoint program.

## Imports ----
library(tidyverse)
library(here)

## Define parameters ----
cfig <- config::get()
data_folder <- cfig$private_data
save_folder <- cfig$working_data
z_rate <- as.numeric(cfig$zero_rate)

## Get rates and counts
std_rates_wide <- readRDS(here(data_folder, "age_std_rates.RDS"))
death_counts <-
    readRDS(here(save_folder, 'total_deaths_suppressed.RDS'))

## Convert to long
std_rates_long <- left_join(
    std_rates_wide %>%
        select(st_fips, abbrev, name, race, year, ends_with("_rate")) %>%
        gather(opioid_type, rate, ends_with("rate")) %>%
        mutate(opioid_type = gsub("_rate", "", fixed = TRUE, opioid_type)),
    std_rates_wide %>%
        select(st_fips, abbrev, name, race, year, ends_with("_var")) %>%
        gather(opioid_type, var, ends_with("var")) %>%
        mutate(opioid_type = gsub("_var", "", fixed = TRUE, opioid_type))
) %>%
    ungroup()

std_rates_long <- std_rates_long %>%
    filter(opioid_type %in% c("opioid", "drug", "natural_opioid",
                              "heroin", "synth_opioid")) %>%
    left_join(
        death_counts %>%
            select(
                st_fips,
                race,
                year,
                opioid = opioid_death,
                drug = drug_death,
                natural_opioid = other_natural_present,
                heroin = heroin_present,
                synth_opioid = other_synth_present,
                abbrev,
                name,
                pop
            ) %>%
            gather(opioid_type, count, opioid:synth_opioid)
    )

## Remove observations (rates) where the population is small *and* the
## observed number of deaths is less than 10. In such situations, the rates
## are unstable. In situations where the population is large (> 100,000) and
## the deaths are small (< 10), we keep them because that is an informative
## lack of observations.
std_rates_long <- std_rates_long %>%
    mutate(rate = case_when(pop < 100000 & is.na(count) ~ NA_real_,
                            pop < 100000 & count == 0 ~ NA_real_,
                            TRUE ~ rate))

## Arrange and filter for joinpoint regression program (must be ordered)
##
## We replace 0 observations with the zero rate defined in the config.yml.
## We impute missing variance values with the minimum variance observed for
## that race/state/opioid type.
opioids_by_type <- std_rates_long %>%
    filter(
        !is.na(rate),
        opioid_type %in% c("opioid", "natural_opioid",
                           "heroin", "synth_opioid")
    ) %>%
    ungroup() %>%
    mutate(
        var  = ifelse(var == 0, NA, var),
        rate = ifelse(rate == config::get()$zero_rate,
                      .01, rate)
    ) %>%
    group_by(st_fips, opioid_type, race) %>%
    mutate(var = ifelse(is.na(var), min(var, na.rm = TRUE), var),
           sd  = sqrt(var)) %>%
    ungroup() %>%
    arrange(race, opioid_type, st_fips, year) %>% 
    select(-count)

## Save
options(scipen = 20)    ## Or else it writes one row using scientific notation
narcan::mkdir_p("./joinpoint_analyses")
write.csv(
    opioids_by_type %>%
        group_by(abbrev, race, opioid_type) %>%
        filter(n() >= 10,
               sd(rate) > 0),
    here("joinpoint_analyses", "opioids_by_type.csv"),
    row.names = FALSE
    )
