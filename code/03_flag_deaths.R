## 03_flag_deaths.R ----
##
## Import the (private) processed files, flag deaths of interest. The
## flag_and_summarize() function takes each year and flags the deaths using
## narcan. Then we combine them at the end. The result is a dataframe with age,
## state, and the number of deaths for that subset and a variety of outcomes.

## Imports ----
library(tidyverse)
library(narcan)     ## devtools::install_github("mkiang/narcan")

## DELETE THIS ----
Sys.setenv(R_CONFIG_ACTIVE = "dev")

## Pull in YAML config ----
cfig <- config::get()

## Define parameters ----
year_0      <- cfig$start_year
year_n      <- cfig$end_year
paral_proc  <- cfig$proc_in_parallel
priv_data   <- cfig$private_data
data_folder <- cfig$working_data
n_cores     <- cfig$num_cores

## Load parallel package if necessary ----
if (paral_proc) {
    library(foreach)
}

## Helper function ----
flag_and_summarize <- function(priv_data, year) {
    ## Read in
    flagged_df <-
        readRDS(sprintf("%s/proc_%s.RDS", priv_data, year))
    
    ## Flag opioid deaths
    flagged_df <- flagged_df %>%
        flag_drug_deaths(year = year) %>%
        flag_opioid_deaths(year = year) %>%
        flag_nonopioid_drug_deaths() %>%
        flag_opioid_types(year = year) %>%
        mutate(st_fips  = substr(countyrs, 1, 2))
    
    ## Flag firearms
    ## See page 61 of
    ## https://www.cdc.gov/nchs/data/nvsr/nvsr66/nvsr66_06.pdf for all the
    ## ICD-10 codes we use. Each regex rule below corresponds to the codes.
    flagged_df <- flagged_df %>%
        mutate(firearm = grepl(
            paste0(
                "\\<U014\\>|",
                "\\<W3[234]\\d{0,1}\\>|",
                "\\<X7[234]\\d{0,1}\\>|",
                "\\<X9[345]\\d{0,1}\\>|",
                "\\<Y2[234]\\d{0,1}\\>|",
                "\\<Y35\\d{0,1}\\>"
            ),
            ucod
        ) + 0)
    
    ## Flag car accidents
    ## See page 33 of
    ## https://www.cdc.gov/nchs/data/nvsr/nvsr66/nvsr66_06.pdf for all the
    ## ICD-10 codes we use. Each regex rule below corresponds to the codes.
    flagged_df <- flagged_df %>%
        mutate(car_accident = grepl(
            paste0(
                "\\<V0[234]\\d{0,1}\\>|",
                ## V02-V04
                "\\<V09[02]\\>|",
                ## V09.0, V09.2
                "\\<V1[234]\\d{0,1}\\>|",
                ## V12-V14
                "\\<V19[012456]\\>|",
                ## V19.0–V19.2, V19.4–V19.6
                "\\<V[234567]\\d{1,2}\\>|",
                ## V20–V79,
                "\\<V80[345]\\>|",
                ## V80.3-V80.5
                "\\<V8[12][01]\\>|",
                ## V81.0-V81.1, V82.0-V82.1
                "\\<V8[3456]\\d{0,1}\\>|",
                ## V83-V86
                "\\<V8[78][012345678]\\>|",
                ## V87.0-V87.8, V88.0-V88.8
                "\\<V89[02]\\>"             ## V89.0, V89.2
            ),
            
            ucod
        ) + 0)
    
    ## Remove columns
    flagged_df <- flagged_df %>%
        select(-ucod, -f_records_all, -countyrs, -month, -countyoc, -sex)
    
    ## Total resident population
    state_opioid_deaths <- flagged_df %>%
        summarize_binary_columns(st_fips, age, age_cat) %>%
        mutate(race_ethnicity = "all_races_all_hisp") %>%
        select(year, st_fips, age, age_cat, race_ethnicity, everything()) %>%
        arrange(race_ethnicity, year, st_fips, age) %>%
        ungroup()
    
    return(state_opioid_deaths)
}

## Run the function ----
if (paral_proc) {
    doParallel::registerDoParallel(cores = n_cores)
    
    state_opioid_data <-
        foreach(year = year_0:year_n,
                .combine = rbind,
                .inorder = FALSE) %dopar% {
                    flag_and_summarize(priv_data = priv_data, year = year)
                }
    
} else {
    state_opioid_data <- NULL
    for (year in year_0:year_n) {
        temp_df <- flag_and_summarize(priv_data = priv_data, year = year)
        state_opioid_data <- rbind(state_opioid_data, temp_df)
        
        rm(temp_df)
        gc()
    }
}

## Save raw flagged opioid data ----
saveRDS(state_opioid_data,
        file = sprintf('%s/state_opioid_deaths_no_pops.RDS',
                       priv_data))
