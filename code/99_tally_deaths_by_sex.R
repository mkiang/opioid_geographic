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
remap_age <- function(df) {
    df %>%
        mutate(
            age_year =
                case_when(
                    age_raw ==  999 ~ NA_real_,
                    age_raw == 1999 ~ NA_real_,
                    age_raw == 2999 ~ 0,
                    age_raw == 4999 ~ 0,
                    age_raw == 5999 ~ 0,
                    age_raw == 6999 ~ 0,
                    age_raw == 9999 ~ NA_real_,
                    age_raw <  200 ~ age_raw,
                    between(age_raw, 200, 699) ~ 0,
                    between(age_raw, 1000, 1998) ~ age_raw - 1000,
                    between(age_raw, 2000, 6999) ~ 1
                )
        )
}

group_by_yearsex <- function(df) {
    bind_rows(
        df,
        df %>%
            mutate(sex = "All"),
        df %>%
            mutate(year = 0000),
        df %>%
            mutate(year = 0000,
                   sex = "All")
    ) %>%
        group_by(year, sex)
}

count_and_summarize <- function(df) {
    df %>%
        select(age_year) %>%
        summarize(
            n = n(),
            n_miss = sum(is.na(age_year)),
            age_mean = mean(age_year, na.rm = TRUE),
            age_sd = sd(age_year, na.rm = TRUE)
        )
}

load_data <- function(priv_data, year) {
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
        select(-ucod, -f_records_all, -countyrs, -month, -countyoc)
    
    ## Remap age
    flagged_df <- flagged_df %>%
        remap_age()
    
    return(flagged_df)
}

## Run the function ----
if (paral_proc) {
    doParallel::registerDoParallel(cores = n_cores)
    
    all_data <-
        foreach(year = year_0:year_n,
                .combine = bind_rows,
                .inorder = FALSE) %dopar% {
                    load_data(priv_data = priv_data, year = year)
                }
    
} else {
    all_data <- NULL
    for (year in year_0:year_n) {
        temp_df <- load_data(priv_data = priv_data, year = year)
        all_data <- rbind(all_data, temp_df)
        
        rm(temp_df)
        gc()
    }
}

all_data <- all_data %>%
    select(
        -age,
        -age_cat,
        -age_raw,
        -st_fips,
        -multi_opioids,
        -unspecified_op_present,
        -methadone_present,
        -opium_present,
        -nonop_drug_death,
        -other_op_present,
        -num_opioids
    )

## Summarize number of deaths by sex and average age
deaths_age_df <- bind_rows(
    all_data %>%
        group_by_yearsex() %>%
        count_and_summarize() %>%
        ungroup() %>%
        mutate(cod = "all_deaths"),
    all_data %>%
        filter(opioid_death == 1) %>%
        group_by_yearsex() %>%
        count_and_summarize() %>%
        ungroup() %>%
        mutate(cod = "all_opioids"),
    all_data %>%
        filter(drug_death == 1) %>%
        group_by_yearsex() %>%
        count_and_summarize() %>%
        ungroup() %>%
        mutate(cod = "all_drugs"),
    all_data %>%
        filter(heroin_present == 1) %>%
        group_by_yearsex() %>%
        count_and_summarize() %>%
        ungroup() %>%
        mutate(cod = "heroin"),
    all_data %>%
        filter(other_natural_present == 1) %>%
        group_by_yearsex() %>%
        count_and_summarize() %>%
        ungroup() %>%
        mutate(cod = "natural"),
    all_data %>%
        filter(other_synth_present == 1) %>%
        group_by_yearsex() %>%
        count_and_summarize() %>%
        ungroup() %>%
        mutate(cod = "synth"),
    all_data %>%
        filter(firearm == 1) %>%
        group_by_yearsex() %>%
        count_and_summarize() %>%
        ungroup() %>%
        mutate(cod = "firearm"),
    all_data %>%
        filter(car_accident == 1) %>%
        group_by_yearsex() %>%
        count_and_summarize() %>%
        ungroup() %>%
        mutate(cod = "car_accident")
)

## Save raw flagged opioid data ----
write_csv(deaths_age_df,
          path = sprintf('%s/deaths_by_sex_and_age.csv',
                         data_folder))
