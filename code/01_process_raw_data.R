## 01_process_raw_data.R ----
## 
## Just takes in the restricted MCOD files, subsets to the columns we want,
## cleans them up using the `narcan` package, and saves the intermediate file
## as working data.
##
## Note, this is still geotagged death data so it must remain private!

## Imports ----
library(tidyverse)
library(narcan)     ## devtools::install_github("mkiang/narcan")

## Define file locations ----
## We need a dictionary of {year:file location} pairs. Preface years with "y"
## and use the full path of the file. Change these paths to match your
## file location.
file_dict <- list(
    y1999 = "./data_restricted/Mort99p2.dat",
    y2000 = "./data_restricted/MORT00US.PT2.dat",
    y2001 = "./data_restricted/Mort01us.dat",
    y2002 = "./data_restricted/Mort02us.dat",
    y2003 = "./data_restricted/Mult03us.dat",
    y2004 = "./data_restricted/Mort04us.dat",
    y2005 = "./data_restricted/mort05usp2.dat",
    y2006 = "./data_restricted/MULT2006.USPART2.dat",
    y2007 = "./data_restricted/MULT2007.USPART2.dat",
    y2008 = "./data_restricted/MULT2008.USPART2.dat",
    y2009 = "./data_restricted/MULT2009.USPART2.dat",
    y2010 = "./data_restricted/MULT2010.USPART2.dat",
    y2011 = "./data_restricted/MULT2011.USPART2.dat",
    y2012 = "./data_restricted/MULT2012.USPART2.dat",
    y2013 = "./data_restricted/MULT2013.USPART2.dat",
    y2014 = "./data_restricted/MULT2014.USPART2.dat",
    y2015 = "./data_restricted/MULT2015.USPART2.dat",
    y2016 = "./data_restricted/MULT2016.USAllCnty.txt"
)

## Pull in YAML config ----
cfig <- config::get()

## Define YAML parameters
year_0     <- cfig$start_year
year_n     <- cfig$end_year
paral_proc <- cfig$proc_in_parallel
priv_data  <- cfig$private_data
force_proc <- cfig$force_process
n_cores    <- cfig$num_cores

## Load parallel package if necessary ----
if (paral_proc) {
    library(foreach)
}

## Helper function for parallel processing ----
convert_raw_data <-
    function(priv_data, rmcod_path, year, force_proc = FALSE) {
        ## Don't run if file already exist and `force_proc` is not TRUE
        if (file.exists(sprintf("%s/proc_%s.RDS", priv_data, year)) &&
            !force_proc) {
            return(year)
        } else {
            ## Load the data
            temp_df <-
                narcan:::.import_restricted_data(rmcod_path, year)
            
            ## Subset to columns we want
            ## NOTE: 1999-2002, countyrs and countyoc were weird NCHS-specific
            ##       codes and in 2003+, they switched to FIPS but kept the
            ##       variable names.
            temp_df <- temp_df %>%
                select(one_of(
                    c("ager27", "age", "sex",
                      "restatus", "ucod", "monthdth")
                ),
                starts_with("record_"),
                one_of(c(
                    "countyrs", "countyoc", "fipsctyo", "fipsctyr"
                ))) %>% 
                rename(age_raw = age) 
            
            ## Fix 1999-2002 FIPS/geo columns and M/F coding
            if (year <= 2002) {
                temp_df <- temp_df %>%
                    select(-countyoc, -countyrs) %>%
                    rename(countyoc = fipsctyo, countyrs = fipsctyr)
                
                temp_df <- temp_df %>%
                    mutate(sex = case_when(sex == 1 ~ "M", 
                                           sex == 2 ~ "F", 
                                           TRUE ~ as.character(sex)))
            }
            
            ## Munging time, add column, subset, unit, etc.
            temp_df <- temp_df %>%
                mutate(year = year) %>%
                subset_residents() %>%
                unite_records() %>%
                convert_ager27() %>%
                mutate(age_cat  = categorize_age_5(age)) %>% 
                select(year,
                       month = monthdth,
                       age,
                       age_cat,
                       age_raw, 
                       sex, 
                       countyoc,
                       countyrs,
                       ucod,
                       f_records_all)
            
            ## Save as RDS (readr::write_csv() has parsing errors)
            saveRDS(temp_df,
                    sprintf("%s/proc_%s.RDS", priv_data, year),
                    compress = "xz")
            
            ## Clean up
            rm(temp_df, processed_df)
            gc()
            
            ## dopar requires you return **something**
            return(year)
        }
    }


## Perform actual processing ----
narcan::mkdir_p(priv_data)

if (paral_proc) {
    doParallel::registerDoParallel(cores = n_cores)
    
    foreach(year = year_0:year_n,
            .combine = c,
            .inorder = FALSE) %dopar% {
                convert_raw_data(priv_data,
                                 file_dict[[paste0("y", year)]],
                                 year)
            }
} else {
    for (year in year_0:year_n) {
        convert_raw_data(priv_data,
                         file_dict[[paste0("y", year)]],
                         year)
    }
}
