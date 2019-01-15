## Helper functions

## Calculate the mode of a vector
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

## To clean up joinpoint output
import_results_data <- function(filename) {
    df <- readr::read_delim(
        filename,
        delim = ";",
        escape_double =  FALSE,
        trim_ws = TRUE
    )
    names(df) <- tolower(names(df))
    names(df) <- gsub(" |#|%|", "", names(df))
    names(df) <- gsub(",|-|\\.", "_", names(df))
    
    return(df)
}

#' Given a dataframe of total deaths, pop, age, and cause-specific deaths,
#' returns a cause-deleted lifetable.
#'
#' @param df
#' @param pop
#' @param total_d
#' @param target_d
#'
#' @return a cause-deleted lifetable as a dataframe
calculate_cdlt <-
    function(df,
             age = age,
             pop = pop,
             total_d = deaths,
             target_d = opioid_death,
             age_bin = 5) {
        ## Lead and lags with defaults -- makes it easier for me to
        ## see what's happening
        lag0  <- function(x)
            lag(x, default = 0)
        lag1  <- function(x)
            lag(x, default = 1)
        lead0 <- function(x)
            lead(x, default = 0)
        
        age <- enquo(age)
        pop <- enquo(pop)
        total_d <- enquo(total_d)
        target_d <- enquo(target_d)
        
        df %>%
            arrange(!!age) %>%
            mutate(
                age   = !!age,
                Dx    = !!total_d / !!pop,
                Dx_no = (!!total_d-!!target_d) / !!pop,
                
                Rx = Dx_no / Dx,
                qx = age_bin * Dx / (1 + age_bin / 2 * Dx),
                px = 1 - qx,
                lx = lag1(cumprod(px)),
                dx = lx - lead0(lx),
                Lx = age_bin * lead0(lx) + (age_bin / 2 * dx),
                Tx = rev(cumsum(rev(Lx))),
                
                px_no = px ^ Rx,
                qx_no = 1 - px_no,
                lx_no = lag1(cumprod(px_no)),
                dx_no = lx_no - lead0(lx_no),
                Lx_no = age_bin * lead0(lx_no) + (age_bin / 2 * dx_no),
                Tx_no = rev(cumsum(rev(Lx_no))),
                
                ex = Tx / lx,
                ex_no = Tx_no / lx_no,
                
                ex_diff = ex_no - ex
            )
    }

## Just a wrapper for above, except this will do the subsetting for you,
## calculate the ex_diffs of interest, and return in a more usable form.
get_ex_diff_all_types <- function(df, age_min = 15, year_x,
                                  st_abbrev, race_x) {
    sub_df <- df %>%
        filter(year == year_x,
               age >= age_min,
               abbrev == st_abbrev,
               race == race_x) %>%
        ungroup()
    
    bind_rows(
        calculate_cdlt(sub_df, target_d = opioid_death) %>%
            mutate(opioid_type = "opioids"),
        calculate_cdlt(sub_df, target_d = drug_death) %>%
            mutate(opioid_type = "drugs"),
        calculate_cdlt(sub_df, target_d = heroin_present) %>%
            mutate(opioid_type = "heroin"),
        calculate_cdlt(sub_df, target_d = other_natural_present) %>%
            mutate(opioid_type = "natural"),
        calculate_cdlt(sub_df, target_d = other_synth_present) %>%
            mutate(opioid_type = "synth"),
        calculate_cdlt(sub_df, target_d = firearm) %>%
            mutate(opioid_type = "firearms"),
        calculate_cdlt(sub_df, target_d = car_accident) %>%
            mutate(opioid_type = "car_accident")
    ) %>%
        select(one_of(
            c(
                "year",
                "st_fips",
                "abbrev",
                "race",
                "age",
                "age_cat",
                "opioid_type",
                "ex_diff"
            )
        ))
}

## Dataframe of state abbreviation, name, fips, and division mapping ----
st_info <- tibble(
    abbrev   = state.abb,
    division = as.character(state.division),
    st_lat   = state.center$y,
    st_lon   = state.center$x
    ) %>%
    ## We have to add DC because it's not a state
    add_row(
        abbrev = "DC",
        division = "South Atlantic",
        st_lat = 38.9072,
        st_lon = -77.0369
    ) %>%
    left_join(narcan::st_fips_map) %>%
    rename(st_fips = fips)
