gen_lel_table <-
    function(lel_df,
             l_age,
             l_year) {
        ## Filter results
        temp_df <- lel_df %>%
            filter(year == l_year, 
                   age == l_age, 
                   !is.na(opioid_cat)) %>% 
            select(-age_cat, -st_fips, -opioid_type) %>% 
            select(year, age, everything())
        
        temp_df <- temp_df %>% 
            mutate(ex_diff_p = sprintf("%0.3f", round(ex_diff, 3)))
        
        print_df <- temp_df %>%
            select(-ex_diff) %>% 
            spread(opioid_cat, ex_diff_p) %>% 
            DT::datatable(
                data = .,
                colnames = c(
                    "Year",
                    "Age",
                    "State",
                    "All opioids",
                    "Natural/Semi-natural",
                    "Heroin", 
                    "Synthetic", 
                    "Car Accidents", 
                    "Firearms"
                ),
                rownames = FALSE,
                filter = list(
                    position = 'top',
                    clear = TRUE,
                    plain = FALSE
                ),
                caption = sprintf(
                    "Life expectancy lost at age %i for %i by state
                    and cause of death.",
                    l_age, l_year
                ),
                options = list(paging = TRUE, searching = FALSE)
            )
        
        return(print_df)
    }
