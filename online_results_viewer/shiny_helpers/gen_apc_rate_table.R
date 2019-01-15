gen_apc_rate_table <-
    function(jp_results,
             h_year) {
        ## Filter results
        temp_df <- jp_results %>%
            filter(year == h_year)
        
        ## Suppress rates with N < 10
        temp_df <- temp_df %>%
            mutate(rate = ifelse(remove_obs == 1, NA, rate))
        
        rate_apc_df <- temp_df %>%
            filter(dupe_row == 0) %>%
            select(
                opioid_cat,
                name_cat,
                rate,
                rate_se,
                apc,
                slope_est,
                slope_se,
                slope_pval,
                slope_qval
            ) %>%
            arrange(opioid_cat, name_cat) %>%
            mutate(
                rate_ci = sprintf(
                    "%0.1f (%0.1f, %0.1f)",
                    round(rate, 1),
                    round(rate - 1.96 * rate_se, 1),
                    round(rate + 1.96 * rate_se, 1)
                ),
                apc_ci = sprintf(
                    "%0.1f (%0.1f, %0.1f)",
                    round((exp(slope_est) - 1) * 100, 1),
                    round((exp(
                        slope_est - 1.96 * slope_se
                    ) - 1) * 100, 1),
                    round((exp(
                        slope_est + 1.96 * slope_se
                    ) - 1) * 100, 1)
                ),
                slope_pval = ifelse(
                    slope_pval < .00001,
                    "<.00001",
                    sprintf("%0.5f", slope_pval)
                ),
                slope_qval = ifelse(
                    slope_qval < .00001,
                    "<.00001",
                    sprintf("%0.5f", slope_qval)
                )
            ) %>%
            ungroup() %>%
            select(opioid_cat,
                   name_cat,
                   rate_ci,
                   apc_ci,
                   slope_pval,
                   slope_qval) %>%
            DT::datatable(
                data = .,
                colnames = c(
                    "Opioid type",
                    "State",
                    "Rate (95% CI)",
                    "APC (95% CI)",
                    "P-value",
                    "Q-value"
                ),
                rownames = FALSE,
                filter = list(
                    position = 'top',
                    clear = TRUE,
                    plain = FALSE
                ),
                caption = sprintf(
                    "Opioid-related mortality rate and
                    annual percent change (APC) by state
                    and opioid type for %i. Annual percent
                    change is estimated using joinpoint
                    regression. The Q-value is the false
                    discovery rate adjusted P-value. Throughout
                    the manuscript, we refer to the P-value
                    because overall it is more conservative than
                    the Q-value in our results. When the rate
                    is based on fewer than 10 observations,
                    it is suppressed in accordance with the
                    NCHS data use agreement. The 95%% CI of
                    the rate is based on the Poisson
                    approximation.",
                    h_year
                ),
                options = list(paging = TRUE, searching = FALSE)
            )
        
        return(rate_apc_df)
    }
