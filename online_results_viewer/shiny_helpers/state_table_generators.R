gen_state_table_fit <-
    function(jp_results,
             s_state,
             s_outcome) {
        ## Filter results
        temp_df <- jp_results %>%
            filter(abbrev == s_state,
                   opioid_type %in% s_outcome)
        
        ## Suppress MSE/SSE with N < 10
        temp_df <- temp_df %>% 
            mutate(mse = ifelse(remove_obs == 1, NA, mse), 
                   sse = ifelse(remove_obs == 1, NA, sse))
        
        state_df <- temp_df %>%
            filter(dupe_row == 0) %>%
            group_by(opioid_cat) %>%
            filter(year == min(year)) %>%
            select(opioid_cat, joinpoints, df, sse, mse) %>%
            mutate(sse = sprintf("%0.3f", sse),
                   mse = sprintf("%0.3f", mse)) %>%
            ungroup() %>%
            arrange(opioid_cat) %>%
            DT::datatable(
                data = .,
                colnames = c("Opioid type",
                             "Joins (N)",
                             "DF",
                             "SSE",
                             "MSE"),
                rownames = FALSE,
                caption = "Model statistics by state and
                opioid type. As required by NCHS, if a model includes
                rates with based on observations with fewer than 10
                deaths, we suppressed the mean squared error (MSE) and
                sum of squared errors (SSE).",
                options = list(paging = FALSE, searching = FALSE)
            )
        
        return(state_df)
    }

gen_state_table_model_estimates <-
    function(jp_results,
             s_state,
             s_outcome) {
        
        ## Filter results
        temp_df <- jp_results %>%
            filter(abbrev == s_state,
                   opioid_type %in% s_outcome)
        
        state_df <- temp_df %>%
            filter(dupe_row == 0) %>%
            group_by(opioid_type, segment) %>%
            slice(1) %>%
            group_by(opioid_cat) %>%
            select(
                opioid_cat,
                segment,
                int_est,
                int_se,
                int_qval, 
                int_pval,
                slope_est,
                slope_se,
                slope_pval,
                slope_qval, 
                slopechange_pval, 
                slopechange_qval
            ) %>%
            mutate(
                segment = sprintf("%i of %i", segment + 1, max(segment) + 1),
                int_est = sprintf("%0.2f", int_est),
                int_se = sprintf("%0.2f", int_se),
                int_pval = ifelse(int_pval < .001, "<.001",
                                  sprintf("%0.3f", int_pval)),
                slope_est = sprintf("%0.2f", slope_est),
                slope_se = sprintf("%0.2f", slope_se),
                slope_pval = ifelse(slope_pval < .001, "<.001",
                                    sprintf("%0.3f", slope_pval)),
                slopechange_pval =  ifelse(
                    slopechange_pval < .001,
                    "<.001",
                    sprintf("%0.3f", slopechange_pval)
                )
            ) %>%
            mutate(
                int_qval = ifelse(int_qval < .001, "<.001",
                                  sprintf("%0.3f", int_qval)),
                slope_qval = ifelse(slope_qval < .001, "<.001",
                                    sprintf("%0.3f", slope_qval)),
                slopechange_qval =  ifelse(
                    slopechange_qval < .001,
                    "<.001",
                    sprintf("%0.3f", slopechange_qval)
                )
            ) %>%
            mutate(
                int_pval = paste(int_pval, "/",
                                 int_qval),
                slope_pval = paste(slope_pval, "/",
                                   slope_qval),
                slopechange_pval = paste(slopechange_pval, "/",
                                         slopechange_qval)
            ) %>%
            select(-int_qval, -slope_qval, -slopechange_qval) %>% 
            arrange(opioid_cat) %>%
            ungroup() %>%
            DT::datatable(
                data = .,
                colnames = c(
                    "Opioid type",
                    "Segment",
                    "Int. Est.",
                    "Int. SE",
                    "Int. P-/Q-Value",
                    "Slope Est.",
                    "Slope SE",
                    "Slope P-/Q-Value",
                    "Slope Change P-/Q-Value"
                ),
                rownames = FALSE,
                caption = "Model statistics by race/ethnicity and
                opioid type. As required by NCHS, if a model includes
                rates with based on observations with fewer than 10
                deaths, we suppressed the mean squared error (MSE) and
                sum of squared errors (SSE).",
                list(paging = FALSE)
            )
        
        return(state_df)
    }

gen_state_table_predictions <-
    function(jp_results,
             s_state,
             s_outcome) {
        
        ## Filter results
        temp_df <- jp_results %>%
            filter(abbrev == s_state,
                   opioid_type %in% s_outcome)
        
        ## Suppress rates with N < 10
        temp_df <- temp_df %>% 
            mutate(rate = ifelse(remove_obs == 1, NA, rate))
        
        state_df <- temp_df %>%
            filter(dupe_row == 0) %>%
            group_by(opioid_cat) %>%
            select(opioid_cat, year, pred_rate, rate, rate_se) %>%
            mutate(
                pred_rate = sprintf("%0.2f", pred_rate),
                rate = sprintf("%0.2f", rate),
                rate_se = sprintf("%0.2f", rate_se)
            ) %>%
            arrange(opioid_cat) %>%
            ungroup() %>%
            DT::datatable(
                data = .,
                colnames = c(
                    "Opioid type",
                    "Year",
                    "Modeled Rate",
                    "Observed Rate",
                    "Observed Rate SE"
                ),
                filter = list(
                    position = 'top',
                    clear = TRUE,
                    plain = FALSE
                ),
                rownames = FALSE,
                caption = "Predicted and observed rates for each year,
                by state and opioid type. Observed rates based
                on observations with fewer than 10 deaths are suppressed
                per NCHS restrictions."
            )
        
        return(state_df)
        
    }
