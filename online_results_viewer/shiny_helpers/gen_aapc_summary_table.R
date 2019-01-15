gen_aapc_summary_table <-
    function(jp_results,
             opioid_types) {
        
        ## Filter results
        temp_df <- jp_results %>%
            filter(opioid_type %in% opioid_types,
                   race == "total")
        
        aapc_df <- temp_df %>%
            select(abbrev,
                   name_cat,
                   opioid_cat,
                   aapc,
                   aapc_lb,
                   aapc_ub,
                   aapc_pval,
                   aapc_qval) %>%
            distinct() %>%
            arrange(name_cat, opioid_cat) %>%
            ungroup()
        
        aapc_df <- aapc_df %>%
            mutate(
                aapc = sprintf("%0.2f", round(aapc, 2)), 
                aapc_ci = sprintf("%0.2f, %0.2f", 
                                  round(aapc_lb, 2), round(aapc_ub, 2)),
                aapc_pv = ifelse(aapc_pval < .00001,
                                 "<.00001",
                                 sprintf("%0.5f", aapc_pval)),
                aapc_qv = ifelse(aapc_qval < .00001,
                                 "<.00001",
                                 sprintf("%0.5f", aapc_qval))
            )
        
        print_table <- aapc_df %>%
            select(abbrev,
                   name_cat,
                   opioid_cat,
                   aapc,
                   aapc_ci,
                   aapc_pv,
                   aapc_qv) %>%
            DT::datatable(
                data = .,
                colnames =
                    c(
                        "Abrev.",
                        "Name",
                        "Opioid Type",
                        "AAPC",
                        "95% CI",
                        "P-Value",
                        "Q-Value"
                    ),
                rownames = FALSE,
                filter = list(
                    position = 'top',
                    clear = TRUE,
                    plain = FALSE
                ),
                caption = "Average annual percent change (AAPC) for all states,
                by opioid type. The average annual percent change is the
                weighted average of the annual percent change estimates
                (i.e., the model slopes expressed as percent change),
                where the weights are the number of years in each
                segment. The Q-value is the false discovery rate adjusted
                P-value."
            )
        
        return(print_table)
    }
