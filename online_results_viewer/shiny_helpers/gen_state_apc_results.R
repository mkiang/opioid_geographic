gen_state_apc_results <-
    function(jp_results,
             s_outcome, 
             s_state,
             s_p_or_q,
             s_sigpvalue,
             s_show_raw,
             s_raw_ci,
             s_model_fit,
             s_linetype_sig,
             s_joinpoint,
             s_joinpoint_sig,
             s_legends_on) {
        
        s_sigpvalue <- as.numeric(s_sigpvalue)
        
        ## Filter results
        temp_df <- jp_results %>%
            filter(abbrev == s_state, 
                   opioid_type %in% s_outcome)
        
        ## Suppress rates with N < 10
        temp_df <- temp_df %>% 
            mutate(rate = ifelse(remove_obs == 1, NA, rate))
        
        ## Generate the p- or q-values according to user input
        if (s_p_or_q == "qval") {
            temp_df <- temp_df %>%
                mutate(aapc_pq = aapc_qval,
                       slope_pq = slope_qval,
                       change_pq = slopechange_qval)
        } else {
            temp_df <- temp_df %>%
                mutate(aapc_pq = aapc_pval,
                       slope_pq = slope_pval,
                       change_pq = slopechange_pval)
        }
        
        ## Make significance categories
        temp_df <- temp_df %>%
            mutate(
                sig_aapc     = ifelse(aapc_pq  < s_sigpvalue, 1, 0),
                sig_slope    = ifelse(slope_pq < s_sigpvalue, 1, 0),
                sig_slopechg = ifelse(change_pq < s_sigpvalue, 1, 0)
            ) %>%
            mutate(
                slope_cat = factor(
                    sig_slope,
                    levels = 0:1,
                    labels = c("Non-Sig.", "Sig."),
                    ordered = TRUE
                ),
                slopechg_cat = factor(
                    sig_slopechg,
                    levels = 0:1,
                    labels = c("Non-Sig.", "Sig."),
                    ordered = TRUE
                )
            )
        
        ## Start plot
        p <- ggplot(data = temp_df)
        
        ## Add all the necessities
        p <- p +
            scale_x_continuous(
                NULL,
                breaks = seq(2000, 2015, 5),
                labels = c("'00", "'05", "'10", "'15"),
                expand = c(0, .25)
            ) +
            scale_y_continuous(paste("Mortality Rate",
                                     "(deaths per 100,000 population)"),
                               expand = c(0, .5), 
                               limits = c(0, NA)) +
            scale_color_brewer("Opioid Type",
                               palette = "Set1",
                               drop = FALSE) 
        
        ## Should we plot the raw?
        if (s_show_raw) {
            if (s_model_fit) {
                alpha_point <- .4
                alpha_line  <- .4
            } else {
                alpha_point <- .8
                alpha_line  <- .5
            }
            
            ## Should we plot the raw CI?
            if (s_raw_ci) {
                p <- p +
                    geom_errorbar(
                        aes(
                            x = year,
                            ymax = rate + 1.96 * rate_se,
                            ymin = rate - 1.96 * rate_se,
                            color = opioid_cat
                        ),
                        width = 0,
                        alpha = alpha_line
                    )
            }
            
            p <- p +
                geom_point(aes(
                    x = year,
                    y = rate,
                    color = opioid_cat
                ),
                alpha = alpha_point,
                shape = 5)
        }
        
        ## Draw model fit?
        if (s_model_fit) {
            if (s_linetype_sig) {
                p <- p +
                    geom_line(
                        aes(
                            x = year,
                            y = pred_rate,
                            color = opioid_cat,
                            group = interaction(opioid_cat,
                                                line_seg,
                                                abbrev,
                                                slope_cat),
                            linetype = slope_cat
                        )
                    )
                p <- p +
                    scale_linetype_manual(
                        name = sprintf(
                            "Slope (%s < %0.3f)",
                            ifelse(s_p_or_q == "pval", "P", "Q"),
                            s_sigpvalue
                        ),
                        limits = c("Non-Sig.", "Sig."),
                        values = c("dashed", "solid"),
                        drop = FALSE
                    )
            } else {
                p <- p +
                    geom_line(aes(
                        x = year,
                        y = pred_rate,
                        color = opioid_cat,
                        group = interaction(opioid_cat, line_seg)
                    ))
            }
        }
        
        ## Draw joinpoints?
        if (s_joinpoint) {
            if (s_joinpoint_sig) {
                p <- p +
                    geom_point(aes(
                        x = j_year,
                        y = pred_rate,
                        color = opioid_cat,
                        shape = slopechg_cat
                    ))
                p <- p +
                    scale_shape_manual(
                        name = sprintf(
                            "Change in slope (%s < %0.3f)",
                            ifelse(s_p_or_q == "pval", "P", "Q"),
                            s_sigpvalue
                        ),
                        values = c(0, 19),
                        limits = c("Non-Sig.", "Sig."),
                        drop = FALSE
                    )
            } else {
                p <- p +
                    geom_point(aes(
                        x = j_year,
                        y = pred_rate,
                        color = opioid_cat
                    ))
            }
        }
        
        ## Show legends?
        if (s_legends_on) {
            p <- p +
                mk_nytimes(
                    panel.border =
                        element_rect(
                            linetype = "solid",
                            fill = NA,
                            color = "grey75"
                        ),
                    legend.position = "bottom",
                    legend.direction = "vertical"
                )
        } else {
            p <- p +
                mk_nytimes(
                    panel.border =
                        element_rect(
                            linetype = "solid",
                            fill = NA,
                            color = "grey75"
                        ),
                    legend.position = "none"
                )
        }
        
        return(p)
    }