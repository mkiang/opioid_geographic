gen_aapc_trends_plot <-
    function(jp_results,
             opioid_types,
             show_raw,
             raw_ci,
             model_fit,
             linetype_sig,
             joinpoint,
             joinpoint_sig,
             ymax,
             legends_on,
             sig_aapc_only,
             sigpvalue,
             p_or_q,
             disable_clip) {
        
        ## Filter results
        temp_df <- jp_results %>%
            filter(opioid_type %in% opioid_types,
                   abbrev != "US")
        
        ## Suppress rates with N < 10
        temp_df <- temp_df %>% 
            mutate(rate = ifelse(remove_obs == 1, NA, rate))
        
        ## Generate the p- or q-values according to user input
        if (p_or_q == "qval") {
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
                sig_aapc     = ifelse(aapc_pq  < sigpvalue, 1, 0),
                sig_slope    = ifelse(slope_pq < sigpvalue, 1, 0),
                sig_slopechg = ifelse(change_pq < sigpvalue, 1, 0)
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
        
        ## Sig AAPCs?
        if (sig_aapc_only) {
            temp_df <- temp_df %>%
                filter(sig_aapc == 1)
        }
        
        ## Start a plot
        p <- ggplot(data = temp_df)
        
        ## Add all the necessities
        p <- p +
            facet_geo(~ abbrev) +
            scale_x_continuous(
                NULL,
                breaks = seq(2000, 2015, 5),
                labels = c("'00", "'05", "'10", "'15"),
                expand = c(0, .25)
            ) +
            scale_y_continuous(paste("Mortality Rate",
                                     "(deaths per 100,000 population)"),
                               expand = c(0, .5)) +
            scale_color_brewer("Opioid Type",
                               palette = "Set1",
                               drop = FALSE)
        
        if (disable_clip) {
            p <- p + coord_cartesian(ylim = c(0, ymax), clip = "off")
        } else {
            p <- p + coord_cartesian(ylim = c(0, ymax), clip = "on")
        }
        
        ## Should we plot the raw?
        if (show_raw) {
            if (model_fit) {
                alpha_point <- .2
                alpha_line  <- .2
            } else {
                alpha_point <- .8
                alpha_line  <- .5
            }
            
            ## Should we plot the raw CI?
            if (raw_ci) {
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
        if (model_fit) {
            if (linetype_sig) {
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
                            ifelse(p_or_q == "pval", "P", "Q"),
                            sigpvalue
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
        if (joinpoint) {
            if (joinpoint_sig) {
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
                            ifelse(p_or_q == "pval", "P", "Q"),
                            sigpvalue
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
        if (legends_on) {
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

# gen_aapc_trends_plot(
#     sub_results,
#     opioid_types = c("opioid", "synth_opioid"),
#     show_raw = TRUE,
#     raw_ci = TRUE,
#     model_fit = TRUE,
#     linetype_sig = TRUE,
#     joinpoint = TRUE,
#     joinpoint_sig = TRUE,
#     ymax = 20,
#     legends_on = TRUE,
#     sig_aapc_only = TRUE,
#     sigpvalue = .05,
#     p_or_q = "qval",
#     disable_clip = TRUE
# )