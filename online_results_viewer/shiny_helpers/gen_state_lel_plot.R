gen_state_lel_plot <- function(lel_df,
                               s_outcome,
                               s_state) {
    ## Match opioid_type to the input
    lel_df <- lel_df %>%
        mutate(
            opioid_type = case_when(
                opioid_type == "opioids" ~ "opioid",
                opioid_type == "natural" ~ "natural_opioid",
                opioid_type == "synth" ~ "synth_opioid",
                TRUE ~ opioid_type
            )
        )
    
    ## Subset to specified age and year
    sub_lel <- lel_df %>%
        filter(age == 15,
               abbrev == s_state) %>%
        select(-age, -age_cat)
    
    ## Subset to opioid types
    sub_lel <- sub_lel %>%
        filter(opioid_type %in% c("car_accident", "firearms", s_outcome))
    
    p <- ggplot(sub_lel,
                aes(
                    x = year,
                    y = ex_diff,
                    color = opioid_cat,
                    group = opioid_cat
                )) +
        geom_line() +
        scale_y_continuous(
            "Life Expectancy Lost at Age 15 (years)",
            expand = c(.01, 0),
            limits = c(0, NA)
        ) +
        scale_x_continuous(
            NULL,
            breaks = seq(2000, 2015, 5),
            labels = c("'00", "'05", "'10", "'15"),
            expand = c(0, .25)
        ) + 
        scale_color_manual(
            NULL,
            values = RColorBrewer::brewer.pal(9, "Set1")[c(1:4, 8:9)],
            drop = FALSE
            ) +
        geom_point() +
        mk_nytimes(
            panel.border =
                element_rect(
                    linetype = "solid",
                    fill = NA,
                    color = "grey75"
                ),
            legend.position = "bottom"
        )
    
    return(p)
}
