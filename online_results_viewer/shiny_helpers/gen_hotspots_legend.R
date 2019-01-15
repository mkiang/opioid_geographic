gen_hotspots_legend <- function(mort_mid_bin, apc_mid_bin) {
    ## Create the plot
    p_legend <-
        ggplot(gen_color_legend(),
               aes(x = mort_lev, y = apc_lev, fill = color_hex)) +
        geom_tile(color = "white") +
        scale_fill_identity(na.value = "grey50") +
        scale_x_discrete(
            "Mortality rate\n(per 100,000)",
            expand = c(0.05, 0),
            labels = c(
                sprintf("Low: 0-%0.1f",
                        mort_mid_bin[1]),
                sprintf(
                    "Medium: %0.1f-%0.1f",
                    mort_mid_bin[1],
                    mort_mid_bin[2]
                ),
                sprintf("High: >%0.1f",
                        mort_mid_bin[2])
            )
        ) +
        scale_y_discrete(
            "APC (%)",
            expand = c(0.05, 0),
            labels = c(
                sprintf("Slow: 0-%i",
                        apc_mid_bin[1]),
                sprintf("Moderate: %i-%i",
                        apc_mid_bin[1],
                        apc_mid_bin[2]),
                sprintf("Rapid: >%i",
                        apc_mid_bin[2])
            )
        ) +
        mk_nytimes(
            panel.grid.major = element_blank(),
            axis.text = element_text(size = 11),
            axis.text.x = element_text(
                angle = 45,
                hjust = 1,
                vjust = 1
            ),
            axis.line = element_line(arrow = arrow(
                length = unit(.15, "inches"),
                type = "open",
                angle = 20
            )),
            plot.subtitle = element_text(size = 13, hjust =
                                             0.5)
        ) +
        labs(subtitle = "Legend") +
        coord_equal()
    
    return(p_legend)
}