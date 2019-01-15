gen_color_legend <- function() {
    c_legend <-
        expand.grid(list(
            mort_lev = c("low_mort", "med_mort", "high_mort"),
            apc_lev  = c("low_apc", "med_apc", "high_apc")
        ),
        stringsAsFactors = TRUE) %>%
        as_tibble() %>%
        mutate(
            color_hex = c(
                "#e8e6f2",
                "#b5d3e7",
                "#4fadd0",
                "#e5b4d9",
                "#b8b3d8",
                "#3983bb",
                "#d34fa6",
                "#b03598",
                "#2a1a8a"
            ),
            color_hex_a = c(
                "#e8e8e8",
                "#e4acac",
                "#c85a5a",
                "#b0d5df",
                "#ad9ea5",
                "#985356",
                "#64acbe",
                "#627f8c",
                "#574249"
            )
        )
    
    return(c_legend)
}