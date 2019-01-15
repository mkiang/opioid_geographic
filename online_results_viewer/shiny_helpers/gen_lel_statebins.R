library(tidyverse)
library(statebins)

gen_lel_statebins <- function(lel_df,
                              l_age,
                              l_year,
                              l_comparison) {
    c_scale <- rev(RColorBrewer::brewer.pal(11, "RdYlGn")[c(1, 6, 11)])
    
    ## Subset to specified age and year
    sub_lel <- lel_df %>%
        filter(age == l_age,
               year == l_year,
               !is.na(opioid_cat)) %>%
        select(-age, -age_cat, -year)
    
    ## DF of only national opioid LEL in wide format (1 row)
    us_lel <- sub_lel %>%
        filter(abbrev == "US",
               opioid_type %in% c("opioids", "heroin", "synth", "natural")) %>%
        select(-st_fips, -abbrev, -opioid_cat) %>%
        rename(us_diff = ex_diff)
    
    ## DF of national car accident and firearm LEL
    us_car_gun_lel <- sub_lel %>%
        filter(opioid_type %in% c("firearms", "car_accident"),
               abbrev == "US") %>%
        select(-opioid_cat, -st_fips) %>%
        spread(opioid_type, ex_diff)
    
    ## DF of only firearms and car accidents in wide format
    st_car_gun_lel <- sub_lel %>%
        filter(opioid_type %in% c("firearms", "car_accident")) %>%
        select(-opioid_cat, -st_fips) %>%
        spread(opioid_type, ex_diff)
    
    ## Join so we can get relative lel comparisons
    sub_lel <- sub_lel %>%
        left_join(st_car_gun_lel) %>%
        left_join(us_lel) %>%
        mutate(us_car = us_car_gun_lel$car_accident,
               us_gun = us_car_gun_lel$firearms)
    
    ## Make relative comparisons
    sub_lel <- sub_lel %>%
        mutate(
            rel_us_car = ex_diff / us_car,
            rel_us_gun = ex_diff / us_gun,
            rel_us_op  = ex_diff / us_diff,
            rel_st_car = ex_diff / car_accident,
            rel_st_gun = ex_diff / firearms
        ) %>%
        select(-car_accident, -firearms, -us_diff, -us_car, -us_gun)
    
    if (l_comparison == "none") {
        p1 <- ggplot(sub_lel %>%
                         filter(abbrev != "US"),
                     aes(state = abbrev, fill = ex_diff)) +
            scale_fill_viridis(
                option = "C",
                name = sprintf("Life Expectancy Lost After Age %i (years)",
                               l_age),
                direction = -1,
                discrete = FALSE,
                na.value = "grey50"
            )
    } else if (l_comparison == "nat_avg_opioids") {
        p1 <- ggplot(sub_lel %>%
                         filter(abbrev != "US"),
                     aes(state = abbrev, fill = rel_us_op)) +
            scale_fill_gradient2(
                low  = c_scale[1],
                mid  = c_scale[2],
                high = c_scale[3],
                midpoint = 0,
                name = sprintf(
                    "Relative Life Expectancy Lost After Age %i (%s)",
                    l_age,
                    "State vs Total US Opioid LEL"
                ),
                na.value = "grey50",
                trans = "log",
                labels = function(x)
                    sprintf("%0.2f", round(x, 2))
            )
    } else if (l_comparison == "nat_avg_firearms") {
        p1 <- ggplot(sub_lel %>%
                         filter(abbrev != "US"),
                     aes(state = abbrev, fill = rel_us_gun)) +
            scale_fill_gradient2(
                low  = c_scale[1],
                mid  = c_scale[2],
                high = c_scale[3],
                midpoint = 0,
                name = sprintf(
                    "Relative Life Expectancy Lost After Age %i (%s)",
                    l_age,
                    "State vs Total US Firearm LEL"
                ),
                na.value = "grey50",
                trans = "log",
                labels = function(x)
                    sprintf("%0.2f", round(x, 2))
            )
    } else if (l_comparison == "nat_avg_cars") {
        p1 <- ggplot(sub_lel %>%
                         filter(abbrev != "US"),
                     aes(state = abbrev, fill = rel_us_car)) +
            scale_fill_gradient2(
                low  = c_scale[1],
                mid  = c_scale[2],
                high = c_scale[3],
                midpoint = 0,
                name = sprintf(
                    "Relative Life Expectancy Lost After Age %i (%s)",
                    l_age,
                    "State vs Total US Car Accident LEL"
                ),
                na.value = "grey50",
                trans = "log",
                labels = function(x)
                    sprintf("%0.2f", round(x, 2))
            )
    } else if (l_comparison == "st_firearms") {
        p1 <- ggplot(sub_lel %>%
                         filter(abbrev != "US"),
                     aes(state = abbrev, fill = rel_st_gun)) +
            scale_fill_gradient2(
                low  = c_scale[1],
                mid  = c_scale[2],
                high = c_scale[3],
                midpoint = 0,
                name = sprintf(
                    "Relative Life Expectancy Lost After Age %i (%s)",
                    l_age,
                    "State Opioids LEL vs State Firearm LEL"
                ),
                na.value = "grey50",
                trans = "log",
                labels = function(x)
                    sprintf("%0.2f", round(x, 2))
            )
    } else if (l_comparison == "st_cars") {
        p1 <- ggplot(sub_lel %>%
                         filter(abbrev != "US"),
                     aes(state = abbrev, fill = rel_st_car)) +
            scale_fill_gradient2(
                low  = c_scale[1],
                mid  = c_scale[2],
                high = c_scale[3],
                midpoint = 0,
                name = sprintf(
                    "Relative Life Expectancy Lost After Age %i (%s)",
                    l_age,
                    "State Opioids LEL vs State Car Accident LEL"
                ),
                na.value = "grey50",
                trans = "log",
                labels = function(x)
                    sprintf("%0.2f", round(x, 2))
            )
    }
    
    p1 <- p1 +
        geom_statebins() +
        coord_equal() +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        facet_wrap( ~ opioid_cat, ncol = 3) +
        mk_nytimes(
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            axis.text = element_blank(),
            panel.border = element_rect(
                linetype = "solid",
                fill = NA,
                color = "grey75"
            )
        ) +
        guides(
            fill =
                guide_colorbar(
                    title.position = "top",
                    barwidth = 25,
                    barheight = .75,
                    label.position = "bottom",
                    label.hjust = .5
                )
        )
    
    return(p1)
}


# ggplot(sub_lel %>%
#            filter(abbrev != "US"),
#        aes(y = ex_diff, x = opioid_cat)) +
#     geom_boxplot() +
#     geom_point(position = position_jitter(.1, 0, seed = 1), alpha = .15) +
#     mk_nytimes() +
#     scale_x_discrete(NULL) +
#     scale_y_continuous(sprintf("Life expectancy lost at age %i, %i",
#                                l_age, l_year))
