library(statebins)
library(tidyverse)
library(usmap)

map_df <- usmap::us_map("states") %>%
    rename(abbrev = abbr)

gen_hotspots_map <-
    function(jp_results,
             h_p_or_q,
             h_sigpvalue,
             mort_mid_bin,
             apc_mid_bin,
             h_year = 2016) {
        sigpvalue  <- as.numeric(h_sigpvalue)
        
        ## Subset to current segment and make apc/mortality categories
        hotspots <- jp_results %>%
            group_by(abbrev, opioid_type) %>%
            filter(year == as.numeric(h_year))
        
        if (h_p_or_q == "pval") {
            hotspots <- hotspots %>%
                filter(slope_pval < sigpvalue)
        } else {
            hotspots <- hotspots %>%
                filter(slope_qval < sigpvalue)
        }
        
        hotspots <- hotspots %>%
            transmute(
                mort_lev =
                    case_when(
                        between(rate, 0, mort_mid_bin[1]) ~ "low_mort",
                        between(rate, mort_mid_bin[1],
                                mort_mid_bin[2]) ~ "med_mort",
                        rate > mort_mid_bin[2] ~ "high_mort",
                        TRUE ~ NA_character_
                    ),
                apc_lev =
                    case_when(
                        between(apc, 0, apc_mid_bin[1]) ~ "low_apc",
                        between(apc, apc_mid_bin[1],
                                apc_mid_bin[2]) ~ "med_apc",
                        apc > apc_mid_bin[2] ~ "high_apc",
                        TRUE ~ NA_character_
                    )
            )
        
        ## Join with color scale and join back with original data
        hotspots <- hotspots %>%
            left_join(gen_color_legend()) %>%
            right_join(jp_results %>%
                           group_by(abbrev, race, opioid_type) %>%
                           filter(year == as.numeric(h_year)))
        
        ## Convert them to factors so they retain order
        hotspots <- hotspots %>%
            mutate(
                color_hex_na = ifelse(is.na(color_hex), "grey97", color_hex),
                mort_lev = factor(
                    mort_lev,
                    levels = c("low_mort", "med_mort", "high_mort"),
                    ordered = TRUE
                ),
                apc_lev = factor(
                    apc_lev,
                    levels = c("low_apc", "med_apc", "high_apc"),
                    ordered = TRUE
                )
            )
        
        map_df <-
            usmap::us_map(regions = "states") %>%
            rename(abbrev = abbr) %>%
            left_join(hotspots %>%
                          select(abbrev, color_hex_na, opioid_cat))
        
        ## Make plot
        p1 <- ggplot() +
            geom_polygon(data = map_df,
                         aes(
                             x = long,
                             y = lat,
                             group = group,
                             fill = color_hex_na
                         ), 
                         color = "black", 
                         size = .15) +
            scale_fill_identity(NULL, na.value = "white") +
            coord_equal() +
            scale_x_continuous(NULL, expand = c(0, 0)) +
            scale_y_continuous(NULL, expand = c(0, 0)) +
            facet_wrap( ~ opioid_cat, ncol = 2) +
            mk_nytimes(
                legend.position = "none",
                panel.grid.major = element_blank(),
                axis.text = element_blank(),
                # panel.border = element_rect(
                #     linetype = "solid",
                #     fill = NA,
                #     color = "grey75"
                # )
            )
        
        return(p1)
    }
