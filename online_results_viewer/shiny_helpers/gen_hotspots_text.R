library(shiny)
library(tidyverse)

gen_hotspots_text <- function(jp_results,
                              h_year,
                              mort_mid_bin,
                              apc_mid_bin,
                              h_p_or_q,
                              h_sigpvalue) {
    
    sigpvalue  <- as.numeric(h_sigpvalue)
    
    ## Subset data to just this outcome and most recent segment
    hotspot_df <- jp_results %>%
        filter(dupe_row == 0,
               year == h_year)
    
    if (h_p_or_q == "qval") {
        hotspot_df$slope_pval <- hotspot_df$slope_qval
    }
    
    mort_lo <-
        mean(hotspot_df$rate < mort_mid_bin[1], na.rm = TRUE)
    mort_hi <-
        mean(hotspot_df$rate < mort_mid_bin[2], na.rm = TRUE)
    
    apc_lo_all <-
        mean(hotspot_df$apc < apc_mid_bin[1], na.rm = TRUE)
    apc_hi_all <-
        mean(hotspot_df$apc < apc_mid_bin[2], na.rm = TRUE)
    
    apc_lo_sig <- mean(hotspot_df %>%
                           filter(slope_pval < sigpvalue) %>%
                           pull(apc) < apc_mid_bin[1],
                       na.rm = TRUE)
    apc_hi_sig <- mean(hotspot_df %>%
                           filter(slope_pval < sigpvalue) %>%
                           pull(apc) < apc_mid_bin[2],
                       na.rm = TRUE)
    
    HTML(
        sprintf(
            "For %i, mortality rates of %0.1f and %0.1f per 100,000 represented
            the %i%%ile and %i%%ile of observed mortality rates,
            respectively. <br> <br>
            
            Annual percent changes (APCs) of %0.0f%% and %0.0f%%
            represent the %i%%ile and %i%%ile of all APCs and the
            %i%%ile and %i%%ile of statistically significant APCs
            (at the %s<%0.3f level), respectively. APCs of %0.0f%% and %0.0f%% 
            mean, at a constant rate of increase, the mortality rate would 
            double about every %0.1f and %0.1f years, respectively.",
            h_year, 
            mort_mid_bin[1],
            mort_mid_bin[2],
            round(mort_lo * 100, 0),
            round(mort_hi * 100, 0),
            apc_mid_bin[1],
            apc_mid_bin[2],
            round(apc_lo_all * 100, 0),
            round(apc_hi_all * 100, 0),
            round(apc_lo_sig * 100, 0),
            round(apc_hi_sig * 100, 0),
            ifelse(h_p_or_q == "pval", "P", "Q"), 
            sigpvalue, 
            apc_mid_bin[1],
            apc_mid_bin[2],
            round(log(2) / log(1 + apc_mid_bin[1]/100), 1), 
            round(log(2) / log(1 + apc_mid_bin[2]/100), 1)
        )
    )
    
    
}
