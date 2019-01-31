library(shiny)

## Textual constants ----
## Paper title
paper_title <-
  "Assessment of Changes in the Geographical Distribution of Opioid-Related Mortality Across the United States by Opioid Type, 1999-2016"

footer_tag  <- HTML(
  "Created in <a href='https://shiny.rstudio.com/'>Shiny</a> by
    <a href='https://mathewkiang.com'>Mathew Kiang</a>.
    Source code is available on
    <a href='https://github.com/mkiang/opioid_geographic'>this paper's
    Github repository</a>."
)

## More info
more_info <- list(
  h4("More information"),
  HTML(
    "For more information, please see our article,
      available at the <a href='https://doi.org/10.1001/jamanetworkopen.2019.0040'>JAMA Network Open page</a> or the
        <a href='https://github.com/mkiang/opioid_geographic'>
        associated Github repository</a>. The full citation for the paper is <i>Kiang MV, Basu S, Chen JT, Alexander MJ. Assessment of Changes in the Geographical Distribution of Opioid-Related Mortality Across the United States by Opioid Type, 1999-2016. JAMA Network Open. 2019;2(2):e190040. doi: 10.1001/jamanetworkopen.2019.0040</i>. This project was funded in part through the National Institutes of Health (Award Number: DP2MD010478)."
  )
)

## Biographical tags
mvk_tag <-
  tags$li(a(href = "https://mathewkiang.com", "Mathew V Kiang"),
          HTML(paste0(
            "(",
            a(href = "https://twitter.com/mathewkiang",
              "@mathewkiang"),
            ")"
          )))
sb_tag <-
  tags$li(a(href = "https://sites.google.com/stanford.edu/basulab/home",
            "Sanjay Basu"))
jtc_tag <- tags$li(a(
  href = paste0(
    "http://www.dfhcc.harvard.edu/",
    "insider/member-detail/member/",
    "jarvis-t-chen-scd/"
  ),
  "Jarvis T Chen"
))
mja_tag <- tags$li(a(href = "http://monicaalexander.com",
                     "Monica J Alexander"),
                   HTML(paste0(
                     "(",
                     a(href = "https://twitter.com/monjalexander",
                       "@monjalexander"),
                     ")"
                   )))

## Make a state name:abbrev dictionary ----
st_name_abbrev <- as.list(c(state.abb, "DC"))
names(st_name_abbrev) <- c(state.name, "District of Columbia")
st_name_abbrev <- as.list(sort(unlist(st_name_abbrev)))

## Start the app UI ----
shinyUI(navbarPage(
  "Results Viewer",
  
  ## Hot spots panel
  tabPanel(
    title = "Epidemic Hotspots",
    
    ## Top row
    fluidRow(
      column(width = 4,
             wellPanel(
               tags$blockquote(em(paper_title)),
               tags$ul(mvk_tag, sb_tag, jtc_tag, mja_tag)
             )),
      
      column(
        width = 8,
        h4("Identifying epidemic hotspots"),
        p(
          "Here, we identify epidemic hotspots — defined as areas where the opioid mortality rate is both high and rapidly increasing. Using the panel below, you can change the year, definition of 'rapid' and 'slow' increases as well as 'low' and 'high' mortality rates, and the level of sigifnicance for either P-values or Q-values (FDR-adjusted P-values). In addition, we provide a table with all data when possible. Note that the NCHS data use agreement does not allow for displaying rates based on observations of fewer than ten deaths due to privacy concerns."
        ),
        
        more_info
      )
    ),
    
    hr(),
    
    ## HOTSPOTS SECTION ----
    fluidRow(column(
      width = 12,
      h3("Epidemic Hotspots"),
      align = "center",
      plotOutput("hotspots_map", height = "600px")
    )),
    fluidRow(
      column(
        offset = 4,
        width = 4,
        align = "center",
        style = "vertical-align: top;",
        plotOutput("hotspots_legend")
      )
    ),
    hr(),
    
    ## Settings row ====
    ## subsetting data ####
    fluidRow(
      column(
        width = 4,
        sliderInput(
          "h_year",
          label = "Year",
          min = 1999,
          max = 2016,
          step = 1,
          value = 2016,
          round = TRUE,
          sep = ""
        ),
        radioButtons(
          "h_p_or_q",
          label = "P- or Q-value",
          choices = list(
            "Unadjusted P-value" = "pval",
            "FDR-adjusted P-value (Q-value)" = "qval"
          ),
          selected = "pval"
        ),
        selectInput(
          "h_sigpvalue",
          label = "Significance level:",
          choices = list(
            "P < 0.100" = 0.100,
            "P < 0.050" = 0.050,
            "P < 0.010" = 0.010,
            "P < 0.005" = 0.005,
            "P < 0.001" = 0.001
          ),
          selected = 0.050
        )
      ),
      
      ## Mortality bins
      column(
        width = 4,
        h4("Define bins and significance:"),
        sliderInput(
          "mort_mid_bin",
          label = "Range of the 'Medium' Mortality bin:",
          min = 2.5,
          max = 30,
          step = 2.5,
          value = c(5, 10)
        ),
        ## APC bins
        sliderInput(
          "apc_mid_bin",
          label = "Range of the 'Moderate' APC bin:",
          min = 5,
          max = 100,
          step = 1,
          value = c(26, 41)
        ),
        checkboxGroupInput(
          "h_statebins",
          choices = list("(Plot as statebin)" = "yes"),
          submitButton(text = "Submit",
                       icon = NULL,
                       width = NULL)
        )
      ),
      ## Plot parameters ####
      column(width = 4,
             h4("In Context"),
             htmlOutput("percentile_hotspots"))
    ),
    
    hr(),
    br(),
    br(),
    
    ## Table row ====
    fluidRow(column(
      width = 12,
      align = "center",
      h3("Mortality Rate and APC Table"),
      DT::dataTableOutput("apc_rate_table")
    )),
    
    ## Footer ====
    fluidRow(column(
      width = 12,
      align = 'center',
      footer_tag
    ))
  ),
  
  
  ## NATIONAL PANEL ----
  tabPanel(
    title = "National Overview",
    
    ## Top row
    fluidRow(
      column(width = 4,
             wellPanel(
               tags$blockquote(em(paper_title)),
               tags$ul(mvk_tag, sb_tag, jtc_tag, mja_tag)
             )),
      
      column(
        width = 8,
        h4("Explore national results"),
        p(
          "For every state and opioid type, we fit joinpoint regression models to assess the temporal trend. Joinpoint models describe a time series as a set of breaks (i.e., joinpoints) and line segments where the slope of the line is expressed as the annual percent change (APC). When possible, we provide the raw data as well as the model fit; however, NCHS data use regulations dictate we remove all observations with fewer than 10 deaths. Using the panel below, you can specify the level of significance for either P- or Q-values as well as which elements are plotted. We provide a table of the average annual percent change (AAPC) for all states and opioid types. To see the joinpoint results for a specific state, use the 'State Results' tab above."
        ),
        
        more_info
      )
    ),
    
    hr(),
    
    ## National plot ####
    fluidRow(column(
      width = 12,
      align = "center",
      h3("Joinpoint Results Plot"),
      plotOutput("state_map", height = "650px", width = "100%")
    )),
    
    hr(),
    
    ## Settings row ====
    ## subsetting data ####
    fluidRow(
      column(
        width = 4,
        h4("Data / Significance"),
        checkboxGroupInput(
          "outcome",
          label = "Opioid type",
          choices = list(
            "All opioids" = "opioid",
            "Natural/Semi-synethtic" = "natural_opioid",
            "Heroin" = "heroin",
            "Synthetic" = "synth_opioid"
          ),
          selected = c("opioid", "natural_opioid",
                       "heroin", "synth_opioid")
        ),
        radioButtons(
          "p_or_q",
          label = "P- or Q-value",
          choices = list(
            "Unadjusted P-value" = "pval",
            "FDR-adjusted P-value (Q-value)" = "qval"
          ),
          selected = "pval"
        ),
        selectInput(
          "sigpvalue",
          label = "Significance level:",
          choices = list(
            "< .100" = 0.100,
            "< .050" = 0.050,
            "< .010" = 0.010,
            "< .005" = 0.005,
            "< .001" = 0.001
          ),
          selected = 0.050
        )
      ),
      
      ## Applying geom_*s ####
      column(
        width = 4,
        h4("Select plotting geometries"),
        checkboxInput("show_raw",
                      "Plot raw data",
                      value = TRUE),
        conditionalPanel(
          condition = "input.show_raw == true",
          checkboxInput("raw_ci",
                        "Plot raw data 95% CI",
                        value = FALSE)
        ),
        checkboxInput("model_fit",
                      "Plot model line",
                      value = TRUE),
        conditionalPanel(
          condition = "input.model_fit == true",
          checkboxInput("linetype_sig",
                        "Show significant segments",
                        value = TRUE)
        ),
        checkboxInput("joinpoint",
                      "Plot joinpoint locations",
                      value = TRUE),
        conditionalPanel(
          condition = "input.joinpoints == true",
          checkboxInput("joinpoint_sig",
                        "Show significant slope changes",
                        value = TRUE)
        )
      ),
      
      ## Plot parameters ####
      column(
        width = 4,
        h4("Select plot parameters"),
        sliderInput(
          "ymax",
          "Maximum y-axis value:",
          min = 5,
          max = 50,
          value = 30,
          step = 1
        ),
        checkboxInput("sig_aapc_only",
                      "Show significant AAPCs only",
                      value = FALSE),
        checkboxInput("legends_on",
                      "Show legends",
                      value = FALSE),
        checkboxInput("disable_clip",
                      "Disable plot clipping",
                      value = FALSE),
        submitButton(text = "Submit",
                     icon = NULL,
                     width = NULL)
      )
    ),
    
    hr(),
    
    ## Bottom row ====
    ## data table ####
    fluidRow(column(
      width = 12,
      align = "center",
      h3("AAPC Summary Table"),
      DT::dataTableOutput("aapc_table")
    )),
    
    hr(),
    
    ## Footer ====
    fluidRow(column(
      width = 12,
      align = 'center',
      footer_tag
    ))
  ),
  ## STATE SECTION ----
  tabPanel(
    title = "State Results",
    ## Top row
    fluidRow(
      column(width = 4,
             wellPanel(
               tags$blockquote(em(paper_title)),
               tags$ul(mvk_tag, sb_tag, jtc_tag, mja_tag)
             )),
      
      column(
        width = 8,
        h4("Explore state results"),
        p(
          "For every state and opioid type, we fit joinpoint regression models to assess the temporal trends and changes in the trend. We plot the results below. In addition, we calculated life expectancy lost due to opioid mortality using cause-deleted life tables (see manuscript for details). Using the panel below, you can change the state, outcome, significance level, as well as which elements are plotted. In addition, we display the model estimates and fit statistics."
        ),
        
        more_info
      )
    ),
    
    hr(),
    
    ## Plotting row ====
    fluidRow(column(
      width = 12,
      align = "center",
      h3("State-specific Results"),
      plotOutput("state_specific", width = "80%")
    )),
    
    fluidRow(column(
      width = 12,
      align = "center",
      plotOutput("state_specific_lel", width = "80%")
    )),
    
    hr(),
    
    ## Plotting parameters ====
    fluidRow(
      ## Subset data ####
      column(
        width = 4,
        h4("Data / Significance"),
        selectInput(
          "s_state",
          label = "Select state",
          choices = st_name_abbrev,
          selected = "Alaska"
        ),
        checkboxGroupInput(
          "s_outcome",
          label = "Opioid type",
          choices = list(
            "All opioids" = "opioid",
            "Natural/Semi-synethtic" = "natural_opioid",
            "Heroin" = "heroin",
            "Synthetic" = "synth_opioid"
          ),
          selected = c("opioid", "natural_opioid",
                       "heroin", "synth_opioid")
        )
      ),
      
      column(
        width = 4,
        h4(""),
        radioButtons(
          "s_p_or_q",
          label = "P- or Q-value",
          choices = list(
            "Unadjusted P-value" = "pval",
            "FDR-adjusted P-value (Q-value)" = "qval"
          ),
          selected = "pval"
        ),
        selectInput(
          "s_sigpvalue",
          label = "Significance level:",
          choices = list(
            "< 0.100" = 0.100,
            "< 0.050" = 0.050,
            "< 0.010" = 0.010,
            "< 0.005" = 0.005,
            "< 0.001" = 0.001
          ),
          selected = 0.050
        )
      ),
      
      column(
        width = 4,
        h4("Select plotting geometries"),
        checkboxInput("s_show_raw",
                      "Plot raw data",
                      value = TRUE),
        conditionalPanel(
          condition = "input.show_raw == true",
          checkboxInput("s_raw_ci",
                        "Plot raw data 95% CI",
                        value = FALSE)
        ),
        checkboxInput("s_model_fit",
                      "Plot model line",
                      value = TRUE),
        conditionalPanel(
          condition = "input.model_fit == true",
          checkboxInput("s_linetype_sig",
                        "Show significant segments",
                        value = TRUE)
        ),
        checkboxInput("s_joinpoint",
                      "Plot joinpoint locations",
                      value = TRUE),
        conditionalPanel(
          condition = "input.joinpoints == true",
          checkboxInput("s_joinpoint_sig",
                        "Show significant slope changes",
                        value = TRUE)
        ),
        h4("Select plot parameters"),
        checkboxInput("s_legends_on",
                      "Show legends",
                      value = FALSE),
        submitButton(text = "Submit",
                     icon = NULL,
                     width = NULL)
      )
    ),
    
    hr(),
    br(),
    br(),
    
    ## Table row ====
    fluidRow(column(
      width = 12,
      align = "center",
      h3("Model parameter estimates"),
      DT::dataTableOutput("state_table_estimates")
    )),
    
    br(),
    br(),
    
    fluidRow(
      column(
        width = 7,
        align = "center",
        h3("Predicted and observed rates"),
        DT::dataTableOutput("state_table_predictions")
      ),
      column(
        width = 4,
        offset = .75,
        align = "center",
        h3("Model fit summary"),
        DT::dataTableOutput("state_table_fit")
      )
    ),
    
    hr(),
    
    ## Footer ====
    fluidRow(column(
      width = 12,
      align = 'center',
      footer_tag
    ))
  ),
  ## LEL15 ----
  tabPanel(
    title = "Life Expectancy Lost",
    ## Top row
    fluidRow(
      column(width = 4,
             wellPanel(
               tags$blockquote(em(paper_title)),
               tags$ul(mvk_tag, sb_tag, jtc_tag, mja_tag)
             )),
      
      column(
        width = 8,
        h4("Explore life expectancy lost."),
        p(
          "In our paper, we present life expectancy lost at age 15 for 2016. Here, using the panel below, you can select other years and ages. In addition, we provide relative comparisons to contextualize the results. For example, across the whole US the life expectancy lost at age 15, in 2016, for car accidents and firearms was 0.30 years and 0.34 years, respectively. We limit the age to 15 years so keep the results conservative (i.e., a lower age will, generally, result in higher life expectancy lost) and avoid issues of competing hazards in early life."
        ),
        
        more_info
      )
    ),
    
    hr(),
    
    ## Plotting row ====
    fluidRow(column(
      width = 12,
      align = "center",
      h3("Life Expectancy Lost Results"),
      plotOutput("lel_map", height = "600px")
    )),
    
    hr(),
    
    ## Settings row ====
    ## subsetting data ####
    fluidRow(
      column(
        width = 4,
        sliderInput(
          "l_year",
          label = "Year",
          min = 1999,
          max = 2016,
          step = 1,
          value = 2016,
          round = TRUE,
          sep = ""
        )
      ),
      column(
        width = 4,
        sliderInput(
          "l_age",
          label = "Life Expectancy Lost at Age:",
          min = 15,
          max = 45,
          step = 5,
          value = 15,
          round = TRUE,
          sep = ""
        )
      ),
      column(
        width = 4,
        selectInput(
          "l_comparison",
          "Relative comparison:",
          choices =
            list(
              "None (absolute)" = "none",
              "National average (opioids)" = "nat_avg_opioids",
              "National average (firearms)" = "nat_avg_firearms",
              "National average (car accidents)" = "nat_avg_cars",
              "State-specific (firearms)" = "st_firearms",
              "State-specific (cars accidents)" = "st_cars"
            )
        ),
        checkboxGroupInput(
          "l_statebins",
          choices = list("(Plot as statebin)" = "yes"),
          submitButton(text = "Submit",
                       icon = NULL,
                       width = NULL)
        )
      )
    ),
    
    hr(),
    br(),
    br(),
    
    ## Table row ====
    fluidRow(column(
      width = 12,
      align = "center",
      DT::dataTableOutput("lel_table")
    )),
    
    hr(),
    # Footer ====
    fluidRow(column(
      width = 12,
      align = 'center',
      footer_tag
    ))
  )
))