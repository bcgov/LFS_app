## LFS app ----
## app title: LFS_app
## app ID: 4116302


## load libraries ----
library(tidyverse)      ## includes: dplyr, ggplot2, tibble, readr, tidyr, purrr, stringr, forcats
library(rsconnect)      ## deployment interface for shiny apps
library(shiny)          ## for most app functions: reactive(), downloadHandler(), renderUI(), shinyApp(), etc.
library(shinydashboard) ## for box()
library(shinyWidgets)   ## for useShinydashboard()
library(cansim)         ## for cansim data
# library(lubridate)    ## for ymd() (date parsing)
# library(janitor)      ## for clean_names() (on cansim data) and round_half_up()
# library(DT)           ## for dataTableOutput(), renderDataTable()
# library(plotly)       ## for plotlyOutput(), ggplotly(), renderPlotly
# library(scales)       ## for label_percent() in plots

options(scipen = 999999999)  ## so chart axes read properly

## chart theme/functions ----
# source("scripts/chart_theme.R")
# source("scripts/functions.R")

## load data ----
# data <- readRDS("data/data.rds") %>%
#   filter(str_sub(ref_date, start = 1, end = 4) >= 2010)
# 
# stats <- manual_data %>%
#   get_mom_stats() %>%
#   get_yoy_stats() %>%
#   get_ytd_stats() %>%
#   arrange(title)
#   
# titles_all <- read_csv("meta_data.csv")
# chart_list <- titles_all %>% select(chart_list) %>% pull()
# charts_multi <- titles_all %>% group_by(order) %>% tally() %>% filter(n > 1) %>% pull(order)


## get cansim data ----
# titles <- titles_all %>% filter(dataset == "cansim_auto") %>% select(-dataset)
# cansim_v <- titles %>% select(vector) %>% pull()
# cansim_t <- titles %>% select(title) %>% pull()
# titles <- titles %>% mutate(title = factor(x = cansim_t, levels = cansim_t))
# 
# cansim_data <- cansim::get_cansim_vector(
#   vectors = cansim_v,
#   start_time = "2010-01-01") %>%
#   mutate(REF_DATE = lubridate::ymd(REF_DATE, truncated = 2)) %>%
#   janitor::clean_names() %>%
#   left_join(titles, by = c("vector")) %>%
#   select(title, label, filter_var, ref_date, value)
# 
# cansim_stats <- cansim_data %>%
#   get_mom_stats() %>%
#   get_yoy_stats() %>%
#   get_ytd_stats() %>%
#   arrange(title)

## merge data ----
# all_data <- bind_rows(cansim_data, manual_data) %>%
#   mutate(title = factor(title, levels = unique(titles_all$title))) %>%
#   left_join(titles_all %>% select(order, title, line, source, chart_list), by = "title")
# 
# all_stats <- all_data %>%
#   get_mom_stats() %>%
#   get_yoy_stats() %>%
#   get_ytd_stats() %>%
#   arrange(title)

## downloadable data ----

# * main data
# data_main <- all_data %>%
#   mutate(Indicator = str_replace_all(title, pattern = "<b>", ""),
#          Indicator = str_replace_all(Indicator, pattern = "</b>", ""),
#          Indicator = str_replace_all(Indicator, "<br>", " ")) %>%
#   select(Indicator, Data = ref_date, Value = value, Source = source)


## start of app ----
# UI demonstrating column layouts
ui <- function(req) {
  fluidPage(shinyWidgets::useShinydashboard(),
            title = "LFS App",
            theme = "bootstrap.css",
            HTML("<html lang='en'>"),
            
            fluidRow(
              ## header column ----
              column(
              width = 12,
              style = "background-color:#003366; border-bottom:2px solid #fcba19; position:fixed; z-index:10000",
                 tags$header(class="header", style="padding:0 0px 0 0px; display:flex; height:80px;
                 width:100%;",
                   tags$div(class="banner", style="display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
                     a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                       img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
                       onclick="gtag"),
                     h1("British Columbia - Labour Market Statistics", style="font-weight:400; color:white; margin: 5px 5px 0 18px;")
                   )
                 )
              ),  ## end of column
    ## main body column ----
    ## Make changes to this column
    column(width = 12,
           style = "margin-top:100px",
           
           ## creating tabs here ----  
           tabsetPanel(id = "tabs",
               
               ## tabPanel 1: Summary ----
               tabPanel(title = "Summary",
                        value = 1,
                        ## "About" column
                        column(width = 3, 
                               tags$fieldset(
                                 tags$legend(h3("About")),
                                 "blurb1",
                                 br(),br(),
                                 "blurb2",
                                 br(),br()
                                 # ,
                                 # selectInput(inputID = "dataset",
                                 #             label = "Choose a dataset to download:",
                                 #             choices = c("Economic Recovery Indicators", "Exports")),
                                 # downloadButton("downloadData", "Download Data"),br(),br(),br(),
                                 # h4("Glossary"),br(),
                                 # "Throughout this dashboard, data is referenced in the following ways:",br(),
                                 # strong("SA:"),"seasonally adjusted",br(),
                                 # strong("NSA:"),"not seasonally adjusted",br(),
                                 # strong("SAAR:"),"seasonally adjusted at annual rates",br(),
                                 # strong("3MMA:"),"three-month moving average"
                               )),
                        ## Data tables
                        column(width = 9,
                        tags$fieldset(
                          tags$legend(h3("Summary Type")),
                          selectInput(
                            inputId = "summary_type",
                            label = NULL,
                            choices = c("Economic Recovery Indicators", "Exports by Destination and Commodity", "Exports by Commodity"),
                            selected = "Economic Recovery Indicators")
                        ),
                        conditionalPanel(condition = "input.summary_type == 'Economic Recovery Indicators'",
                                         br(),
                                         tags$div(
                                           style="margin-left:15px;margin-bottom:20px",
                                           h3("B.C. Summary - Key Economic Recovery Indicators")),
                                         # box(title = "OVERALL ECONOMY", status = "primary",
                                         #     solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                                         #     DT::dataTableOutput("ERI_overall")),
                                         # box(title = "BRITISH COLUMBIANS", status = "primary",
                                         #     solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                                         #     DT::dataTableOutput("ERI_bcians")),
                                         tags$div(
                                           style="margin-left:15px",
                                             '(1) This dataset does not include persons who received the Canada Emergency Response Benefit (CERB). 
                                             Between March and September 2020, CERB was introduced and the number of EI recipients dropped
                                             significantly as persons could not receive both. The CERB program ended on September 27, 2020
                                             and eligibility rules for EI were changed, resulting in a dramatic increase in the
                                             number of EI beneficiaries in October 2020.', 
                                             br(),
                                             'Note: Due to data collection issues and resulting quality concerns, information on 
                                             TransLink weekly ridership is no longer being published. If you’d like more information,
                                             please contact BCStats: BC.Stats@gov.bc.ca'),
                                         br()
                                         ),
                        conditionalPanel(condition = "input.summary_type == 'Exports by Commodity'",
                                         br(),
                                         tags$div(
                                           style="margin-left:15px;margin-bottom:20px",
                                           h3("B.C. Summary - Key Exports by Commodity")),
                                         # box(title = "EXPORTS BY COMMODITY ($Thousands, NSA)", status = "primary",
                                         #     solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                                         #     DT::dataTableOutput("Exp_overall")),
                                         tags$div(
                                           style="margin-left:15px",
                                           'Note: Trends for latest reporting month compared to previous month are not available since data 
                                           are not seasonally adjusted.',br(),
                                           'Source: ', #source_exports,br(),
                                           'For more information on other commodities or countries visit: ', 
                                           tags$a('https://www2.gov.bc.ca/gov/content/data/statistics/business-industry-trade/trade/trade-data')),
                                         br()
                        )
                       )  ## end of Data tables
               ),  ## end of tabPanel 1
               
               ## tabPanel 2: Detailed Summary ----
               tabPanel("Detailed Summary",
                        value = 2,
                        br(),
                        tags$div(
                          style="margin-left:15px;margin-bottom:20px",
                          h3("B.C. Detailed Summary - blahblahblah")
                          ),
                        # box(title = "OVERALL ECONOMY", status = "primary",
                        #     solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                        #     DT::dataTableOutput("DET_overall")
                        #    ),
                        # box(title = "BUSINESSES", status = "primary",
                        #     solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                        #     DT::dataTableOutput("DET_businesses")
                        #   ),
                        # box(title = "BRITISH COLUMBIANS", status = "primary",
                        #     solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                        #     DT::dataTableOutput("DET_bcians")),
                        tags$div(
                          style="margin-left:15px",
                          '(1) This dataset does not include persons who received the Canada Emergency Response Benefit (CERB). 
                                             Between March and September 2020, CERB was introduced and the number of EI recipients dropped
                                             significantly as persons could not receive both. The CERB program ended on September 27, 2020
                                             and eligibility rules for EI were changed, resulting in a dramatic increase in the
                                             number of EI beneficiaries in October 2020.', 
                          br(),
                          'Note: Due to data collection issues and resulting quality concerns, information on 
                                             TransLink weekly ridership is no longer being published. If you’d like more information,
                                             please contact BCStats: BC.Stats@gov.bc.ca'),
                        br()
               ),   ## end of tabPanel 2
               
               ## tabPanel 3: Charts ----
               tabPanel("Charts",
                        value = 3,
                        br(),
                        column(width = 3
                               # , 
                        # tags$fieldset(
                        #   tags$legend(h3("Indicator")),
                        #   #shiny::radioButtons(inline = TRUE,
                        #     shiny::selectInput( ## drop-down list
                        #     inputId = "indicator",
                        #     label = NULL,
                        #     choices = str_replace_all(unique(chart_list), pattern = "<br>", replacement = " "),
                        #     selected = chart_list[1],
                        #     selectize = FALSE,
                        #     size = length(unique(chart_list)),
                        #     width = "100%"))
                        ),
                        column(width = 9,
                        tags$fieldset(
                          br(),br(),br()
                          # ,
                          # plotly::plotlyOutput(outputId = "charts"),
                          # shiny::uiOutput(outputId = "caption")
                        )),
                        br(),br()
               ),  ## end of tabPanel 3
               
               ## tabPanel 4: Data Sources ----
               tabPanel("Data Sources",
                        value = 4,
                        br(),
                        tags$div(
                          style="margin-left:15px;margin-bottom:20px",
                          h3("Data Sources & Permissions")
                          ),
                        tags$div(
                          style="margin-left:15px",
                          'The data in the Economic Recovery Indicators are drawn from a variety of 
                             sources; the sources are shown in the table below, and on the Charts tab.',
                          br(),br(),h4("Statistics Canada"),
                          'Data from Statistics Canada is provided under the Statistics Canada Open License ',
                          tags$a('(https://www.statcan.gc.ca/eng/reference/licence)'), '.',
                          br(),br(),h4("British Columbia (BC Stats)"),
                          'Data from BC Stats is provided under the Open Government Licence - British Columbia ',
                          tags$a('(https://www2.gov.bc.ca/gov/content/data/open-data/open-government-licence-bc)'), '.',
                          br(),br(),h4("US Census Bureau"),
                          'Data from the US Census Bureau is provided as open data ',
                          tags$a('(https://www.census.gov/about/policies/open-gov/open-data.html)'), '.',
                          br(),br(),h4("CBRE Limited"),
                          'Data from CBRE Limited ("CBRE) permitted subject to CBRE Limited Disclaimer / Terms of Use ',
                          tags$a('(https://www.cbre.ca/en/real-estate-services/business-lines/valuation-and-advisory-services/hotels-valuation-and-advisory-services/disclaimer)'), '.',
                          br(),br(),
                          DT::dataTableOutput("sources_list")
                          ),
                        br(),
               ),  ## end of tabPanel 3
               type = "tabs"
             ),  ## end of tabsetPanel

    ), ## End of column to make changes to
    
    ## footer column ----
    column(width = 12,
           style = "background-color:#003366; border-top:2px solid #fcba19;",

            tags$footer(class="footer",
              tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                    tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                )
              )
             )
    )
  )
)}


## define server logic ----
server <- function(input, output, session) {
  
  ## Tab 1: Key Economic Recovery Indicators ----
  
  # output$ERI_overall <- DT::renderDataTable({
  #   
  #   data <- all_stats %>%
  #     filter(filter_var == "overall") %>%
  #     format_summary_data() %>%
  #     datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:4)),
  #                              sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
  #               rownames = FALSE, escape = FALSE, filter = "none")
  #   
  # })

  

  ## Tab 1: Data Download ----
  # 
  # # Reactive value for selected dataset to download
  # datasetInput <- shiny::reactive({
  #   switch(input$dataset,
  #          "Economic Recovery Indicators" = data_main,
  #          "Exports" = data_exp)
  # })
  # 
  # # Downloadable csv of selected dataset to download
  # output$downloadData <- shiny::downloadHandler(
  #   filename = function() {
  #     paste0(str_replace_all(input$dataset, " ", "_"), "_", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     write.csv(datasetInput(), file, row.names = FALSE)
  #   }
  # )
  # 
  ## Tab 2: Detailed Summary ----
  
  # output$DET_overall <- DT::renderDataTable({
  #   
  #   data <- all_stats %>%
  #     filter(filter_var == "overall") %>%
  #     format_detailed_data() %>%
  #     datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:8)),
  #                              sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
  #               rownames = FALSE, container = custom_headers, escape = FALSE, filter = "none")
  #   
  # })
  
  
  ## Tab 3: Charts ----
  
  # get_data <- shiny::reactive({
  #   
  #   req(input$indicator)
  #   
  #   line_chart <- all_data %>% 
  #     filter(chart_list == input$indicator)
  #   
  #   ## tables display units, chart displays in thousands
  #   if(input$indicator == "Housing Starts") {
  #     line_chart <- line_chart %>%
  #       mutate(value = janitor::round_half_up(value/1000, digits = 0),
  #              title = str_replace(title, pattern = "Units", replacement = "Thousands"))
  #   }
  #   ## tables display values, chart displays y-o-y
  #   if(str_detect(input$indicator, "Consumer Price Index")) {
  #     line_chart <- cpi_yoy %>% 
  #       select(-value) %>%
  #       rename(value = yoy_pct)
  #   }
  #   
  #   list(line_chart = line_chart)
  # })
  
  # output$charts <- plotly::renderPlotly({
  # 
  #   if(is.null(get_data()$line_chart)){
  #     NULL
  #     
  #   } else {
  #     
  #     # prep plot
  #     p <- ggplot(get_data()$line_chart,
  #                 aes(x = ref_date, y = value)) +
  #       bcstats_chart_theme +
  #       scale_x_date(limits = c(min(get_data()$line_chart$ref_date),
  #                               max(get_data()$line_chart$ref_date) + months(3)),
  #                    expand = c(0,0),
  #                    date_breaks = "6 months",
  #                    date_labels = "%b %Y")
  #     
  #     ## multi-line charts need geom_line colored by line, with no-title legend
  #     if(any(get_data()$line_chart$order %in% charts_multi)) {
  #       
  #       p <- p +
  #         labs(x = NULL,
  #              y = NULL, 
  #              title = get_data()$line_chart$label %>% head(1) %>% as.character()) +
  #           geom_line(aes(color = line)) +              # color each line differently
  #           scale_color_manual(values = line_colors) +  # use specified colors (up to 4)
  #           bcstats_chart_theme +
  #           theme(legend.title = element_blank(),
  #                 legend.position = "bottom"            # this isn't working: plotly ONLY places legend right (ignores all other positions)
  #                 )
  #      
  #       ## And, if CPI chart, add horizontal line at 0
  #       if(str_detect(input$indicator, "Consumer Price Index")) {
  #         p <- p +
  #           # scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  #           geom_hline(yintercept = 0)
  # 
  #       }
  #     } else {
  #       
  #       p <- p + 
  #         geom_line() + 
  #         labs(x = NULL,
  #              y = NULL, 
  #              title = get_data()$line_chart$title %>% head(1) %>% as.character())
  #       
  #     }
  # 
  #     p <- ggplotly(p)
  # 
  #   }
  # })
  # 
  # 
  # output$caption <- shiny::renderUI({
  #   
  #  
  #   if(any(str_detect(get_data()$line_chart$title, "\\(1\\)"))) {
  #     HTML(paste0("Source: ", get_data()$line_chart$source %>% head(1), " <br>
  #          (1) This dataset does not include persons who received the Canada Emergency Response 
  #          Benefit (CERB). Between March and September 2020, CERB was introduced and the number of 
  #          EI recipients dropped significantly as persons could not receive both. The CERB program 
  #          ended on September 27, 2020 and eligibility rules for EI were changed, resulting in a 
  #          dramatic increase in the number of EI beneficiaries in October 2020."))
  #     
  #   } else {
  #     HTML(paste0("Source: ", get_data()$line_chart$source %>% head(1)))
  #   }
  #   
  # })

  ## Tab 4: Data Sources ----
  
  # output$sources_list <- DT::renderDataTable({
  #   
  #   sources_list <- titles_all %>% 
  #     select(Indicator = title, Source = source) %>%
  #     mutate(Indicator = str_replace_all(Indicator, pattern = "<b>", " "),
  #            Indicator = str_replace_all(Indicator, pattern = "</b>", " "),
  #            Indicator = str_replace_all(Indicator, pattern = "<br>", " ")) %>%
  #     add_row(Indicator = "Exports by (Destination and) Commodity", Source = source_exports, .before = 1)
  #   
  # })
}


## knit together ui and server ----
shiny::shinyApp(ui = ui, server = server)
