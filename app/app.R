# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at 
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

google_tracking <- FALSE

## LFS app ----

## Start of app ----
# UI demonstrating column layouts
ui <- function(req) {
  
  if(cansim_error) { 
    page_fluid( 
      h2("Application temporarily unavailable", style = "padding-top:2rem; padding-bottom:2rem"),
      p("The application was unable to connect to Statistics Canada's data service while loading the required data."),
      p("Please try again later. If the problem persists, ",
        a(href = "https://dpdd.atlassian.net/servicedesk/customer/portal/12", "contact BC Stats."))
      )  
  } else { 
    htmltools::tags$html(
      lang = "en",
      
      htmltools::tagList(
        
        ## styles, etc.
        htmltools::tags$head(
          htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "BC_Sans.css"),  ## set up BC Sans fonts
          htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "variables.css"),## bcgov design tokens
          htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),   ## custom styles
          htmltools::tags$link(rel = "shortcut icon", href = "favicon.png"),  ## add BCGov favicon
          if(google_tracking){  htmltools::includeHTML("www/google-analytics.html") }  ## to add GA tracking code (see global.R for more details)
        ),
        
        ## Custom formatting ----
        ## formatting for icons in valueBoxes
        tags$head(tags$style(HTML('.small-box .icon-large {top: -10px;}'))),
        
        ## formatting for tabBox content
        tags$head(tags$style(HTML('.nav-tabs-custom>.tab-content {border: 1px solid #3c8dbc}'))),
        tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs>li.active
                                      {border-top-color: #3c8dbc; border-left: 1px solid #3c8dbc;'))),
        tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs>li.active>a {border-right: 1px solid #3c8dbc}'))),

        ## Header column ----
        bcsHeaderUI(
          id = "header",
          appname = "Labour Market Statistics for British Columbia",
          mobilename = "Labour Market",
          github = "https://github.com/bcgov/LFS_app"
        ),
        
        ## Main body column ----
        ## Make changes to this column
        page_fluid(
          title = "LFS App",
          class = "app-page",
          
          ## Tabset start ----  
          navset_bar(id = "tabs",
                     gap = "0",
                     ### Highlights tab ----
                     nav_panel(
                       "Highlights",
                       h2("Labour Force Survey", class = "mt-3 mb-0"),
                       h3(textOutput("ref_date_title"), class = "mt-0"),
                       tags$details(
                         tags$summary("About this dashboard"),
                           p("Updated", strong("monthly"), "following the release of
                         Statistics Canada's Labour Force Survey (LFS)
                         this dashboard provides an", strong("up-to-date view of labour market conditions
                         in British Columbia.")),
                           p("Use this dashboard to:"),
                           tags$ul(
                             class = "mb-2",
                             tags$li(strong("Explore key indicators and trends"), "related to employment, unemployment, labour force participation, and workforce characteristics"),
                             tags$li(strong("Track changes over time and compare labour market outcomes"), "across age groups, genders and regions"),
                             tags$li(strong("Access insights"), " that support research, policy development, workforce planning, and evidence-based decision-making")
                           ),
                           p("For additional information about the Labour Force Survey
                           and other labour market statistics, visit the
                           Province of British Columbia's",
                             tags$a("Labour Market Statistics",
                                    href = "https://www2.gov.bc.ca/gov/content/data/statistics/economy/labour-market-statistics"),
                             "webpage.")
                         
                       ),
                       layout_column_wrap(
                         class = "mt-4",
                         width = "300px",
                         fixed_width = FALSE,
                         uiOutput("emp"),
                         uiOutput("unemprate"),
                         uiOutput("lf"),
                         uiOutput("partrate")
                       ),
                       h3("Key indicators", class = "mt-4"),
                       p(class = "mb-2",
                         "Selected indicators showing recent changes in B.C.'s labour market"),
                       withSpinner(reactableOutput("key_indicators_table")),
                       h3("Understanding the labour market", class = "mt-4"),
                       p(class = "mb-2",
                       "The population aged 15 years and over is divided 
                         into the labour force and those not in the labour force. 
                         The labour force consists of people who are employed 
                         or unemployed."),
                       div(class = "flowchart-small",
                           withSpinner(grVizOutput("flow_small" ))),
                       div(class = "flowchart-large",
                           withSpinner(grVizOutput("flow_large", height = 300))),
                       h3("Labour force characteristics", class = "mt-4"),
                       p(class = "mb-2",
                         "Detailed estimates and recent changes for the components of the labour market"),
                       withSpinner(reactableOutput("lf_characteristics_table"))
                     ),
                     ### Monthly comparison tab ----
                     nav_panel(
                       "Monthly comparisons",
                       accordion(
                         class = "mc-accordion",
                         id = "mc_accordion",
                         accordion_panel(
                           title = h2("Provincial comparisons"),
                           value = "provincial_comparisons",
                           layout_column_wrap(
                             width = "450px",
                             fixed_width = FALSE,
                             card(
                               card_header(
                                 div(
                                   div(class = "mb-1",
                                       style = "font:var(--typography-bold-h5)",
                                       "Monthly employment change for Canada and Provinces"),
                                   div(style = "font-size:var(--typography-font-size-small-body);
                                            color:var(--typography-color-secondary)",
                                       "Seasonally adjusted")
                                 )
                               ),
                               card_body(
                                 withSpinner(plotlyOutput("bar1")),
                                 p(class = "mb-0 source",
                                   "Source:",
                                   "Statistics Canada.", 
                                   a(href = "https://doi.org/10.25318/1410028701-eng",
                                     "Table 14-10-0287-03  Labour force characteristics by province, monthly, seasonally adjusted"
                                       )
                                 )
                               )),
                             card(
                               card_header(
                                 div(
                                   div(class = "mb-1",
                                       style = "font:var(--typography-bold-h5)",
                                       "Unemployment rate for Canada and Provinces"),
                                   div(style = "font-size:var(--typography-font-size-small-body);
                                            color:var(--typography-color-secondary)",
                                       "15 years and over, seasonally adjusted")
                                 )
                               ),
                               card_body(
                                 withSpinner(plotlyOutput("bar2")),
                                 p(class = "mb-0 source",
                                   "Source:",
                                   "Statistics Canada.", 
                                   a(href = "https://doi.org/10.25318/1410028701-eng",
                                     "Table 14-10-0287-03  Labour force characteristics by province, monthly, seasonally adjusted"
                                       )
                                 )
                               )))
                         ),
                         accordion_panel(
                           title = h2("Age and gender"),
                           value = "age_and_gender",
                           card(
                             card_header(uiOutput("ag_chart_title")),
                             card_body(
                               layout_sidebar(
                                 sidebar = sidebar(
                                   width = 300,
                                   position = "right",
                                   selectInput(
                                     "ag_selected_indicator",
                                     "Select indicator",
                                     choices = c("Employment", "Unemployment rate", "Participation rate"),
                                     selected = "Employment"),
                                   selectInput(
                                     "ag_selected_group",
                                     "Select filter",
                                     choices = c("Age group and gender", "Age group", "Gender"),
                                     selected = "Age group and gender"),
                                   radioButtons(
                                     "ag_selected_value",
                                     label = NULL,
                                     choices = setNames(
                                       c("current", "mom", "yoy"),
                                       c(current, mom, yoy)),
                                     selected = "current")
                                 ),
                                 withSpinner(plotlyOutput("age_gender_chart", width = "100%", height = 450))
                               ),
                               p(class = "mb-0 source",
                                 "Source:",
                                 "Statistics Canada.", 
                                 a(href = "https://doi.org/10.25318/1410028701-eng",
                                   "Table 14-10-0287-03  Labour force characteristics by province, monthly, seasonally adjusted"
                                     )
                                   
                               )
                             ))
                         ),
                         accordion_panel(
                           title = h2("Regional"),
                           value = "regional",
                           layout_column_wrap(
                             width = "450px",
                             fixed_width = FALSE,
                             card(
                               card_header(
                                 div(
                                   div(class = "mb-1",
                                       style = "font:var(--typography-bold-h5)",
                                       "Unemployment rate by region"),
                                   div(style = "font-size:var(--typography-font-size-small-body);
                                            color:var(--typography-color-secondary)",
                                       "3-month moving average, unadjusted")
                                 )
                               ),
                               div(
                                 class = "map-plot",
                                 withSpinner(plotOutput("hl_reg_map"))
                               ),
                               p(class = "mb-0 source",
                                 "Source:",
                                 "Statistics Canada.", 
                                 a(href = "https://doi.org/10.25318/1410046201-eng",
                                   "Table 14-10-0462-01  Labour force characteristics by economic region, three-month moving average, unadjusted for seasonality"
                                     )
                               )
                             ),
                             card(
                               card_header(
                                 div(
                                   div(class = "mb-1",
                                       style = "font:var(--typography-bold-h5)",
                                       "Unemployment rate by census metropolitan area"),
                                   div(style = "font-size:var(--typography-font-size-small-body);
                                            color:var(--typography-color-secondary)",
                                       "3-month moving average, unadjusted")
                                 )
                               ),
                               div(
                                 class = "map-plot",
                                 withSpinner(plotOutput("hl_cma_map"))
                               ),
                               p(class = "mb-0 source",
                                 "Source:",
                                 "Statistics Canada.", 
                                 a(href = "https://doi.org/10.25318/1410045801-eng",
                                   "Table 14-10-0458-01  Labour force characteristics by census metropolitan area, three-month moving average, unadjusted for seasonality"
                                     )
                               )
                             )
                           ) 
                         )
                       )      
                     ),
                     ### Trends tab ----
                     nav_panel(
                       "Trends",
                       card(
                         card_header(uiOutput("ts_chart_title")),
                         card_body(
                           class = "ps-0",
                           layout_sidebar(
                             class = "ps-0",
                             sidebar = sidebar(
                               width = 300,
                               position = "right",
                               selectInput(
                                 "ts_selected_indicator",
                                 "Select indicator",
                                 choices = c("Employment", "Unemployment rate", "Participation rate"),
                                 selected = "Unemployment rate")
                             ),
                             withSpinner(dygraphOutput("ts_chart"))
                           ),
                           div(class = "ps-3",
                             em("Shaded areas indicate Canadian recessions"),
                             p("To see values, hover over the chart.", class = "mb-0"),
                             p("To zoom in, use the slider below the chart or click and drag on the chart. Double-click the chart to reset."),
                             p(class = "mb-0 source",
                               "Source:",
                               "Statistics Canada.",
                               a(href = "https://doi.org/10.25318/1410028701-eng",
                                 "Table 14-10-0287-03  Labour force characteristics by province, monthly, seasonally adjusted"
                               ))
                           )
                         ))
                     ),
                     ### Data tables tab ----
                     nav_panel(
                       "Data tables",
                       layout_columns(
                         col_widths = c(3, 9),
                         #### Sidebar: Selections ----
                         div(
                           p(strong("Table Selection"), class = "mt-4 mb-3"),
                           p("Browse available labour market tables and download the results."),
                           selectInput(
                             "select_data_table",
                             label = "Select table:",
                             ## Add a default value to be initially selected
                             ## This will be updated once Data tables tab is selected
                             ## Triggering reactive event 
                             ## i.e., won't load cansim data until tab selected
                             choices = c("Select table" = "default", choices_list),
                             selected = "default",
                             width = "90%"),
                           radioButtons(
                             "select_data_type",
                             label = "Select data type:",
                             choices = c("Seasonally adjusted",
                                         "Unadjusted",
                                         "Annual")),
                           downloadButton(outputId = "download_button", label = "Download table (.csv)"),
                           div(
                             class = "mt-4",
                             style = "width: 90%",
                             p(strong("Note:"), "downloaded data will contain all data types for the selected table")
                           )
                         ),
                         #### Content ----
                         div(
                           uiOutput("table_name"),
                           withSpinner(DT::dataTableOutput("data_table")),
                           uiOutput("avg_table_name"),
                           DT::dataTableOutput("avg_table"),
                           div(
                             class = "mt-5 mb-5",
                             p(strong("Prepared by:"), "BC Stats"),
                             p(strong("Source:"),
                               'Statistics Canada, Labour Force Survey.
                          Reproduced and distributed on an "as is"
                          basis with the permission of Statistics Canada.')
                           )
                         )
                       )
                     ),
                     ### Definitions tab ----
                     nav_panel(
                       "Definitions",
                       h2("Labour Force Statistics Information", class = "mt-4 mb-3"),
                       includeMarkdown("Definitions.MD")
                     ),
                     nav_spacer(),
                     nav_item(
                      textOutput("ref_date1")
                     )
          )  ## end of navset
          
        ), ## End of column to make changes to
        
        ## footer column ----
        div(style = "padding-left:10px",
            p(textOutput("ref_date2")),
            p(textOutput("last_updated_date"))),
        bcsFooterUI("footer")
        
      ))
  }
}


## define server logic ----
server <- function(input, output, session) {  
  if(cansim_error) {
    do_nothing <- TRUE
  } else {
  
  bcsapps::bcsHeaderServer(id = 'header', links = TRUE)
  bcsapps::bcsFooterServer(id = 'footer')
  
  output$ref_date_title <- renderText(paste("British Columbia ·", formatted_date))
  output$ref_date1 <- output$ref_date2 <- renderText(paste("Reference date:", formatted_date))
  output$last_updated_date <- renderText(paste("Last updated:", last_updated_date))
  
  ## Highlights tab ----
  
  ### Key indicators table ----
  output$key_indicators_table <- renderReactable({
    
    table <- create_reactable(key_indicators)
    
  })
  
  ### LFC table ----
  output$lf_characteristics_table <- renderReactable({
    
    table <- create_reactable(
      lf_characteristics, 
      ref_date = formatted_date, 
      pos_fill = "#1F497D",
      neg_fill = "#D4D4D4",
      rev_unemp = FALSE
    )
    
  })
  
  ### LFC KPI cards ----
  
  output$pop <- renderUI({
    lf_value_box(
      data_stat = lf_characteristics,
      data_trend = cansim_data,
      indicator = "Population"
    )
  })
  
  output$lf <- renderUI({
    lf_value_box(
      data_stat = lf_characteristics,
      data_trend = cansim_data,
      indicator = "Labour force"
    )
  })

  output$emp <- renderUI({
    lf_value_box(
      data_stat = lf_characteristics,
      data_trend = cansim_data,
      indicator = "Employment"
    )
  })
  
  output$unemp <- renderUI({
    lf_value_box(
      data_stat = lf_characteristics,
      data_trend = cansim_data,
      indicator = "Unemployment"
    )
  })
  
  output$unemprate <- renderUI({
    lf_value_box(
      data_stat = lf_characteristics,
      data_trend = cansim_data,
      indicator = "Unemployment rate",
      rate = TRUE
    )
  })
  
  output$partrate <- renderUI({
    lf_value_box(
      data_stat = lf_characteristics,
      data_trend = cansim_data,
      indicator = "Participation rate",
      rate = TRUE
    )
  })
  
  output$emprate <- renderUI({
    lf_value_box(
      data_stat = lf_characteristics,
      data_trend = cansim_data,
      indicator = "Employment rate",
      rate = TRUE
    )
  })
  
  ### Flowchart ----
  flow_chart_data <- function(data) {
    data1 <<- data %>%
      filter(!str_detect(label, "rate")) %>%
      filter(!str_detect(label, "time")) %>% ## remove full-time/part-time from table
      select(label, current) %>%
      mutate(current = prettyNum(1000 * current, big.mark = ","))
    
    data2 <<- data %>%
      filter(str_detect(label, "rate") & label != "Employment rate") %>%
      mutate(current = paste0(current, "%")) %>%
      select(label, current)
  }
  
  output$flow_small <- renderGrViz({
    flow_chart_data(lf_characteristics)
    DiagrammeR::grViz("www/diagrammerFlow.gv")
   })
  
  output$flow_large <- renderGrViz({
    flow_chart_data(lf_characteristics)
    DiagrammeR::grViz("www/diagrammerFlow_v2.gv")
  })
  
  ## Monthly comparisons tab ----
  output$bar1 <- renderPlotly({
    p <- bar_chart_1()
    p
  })
  
  output$bar2 <- renderPlotly({
    p <- bar_chart_2()
    p
  })
  
  ### Age and Gender chart ----
  output$ag_chart_title <- renderUI({
    
    req(input$ag_selected_indicator)
    req(input$ag_selected_group)
    req(input$ag_selected_value)
    
    units <- case_when(
      input$ag_selected_indicator == "Employment" ~ "('000)",
      input$ag_selected_value == "current" ~ "(%)",
      TRUE ~ "(ppt)"
    )
    
    subtitle <- case_when(
      input$ag_selected_value == "current" ~ current,
      input$ag_selected_value == "mom" ~ mom,
      input$ag_selected_value == "yoy" ~ yoy
    )
    
    div(
      div(class = "mb-1",
          style = "font:var(--typography-bold-h5)",
          paste(input$ag_selected_indicator, "for", str_to_lower(input$ag_selected_group), units)),
      div(style = "font-size:var(--typography-font-size-small-body);
                                            color:var(--typography-color-secondary)",
          paste0(subtitle, ", seasonally adjusted"))
    )
    
  })
  

  ag_reactive <- reactive({
    
    req(input$ag_selected_indicator)
    req(input$ag_selected_group)
    req(input$ag_selected_value)
    
    age_groups <- case_when(
      input$ag_selected_group == "Gender" ~ "All Ages",
      TRUE ~ c("15 to 24 years", "25 to 54 years", "55 years and over")
    )
    
    genders <- case_when(
      input$ag_selected_group == "Age group" ~ "Total",
      TRUE ~ c("Men+", "Women+")
    )
    
    args <- list(
      indicator = input$ag_selected_indicator,
      age_groups = age_groups,
      genders = genders,
      comparator_type = input$ag_selected_value
    )
    
  }) %>% bindCache(input$ag_selected_indicator, input$ag_selected_group, input$ag_selected_value)
  
  output$age_gender_chart <- renderPlotly({
    args <- ag_reactive()
    p <- do.call(bar_chart_3, args)
    p
  })
  

  ### Regions and CMAs ----
  
  output$hl_reg_map <- renderPlot({
    
    vectors_filt <- vectors %>% 
      filter(table == "region",
             labour_force_characteristics == "Unemployment rate",
             data_type == "Unadjusted",
             geo != "British Columbia") 
    
    data <- get_cansim_vector(vectors = vectors_filt %>%
                                pull(vector),
                              start_time = prev_year) %>%
      clean_names() %>%
      select(vector, ref_date, value) %>%
      mutate(ref_date = ymd(ref_date)) %>%
      filter(ref_date %in% c(curr_date)) %>%
      left_join(vectors_filt, by = "vector") %>%
      mutate()
    
    geo_data <- economic_regions %>%
      left_join(data, by = "geo") %>%
      mutate(geo_label = case_when(
        geo == "Lower Mainland-Southwest" ~ "Lower\nMainland\nSouthwest",
        geo == "North Coast and Nechako" ~ "North\nCoast and\nNechako",
        geo == "Thompson-Okanagan" ~ "Thompson\nOkanagan",
        geo == "Vancouver Island and Coast" ~ "Vancouver\nIsland\nand Coast",
        TRUE ~ geo
        ),
        nudge_x = case_when(
          geo == "North Coast and Nechako" ~ -4e4,
          geo == "Northeast" ~ 2e4,
          geo == "Vancouver Island and Coast" ~ -25e4,
          geo == "Lower Mainland-Southwest" ~ 14e4,
          geo == "Kootenay" ~ -1e4,
          geo == "Cariboo" ~ -8e4,
          TRUE ~ 0),
        nudge_y = case_when(
          geo == "North Coast and Nechako" ~ 20e4,
          geo == "Northeast" ~ 1e4,
          geo == "Vancouver Island and Coast" ~ -10e4,
          geo == "Lower Mainland-Southwest" ~ -20e4,
          geo == "Kootenay" ~ -8e4,
          geo == "Cariboo" ~ -7e4,
          TRUE ~ 0),
        text_color = case_when(
          geo %in% c("Vancouver Island and Coast", "Lower Mainland-Southwest") ~ "black",
          value > 0.9*max(value) ~ "white",
          TRUE ~ "black")
        )
    
    ggplot() +
      geom_sf(data = geo_data, aes(fill = value), colour = "dark grey", lwd = 0.5) +
      geom_sf_text(
        data = geo_data, 
        aes(
          label = geo_label,
          color = text_color, 
          nudge_x = nudge_x,
          nudge_y = nudge_y
          ), 
        size = 5, 
        lineheight = 0.9,
        fontface = "bold") +
      coord_sf(clip = "off") +
      labs(x = NULL, y = NULL) +
      scale_fill_viridis(
        name = "Unemployment Rate (%)", 
        direction = -1, 
        breaks = breaks_pretty(n = 5),
        guide = guide_colorbar(
          title.position = "top",
          barwidth = unit(1.5, "strwidth", "Unemployment Rate (%)") ## make width of bar match title width
        )) +
      scale_color_manual(values = c("white" = "#ddd", "black" = "black"))+
      guides(color = "none") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.box.margin = margin(t = 30),
        text = element_text(size = 16, family = "BC Sans"),
        plot.caption = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "transparent"),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(t = 0, r = 0, b = 20, l = 5, unit = "pt")
      ) 
    
    
  })  
  
  output$hl_cma_map <- renderPlot({

    # Get CMA unemployment-rate data
    vectors_filt <- vectors %>%
      filter(
        table == "cma",
        labour_force_characteristics == "Unemployment rate",
        data_type == "Unadjusted",
        geo != "British Columbia"
      )
    
    data <- get_cansim_vector(
      vectors = vectors_filt %>%
        pull(vector),
      start_time = prev_year
    ) %>%
      clean_names() %>%
      select(vector, ref_date, value) %>%
      mutate(ref_date = ymd(ref_date)) %>%
      filter(ref_date %in% c(curr_date)) %>%
      left_join(vectors_filt, by = "vector")
    
    # Join data to CMA geometries
    geo_data <- cmas %>%
      left_join(data, by = "geo") %>%
      mutate(
        label = paste0(geo, "\n", round_half_up(value, 1), "%"),
        nudge_x = case_when(
           geo == "Chilliwack" ~ 5e4,
           geo == "Nanaimo"~ -10e4,
           geo == "Kamloops" ~ -14e4,
           geo == "Kelowna" ~ 4e4,
           geo == "Vancouver" ~ -5e4,
           TRUE ~ 0
        ),
        nudge_y = case_when(
           geo == "Abbotsford-Mission" ~ 4e4,
           geo == "Chilliwack" ~ -4e4,
           geo == "Victoria" ~ -6e4,
           geo == "Nanaimo" ~ -5e4,
           geo == "Kamloops" ~ 5e4,
           geo == "Vancouver" ~ 7e4,
           TRUE ~ 0
         )
      )

    # Project geometries
    geo_data_proj <- geo_data %>% st_transform(3005) ## NAD83 / BC Albers
    bc_proj <- bc %>% st_transform(3005)

    # Create large circle around all CMAs
    bbox <- st_bbox(geo_data_proj)
    
    centre_x <- (bbox["xmin"] + bbox["xmax"]) / 2
    centre_y <- (bbox["ymin"] + bbox["ymax"]) / 1.7 ## make center slightly higher than 50%
    
    radius <- max(
      bbox["xmax"] - centre_x,
      bbox["ymax"] - centre_y
    ) + 200000  # Increase this value for a larger circle
    
    cma_circle <- st_point(
      c(centre_x, centre_y)
    ) %>%
      st_sfc(crs = 3005) %>%
      st_buffer(radius)

    # Clip BC to the circular area
    bc_clipped <- suppressWarnings(
      st_intersection(
        bc_proj,
        cma_circle
      )
    )
    
    # Plot
    p <- ggplot() +
      # Water / background
      geom_sf(
        data = st_as_sf(cma_circle),
        fill = "white",
        colour = "#BDBDBD",
        linewidth = 0.8
      ) +
      # BC land
      geom_sf(
        data = bc_clipped,
        fill = "#D9D9D9",
        colour = "#A6A6A6",
        linewidth = 0.6
      ) +
      # CMA polygons
      geom_sf(
        data = geo_data_proj,
        aes(fill = value),
        colour = "white",
        linewidth = 0.7
      ) +
      # CMA labels
      geom_sf_text(
        data = geo_data_proj,
        aes(
          label = label,
          nudge_x = nudge_x,
          nudge_y = nudge_y
        ),
        hjust = 0,
        size = 5, ## chage to 3.5 if using ggplotly
        colour = "#222222",
        lineheight = 0.8
      ) +
      # Zoom to the entire circle
      coord_sf(
        xlim = st_bbox(cma_circle)[c("xmin", "xmax")],
        ylim = st_bbox(cma_circle)[c("ymin", "ymax")],
        expand = FALSE,
        clip = "off"
      ) +
      scale_fill_viridis(
        name = "Unemployment Rate (%)", 
        direction = -1, 
        breaks = breaks_pretty(n = 5),
        guide = guide_colorbar(
          title.position = "top",
          barwidth = unit(1.5, "strwidth", "Unemployment Rate (%)") ## make width of bar match title width
        )) +
      theme_void() +
      theme(
        legend.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.box.margin = margin(t = 50),
        text = element_text(size = 16, family = "BC Sans"),
        plot.margin = margin(l = 10, t = 30, r = 10, b = 30)
      )
    
    # g <- ggplotly(p)
    # 
    # g %>%
    #   style(
    #     hoverinfo = "none",
    #     textposition = "middle right"
    #     ) %>%
    #   layout(
    #     autosize = TRUE,
    #     modebar = list(bgcolor = "white"),
    #     margin = list(l = 0, t = 10, r = 0, b = 50)
    #     ) %>%
    #   plotly_config()
    p
    
  })
  
  # output$hl_cma_map <- renderPlot({
  #   
  # vectors_filt <- vectors %>%
  #   filter(table == "cma",
  #          labour_force_characteristics == "Unemployment rate",
  #          data_type == "Unadjusted",
  #          geo != "British Columbia")
  # 
  # 
  #   data <- get_cansim_vector(vectors = vectors_filt %>%
  #                               pull(vector),
  #                             start_time = prev_year) %>%
  #     clean_names() %>%
  #     select(vector, ref_date, value) %>%
  #     mutate(ref_date = ymd(ref_date)) %>%
  #     filter(ref_date %in% c(curr_date)) %>%
  #     left_join(vectors_filt, by = "vector")
  # 
  #   geo_data <- cmas %>%
  #     left_join(data, by = "geo") %>%
  #     mutate(vjust = case_when(geo == "Victoria" ~ 1.7,
  #                              geo == "Abbotsford-Mission" ~ 1,
  #                              geo == "Chilliwack" ~ 2.3,
  #                              TRUE ~ -1.1),
  #            hjust = case_when(geo == "Abbotsford-Mission" ~ -0.25,
  #                              geo == "Chilliwack" ~ 0.3,
  #                              geo == "Kelowna" ~ 0,
  #                              geo == "Victoria" ~ 0.9,
  #                              geo == "Vancouver" ~ 0.05))
  #   
  #   ggplot() +
  #     geom_sf(data = bc, lwd = 0.05) +
  #     geom_sf(data = geo_data, aes(fill = value), colour = "dark grey", lwd = 0.4) +
  #     geom_sf_text(data = geo_data, aes(label = geo, vjust = vjust, hjust = hjust), size = 2.5, fontface = "bold") +
  #     labs(x = NULL, y = NULL)+#,
  #          #caption = "Unadjusted\n3 Month Moving Average") +
  #     scale_fill_viridis(name = "Unemployment\nRate (%)", direction = -1, breaks = breaks_pretty(n = 5)) +
  #     theme_minimal() +
  #     theme(
  #       text = element_text(size = 16, family = "BCSans"),
  #       # legend.title = element_text(size = 11),
  #       # legend.text = element_text(size = 10),
  #       plot.caption = element_text(hjust = 0.5),
  #       panel.grid.major = element_line(colour = "transparent"),
  #       axis.text = element_blank(),
  #       plot.title = element_text(hjust = 0.5, face="bold"),
  #       plot.subtitle = element_text(hjust = 0.5),
  #       plot.margin = margin(t = 0, r = 0, b = 20, l = 5, unit = "pt")
  #     )
  #   
  # })
  # 
  ## Trends tab ----
  
  ### Dygraphs ----
  output$ts_chart_title <- renderUI({
    req(input$ts_selected_indicator)
    
    units <- case_when(
      input$ts_selected_indicator == "Employment" ~ "('000)",
      TRUE ~ "(%)"
    )

    div(
      div(class = "mb-1",
          style = "font:var(--typography-bold-h5)",
          paste("B.C.", str_to_lower(input$ts_selected_indicator), units)),
      div(style = "font-size:var(--typography-font-size-small-body);
                                            color:var(--typography-color-secondary)",
          "Seasonally adjusted")
    )
  })
  
  tseries <- reactive({
    
    req(input$ts_selected_indicator)
    
    vector <- case_when(
      input$ts_selected_indicator == "Unemployment rate" ~ "v2064705",
      input$ts_selected_indicator == "Participation rate" ~ "v2064706",
      input$ts_selected_indicator == "Employment" ~ "v2064701")

    data <- get_cansim_vector(vectors = vector,
                              start_time = "1976-01-01") %>%
      select(Date = REF_DATE, Value = VALUE) %>%
      mutate(Date = ymd(Date))

  }) %>% bindCache(input$ts_selected_indicator)
  
  output$ts_chart <- renderDygraph({
    
    req(input$ts_selected_indicator)
    
    data <- tseries()
    data <- xts(data, order.by = data$Date)
    
    ## format legend value (add comma or %)
    value_formatter <- if(input$ts_selected_indicator == "Employment"){
      JS("function(y) { return Number(y).toLocaleString('en-CA'); }")
    } else {
      JS("function(y) { return y + '%'; }")
    }
    
    dygraph(data) %>%
      dyRangeSelector(fillColor = "#e6f2fd", strokeColor = "#3c8dbc") %>%
      dyLegend(show = "onmouseover") %>% 
      dyShading(from = "1980-1-1", to = "1980-6-1") %>%
      dyShading(from = "1981-6-1", to = "1982-10-1") %>%
      dyShading(from = "1990-3-1", to = "1991-4-1") %>%
      dyShading(from = "2008-10-1", to = "2009-5-1") %>%
      dyShading(from = "2020-3-1", to = "2020-5-1") %>%
      dyAxis("x", drawGrid = FALSE, axisLineColor = "#000000", axisLineWidth = 2) %>%
      dyAxis("y", axisLineColor = "#FFFFFF", gridLineWidth = 0.1, valueFormatter = value_formatter) %>%
      dyHighlight(highlightCircleSize = 5) %>%
      dyOptions(axisLabelColor = "#666666",
                axisLabelFontSize = 14,
                colors = "#3c8dbc", 
                strokeWidth = 2)
    
  }) %>% bindCache(input$ts_selected_indicator)
  
  ## Data tables tab: content ---- 
  
  ### Datatable ----
  
  ## Update selectInput to summary when Data tables tab is selected
  observe({
    req(input$tabs)
    
    if(input$tabs == "Data tables") {
      updateSelectInput(session, inputId = "select_data_table", selected = "summary")
    } else {
      return()
    }
    
  })
 
  selected_table <- reactive({
    
    req(input$select_data_table)
    
    input$select_data_table
    
  })
  
  #### Table heading ----
  output$table_name <- renderUI({
    
    if(selected_table() == "default") {
      return()
    }
    
    else if(selected_table() %in% c("region", "cma") & input$select_data_type == "Unadjusted") {
      tagList(
        h2(names(choices_list[choices_list == selected_table()]), class = "mt-4"),
        h3(input$select_data_type, "- 3 Month Moving Average", class = "mb-3")
      )
      
    } else {
      tagList(
        h2(names(choices_list[choices_list == selected_table()]), class = "mt-4"),
        h3(input$select_data_type, class = "mb-3")
      )
    }
  })
  
  #### Get table data ----
  table_reactive <- reactive({
    
    req(input$select_data_table)
    
    if(selected_table() == "default") {
      t <- NULL
    }
    
    ## Read cansim data - method varies for some tables
    else if(selected_table() == "summary"){
      t <- get_summary_table() 
    }
    
    else {
      
      if(selected_table() %in% c("prov_emp_growth", "prov_emp_jobs")) {
        t <- get_data_for_tbl_vectors("prov_emp")
      }
      
      else {
        t <- get_data_for_tbl_vectors(selected_table())
      }
      
      ## create "label" column for tables with multi headers
      if(selected_table() %in% c("age_gender", "age_gender_rate")) {t <- t %>% mutate(label = interaction(age_group, labour_force_characteristics, gender, sep = "_"))}
      
      if(selected_table() == "ftpt_gender") {t <- t %>% mutate(label = interaction(labour_force_characteristics, gender, sep = "_"))}
      
      if(selected_table() == "occupation") {t <- t %>% mutate(label = interaction(labour_force_characteristics, national_occupational_classification_noc,  sep = "_"))}
      
      if(selected_table() %in% c("region", "cma")) {t <- t %>% mutate(label = interaction(labour_force_characteristics, geo,  sep = "_"))}
      
    }
    
    t
    
  }) %>% bindCache(input$select_data_table)
  
  #### Render table ----
  output$data_table <- renderDataTable({
    
    ## Note to display if table is not available for a selected data type:
    zero_data_note <- paste0("<div align = 'left'>", input$select_data_type, " data not available. Select another data type from the left.</div>")
    
    if(selected_table() == "default") {
      DT::datatable(NULL)
    }
    
    else if(selected_table() == "summary") {
      
      print_summary_table(table_reactive()  %>%
                            filter(data_type == input$select_data_type),
                          zero_data_note)
      
    } else {
      
      data <- table_reactive() 
      
      ## Format table
      
      ### Need grouping to be symbol for function, define here and use !! in function call
      grouping <- dt_details %>% filter(table_id == selected_table()) %>% pull(grouping) %>% rlang::sym() 
      
      data <- data %>% format_data(grouping = !!grouping,
                                   include_pct_chg =  dt_details %>% filter(table_id == selected_table()) %>% pull(include_pct_chg),
                                   include_diff =  dt_details %>% filter(table_id == selected_table()) %>% pull(include_diff))
      
      ### custom changes for certain tables
      
      if(selected_table() == "prov_emp_growth") {data <- data %>% select(ref_date, data_type, ends_with("pct_chg"), date)} ## will drop the "value" columns
      
      if(selected_table() == "prov_emp_jobs") {data <- data %>% select(ref_date, data_type, ends_with("diff"), date)} ## will drop the "value" columns
      
      if(selected_table() == "ftpt_gender") {
        data <- data %>%
          mutate(`(%)_Part-Time as % of Total_Men+` = 100*as.numeric(gsub(",","",`Part-time employment_Men+_value`))/as.numeric(gsub(",","",`Total_Men+_value`)),
                 `(%)_Part-Time as % of Total_Women+` = 100*as.numeric(gsub(",","",`Part-time employment_Women+_value`))/as.numeric(gsub(",","",`Total_Women+_value`)),
                 `(%)_Part-Time as % of Total_Total` = 100*as.numeric(gsub(",","",`Part-time employment_Total_value`))/as.numeric(gsub(",","",`Total_Total_value`))) %>%
          mutate_at(vars(contains("Part-Time as % of Total")),  ~ format(round_half_up(.x, digits = 1), big.mark = ",", nsmall = 1, zero.print = "")) %>%
          select(ref_date, data_type, contains("_Men+"), contains("_Women+"), contains("_Total"), date) 
      }
      
      data <- data %>%
        filter(data_type == input$select_data_type) %>%
        arrange(desc(date)) %>%
        select(ref_date, date, everything(), -data_type) %>%
        rename(`Reference Date` = ref_date) %>%
        rename_at(vars(-`Reference Date`), str_to_upper) %>%
        rename_at(vars(contains("RATE")), ~ paste0("(%)_", str_replace(., "_VALUE", ""))) %>%
        rename_at(vars(ends_with("_VALUE")), ~ paste0("('000)_", str_replace(., "_VALUE", ""))) %>%
        rename_at(vars(ends_with("_PCT_CHG")), ~ paste0("(% CHG)_", str_replace(., "_PCT_CHG",""))) %>%
        rename_at(vars(ends_with("_DIFF")), ~ paste0("(CHG)_",str_replace(., "_DIFF","")))
      
      if(selected_table() %in% c("prov_unempr", "prov_empr")) {
        names(data) <- str_replace(names(data), "'000", "%") ## prov tables headers don't include lfs label (i.e. don't contain 'rate')
      }
      
      ## Render table
      
      DT::datatable(data, 
                    options = list(#dom = "lpt",info = FALSE, paging = TRUE, pagingType = "simple", 
                                   info = FALSE, paging = FALSE,
                                   searching = FALSE, scrollX = TRUE, scrollY = "275px",
                                   columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(targets = 0, orderData = 1),
                                                     list(targets = 1, visible = FALSE)),
                                   language = list(zeroRecords = zero_data_note)),
                    rownames = FALSE,
                    container = format_header(data),
                    class = 'cell-border stripe',
                    caption = tags$caption(style = 'caption-side: bottom; text-align: left;',
                                           HTML(table_captions %>% filter(table_id == selected_table()) %>% pull(caption)))
      )
      
    }
    
  })

  ### YTD avg table ----
  
  #### Table heading ----
  output$avg_table_name <- renderUI({
    
    no_avg_list <- dt_details %>% filter(is.na(include_avg_pct_chg)) %>% pull(table_id)
    
    if(selected_table() %in% c("default", no_avg_list) | input$select_data_type == "Annual"){ 
      
      NULL 
      
    } else {
      
      h3(HTML("Year-to-date Averages &#8212; "), input$select_data_type, class = "mt-4 mb-3")
    }
    
  })
  
  
  #### Render table ----
  output$avg_table <- renderDataTable({
    
    no_avg_list <- dt_details %>% filter(is.na(include_avg_pct_chg)) %>% pull(table_id)
    
    if(selected_table() %in% c("default", no_avg_list) | input$select_data_type == "Annual"){ 
      DT::datatable(NULL) 
    
    } else {
      ### Need grouping to be symbol for function, define here and use !! in function call
      grouping <- dt_details %>% filter(table_id == selected_table()) %>% pull(grouping) %>% rlang::sym() 
      
      data <- table_reactive() %>%
        get_ytd_avg(grouping = !!grouping,
                    include_pct_chg = (dt_details %>%
                                         filter(table_id == selected_table()) %>%
                                         pull(include_avg_pct_chg)))
      
      if(selected_table() == "ftpt_gender") {
        data <- data %>%
          mutate(`Part-Time as % of Total_Men+` = ifelse(str_detect(Average, "%"), 0, 100*as.numeric(gsub(",","",`Part-time employment_Men+`))/as.numeric(gsub(",","",`Total_Men+`))),
                 `Part-Time as % of Total_Women+` = ifelse(str_detect(Average, "%"), 0, 100*as.numeric(gsub(",","",`Part-time employment_Women+`))/as.numeric(gsub(",","",`Total_Women+`))),
                 `Part-Time as % of Total_Total` = ifelse(str_detect(Average, "%"), 0, 100*as.numeric(gsub(",","",`Part-time employment_Total`))/as.numeric(gsub(",","",`Total_Total`)))) %>%
          mutate_at(vars(contains("Part-Time as % of Total")),  ~ format(round_half_up(.x, digits = 1), big.mark = ",", nsmall = 1, zero.print = "")) %>%
          select(data_type, Average, contains("_Men+"), contains("_Women+"), contains("_Total")) 
      }
      
      data <- data %>%
        filter(data_type == input$select_data_type) %>%
        select(-data_type) %>%
        rename_all(str_to_upper)
      
      DT::datatable(data, 
                    options = list(info = FALSE, paging = FALSE, searching = FALSE, scrollX = TRUE,# scrollY = "380px",
                                   columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                                   #language = list(zeroRecords = zero_data_note)),
                    rownames = FALSE,
                    container = format_header(data),
                    class = 'cell-border stripe'#,
                    # caption = tags$caption(style = 'caption-side: bottom; text-align: left;',
                    #                        HTML(table_captions %>% filter(table_id == selected_table()) %>% pull(caption)))
      )
      
    }
    
  })
  
  ## Data tables tab: Side bar  ----
  
  ### Data type radio ----
  observe({
    
    if(selected_table() == "default") {
      return()
    }
    
    x <- table_reactive() %>% pull(data_type) %>% unique()
    
    # Can also set the label and select items
    updateRadioButtons(session, "select_data_type",
                       label = paste("Select data type:"),
                       choices = x,
                       selected = x[1]
    )
  })
  
  
  ### Download data ----
  output$download_button <- downloadHandler(
    filename = function() {
      paste0(selected_table(), "_lfs_results.csv")
    },
    content = function(file_lfs) {
      if(selected_table() == "default") table <- data.frame(NULL)
      else table <- table_reactive()
      
      write.csv(table, file_lfs, row.names = FALSE, na = "")
    }
  )
  
 
}}


## knit together ui and server ----
shiny::shinyApp(ui = ui, server = server)
