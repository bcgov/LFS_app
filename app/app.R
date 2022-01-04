## LFS app ----

## Start of app ----
# UI demonstrating column layouts
ui <- function(req) {
  fluidPage(shinyWidgets::useShinydashboard(),
            title = "LFS App",
            theme = "bootstrap.css",
            HTML("<html lang='en'>"),
            
            ## Custom formatting ----
            ## formatting for icons in valueBoxes
            tags$head(tags$style(HTML('.small-box .icon-large {top: -10px;}'))),
            
            ## formatting for tabBox content
            tags$head(tags$style(HTML('.nav-tabs-custom>.tab-content {border: 1px solid #3c8dbc}'))),
            tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs>li.active
                                      {border-top-color: #3c8dbc; border-left: 1px solid #3c8dbc;'))),
            tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs>li.active>a {border-right: 1px solid #3c8dbc}'))),
            
            
            fluidRow(
              ## Header column ----
              column(
              width = 12,
              style = "background-color:#003366; border-bottom:2px solid #fcba19; position:fixed; z-index:10000",
                 tags$header(class="header", style="padding:0 0px 0 0px; display:flex; height:80px; width:100%;",
                   tags$div(class="banner", style="width:100%; display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
                     a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                       img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
                       onclick="gtag"),
                     h1("British Columbia - Labour Market Statistics", style="font-weight:400; color:white; margin: 5px 5px 0 18px;"),
                     ## MODULE CODE
                     tags$div(style = "margin-left:auto; margin-right:0;",linkModUI('links'))
                   )
                 )
              ),  ## end of column
    ## Main body column ----
    ## Make changes to this column
    column(width = 12,
           style = "margin-top:100px",
           
           ## Tabset start ----  
           tabsetPanel(id = "tabs",
                       ### Highlights tab ----
                tabPanel("Highlights",
                         #### Sidebar: About column ----
                         column(width = 3, 
                                tags$fieldset(style = "width: 90%",
                                  br(),
                                  tags$legend(h2(formatted_date)),
                                  "Statistics Canada's monthly Labour Force Survey (LFS) captures data
                                  about the labour market and provides estimates of 
                                  employment and unemployment which are the most 
                                  timely and important measures of performance of the Canadian economy.",
                                  br(), br(),
                                  "Navigate the tabs to find statistics that reflect the 
                                  labour market characteristics of the population of B.C.",
                                  br(),br(),
                                  "To zoom in on dates for the Employment, Unemployment Rate, and Participation Rate
                                  charts under the TRENDS box, move the slider or select part of the chart with your mouse.",
                                  br(), br(),
                                  "Learn more ", 
                                  tags$a("about the Labour Force Survey", 
                                         href = "https://www2.gov.bc.ca/gov/content/data/statistics/employment-labour/about-labour-force-survey"),
                                  br(),br()
                                  )),
                         #### Content ----
                         column(width = 9, 
                                br(),
                                fluidRow(column(width = 6,
                                       fluidRow(valueBoxOutput(width = NULL, "emp")),
                                       fluidRow(valueBoxOutput(width = NULL, "unemprate")),
                                       fluidRow(valueBoxOutput(width = NULL, "partrate"))),
                                column(width = 6,
                                       grVizOutput("flow", height = 300))),
                                fluidRow(br(),br(),
                                         shinydashboard::box(
                                  id = "trendsbox",
                                  title = "TRENDS",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  collapsible = TRUE,
                                  collapsed = TRUE,
                                  tabBox(id = "hl_ts",
                                       width = NULL,
                                       selected = "Employment",
                                       side = "left",
                                       tabPanel("Employment",
                                                dygraphOutput("hl_emp_cht")),
                                       tabPanel("Unemployment Rate",
                                                dygraphOutput("hl_unemp_cht")),
                                       tabPanel("Participation Rate",
                                                dygraphOutput("hl_part_cht"))),
                                       tags$fieldset(tags$em("Shaded areas indicate Canadian recessions")),
                                        br(), br()),
                                  shinydashboard::box(
                                    id = "agebox",
                                    title = "AGE AND GENDER",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    tabBox(id = "hl_ag",
                                           width = NULL,
                                           selected = "Employment",
                                           side = "left",
                                           tabPanel("Employment",
                                                    radioButtons("emp_m_or_y", 
                                                                 label = NULL,
                                                                 choices = c("Change from previous month" = "mom",
                                                                             "Change from same month, previous year" = "yoy"),
                                                                 selected = "mom", 
                                                                 inline = TRUE),
                                                    br(),
                                                    plotOutput("hl_emp_ag_m_or_y")
                                                    ),
                                           tabPanel("Unemployment Rate",
                                                    radioButtons("unemp_m_or_y", 
                                                                 label = NULL,
                                                                 choices = c("Change from previous month" = "mom",
                                                                             "Change from same month, previous year" = "yoy"),
                                                                 selected = "mom", 
                                                                 inline = TRUE),
                                                    br(),
                                                    plotOutput("hl_unemp_ag_m_or_y")
                                                    ),
                                           tabPanel("Participation Rate",
                                                    radioButtons("part_m_or_y", 
                                                                 label = NULL,
                                                                 choices = c("Change from previous month" = "mom",
                                                                             "Change from same month, previous year" = "yoy"),
                                                                 selected = "mom", 
                                                                 inline = TRUE),
                                                    br(),
                                                    plotOutput("hl_part_ag_m_or_y")
                                                    ))
                                  ),
                                  shinydashboard::box(
                                    id = "regionbox",
                                    title = "REGIONS",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    column(width = 6,
                                           plotOutput("hl_reg_map")),
                                    column(width = 6,
                                           plotOutput("hl_cma_map"))
                                   
                                  )))),
                ### Data tables tab ----
                tabPanel("Data tables",
                         #### Sidebar: Selections ----
                         column(width = 3,
                                tags$fieldset(
                                  br(),
                                  tags$legend(h2(formatted_date)),
                                  selectInput("select_data_table",
                                              label = "Select table:",
                                              ## Add a default value to be initially selected
                                              ## This will be updated once Data tables tab is selected
                                              ## Triggering reactive event 
                                              ## i.e., won't load cansim data until tab selected
                                              choices = c("Select table" = "default", choices_list),
                                              selected = "default",
                                              width = "90%"),
                                  radioButtons("select_data_type",
                                               label = "Select data type:",
                                               choices = c("Seasonally adjusted",
                                                           "Unadjusted",
                                                           "Annual")),
                                  downloadButton(outputId = "download_button", label = "Download table (.csv)"),
                                  br(),br(),
                                  tags$div("Note: downloaded data will contain all data types for the selected table",
                                           style = "width: 90%")
                                  )),
                         #### Content ----
                         column(width = 9,
                                uiOutput("table_name"),
                                DT::dataTableOutput("data_table"),
                                uiOutput("avg_table_name"),
                                DT::dataTableOutput("avg_table"),
                                br(),br(),
                                tags$fieldset(tags$b("Prepared by: BC Stats"),
                                              br(), 
                                              tags$b("Source:"),
                                              'Statistics Canada, Labour Force Survey. 
                                       Reproduced and distributed on an "as is" 
                                       basis with the permission of Statistics Canada.',
                                              br(), br()))
                         
                         ),
                ### Definitions tab ----
                tabPanel("Definitions",
                         column(width = 12,
                                style = "margin-top:25px",
                                tags$fieldset(
                                  tags$legend(h2("Labour Force Statistics Information")),
                                  includeMarkdown("Definitions.MD")
                                )
                         )
                ), 
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
  
  ## MODULE CODE
  linkModServer('links')
  
  ## Tab 0: Highlights ----
  
  ### Valueboxes ----
  output$unemprate <- renderValueBox({
    
    data <- hl_stats %>%
      filter(label == "Unemployment Rate")
    
    sign <- case_when(data$change > 0 ~ paste("Up", abs(data$change), "percentage points from last month", sep = " "),
                      data$change == 0 ~ "No change from last month",
                      data$change < 0 ~ paste("Down", abs(data$change), "percentage points from last month", sep = " "))
    
    icon <- case_when(data$change > 0 ~ "arrow-alt-circle-up",
                      data$change == 0 ~ "arrow-alt-circle-right",
                      data$change < 0 ~ "arrow-alt-circle-down")
    
    valueBox(
      value = tags$p(paste0(data$label, ": ", data$current, "%"), style = "font-size: 50%;"),
      subtitle = sign,
      icon = icon(icon),
      color = "light-blue"
    )
  })
  
  output$partrate <- renderValueBox({
    
    data <- hl_stats %>%
      filter(label == "Participation Rate")
    
    sign <- case_when(data$change > 0 ~ paste("Up", abs(data$change), "percentage points from last month", sep = " "),
                      data$change == 0 ~ "No change from last month",
                      data$change < 0 ~ paste("Down", abs(data$change), "percentage points from last month", sep = " "))
    
    icon <- case_when(data$change > 0 ~ "arrow-alt-circle-up",
                      data$change == 0 ~ "arrow-alt-circle-right",
                      data$change < 0 ~ "arrow-alt-circle-down")
    
    valueBox(
      value = tags$p(paste0(data$label, ": ", data$current, "%"), style = "font-size: 50%;"),
      subtitle = sign,
      icon = icon(icon),
      color = "light-blue"
    )
  })
  
  output$emp <- renderValueBox({
    
    data <- hl_stats %>%
      filter(label == "Employment")
    
    sign <- case_when(data$change > 0 ~ paste("Up", prettyNum(abs(data$change), big.mark = ","), "from last month", sep = " "),
                      data$change == 0 ~ "No change from last month",
                      data$change < 0 ~ paste("Down", prettyNum(abs(data$change), big.mark = ","), "from last month", sep = " "))
    
    icon <- case_when(data$change > 0 ~ "arrow-alt-circle-up",
                      data$change == 0 ~ "arrow-alt-circle-right",
                      data$change < 0 ~ "arrow-alt-circle-down")
    
    valueBox(
      value = tags$p(paste0(data$label, ": ", prettyNum(data$current, big.mark = ",")), style = "font-size: 50%;"),
      subtitle = sign,
      icon = icon(icon),
      color = "light-blue"
    )
  })
  
  ### Flowchart ----
  output$flow <- renderGrViz({
    
    data1 <- hl_stats %>%
      filter(!str_detect(label, "Rate")) %>%
      select(label, current)
    
    data1 <<- data1 %>%
      rbind(data.frame(label = "Not in \n Labour Force", 
                       current = data1 %>% filter(label == "Population") %>% pull(current) - 
                         data1 %>% filter(label == "Labour Force") %>% pull(current))) %>%
      mutate(current = prettyNum(current, big.mark = ","))
    
    data2 <<- hl_stats %>%
      filter(str_detect(label, "Rate") & label != "Employment Rate") %>%
      mutate(current = paste0(current, "%")) %>%
      select(label, current)
    
    
    DiagrammeR::grViz("www/diagrammerFlow.gv")

   })
  
  ### Dygraphs ----
  tseries <- reactive({
    
    vector <- case_when(
      input$hl_ts == "Unemployment Rate" ~ "v2064705",
      input$hl_ts == "Participation Rate" ~ "v2064706",
      input$hl_ts == "Employment" ~ "v2064701")

    data <- get_cansim_vector(vectors = vector,
                              start_time = "1976-01-01") %>%
      select(REF_DATE, VALUE) %>%
      mutate(REF_DATE = ymd(REF_DATE))

  }) %>% bindCache(input$hl_ts)
  
  output$hl_unemp_cht <- 
    output$hl_part_cht <-
    output$hl_emp_cht <-
    renderDygraph({
      
        data <- tseries()
        data <- xts(data, order.by = data$REF_DATE)
        label <- ifelse(str_detect(input$hl_ts, "Rate"),
                        paste("B.C.", input$hl_ts, "(%)"),
                        paste("B.C.", input$hl_ts, "('000)"))

        dygraph(data, main = label) %>%
          dyRangeSelector() %>%
          dyShading(from = "1980-1-1", to = "1980-6-1") %>%
          dyShading(from = "1981-6-1", to = "1982-10-1") %>%
          dyShading(from = "1990-3-1", to = "1991-4-1") %>%
          dyShading(from = "2008-10-1", to = "2009-5-1") %>%
          dyAxis("y") %>%
          dyOptions(colors = RColorBrewer::brewer.pal(8, "Set2"), drawGrid = FALSE)
          
  }) %>% bindCache(input$hl_ts)
  
  ### Age and Gender chart ----
  ag_reactive <- reactive({
    
    vectors_filt <- vectors %>% 
      filter(str_detect(table, "age_gender") & 
               labour_force_characteristics == str_to_sentence(input$hl_ag) &
               data_type == "Seasonally adjusted") 
    
    
    data <- get_cansim_vector(vectors = vectors_filt %>%
                                pull(vector),
                              start_time = prev_year) %>%
      clean_names() %>%
      select(vector, ref_date, value) %>%
      mutate(ref_date = ymd(ref_date)) %>%
      filter(ref_date %in% c(curr_date, prev_month, prev_year)) %>%
      left_join(vectors_filt, by = "vector") %>%
      mutate(date = case_when(ref_date == curr_date ~ "Current",
                              ref_date == prev_month ~ "Previous month",
                              ref_date == prev_year ~ "Same month, previous year")) 
    
  }) %>% bindCache(input$hl_ag)
  
  output$hl_emp_ag_m_or_y <-
    output$hl_unemp_ag_m_or_y <-
    output$hl_part_ag_m_or_y <- renderPlot({
      
      m_or_y <- case_when(input$hl_ag == "Employment" ~ input$emp_m_or_y,
                          input$hl_ag == "Unemployment Rate" ~ input$unemp_m_or_y, 
                          input$hl_ag == "Participation Rate" ~ input$part_m_or_y)
      
      data <- ag_reactive() %>%
        select(-ref_date) %>%
        pivot_wider(names_from = "date", values_from = "value") %>%
        mutate(mom = Current - `Previous month`,
               yoy = Current - `Same month, previous year`) %>%
        pivot_longer(cols = c(mom, yoy), names_to = "comparison", values_to = "value") %>%
        filter(comparison == m_or_y) %>%
        mutate(vjust = ifelse(value > 0, 1.5, -1.5))
      
      data$sex <- factor(data$sex, levels = c("Total", "Males", "Females"))
      data <- data %>% arrange(desc(sex))
      
      label <- case_when(str_detect(input$hl_ag, "Rate") & m_or_y == "mom" ~ "Change from previous month (ppt)",
                         str_detect(input$hl_ag, "Rate") & m_or_y == "yoy" ~ "Change from same month, previous year (ppt)",
                         !str_detect(input$hl_ag, "Rate") & m_or_y == "mom" ~ "Change from previous month ('000)",
                         !str_detect(input$hl_ag, "Rate") & m_or_y == "yoy" ~ "Change from same month, previous year ('000)")
      
      colors <- RColorBrewer::brewer.pal(5, name = "Blues")[2:4]
      names(colors) <- data %>% pull(sex) %>% unique()
      
      p <- ggplot(data, aes(x = age_group, y = value, fill = sex)) +
        geom_col(position = position_dodge(width = 0.5), width = 0.5) +
        geom_hline(yintercept = 0) + 
        labs(x = "", y = "", fill = "", 
             title = paste("B.C.", input$hl_ag, "by Age and Gender"), 
             subtitle = label) +
        guides(fill = guide_legend(reverse = TRUE)) +
        geom_text(aes(label = format(round_half_up(value, digits = 1),  big.mark = ",", nsmall = 1), vjust = vjust),
                  position = position_dodge(width = 0.5),
                  size = 5) +
        scale_fill_manual(values = colors) +
        bcstats_chart_theme +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) 
      
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
      left_join(vectors_filt, by = "vector") 
    
    geo_data <- economic_regions %>%
      left_join(data, by = "geo") %>%
      mutate(geo_label = str_wrap(str_extract(geo, "^([^,])+"), width = 10))
    
    ggplot() +
      geom_sf(data = geo_data, aes(fill = value ), colour = "white", lwd = .2) +
      geom_sf_text(data = geo_data, aes(label = geo_label), size = 4) +
      labs(x = NULL, y = NULL,
           caption = "Unadjusted - 3 Month Moving Average",
           title = "Unemployment Rate (%)",
           subtitle = "by Region") +
      scale_fill_viridis(name = "Unemployment\nRate (%)", direction = -1, breaks = breaks_pretty(n = 5)) +
      theme_minimal() +
      theme(
        text = element_text(size = 16, family = "BCSans"),
        # legend.title = element_text(size = 11),
        # legend.text = element_text(size = 10),
        plot.caption = element_text(hjust = 0.7),
        panel.grid.major = element_line(colour = "transparent"),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    
  })  
  
  output$hl_cma_map <- renderPlot({
    
    vectors_filt <- vectors %>% 
      filter(table == "cma",
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
      left_join(vectors_filt, by = "vector") 
    
    geo_data <- cmas %>%
      left_join(data, by = "geo") %>%
      mutate(vjust = ifelse(geo %in% c("Victoria","Abbotsford-Mission"), 1.7, -1.1),
             hjust = case_when(geo == "Abbotsford-Mission" ~ -0.1,
                               geo == "Kelowna" ~ 0,
                               geo == "Victoria" ~ 0.9,
                               geo == "Vancouver" ~ 0.05))
    
    ggplot() +
      geom_sf(data = bc_bound()) +
      geom_sf(data = geo_data, aes(fill = value), colour = "white", lwd = .2) +
      geom_sf_text(data = geo_data, aes(label = geo, vjust = vjust, hjust = hjust), size = 4) +
      labs(x = NULL, y = NULL,
           caption = "Unadjusted - 3 Month Moving Average",
           title = "Unemployment Rate (%)",
           subtitle = "by Census Metropolitan Area") +
      scale_fill_viridis(name = "Unemployment\nRate (%)", direction = -1, breaks = breaks_pretty(n = 5)) +
      theme_minimal() +
      theme(
        text = element_text(size = 16, family = "BCSans"),
        # legend.title = element_text(size = 11),
        # legend.text = element_text(size = 10),
        plot.caption = element_text(hjust = 0.7),
        panel.grid.major = element_line(colour = "transparent"),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
  })
  
  ## Tab 1: Data tables ---- 
  
  ### Datatable ----
  
  ## Update selectInput to summary when Data tables tab is selected
  observe({
    
    if(input$tabs == "Data tables") {
      updateSelectInput(session, inputId = "select_data_table", selected = "summary")
    } else {
      return()
    }
    
  })
 
  selected_table <- reactive({
    
    input$select_data_table
    
  })
  
  #### Table heading ----
  output$table_name <- renderUI({
    
    if(selected_table() == "default") {
      return()
    }
    
    else if(selected_table() %in% c("region", "cma") & input$select_data_type == "Unadjusted") {
      tags$legend(h2(names(choices_list[choices_list == selected_table()])),
                  h3(input$select_data_type, "- 3 Month Moving Average"))
      
    }else {
    
    tags$legend(h2(names(choices_list[choices_list == selected_table()])),
                h3(input$select_data_type))
    }
  })
  
  #### Get table data ----
  table_reactive <- reactive({
    
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
      if(selected_table() %in% c("age_gender", "age_gender_rate")) {t <- t %>% mutate(label = interaction(age_group, labour_force_characteristics, sex, sep = "_"))}
      
      if(selected_table() == "ftpt_gender") {t <- t %>% mutate(label = interaction(labour_force_characteristics, sex, sep = "_"))}
      
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
          mutate(`(%)_Part-Time as % of Total_Males` = 100*as.numeric(gsub(",","",`Part-time employment_Males_value`))/as.numeric(gsub(",","",`Total_Males_value`)),
                 `(%)_Part-Time as % of Total_Females` = 100*as.numeric(gsub(",","",`Part-time employment_Females_value`))/as.numeric(gsub(",","",`Total_Females_value`)),
                 `(%)_Part-Time as % of Total_Total` = 100*as.numeric(gsub(",","",`Part-time employment_Total_value`))/as.numeric(gsub(",","",`Total_Total_value`))) %>%
          mutate_at(vars(contains("Part-Time as % of Total")),  ~ format(round_half_up(.x, digits = 1), big.mark = ",", nsmall = 1, zero.print = "")) %>%
          select(ref_date, data_type, contains("_Males"), contains("_Females"), contains("_Total"), date) 
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
      
      tags$legend(br(),h3(HTML("Year-to-date Averages &#8212; "),input$select_data_type))
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
          mutate(`Part-Time as % of Total_Males` = ifelse(str_detect(Average, "%"), 0, 100*as.numeric(gsub(",","",`Part-time employment_Males`))/as.numeric(gsub(",","",`Total_Males`))),
                 `Part-Time as % of Total_Females` = ifelse(str_detect(Average, "%"), 0, 100*as.numeric(gsub(",","",`Part-time employment_Females`))/as.numeric(gsub(",","",`Total_Females`))),
                 `Part-Time as % of Total_Total` = ifelse(str_detect(Average, "%"), 0, 100*as.numeric(gsub(",","",`Part-time employment_Total`))/as.numeric(gsub(",","",`Total_Total`)))) %>%
          mutate_at(vars(contains("Part-Time as % of Total")),  ~ format(round_half_up(.x, digits = 1), big.mark = ",", nsmall = 1, zero.print = "")) %>%
          select(data_type, Average, contains("_Males"), contains("_Females"), contains("_Total")) 
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
  
  ## Tab 1: Side bar  ----
  
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
  
 
}


## knit together ui and server ----
shiny::shinyApp(ui = ui, server = server)
