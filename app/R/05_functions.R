
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

## Functions for valueboxes ----
## code from: https://rstudio.github.io/bslib/reference/value_box.html#bottom-showcase
sparkline_plot_prep <- function(data, indicator) {
  
  df <- data %>%
    filter(label == indicator)
  
  timeseries <- df$value
  x_axis <- seq_along(timeseries)
  x_lim <- c(1, length(timeseries))
  y_offset <- case_when(
    max(timeseries) < 100 ~ 0.1,
    between(max(timeseries), 100, 1000) ~ 1,
    TRUE ~ 3)
  y_lim <- range(timeseries) + c(-1*y_offset, y_offset)
  pts <- c(1, 12, 13) # previous year, previous month, current
  
  par(mar = c(0, 0, 0, 0))
  
  # Set up the plot area
  plot(
    timeseries, type = "n",
    axes = FALSE, frame.plot = FALSE,
    ylim = y_lim, xlim = x_lim,
    ylab = "",    xlab = "",
    yaxs = "i",   xaxs = "i",
  )
  
  # Add the sparkline line
  lines(timeseries, type = "l", pch = NA, col = "#3c8dbc", lwd = 3)
  
  # Create polygon coordinates for shading
  polygon_x <- c(1, x_axis, length(timeseries))
  polygon_y <- c(min(y_lim), timeseries, min(y_lim))
  
  # Add shading under the line
  polygon(polygon_x, polygon_y, col = "#e6f2fd", border = NA)
  
  # Add highlighted points
  # points(
  #   x_axis[pts],
  #   timeseries[pts],
  #   pch = 21,
  #   bg = "white",
  #   col = "#3c8dbc",
  #   lwd = 1.5,
  #   cex = 2
  # )
  
}

sparkline_plot <- function(data, indicator) {
  as_fill_item(
    htmltools::plotTag(
      sparkline_plot_prep(data, indicator),
      width = 500,
      height = 125,
      suppressSize = "xy",
      alt = paste(
        "Sparkline showing monthly trend for employment.",
        "Current month, previous month, and year-ago values are highlighted."
      )
    )
  )
}

lf_value_box <- function(data_stat, data_trend, indicator, rate = FALSE) {
  
  data_stat <- data_stat %>% filter(label == indicator)
  data_trend <- data_trend %>% filter(label == indicator)
  accuracy <- ifelse(
    indicator %in% c("Population", "Labour force"),
    0.01,
    0.1
  )
  
  card(
    height = "200px",
    card_header(data_stat$label, class = "lfs-card-header"),
    card_body(
      class = "pb-1",
      gap = 0,
      h4(paste0(prettyNum(data_stat$current, big.mark = ","), ifelse(rate, "%", " thousand"))),
      # span(icon(data_stat$mom_arrow, style = paste0("color:", data_stat$mom_color)), 
      #      prettyNum(data_stat$mom_change, big.mark = ","), 
      #      ifelse(rate, "ppt", ""),
      #      if(!rate) { paste0(" | ", percent(data_stat$mom_pct_change, accuracy = accuracy, f = janitor::round_half_up)) },
      #      "from last month"),
      # span(icon(data_stat$yoy_arrow, style = paste0("color:", data_stat$yoy_color)), 
      #      prettyNum(data_stat$yoy_change, big.mark = ","),  
      #      ifelse(rate, "ppt", ""),
      #      if(!rate) { paste0(" | ", percent(data_stat$yoy_pct_change, accuracy = accuracy, f = janitor::round_half_up)) },
      #      "from last year"),
      sparkline_plot(data_trend, indicator),
      p(class = "mb-0",
        style = "font-size:var(--typography-font-size-label)",
        "Seasonally adjusted one year trend")
    )
  )
}
#percent(value, accuracy = 0.1, f = janitor::round_half_up)


## Functions for data tables ----
## code for renaming, may need later:
# rename_at(vars(ends_with("_pct_chg")), ~ str_remove_all(., "_pct_chg"))

## Functions for tables with Annual, Seasonally adjusted and Unadjusted data
## all data must contain the columns: ref_date, data_type, value
## the following date values must be defined: 
##     curr_date, 
##     prev_year, 
##     prev_year_jan, 
##     curr_year_jan, 
##     monthly_start_month, 
##     annual_start_year, 
##     annual_display_year

get_data_for_tbl_vectors <- function(tbl){
  
  tmp = get_cansim_vector(vectors = vectors %>% filter(table == tbl, data_type != "Annual") %>% pull(vector), start_time = monthly_start_month) %>%
    bind_rows(get_cansim_vector(vectors = vectors %>% filter(table == tbl, data_type == "Annual") %>% pull(vector), start_time = annual_start_year)) %>%
    clean_names() %>%
    select(everything(), -any_of(names(vectors)), vector) %>%
    left_join(vectors %>%  filter(table == tbl), by = c("vector")) 
  
}


get_ytd_avg <- function(data, grouping, include_pct_chg = FALSE){
  
  tmp <- data %>%
    filter(data_type != "Annual") %>%
    mutate(ytd = case_when(between(date, prev_year_jan, prev_year) ~ "prev",
                           between(date, curr_year_jan, curr_date) ~ "curr",
                           TRUE ~ NA_character_)) %>%
    filter(!is.na(ytd)) %>%
    group_by({{grouping}}, data_type, ytd) %>%
    summarize(avg = mean(value), .groups = "drop") %>%
    pivot_wider(names_from = ytd, values_from = avg)
  
  if(include_pct_chg) {
    
    tmp <- tmp %>%
      mutate(pct_chg = round_half_up(((curr / prev) - 1) * 100, digits = 1)) 
  } 
  
  tmp <- tmp %>%
    pivot_longer(cols = any_of(c("prev", "curr", "pct_chg")), names_to = "Average", values_to = "Estimate") %>% ## Note order of cols = row order after pivot
    mutate(Estimate = format(round_half_up(Estimate, digits = 1), big.mark = ",", nsmall = 1),
           Average = case_when(Average == "prev" ~ paste("Jan -", month(curr_date, label = TRUE, abbr = TRUE), year(prev_year)),
                               Average == "curr" ~ paste("Jan -", month(curr_date, label = TRUE, abbr = TRUE), year(curr_date)),
                               Average == "pct_chg" ~ "% Change")) %>%
    pivot_wider(names_from = {{grouping}}, values_from = Estimate) %>%
    ## remove the % change from rate columns
    mutate_at(vars(contains("rate")), ~ ifelse(Average == "% Change", "", .)) %>%
    ungroup()
  
}

format_data <- function(data, grouping, include_pct_chg = FALSE, include_diff = FALSE, selection = c(ref_date, data_type, everything())) {
  
  grouping_txt <- enquo(grouping)

  tmp <- data %>%
    select(ref_date, {{grouping}}, data_type, value) %>%
    group_by({{grouping}}, data_type) %>%
    arrange(data_type, {{grouping}}, ref_date) 
  
  
  if(include_pct_chg) {
    
    tmp <- tmp %>%
      mutate(pct_chg = case_when(str_detect({{grouping}}, "rate") ~ NA_character_,
                                 data_type == "Unadjusted" ~  format(round_half_up(((value / dplyr::lag(value, n = 12)) - 1) * 100, digits = 1), big.mark = ",", nsmall = 1),
                                 data_type == "Seasonally adjusted" ~  format(round_half_up(((value / dplyr::lag(value, n = 1)) - 1) * 100, digits = 1), big.mark = ",", nsmall = 1),
                                 data_type == "Annual" ~ format(round_half_up(((value / dplyr::lag(value, n = 1)) - 1) * 100, digits = 1), big.mark = ",", nsmall = 1)))
  }
  
  if(include_diff) {
    
    tmp <- tmp %>%
      mutate(diff = case_when(str_detect({{grouping}}, "rate") ~ NA_character_,
                             data_type == "Unadjusted" ~  format(round_half_up(value - dplyr::lag(value, n = 12), digits = 1), big.mark = ",", nsmall = 1),
                             data_type == "Seasonally adjusted" ~  format(round_half_up(value - dplyr::lag(value, n = 1), digits = 1), big.mark = ",", nsmall = 1),
                             data_type == "Annual" ~ format(round_half_up(value - dplyr::lag(value, n = 1), digits = 1), big.mark = ",", nsmall = 1)))
  }
  
  tmp <- tmp %>%
    mutate(value = format(value, big.mark = ",", nsmall = 1))
    # mutate(value = case_when(str_detect({{grouping}}, "rate") ~ as.character(value), 
    #                          TRUE ~ format(value, big.mark = ",", nsmall = 1)))
  
  pivot_spec <- tmp %>% 
    build_wider_spec(names_from = {{grouping}}, 
                     values_from = any_of(c("value", "pct_chg", "diff")), 
                     names_glue = paste0("{", rlang::quo_text(grouping_txt),"}_{.value}")) %>%
    arrange({{grouping}}, desc(.value)) ## descending .value so order will be value, pct_chg, diff
  
    tmp <- tmp %>%
      pivot_wider_spec(pivot_spec) %>% 
      filter((data_type != "Annual" & ref_date >= prev_year_jan) | (data_type == "Annual" & ref_date >= annual_display_year)) %>%
      rename(date = ref_date) %>%
      mutate(ref_date = ifelse(data_type == "Annual", paste(year(date)), paste(year(date), month(date, label = TRUE, abbr = TRUE)))) %>%
      select(where(~!all(is.na(.x)))) %>%
      select({{selection}}, date) %>%
      ungroup()
    
  
}

format_header <- function(data) {
  
  ## first column is always Reference Date, # _'s is consistent in remaining columns
  ## count # of _'s in 2nd column
  ## rowspan = this +1 (one for each label +one for ('000))
  n_labs <- str_count(names(data) %>% tail(1), "_") +1
  
  headers <- str_split_fixed(names(data)[-1], "_", n_labs) %>% 
    as.data.frame() %>%
    mutate_all(fct_inorder) ## convert columns to factor with level order = order of appearance
  
  if(n_labs == 1) {
    V1 <- headers %>%
      summarize(colspan_V1 = n(), .by = V1) %>%
      mutate(html = paste0('<th colspan="',colspan_V1,'">', V1, '</th>'))
    
    tbl_rows <- paste0(
      '<tr>
        <th rowspan="',n_labs,'">',names(data)[1],'</th>\n',
      V1$html %>% paste(collapse = "\n"),
      '</tr>'
    )
  
  ## cols LFS or PROV, prefaced by ('000), (%), (% CHG), (CHG)
  } else if(n_labs == 2){
    
    V1 <- headers %>%
      summarize(colspan_V1 = n(), .by = c(V2, V1)) %>%
      mutate(html = paste0('<th colspan="',colspan_V1,'">', V1, '</th>'))
    
    V2 <- headers %>%
      summarize(colspan_V2 = n(), .by = V2) %>%
      mutate(html = paste0('<th colspan="',colspan_V2,'">', V2, '</th>'))
    
    tbl_rows <- paste0(
      '<tr>
        <th rowspan="',n_labs,'">',names(data)[1],'</th>\n',
      V2$html %>% paste(collapse = "\n"),
      '</tr>
       <tr>\n',
      V1$html %>% paste(collapse = "\n"),
      '</tr>'
    )
    
  ## cols demog followed by LFS, prefaced by ('000), (%), (% CHG), (CHG)
  } else if(n_labs == 3){
    
    V1 <- headers %>%
      summarize(colspan_V1 = n(), .by = c(V3, V2, V1)) %>%
      mutate(html = paste0('<th colspan="',colspan_V1,'">', V1, '</th>'))
    
    V2 <- headers %>%
      summarize(colspan_V2 = n(), .by = c(V3,V2)) %>%
      mutate(html = paste0('<th colspan="',colspan_V2,'">', V2, '</th>'))
    
    V3 <- headers %>%
      summarize(colspan_V3 = n(), .by = V3) %>%
      mutate(html = paste0('<th colspan="',colspan_V3,'">', V3, '</th>'))
    
    tbl_rows <- paste0(
      '<tr>
        <th rowspan="',n_labs,'">',names(data)[1],'</th>\n',
      V3$html %>% paste(collapse = "\n"),
      '</tr>
       <tr>\n',
      V2$html %>% paste(collapse = "\n"),
      '</tr>
       <tr>\n',
      V1$html %>% paste(collapse = "\n"),
      '</tr>'
    )
    
  } else { ## (n_labs == 4)
    
    V1 <- headers %>%
      summarize(colspan_V1 = n(), .by = c(V4, V3, V2, V1)) %>%
      mutate(html = paste0('<th colspan="',colspan_V1,'">', V1, '</th>'))
    
    V2 <- headers %>%
      summarize(colspan_V2 = n(), .by = c(V4, V3, V2)) %>%
      mutate(html = paste0('<th colspan="',colspan_V2,'">', V2, '</th>'))
    
    V3 <- headers %>%
      summarize(colspan_V3 = n(), .by = c(V4, V3)) %>%
      mutate(html = paste0('<th colspan="',colspan_V3,'">', V3, '</th>'))
    
    V4 <- headers %>%
      summarize(colspan_V4 = n(), .by = V4) %>%
      mutate(html = paste0('<th colspan="',colspan_V4,'">', V4, '</th>'))
    
    tbl_rows <- paste0(
      '<tr>
        <th rowspan="',n_labs,'">',names(data)[1],'</th>\n',
        V4$html %>% paste(collapse = "\n"),
      '</tr>
       <tr>\n',
        V3$html %>% paste(collapse = "\n"),
      '</tr>
       <tr>\n',
        V2$html %>% paste(collapse = "\n"),
      '</tr>
       <tr>\n',
        V1$html %>% paste(collapse = "\n"),
      '</tr>'
    )
  }
  
  cust_header <- HTML(paste0(
    '<table class="display">
     <style>
      th {
      text-align: center;
      border: 1px solid #dddddd;
      border-collapse: collapse;
      }
     </style>
      <thead>',
       tbl_rows,
     '</thead>
     </table>'
  ))
  
  cust_header
  
}

get_summary_table <- function() {
  
  summary <- get_cansim_vector(vectors = vectors %>% 
                                 filter(table == "summary") %>%
                                 pull(vector), start_time = prev_year) %>%
    clean_names() %>%
    select(everything(), -any_of(names(vectors)), vector) %>%
    left_join(vectors %>%  filter(table == "summary"), by = c("vector")) %>%
    filter(date %in% c(curr_date, prev_month, prev_year)) %>%
    mutate(month = case_when(date == curr_date ~ "curr_month",
                             date == prev_month ~ "prev_month",
                             date == prev_year ~ "prev_year")) %>%
    select(month, geo, labour_force_characteristics, data_type, value) %>%
    pivot_wider(names_from = month, values_from = value) %>%
    mutate(change_prev_month = ifelse(str_detect(labour_force_characteristics, "rate"), "",
                                      paste0(format(round_half_up(100*(curr_month - prev_month)/prev_month, digits = 1), big.mark = ",", nsmall = 1) , "%"))) %>%
    mutate(change_prev_year = ifelse(str_detect(labour_force_characteristics, "rate"), "",
                                     paste0(format(round_half_up(100*(curr_month - prev_year)/prev_year, digits = 1), big.mark = ",", nsmall = 1), "%"))) %>%
    mutate_at(.vars = c("curr_month", "prev_month", "prev_year"), 
              ~ ifelse(str_detect(labour_force_characteristics, "rate"), paste0(format(.x, big.mark = ",", nsmall = 1), "%"), format(.x, big.mark = ",", nsmall = 1))) %>%
    mutate(labour_force_characteristics = fct_recode(labour_force_characteristics, 
                                                     `Population, 15 and older ('000)` = 'Population',
                                                     `Labour force ('000)` = 'Labour force',
                                                     `Employment ('000)` = 'Employment',
                                                     `Unemployment ('000)` = 'Unemployment',
                                                     `Unemployment rate (%)` = 'Unemployment rate',
                                                     `Participation rate (%)` = 'Participation rate',
                                                     `Employment rate (%)` = 'Employment rate')) %>%
    select(geo, labour_force_characteristics, data_type, curr_month, prev_month, prev_year, change_prev_month, change_prev_year) %>%
    arrange(geo, labour_force_characteristics)
  
}

print_summary_table <- function(data, zero_data_note) {
  
  
  
  if(nrow(data) == 0){ ## ie, selected data_type = Annual
    
    summary_dt <- DT::datatable(data, 
                                options = list(info = FALSE, 
                                               paging = FALSE, 
                                               searching = FALSE, 
                                               scrollX = TRUE, 
                                               columnDefs = list(list(className = 'dt-left', targets = 0),
                                                                 list(className = 'dt-head-center', targets = "_all")),
                                               language = list(zeroRecords = zero_data_note)
                                ),
                                rownames = FALSE,
                                colnames = c("INDICATOR", "CURRENT MONTH", "PREVIOUS MONTH", "SAME MONTH PREVIOUS YEAR", "CHANGE FROM PREVIOUS MONTH", "CHANGE FROM SAME MONTH PREVIOUS YEAR" ),
                                caption = tags$caption(style = 'caption-side: bottom; text-align: left;',
                                                       HTML(table_captions %>% filter(table_id == "summary") %>% pull(caption)))
                               )
    
  } else  {
    
    tbl <- bind_rows(
      data.frame(labour_force_characteristics = "<strong>British Columbia</strong>", 
                 curr_month = NA,
                 prev_month = NA,
                 prev_year = NA,
                 change_prev_month = NA,
                 change_prev_year = NA),
      data %>% filter(geo == "British Columbia") %>% select(-geo, -data_type),
      data.frame(labour_force_characteristics = "<strong>Canada</strong>", 
                 curr_month = NA,
                 prev_month = NA,
                 prev_year = NA,
                 change_prev_month = NA,
                 change_prev_year = NA),
      data %>% filter(geo == "Canada") %>% select(-geo, -data_type))
      
      summary_dt <- DT::datatable(tbl, 
                                  options = list(info = FALSE, 
                                                 paging = FALSE, 
                                                 searching = FALSE, 
                                                 scrollX = TRUE, 
                                                 columnDefs = list(list(className = 'dt-left', targets = 0),
                                                                   list(className = 'dt-head-center', targets = "_all")),
                                                 language = list(zeroRecords = zero_data_note)
                                  ),
                                  rownames = FALSE,
                                  colnames = c("INDICATOR", "CURRENT MONTH", "PREVIOUS MONTH", "SAME MONTH PREVIOUS YEAR", "CHANGE FROM PREVIOUS MONTH", "CHANGE FROM SAME MONTH PREVIOUS YEAR" ),
                                  escape = FALSE,
                                  class = 'cell-border stripe',
                                  caption = tags$caption(style = 'caption-side: bottom; text-align: left;',
                                                         HTML(table_captions %>% filter(table_id == "summary") %>% pull(caption)))) %>% 
                                    formatStyle('labour_force_characteristics',
                                                whiteSpace="nowrap",
                                                paddingLeft = styleEqual(data$labour_force_characteristics%>% unique(), rep('35px', 7))) %>%
                                    formatStyle(names(tbl),
                                                borderTop = styleRow(which(tbl$labour_force_characteristics=="<strong>Canada</strong>"),'1px solid black')) %>%
                                    formatStyle(names(tbl)[-1],
                                                textAlign = 'center')
      }
  
  summary_dt
  
}

# data<- get_summary_table() %>%
#   filter(data_type == "Seasonally adjusted") %>%
#   select(-data_type)


## Functions for highlights tab tables ----

# Render a bar chart with a label on the inside (if beyond label_threshold) or on left
## code from https://glin.github.io/reactable/articles/cookbook/cookbook.html?q=image#bar-charts
bar_chart <- function(label, value, max_value = 1,
                      height = "1.5rem",
                      fill = "#4F81BD",
                      background = NULL,
                      label_threshold = 0.80) {
  
  width <- value / max_value
  width_pct <- paste0(width * 100, "%")
  
  # Put label inside the bar if it is wide enough
  label_inside <- width >= label_threshold
  
  bar <- div(
    style = list(
      background = fill,
      width = width_pct,
      height = height
    )
  )
  
  label_div <- div(
    label,
    style = list(
      position = "absolute",
      left = if (label_inside) width_pct else paste0(width * 100 + 5, "%"),
      transform = if (label_inside) "translateX(-100%)" else NULL,
      paddingRight = if (label_inside) "0.25rem" else NULL,
      color = if (label_inside) "white" else "inherit",
      whiteSpace = "nowrap"
    )
  )
  
  div(
    style = list(
      position = "relative",
      width = "100%",
      minHeight = "2.25rem",
      display = "flex",
      alignItems = "center",
      background = background
    ),
    
    # Zero/reference line
    div(
      style = list(
        position = "absolute",
        left = "0%",
        top = 0,
        bottom = 0,
        marginLeft = "0.5rem",
        width = "1px",
        height = "2.25rem",
        background = "#bdbdbd"
      )
    ),
    
    # Bar
    div(
      style = list(
        marginLeft = "0.5rem",
        height = height,
        width = width_pct,
        background = fill
      )
    ),
    
    # Label
    label_div
  )
}

## Render a bar chart with positive and negative values
## code from https://glin.github.io/reactable/articles/cookbook/cookbook.html?q=image#bar-charts
bar_chart_pos_neg <- function(label, value, max_value = 1, height = "1.5rem",
                              pos_fill = "#4F81BD", neg_fill = "#C00000", unemp = FALSE) {
  
  neg_chart <- div(style = list(flex = "1 1 0"))
  pos_chart <- div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value / max_value) * 40, "%")
  
  if (value < 0) {
    bg_fill <- ifelse(unemp, pos_fill, neg_fill)
    bar <- div(style = list(marginLeft = "0.5rem", background = bg_fill, width = width, height = height))
    chart <- div(
      style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
      label,
      bar
    )
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bg_fill <- ifelse(unemp, neg_fill, pos_fill)
    bar <- div(style = list(marginRight = "0.5rem", background = bg_fill, width = width, height = height))
    chart <- div(style = list(display = "flex", alignItems = "center"), bar, label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }
  
  div(style = list(display = "flex", 
                   position = "relative",
                   width = "100%",
                   minHeight = "2.25rem",
                   alignItems = "center"),
      div(
        style = list(
          position = "absolute",
          left = "50%",    
          top = 0,
          bottom = 0,
          width = "1px",
          height = "2.25rem",
          background = "#bdbdbd")),
      
      neg_chart,
      pos_chart
    )
}

create_reactable <- function(data, ref_date = NULL, pos_fill = "#4F81BD", neg_fill = "#C00000", rev_unemp = TRUE) {
  
  ## define default columns
  columns <- list(
    
    label_order = colDef(
      sticky = "left",
      show = TRUE,
      name = "",
      align = "left",
      minWidth = 160,
      # minWidth = 230,
      style = list(overflowWrap = "break-word",
                   alignItems = "center"),
      cell = function(value, index) {
        data$label[index]
      }
    ),
    
    mom_change = colDef(
      show = TRUE,
      name = "Change from<br>previous month",
      style = list(alignItems = "center"),
      cell = function(value, index){
        unit_group <- data$group[index]
        max_value <- data$bar_max_chg[index]
        unemp <- str_detect(data$label[index], "Unemployment") && rev_unemp
        
        
        label = switch(
          unit_group,
          "1" = comma(value),
          "2" = paste0(value, "ppt"),
          "3" = dollar(value)
        )
        
        bar_chart_pos_neg(label, value, max_value = max_value, pos_fill = pos_fill, neg_fill = neg_fill, unemp = unemp)
      },
      align = "center",
      minWidth = 200
      
    ),
    
    yoy_change = colDef(
      show = TRUE,
      name = "Change from<br>previous year",
      style = list(alignItems = "center"),
      cell = function(value, index){
        unit_group <- data$group[index]
        max_value <- data$bar_max_chg[index]
        unemp <- str_detect(data$label[index], "Unemployment") && rev_unemp
        
        label = switch(
          unit_group,
          "1" = comma(value),
          "2" = paste0(value, "ppt"),
          "3" = dollar(value)
        )
        
        bar_chart_pos_neg(label, value, max_value = max_value, pos_fill = pos_fill, neg_fill = neg_fill, unemp = unemp)
      },
      align = "center",
      minWidth = 200
    ),
    
    yoy_pct_change = colDef(
      show = TRUE,
      name = "Change from<br>previous year (%)",
      style = list(alignItems = "center"),
      cell = function(value, index){
        if(is.na(value)) {
          ""
        } else {
          max_value <- data$bar_max_pct[index]
          unemp <- str_detect(data$label[index], "Unemployment") && rev_unemp
          
          label = percent(value, accuracy = 0.1, f = janitor::round_half_up)
          
          bar_chart_pos_neg(label, value, max_value = max_value, pos_fill = pos_fill, neg_fill = neg_fill, unemp = unemp)
        }
      },
      align = "center",
      minWidth = 200
    )
    
  )
  
  ## define column "arrow" for key indicator table
  if("arrow" %in% names(data)) {
    columns$arrow = colDef(
      show = TRUE,
      name = "",
      align = "center",
      sortable = FALSE,
      minWidth = 70,
      maxWidth = 70,
      cell = function(value, index) {
        
        colour <- data$color[index]
        
        tags$span(
          style = paste0(
            "background-color:", colour, "18;", # add opacity hex
            "border-radius:15%;",
            "width:100%;",
            "height:100%;",
            "display:inline-flex;",
            "align-items:center;",
            "justify-content:center;",
            "padding:0px"
          ),
          icon(
            name = value,
            class = "fa-xl",
            style = paste0(
              "color:", colour, ";"
            )
          )
        )
      }
    )
  }
  
  ## define column "current" for lf characteristics table
  if("current" %in% names(data)){
    columns$current = colDef(
      show = TRUE,
      name = ifelse(!is.null(ref_date), paste(ref_date, "estimate"), "Monthly estimate"),
      style = list(alignItems = "center"),
      cell = function(value, index){
        unit_group <- data$group[index]
        max_value <- data$bar_max_est[index]
        
        label = switch(
          unit_group,
          "1" = comma(1000*value),
          "2" = percent(value/100, accuracy = 0.1)
        )
        
        bar_chart(label, value, max_value = max_value, fill = pos_fill)
      },
      align = "left",
      minWidth = 240
    )
  }
  
  ## create table
  reactable(
    data,
    bordered = FALSE,
    striped = TRUE,
    highlight = FALSE,
    compact = TRUE,
    pagination = FALSE,
    searchable = FALSE,
    style = list(fontFamily = "BC Sans",
                 maxWidth = "1600px"),
    defaultColDef = colDef(
      show = FALSE,
      html = TRUE,
      align = "right"
    ),
    columns = columns
  )
}
