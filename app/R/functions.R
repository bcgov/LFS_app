
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
    rbind(get_cansim_vector(vectors = vectors %>% filter(table == tbl, data_type == "Annual") %>% pull(vector), start_time = annual_start_year)) %>%
    clean_names() %>%
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
    summarize(avg = mean(value)) %>%
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
      group_by(V1) %>%
      summarize(colspan_V1 = n()) %>%
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
      group_by(V2,V1) %>%
      summarize(colspan_V1 = n()) %>%
      mutate(html = paste0('<th colspan="',colspan_V1,'">', V1, '</th>'))
    
    V2 <- headers %>%
      group_by(V2) %>%
      summarize(colspan_V2 = n()) %>%
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
      group_by(V3, V2, V1) %>%
      summarize(colspan_V1 = n()) %>%
      mutate(html = paste0('<th colspan="',colspan_V1,'">', V1, '</th>'))
    
    V2 <- headers %>%
      group_by(V3,V2) %>%
      summarize(colspan_V2 = n()) %>%
      mutate(html = paste0('<th colspan="',colspan_V2,'">', V2, '</th>'))
    
    V3 <- headers %>%
      group_by(V3) %>%
      summarize(colspan_V3 = n()) %>%
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
      group_by(V4, V3, V2, V1) %>%
      summarize(colspan_V1 = n()) %>%
      mutate(html = paste0('<th colspan="',colspan_V1,'">', V1, '</th>'))
    
    V2 <- headers %>%
      group_by(V4, V3, V2) %>%
      summarize(colspan_V2 = n()) %>%
      mutate(html = paste0('<th colspan="',colspan_V2,'">', V2, '</th>'))
    
    V3 <- headers %>%
      group_by(V4, V3) %>%
      summarize(colspan_V3 = n()) %>%
      mutate(html = paste0('<th colspan="',colspan_V3,'">', V3, '</th>'))
    
    V4 <- headers %>%
      group_by(V4) %>%
      summarize(colspan_V4 = n()) %>%
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
