## load libraries ----
library(tidyverse)      ## includes: dplyr, ggplot2, tibble, readr, tidyr, purrr, stringr, forcats
library(rsconnect)      ## deployment interface for shiny apps
library(shiny)          ## for most app functions: reactive(), downloadHandler(), renderUI(), shinyApp(), etc.
library(shinydashboard) ## for box()
library(shinyWidgets)   ## for useShinydashboard()
library(cansim)         ## for cansim data
library(lubridate)    ## for ymd() (date parsing)
library(janitor)      ## for clean_names() (on cansim data) and round_half_up()
library(DT)           ## for dataTableOutput(), renderDataTable()
library(plotly)       ## for plotlyOutput(), ggplotly(), renderPlotly
library(scales)       ## for label_percent() in plots
library(DiagrammeR)   ## for flow chart
library(dygraphs)
library(xts)
library(bcstatslinks)

options(scipen = 999999999)  ## so chart axes read properly

### vectors and metadata ----
vectors <- qs::qread("vector_metadata.qs")

## Load starting data ----
hl_stats_meta <- data.frame(
  label = c("Population", "Labour Force", 
            "Employment", "Unemployment", 
            "Employment Rate", "Unemployment Rate", 
            "Participation Rate"),
  vector = c("v2064699", "v2064700", "v2064701", 
             "v2064704", "v2064707", "v2064705", "v2064706")
)

hl_data <- get_cansim_vector_for_latest_periods(
  vectors = hl_stats_meta$vector,
  periods = 2) %>%
  normalize_cansim_values() %>%
  clean_names() 

hl_stats <- hl_data %>%
  left_join(hl_stats_meta, by = "vector") %>%
  mutate(month = ifelse(ref_date == max(ref_date), "current", "previous")) %>%
  select(label, month, value) %>%
  pivot_wider(names_from = month, values_from = value) %>%
  mutate(change = round_half_up(current - previous, digits = 1))

### Date References ----

## Summary requires curr_date, prev_month and prev_year

## MOM calculations require monthly_start_month (Jan of 2 years ago) to curr_date
###  and display from prev_year_jan to curr_date

## YOY calculations require annual_start_year (11 years ago) 
###  and display from annual_display year

## YTD calculations require prev_year_jan to prev_year and curr_year_jan to curr_date

## curr_date = latest Labour Force date
curr_date <- max(hl_data$date)
prev_month <- curr_date - months(1)
prev_year <- curr_date - years(1)
prev_year_jan <- paste0(year(curr_date - years(1)),"-01-01") %>% ymd()
curr_year_jan <- paste0(year(curr_date), "-01-01") %>% ymd()
monthly_start_month <- paste0(year(curr_date - years(2)),"-01-01") %>% ymd()
annual_start_year <- paste0(year(curr_date - years(11)),"-01-01") %>% ymd()
annual_display_year <- paste0(year(curr_date - years(10)), "01-01") %>% ymd()

formatted_date <- paste(month(curr_date, label = TRUE, abbr = FALSE), year(curr_date))

## Table Details for App ----

dt_details <- tibble::tribble(
                        ~table_id,                                                             ~table_name,                                             ~grouping, ~include_avg_pct_chg, ~include_pct_chg, ~include_diff,
                        "summary",                                   "BC and Canada Labour Market Changes",                                                    NA,                   NA,               NA,            NA,
                         "bc_lfd",                                       "BC Labour Market Key Indicators",                        "labour_force_characteristics",                 TRUE,             TRUE,         FALSE,
                       "prov_emp",                                          "Total Employment by Province",                                             "geo_abb",                 TRUE,            FALSE,         FALSE,
                "prov_emp_growth",                                     "Employment Growth (%) by Province",                                             "geo_abb",                   NA,             TRUE,         FALSE,
                  "prov_emp_jobs",                          "Employment Growth (level change) by Province",                                             "geo_abb",                   NA,            FALSE,          TRUE,
                    "prov_unempr",                                    "Unemployment Rates (%) by Province",                                             "geo_abb",                FALSE,            FALSE,         FALSE,
                      "prov_empr",                                      "Employment Rates (%) by Province",                                             "geo_abb",                FALSE,            FALSE,         FALSE,
                     "age_gender",                      "BC Employment and Unemployment by Age and Gender",                                               "label",                 TRUE,            FALSE,         FALSE,
                "age_gender_rate","BC Employment, Unemployment, and Participation Rates by Age and Gender",                                               "label",                FALSE,            FALSE,         FALSE,
                    "ftpt_gender",                           "BC Employment by Full-Time/Part-Time Status",                                               "label",                 TRUE,            FALSE,         FALSE,
                       "industry",                                             "BC Employment by Industry", "north_american_industry_classification_system_naics",                 TRUE,             TRUE,         FALSE,
                     "occupation",                     "BC Employment and Unemployment Rate by Occupation",                                               "label",                 TRUE,            FALSE,         FALSE,
                         "region",                         "BC Employment and Unemployment Rate by Region",                                               "label",                   NA,            FALSE,         FALSE,
                            "cma",        "BC Employment and Unemployment Rate by Census Metropoitan Area",                                               "label",                   NA,            FALSE,         FALSE,
                            "cow",                                      "BC Employment by Class of Worker",                                     "class_of_worker",                 TRUE,             TRUE,         FALSE
                )

  
  
choices_list <- as.list(dt_details$table_id)
names(choices_list) <- dt_details$table_name

table_captions <- tibble::tribble(
                            ~table_id, ~caption,
                            "summary", "NOTE: Employment rate is calculated as employment as a percent of the population aged 15 and older.",
                             "bc_lfd", "NOTE: Percent changes for unadjusted data are calculated as month over same month previous year. For seasonally adjusted data, percent changes are calculated as month over previous month.",
                           "prov_emp", "",
                    "prov_emp_growth", "NOTE: Percent changes for unadjusted data are calculated as month over same month previous year. For seasonally adjusted data, percent changes are calculated as month over previous month.",
                      "prov_emp_jobs", "NOTE: Changes for unadjusted data are calculated as month less same month previous year. For seasonally adjusted data, changes are calculated as month less previous month.",
                        "prov_unempr", "",
                          "prov_empr", "",
                         "age_gender", "NOTE: due to relatively small sample sizes from which disaggregated data are derived, caution should be used in the interpretation of month-to-month changes in the estimates",
                    "age_gender_rate", "NOTE: due to relatively small sample sizes from which disaggregated data are derived, caution should be used in the interpretation of month-to-month changes in the estimates",
                        "ftpt_gender", "",
                           "industry", "NOTE: Percent changes for unadjusted data are calculated as month over same month previous year. For seasonally adjusted data, percent changes are calculated as month over previous month.",
                         "occupation", "NOTES:<br/>
                                       1. Occupation specific Unemployment Rates include only those unemployed who worked in the past year. The \"All Occupations\" rate incorporates total unemployed persons which includes an \"Occupation Unclassified\" category.<br/>
                                       2. Due to the relatively small sample sizes from which disaggregated data are derived, caution should be used in the interpretation of month-to-month changes in the estimates.<br/>
                                       3. For the unemployed, industry or occupation is based on the last job held in the previous year. No information is collected on industry or occupation of job search.",
                             "region", "NOTE: Revised in January 2021 to reflect 2016 Census population and 2016 Standard Geographic Classification.",
                                "cma", "NOTE: Revised in January 2021 to reflect 2016 Census population and 2016 Standard Geographic Classification.",
                                "cow", ""
                    )




