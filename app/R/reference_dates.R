# Copyright 2026 Province of British Columbia
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

cansim_error <<- FALSE
latest_update_data <- tryCatch(
  {
    ## Get the latest Labour force data
    get_cansim_vector_for_latest_periods(vectors = "v2064700", periods = 1)
  },
  error = function(e) {
    cansim_error <<- TRUE
    NULL
  }
)

if(!cansim_error) {
  
  ### Date References ----
  
  ## For data tables:
  ## Summary requires curr_date, prev_month and prev_year
  
  ## MOM calculations require monthly_start_month (Jan of 2 years ago) to curr_date
  ###  and display from prev_year_jan to curr_date
  
  ## YOY calculations require annual_start_year (11 years ago) 
  ###  and display from annual_display year
  
  ## YTD calculations require prev_year_jan to prev_year and curr_year_jan to curr_date
  
  ## For monthly comparison charts:
  ## Age and gender chart requires formatted dates
  ##  current, mom, yoy (formatted versions of curr_date, prev_month and prev_year)
  
  ## Need release_date from cansim for "last updated" date on the app
  
  ## curr_date = latest Labour Force date
  
  ## Dates ----
  release_date <- latest_update_data %>% pull(releaseTime)
  curr_date <- latest_update_data  %>% pull(Date) %>% max()
  prev_month <- curr_date - months(1)
  prev_year <- curr_date - years(1)
  prev_year_jan <- paste0(year(curr_date - years(1)),"-01-01") %>% ymd()
  curr_year_jan <- paste0(year(curr_date), "-01-01") %>% ymd()
  monthly_start_month <- paste0(year(curr_date - years(2)),"-01-01") %>% ymd()
  annual_start_year <- paste0(year(curr_date - years(11)),"-01-01") %>% ymd()
  annual_display_year <- paste0(year(curr_date - years(10)), "01-01") %>% ymd()
  
  ## reference dates to display in header/footer 
  formatted_date <- paste(month(curr_date, label = TRUE, abbr = FALSE), year(curr_date))
  last_updated_date <- ymd_hm(release_date) %>% format(format = "%B %e, %Y")
  
  ## reference dates for charts
  current <- formatted_date
  mom <- paste("Change from", month(prev_month, label = TRUE, abbr = FALSE), year(prev_month))
  yoy <- paste("Change from", month(prev_year, label = TRUE, abbr = FALSE), year(prev_year))
  
}