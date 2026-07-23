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

# Highlights key indicators ----

## Define the label/vectors to include in the highlights table ----
key_indicators_vectors <- tribble(
  ~group, ~label, ~vector,
  # Table 14-10-0287-01: Labour force characteristics by sex and age group, annual
  1, "Employment", "v2064701", # Employment, BC, 15 years and over
  1, "Full-time employment", "v2064702", # Full-time employment, BC, 15 years and over
  
  # Table 14-10-0288-01: Employment by public and private sectors
  1, "Private sector employment", "v2067020", # BC private sector employment
  
  # Table 14-10-0355-01: Employment by industry, goods and services
  1, "Goods sector employment", "v2057794", # Goods industries employment BC 15+ both (goods-producing sector, Seasonally Adjusted)
  
  # Table 14-10-0401: Employment by Indigenous identity and age group
  1, "Indigenous employment", "v1411942612", # BC indigenous population employment 15+ both
  
  # Table 14-10-0471-01: Employment by immigrant status
  1, "Immigrant employment", "v1642915778", # BC immigrants (10+ yrs) employment, 15+
  
  # Table 14-10-0287-01: Labour force characteristics by sex and age group, annual
  2, "Unemployment rate", "v2064705", # Unemployment rate, BC, 15 years and over
  2, "Participation rate", "v2064706", # Participation rate, BC, 15 years and over
  
  # Table 14-10-0287-01 (continued)
  2, "Core age employment rate", "v2064842", # BC employment rate, 25-54, both, Seasonally Adjusted
  
  # Table 14-10-0063: Average hourly wages of employees by industry
  3, "Average hourly wages", "v2166779" # Total average hourly earnings
)

## Download the latest data from cansim ----
## (13 periods = 12 months + 1)

#cansim_error <- NULL

cansim_data <- tryCatch(
 {
    # Download the data
    left_join(
      key_indicators_vectors,
      get_cansim_vector_for_latest_periods(
        vectors = key_indicators_vectors$vector,
        periods = 13) %>%
        clean_names(),
      by = "vector"
    )
  },
  error = function(e) {
    cansim_error <<- paste("Failed connection to Statistics Canada. Please try again later")
    NULL
  }
)


## Calculate stats ----
key_indicators_stats <- cansim_data %>%
  mutate(ref_date = ymd(ref_date),
         date = case_when(ref_date == max(ref_date) ~ "current",
                          ref_date == max(ref_date) - months(1) ~ "previous_month",
                          ref_date == max(ref_date) - years(1) ~ "previous_year")) %>%
  select(group, label, date, value) %>%
  filter(!is.na(date)) %>%
  pivot_wider(names_from = date, values_from = value) %>%
  ## multiply values by 1 or 1000 (if in group 1)
  mutate(
    mom_change = round_half_up(current - previous_month, digits = 2) * (1 + 999 * (group == 1)),
    yoy_change = round_half_up(current - previous_year, digits = 2) * (1 + 999 * (group == 1)),
    yoy_pct_change = case_when(
      str_detect(label, "rate") ~ NA,
      TRUE ~ round_half_up(100 * (current - previous_year)/ previous_year, digits = 1))
  ) %>%
  ## max values for bar chart proportioning
  mutate(
    mom_max = max(abs(mom_change)),
    yoy_max = max(abs(yoy_change)),
    .by = group
  ) %>%
  mutate(yoy_pct_max = max(abs(yoy_pct_change), na.rm = TRUE))

## Format values and assign arrows and colours ----
key_indicators_stats_fmtd <- key_indicators_stats %>%
  mutate(
    arrow = case_when(
      mom_change > 0 ~ "up-long",
      mom_change == 0 ~ "minus",
      mom_change < 0 ~ "down-long"
    ),
    
    ## multiply values by 1 or -1 (if Unemployment rate)
    color = case_when(
      (1 - 2 * (label == "Unemployment rate")) * mom_change > 0 ~ "#00B050",
      (1 - 2 * (label == "Unemployment rate")) * mom_change < 0 ~ "#C00000",
      TRUE ~ "#1d1d1d"
    ),
    
    label = case_when(
      group == 1 ~ paste0("<strong>", label, "</strong><br><span style = 'padding-left:10px'>", prettyNum(current, big.mark = ","), " thousand</span>"),
      group == 2 ~ paste0("<strong>", label, "</strong><br><span style = 'padding-left:20px'>", current, " per cent</span>"),
      group == 3 ~ paste0("<strong>", label, "</strong><br><span style = 'padding-left:20px'>$", current, "/hr</span>")
    ),
    
    label_order = row_number()
  ) %>%
  select(group, label_order, label, arrow, color, starts_with("mom"), starts_with("yoy"))


