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

## Industry colours ----
## define industry colours and create css variables
industry_colors <- tribble(
  ~Industry,                                              ~color,
  "BC total (all industries)",                             "black",
  "Goods-producing sector",                              "#1b9e77",
  "Agriculture",                                         "#009E73",
  "Forestry, fishing, mining, quarrying, oil and gas",   "#56B4E9",
  "Utilities",                                           "#E69F00",
  "Construction",                                        "#0072B2",
  "Manufacturing",                                       "#D55E00",
  "Services-producing sector",                           "#7570b3",
  "Wholesale and retail trade",                          "#67001f",
  "Transportation and warehousing",                      "#D95F02",
  "Finance, insurance, real estate, rental and leasing", "#807dba",
  "Professional, scientific and technical services",     "#E7298A",
  "Business, building and other support services",       "#66A61E",
  "Educational services",                                "#E6AB02",
  "Health care and social assistance",                   "#A6761D",
  "Information, culture and recreation",                 "#666666",
  "Accommodation and food services",                     "#1F78B4",
  "Other services (except public administration)",       "#FB9A99",
  "Public administration",                               "#6A3D9A"
)

css_colours <- paste0("--colour-", to_snake_case(industry_colors$Industry), ": ", industry_colors$color, ";")
css <- paste(":root {", paste(css_colours, collapse = "\n"), "}", sep = "\n")
writeLines(css, "www/colours.css")

## Industry vectors ----
industry_vectors <- tribble(
  ~Sector,  ~Industry, ~vector,
  "",         "BC total (all industries)", "v2057793",
  "",         "Goods-producing sector", "v2057794",
  "Goods",    "Agriculture", "v2057795",
  "Goods",    "Forestry, fishing, mining, quarrying, oil and gas", "v2057796",
  "Goods",    "Utilities", "v2057797",
  "Goods",    "Construction", "v2057798",
  "Goods",    "Manufacturing", "v2057799",
  "",         "Services-producing sector", "v2057800",
  "Services", "Wholesale and retail trade", "v2057801",
  "Services", "Transportation and warehousing", "v2057802",
  "Services", "Finance, insurance, real estate, rental and leasing", "v2057803",
  "Services", "Professional, scientific and technical services", "v2057804",
  "Services", "Business, building and other support services", "v2057805",
  "Services", "Educational services", "v2057806",
  "Services", "Health care and social assistance", "v2057807",
  "Services", "Information, culture and recreation", "v2057808",
  "Services", "Accommodation and food services", "v2057809",
  "Services", "Other services (except public administration)", "v2057810",
  "Services", "Public administration", "v2057811")

# Download industry data ----
cansim_error <<- FALSE
industry_data <- tryCatch(
  {
    ## Get the industry employment data
    left_join(
      industry_vectors,
      get_cansim_vector(
        vectors = industry_vectors$vector,
        start_time = "1976-01-01") %>%
        clean_names(),
      by = "vector")  %>%
      select(Industry, Date = ref_date, value) %>%
      mutate(Date = ymd(Date))
  },
  error = function(e) {
    cansim_error <<- TRUE
    NULL
  }
)

