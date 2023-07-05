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


library(tidyverse)
library(cansim)
library(janitor)
library(lubridate)
library(here)

## 1. Get Cansim Tables ----

### Province, Gender, Age Group ----
## Population, Labour force, Employment, Full-time employment, Part-time employment,
## Unemployment rate, Participation rate, Employment rate
lfs_data_monthly <- get_cansim("14-10-0287") %>%  add_provincial_abbreviations() %>% clean_names()
lfs_data_annual <- get_cansim("14-10-0327") %>%  add_provincial_abbreviations() %>% clean_names()

### Industry ----
## Employment
industry_data_monthly <- get_cansim("14-10-0355") %>% clean_names()
industry_data_annual <- get_cansim("14-10-0023") %>% clean_names()

### Occupation ----
## Employment, Unemployment Rate
occupation_data_monthly <- get_cansim("14-10-0421") %>% clean_names()
occupation_data_annual <- get_cansim("14-10-0416") %>% clean_names()

### Class of Worker ----
## Employment
cow_data_monthly <- get_cansim("14-10-0288") %>% clean_names()
cow_data_annual <- get_cansim("14-10-0027")%>% clean_names()

### Economic Region ----
## Employment, Unemployment Rate
## 3 month moving average, unadjusted
er_data_monthly <- get_cansim("14-10-0387") %>%  clean_names()
er_data_annual <- get_cansim("14-10-0393") %>% clean_names()

### Census Metropolitan Area ----
## Employment, Unemployment Rate
## 3 month moving average, unadjusted
cma_data_monthly <- get_cansim("14-10-0378") %>% clean_names()
cma_data_annual <- get_cansim("14-10-0385") %>% clean_names()


## Get Cansim Vectors ----

## Note: Want to include these columns for all vectors: 
## vector - cansim vector number
## labour_force_characteristics
## data_type - Unadjusted, Seasonally adjusted, or Annual
## table - short name of table
## geo_abb - provincial abbreviation
## geo - name of geography (province, region, cma, "Canada"),
## age_group
## sex
## north_american_industry_classification_system_naics
## national occupational_classificaiton_noc
## class_of_worker

### Summary ----

summary_vectors <- lfs_data_monthly %>% 
  filter(geo == "British Columbia" | geo == "Canada") %>% 
  filter(labour_force_characteristics %in%
           c("Population", "Labour force", "Employment", "Unemployment", 
             "Employment rate", "Unemployment rate", "Participation rate")) %>% 
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  filter(statistics == "Estimate") %>%
  filter(data_type %in% c("Seasonally adjusted", "Unadjusted")) %>%
  mutate(table = "summary",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>%
  unique()

### BC Labour Force Data ----

bc_lfd_m <- lfs_data_monthly %>%
  filter(geo == "British Columbia") %>% 
  filter(vector %in% (summary_vectors %>% 
                        filter(labour_force_characteristics %in% 
                                 c("Labour force", "Employment", "Unemployment", "Unemployment rate", "Participation rate")) %>%
                        pull(vector))) %>%
  mutate(table = "bc_lfd",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>%
  unique()

bc_lfd_a <- lfs_data_annual %>% 
  filter(geo == "British Columbia") %>% 
  filter(labour_force_characteristics %in%
           c("Labour force", "Employment", "Unemployment", 
             "Unemployment rate", "Participation rate")) %>% 
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  mutate(table = "bc_lfd",
         data_type = "Annual",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

### Prov Employment ----

prov_emp_m <- lfs_data_monthly %>% 
  filter(labour_force_characteristics == "Employment") %>% 
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  filter(statistics == "Estimate") %>%
  filter(data_type %in% c("Seasonally adjusted", "Unadjusted")) %>%
  mutate(table = "prov_emp",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique()  

prov_emp_a <- lfs_data_annual %>% 
  filter(labour_force_characteristics == "Employment") %>% 
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  mutate(table = "prov_emp",
         data_type = "Annual",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

### Prov Unemployment Rate ----

prov_unempr_m <- lfs_data_monthly %>% 
  filter(labour_force_characteristics == "Unemployment rate") %>% 
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  filter(statistics == "Estimate") %>%
  filter(data_type %in% c("Seasonally adjusted", "Unadjusted"))%>%
  mutate(table = "prov_unempr",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique()  

prov_unempr_a <- lfs_data_annual %>% 
  filter(labour_force_characteristics == "Unemployment rate") %>% 
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  mutate(table = "prov_unempr",
         data_type = "Annual",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

### Prov Employment Rate ----

prov_empr_m <- lfs_data_monthly %>% 
  filter(labour_force_characteristics == "Employment rate") %>% 
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  filter(statistics == "Estimate") %>%
  filter(data_type %in% c("Seasonally adjusted", "Unadjusted")) %>%
  mutate(table = "prov_empr",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

prov_empr_a <- lfs_data_annual %>% 
  filter(labour_force_characteristics == "Employment rate") %>% 
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  mutate(table = "prov_empr",
         data_type = "Annual",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

### BC Labour Force Data by Age and Gender----

age_gender_m <- lfs_data_monthly %>%
  filter(geo_abb == "BC") %>%
  filter(labour_force_characteristics %in% 
           c("Employment", "Unemployment", 
             "Unemployment rate", "Participation rate","Employment rate")) %>%
  filter(age_group %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over")) %>%
  filter(statistics == "Estimate") %>%
  filter(data_type %in% c("Unadjusted", "Seasonally adjusted")) %>%
  mutate(table = ifelse(str_detect(labour_force_characteristics, "rate"),"age_gender_rate","age_gender"),
         age_group = ifelse(age_group == "15 years and over", "All Ages", as.character(age_group)),
         sex = ifelse(sex == "Both sexes", "Total", as.character(sex)),
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>%
  unique()

age_gender_a <- lfs_data_annual %>% 
  filter(geo_abb == "BC") %>%
  filter(labour_force_characteristics %in% 
           c("Employment", "Unemployment", 
             "Unemployment rate", "Participation rate","Employment rate")) %>%
  filter(age_group %in% c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over")) %>%
  mutate(table = ifelse(str_detect(labour_force_characteristics, "rate"),"age_gender_rate","age_gender"),
         data_type = "Annual",
         age_group = ifelse(age_group == "15 years and over", "All Ages", as.character(age_group)),
         sex = ifelse(sex == "Both sexes", "Total", as.character(sex)),
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>%
  unique() 

### BC Employment by FT/PT and Gender----

ftpt_gender_m <- lfs_data_monthly %>%
  filter(geo_abb == "BC") %>%
  filter(labour_force_characteristics %in% 
           c("Employment", "Full-time employment", "Part-time employment")) %>%
  filter(age_group == "15 years and over") %>%
  filter(statistics == "Estimate") %>%
  filter(data_type %in% c("Unadjusted", "Seasonally adjusted")) %>%
  mutate(table = "ftpt_gender",
         labour_force_characteristics = ifelse(labour_force_characteristics == "Employment", "Total", as.character(labour_force_characteristics)),
         sex = ifelse(sex == "Both sexes", "Total", as.character(sex)),
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

ftpt_gender_a <- lfs_data_annual %>%
  filter(geo_abb == "BC") %>%
  filter(labour_force_characteristics %in% 
           c("Employment", "Full-time employment", "Part-time employment")) %>%
  filter(age_group == "15 years and over") %>%
  mutate(table = "ftpt_gender",
         data_type = "Annual",
         labour_force_characteristics = ifelse(labour_force_characteristics == "Employment", "Total", as.character(labour_force_characteristics)),
         sex = ifelse(sex == "Both sexes", "Total", as.character(sex)),
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

### BC Employment by Industry ----

## Crosswalk between NAICS in data and cleaned
## some slight differences in names between monthly and annual data
industries_df <-data.frame(
  industries_m =  c("Total employed, all industries", "Goods-producing sector", 
                  "Agriculture [111-112, 1100, 1151-1152]", 
                  "Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]", 
                  "Utilities [22]", "Manufacturing [31-33]", "Construction [23]", 
                  "Services-producing sector", "Transportation and warehousing [48-49]", 
                  "Wholesale and retail trade [41, 44-45]", 
                  "Finance, insurance, real estate, rental and leasing [52-53]", 
                  "Professional, scientific and technical services [54]", "Educational services [61]", 
                  "Health care and social assistance [62]", 
                  "Accommodation and food services [72]", 
                  "Other services (except public administration) [81]", 
                  "Public administration [91]"),
  industries_a = c("Total, all industries", "Goods-producing sector", 
                    "Agriculture [111-112, 1100, 1151-1152]", 
                    "Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]", 
                    "Utilities [22]", "Manufacturing [31-33]", "Construction [23]", 
                    "Services-producing sector", "Transportation and warehousing [48-49]", 
                    "Wholesale and retail trade [41, 44-45]", 
                    "Finance, insurance, real estate, rental and leasing [52, 53]", 
                    "Professional, scientific and technical services [54]", "Educational services [61]", 
                    "Health care and social assistance [62]", 
                    "Accommodation and food services [72]", 
                    "Other services (except public administration) [81]", 
                    "Public administration [91]"),
  industries_clean = c("All industries", "Total goods-producing industries", 
                       "Agriculture", 
                       "Forestry, fishing, mining, quarrying, oil and gas", 
                       "Utilities", "Manufacturing", "Construction", 
                       "Total services-producing industries", "Transportation and warehousing", 
                       "Wholesale and retail trade", 
                       "Finance, insurance, real estate, rental and leasing", 
                       "Professional, scientific and technical services", "Educational services", 
                       "Health care and social assistance", 
                       "Accommodation and food services", 
                       "Other services (except public administration)", 
                       "Public administration"))
  

industry_m <- industries_df %>%
  left_join(industry_data_monthly, by = c("industries_m" = "north_american_industry_classification_system_naics")) %>%
  filter(geo == "British Columbia") %>%
  filter(statistics == "Estimate") %>%
  filter(data_type %in% c("Unadjusted", "Seasonally adjusted")) %>%
  mutate(table = "industry",
         labour_force_characteristics = "Employment",
         geo_abb = "BC",
         age_group = "15 years and over",
         sex = "Both sexes",
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics = industries_clean,
         national_occupational_classification_noc,
         class_of_worker) %>%
  unique()

industry_a <- industries_df %>%
  left_join(industry_data_annual, by = c("industries_a" = "north_american_industry_classification_system_naics")) %>%
  filter(geo == "British Columbia") %>%
  filter(labour_force_characteristics == "Employment") %>%
  filter(age_group == "15 years and over") %>%
  filter(sex == "Both sexes") %>%
  mutate(table = "industry",
         data_type = "Annual",
         geo_abb = "BC",
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics = industries_clean,
         national_occupational_classification_noc,
         class_of_worker) %>%
  unique()
  
### BC Employment and Unemployment Rate by Occupation ----

## crosswalk between NOC in data and cleaned
occupations_df <- data.frame(
  occupations = c("Total, all occupations [00-95]", 
                  "Management occupations [00, 10, 20, 30, 40, 50, 60, 70, 80, 90]",   
                  "Business, finance and administration occupations, except management [11-14]", 
                  "Natural and applied sciences and related occupations, except management [21-22]", 
                  "Health occupations, except management [31-33]", 
                  "Occupations in education, law and social, community and government services, except management [41-45]", 
                  "Occupations in art, culture, recreation and sport, except management [51-55]",
                  "Sales and service occupations, except management [62-65]",
                  "Trades, transport and equipment operators and related occupations, except management [72-75]",
                  "Natural resources, agriculture and related production occupations, except management [82-85]", 
                  "Occupations in manufacturing and utilities, except management [92-95]"),
  occupations_clean = c("All occupations", 
                        "Management occupations",
                        "Business, finance and administration occupations, except management",
                        "Natural and applied sciences and related occupations, except management",
                        "Health occupations, except management",
                        "Occupations in education, law and social, community and government services, except management",
                        "Occupations in art, culture, recreation and sport, except management",
                        "Sales and service occupations, except management",
                        "Trades, transport and equipment operators and related occupations, except management",
                        "Natural resources, agriculture and related production occupations, except management",
                        "Occupations in manufacturing and utilities, except management"))

occupation_m <- occupations_df %>%
  left_join(occupation_data_monthly, by = c("occupations" = "national_occupational_classification_noc")) %>%
  filter(geo == "British Columbia") %>%
  filter(labour_force_characteristics %in% c("Employment", "Unemployment rate")) %>%
  filter(sex == "Both sexes") %>%
  mutate(table = "occupation",
         data_type = "Unadjusted",
         geo_abb = "BC",
         age_group = "15 years and over",
         north_american_industry_classification_system_naics = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc = occupations_clean,
         class_of_worker) %>%
  unique()

occupation_a <- occupations_df %>%
  left_join(occupation_data_annual, by = c("occupations" = "national_occupational_classification_noc")) %>%
  filter(geo == "British Columbia") %>%
  filter(labour_force_characteristics %in% c("Employment", "Unemployment rate")) %>%
  filter(sex == "Both sexes") %>%
  mutate(table = "occupation",
         data_type = "Annual",
         geo_abb = "BC",
         age_group = "15 years and over",
         north_american_industry_classification_system_naics = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc = occupations_clean,
         class_of_worker) %>%
  unique()
  
### BC Employment and Unemployment by Region ----

region_m <- er_data_monthly %>%
  filter(str_detect(geo, "British Columbia")) %>%
  filter(labour_force_characteristics %in% c("Employment", "Unemployment rate")) %>%
  filter(statistics == "Estimate") %>%
  mutate(table = "region",
         data_type = "Unadjusted",
         geo = str_remove_all(geo, ", British Columbia"),
         geo_abb = "BC",
         age_group = "15 years and over",
         sex = "Both sexes",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique()  


region_a <- er_data_annual %>% 
  filter(str_detect(geo, "British Columbia")) %>%
  filter(labour_force_characteristics %in% c("Employment", "Unemployment rate")) %>%
  mutate(table = "region",
         data_type = "Annual",
         geo = str_remove_all(geo, ", British Columbia"),
         geo_abb = "BC",
         age_group = "15 years and over",
         sex = "Both sexes",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

### BC Employment and Unemployment by CMA ----

cma_m <- cma_data_monthly %>%
  filter(str_detect(geo, "British Columbia")) %>%
  filter(labour_force_characteristics %in% c("Employment", "Unemployment rate")) %>%
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  mutate(table = "cma",
         data_type = "Unadjusted",
         geo = str_remove_all(geo, ", British Columbia"),
         geo_abb = "BC",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique()  

cma_a <- cma_data_annual %>% 
  filter(str_detect(geo, "British Columbia")) %>%
  filter(labour_force_characteristics %in% c("Employment", "Unemployment rate")) %>%
  filter(sex == "Both sexes") %>%
  filter(age_group == "15 years and over") %>%
  mutate(table = "cma",
         data_type =  "Annual",
         geo = str_remove_all(geo, ", British Columbia"),
         geo_abb = "BC",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = NA) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 
  
### BC Employment by Class of Worker ----
cow_m <- cow_data_monthly %>%
  filter(geo == "British Columbia") %>%
  filter(statistics == "Estimate") %>%
  filter(sex == "Both sexes") %>%
  filter(data_type %in% c("Unadjusted", "Seasonally adjusted")) %>%
  mutate(table = "cow",
         labour_force_characteristics = "Employment",
         geo_abb = "BC",
         age_group = "15 years and over",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = as.character(class_of_worker)) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique()  

cow_a <- cow_data_annual %>% 
  filter(geo == "British Columbia") %>%
  filter(sex == "Both sexes") %>%
  filter(north_american_industry_classification_system_naics == "Total employed, all industries") %>%
  filter(!str_detect(class_of_worker, "paid")) %>%
  mutate(table = "cow",
         data_type = "Annual",
         labour_force_characteristics = "Employment",
         geo_abb = "BC",
         age_group = "15 years and over",
         north_american_industry_classification_system_naics = NA,
         national_occupational_classification_noc = NA,
         class_of_worker = as.character(class_of_worker)) %>%
  select(vector, table, labour_force_characteristics, data_type, geo_abb, geo, age_group, sex, 
         north_american_industry_classification_system_naics,
         national_occupational_classification_noc,
         class_of_worker) %>% 
  unique() 

## Combine all vectors ----

vector_metadata <- bind_rows(
  summary_vectors,
  bc_lfd_m,
  bc_lfd_a,
  prov_emp_m,
  prov_emp_a,
  prov_unempr_m,
  prov_unempr_a,
  prov_empr_m,
  prov_empr_a,
  age_gender_m,
  age_gender_a,
  ftpt_gender_m,
  ftpt_gender_a,
  industry_m,
  industry_a,
  occupation_m,
  occupation_a,
  region_m,
  region_a,
  cma_m,
  cma_a,
  cow_m,
  cow_a)

## Create factors ----

vector_metadata$labour_force_characteristics <- factor(vector_metadata$labour_force_characteristics,
                                               levels = c("Population", "Labour force", "Employment", "Unemployment", 
                                                          "Employment rate", "Unemployment rate", "Participation rate",  
                                                          "Full-time employment", "Part-time employment", "Total"))

vector_metadata$data_type <- factor(vector_metadata$data_type,
                            levels = c("Annual", "Unadjusted", "Seasonally adjusted")) 

vector_metadata$geo_abb <- factor(vector_metadata$geo_abb,
                          levels = c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL", "CAN"))

vector_metadata$geo <- factor(vector_metadata$geo, 
                      levels = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", 
                                 "Ontario", "Quebec", "New Brunswick", "Nova Scotia", 
                                 "Prince Edward Island", "Newfoundland and Labrador", "Canada",
                                 "Vancouver", "Victoria", "Abbotsford-Mission", "Kelowna",
                                 "Vancouver Island and Coast", "Lower Mainland-Southwest", 
                                 "Thompson-Okanagan", "Kootenay", "Cariboo", "North Coast and Nechako",
                                 "Northeast"))

vector_metadata$age_group <- factor(vector_metadata$age_group,
                            levels = c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over", "All Ages"))

vector_metadata$sex = factor(vector_metadata$sex,
                     levels = c("Both sexes", "Males", "Females", "Total"))

vector_metadata$north_american_industry_classification_system_naics <- factor(vector_metadata$north_american_industry_classification_system_naics,
                                                                      levels = c(NA, "All industries",
                                                                                 "Total goods-producing industries",
                                                                                 "Agriculture",
                                                                                 "Forestry, fishing, mining, quarrying, oil and gas",
                                                                                 "Utilities",  
                                                                                 "Manufacturing",
                                                                                 "Construction",
                                                                                 "Total services-producing industries", 
                                                                                 "Transportation and warehousing",
                                                                                 "Wholesale and retail trade",
                                                                                 "Finance, insurance, real estate, rental and leasing",
                                                                                 "Professional, scientific and technical services",
                                                                                 "Educational services",
                                                                                 "Health care and social assistance",
                                                                                 "Accommodation and food services",
                                                                                 "Other services (except public administration)",
                                                                                 "Public administration"))

vector_metadata$national_occupational_classification_noc <- factor(vector_metadata$national_occupational_classification_noc,
                                                           levels = c(NA, 
                                                                      "Management occupations",
                                                                      "Business, finance and administration occupations, except management",
                                                                      "Natural and applied sciences and related occupations, except management",
                                                                      "Health occupations, except management",                                                       
                                                                      "Occupations in education, law and social, community and government services, except management",                                                
                                                                      "Occupations in art, culture, recreation and sport, except management",      
                                                                      "Sales and service occupations, except management",
                                                                      "Trades, transport and equipment operators and related occupations, except management",
                                                                      "Natural resources, agriculture and related production occupations, except management",         
                                                                      "Occupations in manufacturing and utilities, except management",
                                                                      "All occupations")) 
vector_metadata$class_of_worker <- factor(vector_metadata$class_of_worker,
                                  levels = c(NA, "Total employed, all classes of workers",
                                             "Employees",
                                             "Public sector employees",
                                             "Private sector employees",
                                             "Self-employed"))

qs::qsave(vector_metadata, here("app", "vector_metadata.qs"))

