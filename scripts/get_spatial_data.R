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
library(bcdata)
library(sf)
library(bcmaps)
library(rmapshaper)
library(janitor)
#library(viridis)

#economic regions spatial data from the B.C. Data Catalogue using the bcdata package
# https://catalogue.data.gov.bc.ca/dataset/1aebc451-a41c-496f-8b18-6f414cde93b7
economic_regions <-
  bcdc_get_data("1aebc451-a41c-496f-8b18-6f414cde93b7") %>%
  clean_names() %>%
  mutate(geo = case_when(economic_region_id == 5910 ~ "Vancouver Island and Coast",
                         economic_region_id == 5920 ~ "Lower Mainland-Southwest",
                         economic_region_id == 5930 ~ "Thompson-Okanagan",
                         economic_region_id == 5940 ~ "Kootenay",
                         economic_region_id == 5950 ~ "Cariboo",
                         economic_region_id == 5960 ~ "North Coast and Nechako",
                         economic_region_id == 5970 ~ "North Coast and Nechako",
                         economic_region_id == 5980 ~ "Northeast")) %>%
  group_by(geo) %>%
  summarise() %>%
  rmapshaper::ms_clip(bcmaps::bc_bound(class = "sf")) %>%
  ms_simplify(keep = 0.075, sys = TRUE)

## cmas 
# census metropolitan areas spatial data from the B.C. Data Catalogue using the bcdata package 
# https://catalogue.data.gov.bc.ca/dataset/a6fb34b7-0937-4718-8f1f-43dba2c0f407
cmas <- 
  bcdc_get_data("a6fb34b7-0937-4718-8f1f-43dba2c0f407") %>%
  clean_names() %>%
  filter(census_metro_area_name %in% c("Kelowna", "Abbotsford - Mission", "Vancouver", "Victoria")) %>%
  mutate(geo = str_remove_all(census_metro_area_name, " ")) 

bc <- bc_bound() %>%
  select(-island) %>%
  mutate(id = row_number()) %>%
  ms_simplify(keep = 0.25, sys = TRUE)

qs::qsave(economic_regions, here::here("app", "economic_regions.qs"))
qs::qsave(cmas, here::here("app", "cmas.qs"))
qs::qsave(bc, here::here("app", "bc.qs"))

## health authorities
# https://catalogue.data.gov.bc.ca/dataset/7bc6018f-bb4f-4e5d-845e-c529e3d1ac3b
has <-
  bcdc_get_data('7bc6018f-bb4f-4e5d-845e-c529e3d1ac3b', resource = 'dfd14c9b-45f8-4a7e-ad42-9a881778e417') %>%
  clean_names() 

