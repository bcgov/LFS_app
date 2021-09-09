library(bcdata)
library(sf)
library(bcmaps)
library(rmapshaper)
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
  rmapshaper::ms_clip(bcmaps::bc_bound(class = "sf"))

## cmas 
# census metropolitan areas spatial data from the B.C. Data Catalogue using the bcdata package 
# https://catalogue.data.gov.bc.ca/dataset/a6fb34b7-0937-4718-8f1f-43dba2c0f407
cmas <- 
  bcdc_get_data("a6fb34b7-0937-4718-8f1f-43dba2c0f407") %>%
  clean_names() %>%
  filter(census_metro_area_name %in% c("Kelowna", "Abbotsford - Mission", "Vancouver", "Victoria")) %>%
  mutate(geo = str_remove_all(census_metro_area_name, " "))

qs::qsave(economic_regions, here::here("app", "economic_regions.qs"))
qs::qsave(cmas, here::here("app", "cmas.qs"))
