library(tidyverse)
library(dplyr)
library(janitor)
library(jsonlite)
library(stringr)
library(forcats)
source("census.R")
source("general_purpose.R")
source("nces.R")
source("exceptions.R")
source("functions.R")
options(scipen = 999)

# only select some fields to display on datatool
fields_to_select <- c("state.abb", "state.name", "id", "geo_id", "year", "name", 
                      "identifier", "category",
                      "total_liabilities", "current_liabilities",
                      "net_pension_liability", "net_pension_assets",
                      "net_opeb_liability", "net_opeb_assets", 
                      "total_assets", "current_assets", "compensated_absences",
                      "expenses", "revenues",
                      "unrestricted",
                      "population", "urban_pop", "pct_urban_pop", "median_hh_income_21")


####State######


####County####



####Incorporated Place & Minor Civil Division####



#Top 200
top200_cities <- city_gov %>% filter(year == 2023) %>% View()
  filter((geo_id %in% census_city_top200$geo_id) | 
           name == "district of columbia") %>% distinct() %>% 
  mutate(population = ifelse(name == "district of columbia", 689546, population)) 
  #filter(year != 2023)

#missing cities 2023
missing_city <- top200_cities %>% add_count(geo_id) %>% filter(n<4) %>% 
  arrange(state.abb, name, year) %>% 
  select(state.name, name, year, id, n) 

read.csv("tmp/top200cities_missing2023.csv") %>% 
  filter(X.1 != "uploaded 2023") %>% 
  filter(!year %in% c(2020, 2021)) %>% 
  select(state.name, name) %>% distinct() %>% write.csv("tmp/missing_cities_2023.csv")


####School districts####
dictionary <- readRDS("data/dictionary.RDS")

school_districts_ <- readRDS("data/acfrs_data.RDS") %>% 
  filter(name == "norfolk public schools")
  
  filter(category == "School District") %>% 
  mutate(id = as.character(id)) %>% 
  select(any_of(fields_to_select)) 

#append URLs

school_districts <- append_url(school_districts_) %>% select(-identifier)

# filter only top 100
dict_top100_ELSI <- dictionary %>% 
  filter(ncesID %in% top_schools_by_year$ncesID) %>% 
  drop_na(id) %>% select(-name)

top100_school_districts <- school_districts %>% 
  filter(id %in% dict_top100_ELSI$id) %>% 
  left_join(dict_top100_ELSI, by = c("id",  "state.abb")) %>% 
  
  #join with nces to get county, city info
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) %>% 
  
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() 


# top 200

dict_top200_ELSI <- dictionary %>% 
  filter(ncesID %in% top200_schools_by_year$ncesID) %>% 
  drop_na(id) %>% select(-name)

top200_school_districts <- school_districts %>% 
  filter(id %in% dict_top200_ELSI$id) %>% 
  left_join(dict_top200_ELSI, by = c("id",  "state.abb")) %>% 
  
  #join with nces to get county, city info
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) %>% 
  
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() 

#top 300
top300_schools_by_year

dict_top300_ELSI <- dictionary %>% 
  filter(ncesID %in% top300_schools_by_year$ncesID) %>% 
  drop_na(id) %>% select(-name)

top300_school_districts <- school_districts %>% 
  filter(id %in% dict_top300_ELSI$id) %>% 
  left_join(dict_top300_ELSI, by = c("id",  "state.abb")) %>% 
  
  #join with nces to get county, city info
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) %>% 
  
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() 


#TODO: check back missing: 
#GA Clayton County Board of education.
#https://www.clayton.k12.ga.us/departments/business-services/financial-reports
# Uploaded: williamson county schools, reported in county acfrs. Uploaded county's acfrs to replace


missing_sd <- top200_school_districts %>% 
  add_count(ncesID) %>% filter(n < 4) %>% 
  select(state.abb, ncesID, year, name, n, id) 
  
missing_sd_top300 <- top300_school_districts %>% 
  add_count(ncesID) %>% filter(n < 4) %>% 
  select(state.abb, ncesID, year, name, n, id) 


top200_school_districts %>% write.csv("output/top200_sd.csv")
top100_school_districts %>% write.csv("output/top100_sd.csv")
school_districts %>% write.csv("output/all_schooldistricts_3years.csv")
#TODO: ask Geoff about this revenues = expense cases




