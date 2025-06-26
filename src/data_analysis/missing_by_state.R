library(tidyverse)
library(dplyr)
library(janitor)
library(writexl)
source("src/data_processing/functions.R")
source("src/data_processing/census.R")
source("src/data_processing/nces.R")
source("src/data_processing/exceptions.R")
options(scipen = 9999)

####County####
county_by_state <- county_census_acfr_2023 %>% 
  mutate(geo_id = paste0(state, county)) %>% 
  select(state.abb, state.name, id, name, name_census, population, geo_id) %>% 
  group_by(state.abb) %>% 
  summarise(pop_census = sum(population, na.rm = TRUE),
            count_census = n_distinct(geo_id),
            
            pop_acfr = sum(ifelse(!is.na(id), population, 0), na.rm = TRUE),
            count_acfr = sum(!is.na(id)),
            
            .groups = "drop") %>% 
  
  mutate(pop_pct_accounted = round((pop_acfr/pop_census)*100,2),
         count_pct_accounted = round((count_acfr/count_census)*100,2))

####Municipality####
municipality_final_2023 %>% filter(!str_detect(name, "township|town")) %>% 
  summarise(tot = sum(population, na.rm = TRUE))

municipality_final_2023 %>% filter(state.abb == "HI") %>% View()

#This subset includes active incorporated places, minor civil divisions, and other local government units
#It filters out inactive or fictitious entities (funcstat != "F")
municipality_census <- census_all %>% 
 filter(funcstat != "F") %>% 
  filter(sumlev %in% c(162, 170, 61)) %>% 
  select(state.abb, state.name, name_census, population, sumlev) %>% 
  group_by(state.abb) %>% 
  summarise(pop_census = sum(population, na.rm = TRUE),
            count_census = n_distinct(name_census, sumlev),
            .groups = "drop") %>% 
  
  mutate(pop_census = ifelse(state.abb == "HI", 1016506, pop_census))

municipality_acfr <- municipality_final_2023 %>% 
  select(state.abb, state.name, id, population) %>% 
  group_by(state.abb) %>% 
  summarise(pop_acfr = sum(population, na.rm = TRUE),
            count_acfr = n_distinct(id),
            .groups = "drop")

municipality_by_state <- municipality_census %>% left_join(municipality_acfr) %>% 
  mutate(pop_pct_accounted = round((pop_acfr/pop_census)*100, 2),
         count_pct_accounted = round((count_acfr/count_census)*100,2)) 

####School####

school_acfr <- school_districts_final_2023 %>% 
  select(state.abb, state.name, id, enrollment_23) %>% 
  group_by(state.abb) %>% 
  summarise(pop_acfr = sum(enrollment_23, na.rm = TRUE),
            count_acfr = n_distinct(id),
            .groups = "drop") %>% 
  mutate(count_acfr = ifelse(state.abb == "DC", 0, count_acfr))

school_nces <- nces %>% 
  select(state.abb, state.name, ncesID, enrollment_23) %>% 
  group_by(state.abb) %>% 
  summarise(pop_census = sum(enrollment_23, na.rm = TRUE),
            count_census = n_distinct(enrollment_23),
            .groups = "drop") 

school_by_state <- school_acfr %>% left_join(school_nces) %>% 
mutate(pop_pct_accounted = round((pop_acfr/pop_census)*100, 2),
       count_pct_accounted = round((count_acfr/count_census)*100,2)) 

county_by_state %>% write.csv("tmp/county_by_state.csv")
municipality_by_state %>% write.csv("tmp/municipality_by_state.csv")
school_by_state %>% write.csv("tmp/school_by_state.csv")
