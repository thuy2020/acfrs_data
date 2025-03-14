library(tidyverse)
library(dplyr)
library(janitor)

options(scipen = 999)

fields_to_display <- c("state.abb",  "state.name"     ,"id"    ,"year",
  "name"                  ,"category"              ,"total_liabilities"     ,"current_liabilities"  ,
  "net_pension_liability" ,"net_pension_assets"    ,"net_opeb_liability"   , "net_opeb_assets"      ,
  "total_assets"          ,"compensated_absences",  "expenses"             ,
  "revenues"              ,"url"  )


data_for_acfr_shinyapp <- rbind(
  state_all %>% 
    select(all_of(fields_to_display), population) %>% 
    mutate(category = "State"),
  county_all %>% select(all_of(fields_to_display), population) %>% 
    mutate(category = "Counties"),
  municipality_all %>% select(all_of(fields_to_display), population) %>% 
    mutate(category = "Municipalities"),
  school_districts_all %>% 
    select(all_of(fields_to_display), enrollment_22) %>% 
    mutate(category = "School Districts") %>% 
    rename(population = enrollment_22) 
) %>% 
  filter(!state.name %in% c("Federated States of Micronesia", "Guam", "Puerto Rico"))


#%>% 
  #filter(year != 2023)

# check

state_all %>% 
  filter(state.abb == "MI") %>% View()
  
  county_all %>% 
    filter(state.abb == "MI") %>% 
    group_by(year) %>% 
    summarise(count= n()) %>% 
    View()
  
  anti_join((county_all %>% 
              filter(state.abb == "MI" & year == 2022)), 
            (county_all %>% 
               filter(state.abb == "MI" & year == 2023)), by = "id") %>% View()
  

  municipality_all %>% 
    filter(state.abb == "MI") %>% 
    group_by(year) %>% 
    summarise(count= n()) %>% 
    View()
  
  anti_join((municipality_all %>% 
               filter(state.abb == "MI" & year == 2022)), 
            (municipality_all %>% 
               filter(state.abb == "MI" & year == 2023)), by = "id") %>% View()
  
  
  school_districts_all %>% 
    filter(state.abb == "MI") %>% 
    group_by(year) %>% 
    summarise(count= n()) %>% 
    View()
  
  anti_join((school_districts_all %>% 
               filter(state.abb == "MI" & year == 2022)), 
            (school_districts_all %>% 
               filter(state.abb == "MI" & year == 2023)), by = "id") %>% View()
  
  
data_for_acfr_shinyapp %>% saveRDS("data/data_for_acfr_shinyapp.RDS")
