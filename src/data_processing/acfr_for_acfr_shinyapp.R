library(tidyverse)
library(dplyr)
library(janitor)

options(scipen = 999)

fields_to_display <- c("state.abb",  "state.name"     ,"id"    ,"year",
  "name"                  ,"category"              ,"total_liabilities"     ,"current_liabilities"  ,
  "net_pension_liability" ,"net_pension_assets"    ,"net_opeb_liability"   , "net_opeb_assets"      ,
  "total_assets"          ,"compensated_absences",  "expenses"             ,
  "revenues"              ,"url"  )


d <- rbind(
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
  filter(year != 2023)

d %>% saveRDS("data/data_for_acfr_shinyapp.RDS")

