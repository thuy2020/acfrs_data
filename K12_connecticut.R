library(dplyr)
library(stringr)
library(rio)
library(tidyr)
source("nces.R")

####Apportion SD based on full-time equivalent empployee

CT_general_purpose <- acfrs_general_purpose %>% filter(state.abb == "CT") %>% 
  select(-c(source_year, category, nces_district_id, government_id, identifier, name_midfile)) %>% 
  #filter(year == 2022) %>% 
  #select(state.abb, name, id, year, net_pension_liability, net) %>% 
  mutate(name = str_squish(name)) %>% 
  rename(name_general_purpose = name)

# CT has 178 school district
CT_nces <- nces %>% filter(state.abb == "CT") %>% 
  select(name_nces, ncesID, enrollment_22) %>% 
  drop_na(enrollment_22) %>% 
  filter(name_nces != "department of mental health and addiction services") %>% 
  
  #regional sd have ACFR, no need to apportion
  filter(!str_detect(name_nces, "(regional)|(capitol region)")) %>% 
  mutate(name_school_district = name_nces) %>% 

  #clean the name to match with general purpose entities
  mutate(name_nces = str_remove(name_nces, "school district")) %>% 
  mutate(name_nces = str_squish(name_nces)) 
  

CT_ <- CT_general_purpose %>% left_join(CT_nces, by = c("name_general_purpose" = "name_nces")) 
# full-time employee equivalent 

CT_employee <- readxl::read_xlsx("data/CT_education_total_full time equivalent.xlsx")

CT_employee %>% left_join(CT_, by = c("name_general_purpose", "year")) %>% 
  write.csv("tmp/CT_K12_apportion.csv")


####### SD that produce ACFRs

CT_dictionary <- readRDS("data/dictionary.RDS") %>% 
  select(-name) %>% 
  distinct() %>% filter(state.abb == "CT")

school_districts %>% filter(state.abb == "CT") %>% write.csv("tmp/CT_K12_reportedACFRs.csv")






anti_join(school_districts %>% 
            filter(year == 2020) %>% 
            filter(state.abb == "CT") %>% select(state.abb, name, id, net_pension_liability),
          
          school_districts %>% 
            filter(year == 2023) %>% 
            filter(state.abb == "CT") %>% select(state.abb, name, id, net_pension_liability), by = "id"
  
) %>% View()
