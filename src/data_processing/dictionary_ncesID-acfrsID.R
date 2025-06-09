library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(stringdist)
source("src/data_processing/nces.R")

dictionary_tmp <- readRDS("data/_dictionary_tmp.RDS") %>% 
  select(-name) %>% 
  distinct()

dictionary_fuzzy_manual <- readxl::read_xls("data/_dictionary_fuzzy_match1.xls") %>% 
  drop_na(state.abb) %>% 
  select(id, ncesID, state.abb)
 
dict_13 <- readxl::read_xls("data/_dictionary_13.xls") %>% 
   select(id, ncesID, state.abb) %>% 
   mutate(across(everything(), as.character))
 
 # In Montana, some separate school districts are reported in one acfrs
 # need to combine students from these sd to reflect true number of students that these acfrs cover
 dict_montana <- readxl::read_xlsx("data/_dictionary_montana_school_districts.xlsx") %>% 
   select(id, ncesID, state.abb) %>% 
   mutate(across(everything(), as.character))
 
# id in dictionary but no longer exist in acfr database
  id_nolonger_exist <- anti_join(dictionary_tmp, school_districts_all, by = "id") 
  
  
####Final merge####
  dictionary <- dictionary_tmp %>% 
    filter(!id %in% id_nolonger_exist$id) %>% 
    add_count(id) %>% filter(n == 1) %>% select(-n) %>% 
    add_count(ncesID) %>% filter(n == 1) %>% select(-n) %>% 
  
    # add fuzzy match result - strong match
    rbind(dictionary_fuzzy_manual) %>% 
    
    #add some trailing ones got fixed manually
    rbind(dict_13) %>% 
    
    #add montana
    rbind(dict_montana) %>% 
    
    mutate(ncesID = as.character(ncesID),
           ncesID = str_squish(ncesID)) %>% 
    
    mutate(ncesID = ifelse(nchar(ncesID) == 7 & str_detect(ncesID, "^0"), 
                           str_remove(ncesID, "^0"), 
                           ncesID)) %>% 
    
    distinct() %>% 
    #final correction: 
    mutate(ncesID = case_when(id == "161618" ~ "2622320",
                              TRUE ~ ncesID))
    

  saveRDS(dictionary, "data/dictionary.RDS")
 
  
  ####Final summary###
  
  nces_matched <- nces %>% 
    filter(ncesID %in% dictionary$ncesID) %>% 
    select(state.abb, name_nces, ncesID, county_nces, enrollment_23) 
  
  nces_not_matched <- nces %>% 
    filter(!ncesID %in% dictionary$ncesID) %>% 
    select(state.abb, name_nces, ncesID, county_nces, state_agency_id, agency_type, enrollment_23) 

      filter(enrollment_23 > 1000)
  
  sum(nces_not_matched$enrollment_23, na.rm = TRUE)/
    sum(nces$enrollment_23, na.rm = TRUE)
  