library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(stringdist)
source("src/data_processing/nces.R")

#####Dic_13 Manual adding#####

#This should over write all other code that generates dictionary
dict_13 <- readxl::read_xlsx("data/_dictionary_13.xlsx") %>% 
  select(id, ncesID, state.abb) %>% 
  mutate(across(everything(), as.character))

dict_14 <- readxl::read_xlsx("data/_dictionary_14.xlsx") %>% 
  select(id, ncesID, state.abb)


dictionary_tmp <- readRDS("data/_dictionary_tmp.RDS") %>% 
  select(-name) %>% 
  distinct() %>% 
  filter(!id %in% c(dict_13$id, dict_14$id)) %>% 
  filter(!ncesID %in% c(dict_13$ncesID, dict_14$ncesID)) 


#####Fuzzy match#####
dictionary_fuzzy_manual <- readxl::read_xls("data/_dictionary_fuzzy_match1.xls") %>% 
  drop_na(state.abb) %>% 
  select(id, ncesID, state.abb) %>% 
  filter(!id %in% dict_13$id) %>% 
  filter(!ncesID %in% dict_13$ncesID)

 
# id in dictionary but no longer exist in acfr database due to deleted, merged acfrs 
  id_nolonger_exist <- (anti_join(dictionary_tmp, school_districts_all, by = "id"))
  
  id_delted <- c("1238328")
  
#####Montana#####
#In Montana, some separate school districts are reported in one acfrs
# so one acfr id get to map to 2 nces school districts
  #--> resolve this in exceptions.R

  
####Final merge####
  dictionary <- dictionary_tmp %>% 
    filter(!id %in% id_nolonger_exist$id) %>% 
    add_count(ncesID) %>% filter(n == 1) %>% select(-n) %>% 
  
    # add fuzzy match result - strong match
    rbind(dictionary_fuzzy_manual) %>% 
    
    #add the ones got fixed manually
    rbind(dict_13) %>% 
    rbind(dict_14) %>% 
    
    mutate(ncesID = as.character(ncesID),
           ncesID = str_squish(ncesID)) %>% 
    
    mutate(ncesID = ifelse(nchar(ncesID) == 7 & str_detect(ncesID, "^0"), 
                           str_remove(ncesID, "^0"), 
                           ncesID)) %>% 
    
    distinct() 
  
  
  # there are duplicated ncesID, but this does not cause harm. Some ncesID mapped to 2 acfr id. But 1 of 
  #the acfr id no longer exist.   
  dup_ncesid <- dictionary %>% 
    filter(duplicated(ncesID) | duplicated(ncesID, fromLast = TRUE)) %>%
    select(ncesID, id)
  
  dictionary %>% 
    filter(duplicated(id) | duplicated(id, fromLast = TRUE)) %>%
    select(ncesID, id) %>% View()
  
  
  saveRDS(dictionary, "data/dictionary.RDS")
 
  
  ####Final summary###
  
  nces_matched <- nces %>% 
    filter(ncesID %in% dictionary$ncesID) %>% 
    select(state.abb, name_nces, ncesID, county_nces, enrollment_23) 
  
  #remaining NCES entities NOT matched with acfrs
  nces_not_matched <- nces %>% 
    filter(!ncesID %in% dictionary$ncesID) %>% 
    select(state.abb, name_nces, ncesID, county_nces, state_agency_id, agency_type, enrollment_23) 
      #filter(enrollment_23 > 1000)
  
  # This rate does not account for NYC, Boston and few other
  sum(nces_not_matched$enrollment_23, na.rm = TRUE)/
    sum(nces$enrollment_23, na.rm = TRUE)
  