library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(stringdist)
source("src/data_processing/nces.R")

#####Dict_13 Manual adding#####
id_nolonger_exist <- read.csv("tmp/id_nolonger_exist.csv")

#This should overwrite all other code that generates dictionary
dict_13 <- readxl::read_xlsx("data/_dictionary_13.xlsx") %>% 
  select(id, ncesID, state.abb) %>% 
  mutate(across(everything(), as.character)) %>% distinct() 

#####Dict_14 Manual adding#####
dict_14 <- readxl::read_xlsx("data/_dictionary_14.xlsx") %>% 
  select(id, ncesID, state.abb) %>% 
  drop_na(id) %>% 
  #remove id no longer exist
  filter(!id %in% id_nolonger_exist$id) %>% distinct() %>% 
  mutate(ncesID = ifelse(nchar(ncesID) == 7 & str_detect(ncesID, "^0"), 
                         str_remove(ncesID, "^0"), 
                         ncesID)) %>% distinct()

#####Fuzzy match 1 #####
dictionary_fuzzy_manual <- readxl::read_xls("data/_dictionary_fuzzy_match1.xls") %>% 
  drop_na(state.abb) %>% 
  select(id, ncesID, state.abb) %>% 
  
  #exclude those already in dict_13, dict_14
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  filter(!ncesID %in% dict_13$ncesID) %>%
  filter(!ncesID %in% dict_14$ncesID) 

#####Fuzzy match 2 #####
dictionary_fuzzy_manual_2 <- readxl::read_xlsx("data/_dictionary_fuzzy_match_sep082025.xlsx") %>% 
  drop_na(state.abb) %>% 
  select(id, ncesID, state.abb) %>% 
  
  #exclude those already in dict_13, dict_14, dictionary_fuzzy_manual
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  filter(!ncesID %in% dict_13$ncesID) %>%
  filter(!ncesID %in% dict_14$ncesID)  %>% 
  filter(!id %in% dictionary_fuzzy_manual$id)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual$ncesID)  
  

#####Fuzzy match 3 #####
dictionary_fuzzy_manual_3 <- read_csv("data/_dictionary_fuzzy_manual_3.csv") %>% 
  drop_na(state.abb) %>% 
  select(id, ncesID, state.abb) %>% 
  
  #exclude those already in dict_13, dict_14, dictionary_fuzzy_manual, dictionary_fuzzy_manual_2
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  filter(!ncesID %in% dict_13$ncesID) %>%
  filter(!ncesID %in% dict_14$ncesID)  %>% 
  
  filter(!id %in% dictionary_fuzzy_manual$id)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual$ncesID)  %>% 
  
  filter(!id %in% dictionary_fuzzy_manual_2$id)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual_2$ncesID)  

#####Dict tmp (first batch) #####

dictionary_tmp <- readRDS("data/_dictionary_tmp.RDS") %>% 
  select(-name) %>% 
  drop_na(id) %>% 
  filter(ncesID != "099999") %>% 
  filter(!id %in% id_nolonger_exist$id) %>% 
  
  #exclude those already in dict_13, dict_14, dictionary_fuzzy_manual, dictionary_fuzzy_manual_2
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  filter(!ncesID %in% dict_13$ncesID) %>%
  filter(!ncesID %in% dict_14$ncesID)  %>% 
  
  filter(!id %in% dictionary_fuzzy_manual$id)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual$ncesID)  %>% 
  
  filter(!id %in% dictionary_fuzzy_manual_2$id)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual_2$ncesID)  %>% 

  filter(!id %in% dictionary_fuzzy_manual_3$id)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual_3$ncesID)  %>% distinct()

####Final merge####
dictionary <- rbind(dict_13, 
      dict_14, 
      dictionary_fuzzy_manual, 
      dictionary_fuzzy_manual_2, 
      dictionary_fuzzy_manual_3,
      dictionary_tmp
      ) %>% distinct() %>% 
  drop_na(id, ncesID) %>%  
  mutate(ncesID = case_when(id == "1269433" ~ "618870",#CA Jefferson Elementary School District (San Mateo)
                            id == "67707" ~ "630520", #CA pioneer union school district (butte county)
                            TRUE ~ as.character(ncesID))) %>%  

  mutate(id = case_when(ncesID == "1802040" ~ "93917", #IN Northwestern School Corporation
                         ncesID == "3173230" ~ "190314", #NE Madison Public Schools District No. 1
                         ncesID == "3800406" ~ "1239333", #ND grafton public school district no. 18
                        ncesID == "630520" ~ "67707",
                         TRUE ~ as.character(id))) %>% distinct()
  
  
  # check duplicates   
  dictionary %>% 
    filter(duplicated(ncesID) | duplicated(ncesID, fromLast = TRUE)) %>%
    select(ncesID, id) 
  
  dictionary %>% 
    filter(duplicated(id) | duplicated(id, fromLast = TRUE)) %>%
    select(ncesID, id) 
  
  saveRDS(dictionary, "data/dictionary.RDS")
 
  
####Summary####
  
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
  