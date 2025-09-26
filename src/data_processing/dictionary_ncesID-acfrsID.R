library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(stringdist)
source("src/data_processing/nces.R")

#####Dict_13 Manual adding#####
id_nolonger_exist <- read.csv("tmp/id_nolonger_exist.csv")
id_nolonger_exist_2 <- c("30577", "145929", "399390", "146511", "1269705", "1268252", 
                         "93770", "82096", "81333", "1268783", "1268144", "1270501",
                         "1240267", "1268500", "1206881")

#This should overwrite all other code that generates dictionary
dict_13 <- readxl::read_xlsx("data/_dictionary_13.xlsx") %>% 
  select(id, ncesID, state.abb) %>% 
  mutate(across(everything(), as.character)) %>% 
 # filter(!id %in% id_nolonger_exist$id) %>% 
  distinct() 


#####Dict_14 Manual adding#####
dict_14 <- readxl::read_xlsx("data/_dictionary_14.xlsx") %>% 
  select(id, ncesID, state.abb) %>% 
  drop_na(id) %>% 
  mutate(ncesID = ifelse(nchar(ncesID) == 7 & str_detect(ncesID, "^0"), 
                         str_remove(ncesID, "^0"), 
                         ncesID)) %>% distinct() %>% 
#remove id no longer exist
  filter(!id %in% id_nolonger_exist$id) %>% distinct() %>% 
  
  filter(!id %in% dict_13$id) %>% 
  filter(!ncesID %in% dict_13$ncesID)


#####Dict_15 Manual adding#####
dict_15 <- read_csv("data/_dictionary_15.csv") %>% 
  drop_na(id, ncesID) %>% 
  select(-name) %>% 
  filter(!id %in% id_nolonger_exist$id) %>% 
  
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  
  filter(!ncesID %in% dict_13$ncesID) %>% 
  filter(!ncesID %in% dict_14$ncesID)


#####Fuzzy match 1 #####
dictionary_fuzzy_manual <- readxl::read_xls("data/_dictionary_fuzzy_match1.xls") %>% 
  drop_na(state.abb) %>% 
  select(id, ncesID, state.abb) %>%
    
  filter(!id %in% id_nolonger_exist$id) %>% 
  #exclude those already in dict_13, dict_14
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  filter(!id %in% dict_15$id) %>% 
  
  filter(!ncesID %in% dict_13$ncesID) %>%
  filter(!ncesID %in% dict_14$ncesID) %>% 
  filter(!ncesID %in% dict_15$ncesID) 

#####Fuzzy match 2 #####
dictionary_fuzzy_manual_2 <- readxl::read_xlsx("data/_dictionary_fuzzy_match_sep082025.xlsx") %>% 
  drop_na(state.abb) %>% 
  select(id, ncesID, state.abb) %>% 
  
  filter(!id %in% id_nolonger_exist$id) %>% 
  #exclude those already in dict_13, dict_14, dictionary_fuzzy_manual
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  filter(!id %in% dict_15$id) %>%
  filter(!id %in% dictionary_fuzzy_manual$id)  %>% 
  
  filter(!ncesID %in% dict_13$ncesID) %>%
  filter(!ncesID %in% dict_14$ncesID)  %>% 
  filter(!ncesID %in% dict_15$ncesID)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual$ncesID)   


#####Fuzzy match 3 #####
dictionary_fuzzy_manual_3 <- read_csv("data/_dictionary_fuzzy_manual_3.csv") %>% 
  drop_na(state.abb) %>% 
  select(id, ncesID, state.abb) %>% 
    
  filter(!id %in% id_nolonger_exist$id) %>% 
  #exclude those already in dict_13, dict_14, dictionary_fuzzy_manual, dictionary_fuzzy_manual_2
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  filter(!id %in% dict_15$id) %>% 
  filter(!id %in% dictionary_fuzzy_manual$id) %>%
  filter(!id %in% dictionary_fuzzy_manual_2$id) %>% 
  
  
  filter(!ncesID %in% dict_13$ncesID) %>%
  filter(!ncesID %in% dict_14$ncesID)  %>% 
  filter(!ncesID %in% dict_15$ncesID)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual$ncesID) %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual_2$ncesID)  

  
#####Dict tmp (first batch) #####
dictionary_tmp <- readRDS("data/_dictionary_tmp.RDS") %>% 
  select(-name) %>% 
  drop_na(id) %>% 
  filter(ncesID != "099999") %>% 
  filter(!id %in% id_nolonger_exist$id) %>% 
  
  #exclude 17 entries with mistake - too long ncesID
  mutate(ncesID = ifelse(nchar(ncesID) >7, 
         substr(ncesID, 1, 7),
         ncesID)) %>% 
  #take out some wrong pair
  filter(!id %in% c("68544","68709")) %>% 

  #exclude those already in dict_13, dict_14, dictionary_fuzzy_manual, dictionary_fuzzy_manual_2
  filter(!id %in% dict_13$id) %>% 
  filter(!id %in% dict_14$id) %>% 
  filter(!id %in% dict_15$id) %>% 
  filter(!id %in% dictionary_fuzzy_manual$id) %>%
  filter(!id %in% dictionary_fuzzy_manual_2$id) %>% 
  filter(!id %in% dictionary_fuzzy_manual_3$id) %>% 
  
  
  filter(!ncesID %in% dict_13$ncesID) %>%
  filter(!ncesID %in% dict_14$ncesID)  %>% 
  filter(!ncesID %in% dict_15$ncesID)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual$ncesID) %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual_2$ncesID)  %>% 
  filter(!ncesID %in% dictionary_fuzzy_manual_3$ncesID)  %>% distinct()

dictionary_tmp %>% add_count(id) %>% filter(n>1)

####Final merge####
dictionary <- rbind(dict_13, 
      dict_14, 
      dict_15,
      dictionary_fuzzy_manual, 
      dictionary_fuzzy_manual_2, 
      dictionary_fuzzy_manual_3,
      dictionary_tmp) %>% 
    mutate(ncesID = ifelse(nchar(ncesID) == 7 & str_detect(ncesID, "^0"), 
                           str_remove(ncesID, "^0"), 
                           ncesID)) %>% 
    filter(!id %in% id_nolonger_exist_2) %>% 
    
    #final fix
    mutate(id = case_when(ncesID == "630480" ~ "67708",
                          TRUE ~ id)) %>% #CA Pioneer Union School District (El Dorado County)
    
    distinct() %>% 
    drop_na(id, ncesID) 


  dictionary %>% 
    filter(!id %in% id_nolonger_exist$id) %>% 
    filter(duplicated(ncesID) | duplicated(ncesID, fromLast = TRUE)) %>%
    select(ncesID, id, state.abb) 
  #TODO: only 1 case of ND grafton 3 - grafton 18? which one exists?
  
  
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
  