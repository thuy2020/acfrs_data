
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
options(scipen = 999)

# Problem: School districts ACFRS database have names that are different from the names in NCES.
 
# Goal: construct a dictionary that links entity name, acfrs id, nces id

# Input data: 
#1. using result from round 1 (mapping based on regular expression)
#2. Hgarb file
 
# acfrs data 
sd_db <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "School District") %>% 
  rename(ncesID = nces_district_id) %>% 
  # for sd collected ID that has only 6 digits, adding a leading 0
  mutate(ncesID = ifelse(str_length(ncesID) == 6, paste0("0", ncesID), ncesID)) %>% 
  select(id, state, name, ncesID) %>% 
  distinct()


###########Dictionary 1: names matching with NCES############

# First, filter school districts in ACFRs database that have NCES ID. 
#These ncesIDs were added by Thuy using regular expression and matching names with nces in the first round. 
#Ron integrated it into the database. 

dictionary1 <-  sd_db %>% 
  filter(!is.na(ncesID))

# the remaining that need nces ID
acfrs_wo_ncesID <-  sd_db %>% filter(is.na(ncesID)) %>% select(id, state, name) 


##########Dictionary 2: Using Hgarb collected############
#This file was collected by Hgarb (extending from the file Thuy matched names), has NCES ID and acfrs entity name

 
# HGarb team emailed, added a couple thousand on Thuy's matched list. 
sd2020_namematched_Hgarb <- rio::import("data/_K12 School District 12_01_22 - Consolidated.csv") %>% 
  select(-c(Comments, `Employer name in GASB 68/ACFR`, `V13`)) %>% 
  rename(ncesID = `NCES ID`, 
         name = `School District (Portal Name)`,
         state = State) %>% 
  mutate(ncesID = as.character(ncesID)) %>% 
  
  # removing a total line at bottom
  filter(name != "Totals") %>% 
  # for sd collected ID that has only 6 digits, adding a leading 0
  mutate(ncesID = ifelse(str_length(ncesID) < 7, paste0("0", ncesID), ncesID)) %>% 
  
  # special cases 
  mutate(ncesID = ifelse(ncesID == "4833090", "4833120", ncesID)) # Hgarb attributed wrong NCES ID to this entity --> its nces ID should be 4833120

# part of the list of Hgarb whose ncesIDs are not in dictionary 1
dictionary2 <- sd2020_namematched_Hgarb %>% 
  filter(!ncesID %in% dictionary1$ncesID) %>% 
  
  # this list has ncesID, acfrs name, --> need to use acfrs names to join with acfrs db list
  select(ncesID, state, name) %>% 
  left_join(acfrs_wo_ncesID) %>% 
  filter(!is.na(id)) %>% 
  filter(!is.na(ncesID))

dictionary1_2 <- dictionary1 %>% rbind(dictionary2) 

############Dictionary 3 ###############
#join left-over of Hgarb list & acfrs db list

 
# list of Hgarb whose ncesID are not in dictionary1_2
sd2020_namematched_Hgarb %>% 
  filter(!ncesID %in% dictionary1_2$ncesID) %>% 
  select(ncesID, state, name) %>% filter(!is.na(ncesID)) %>% arrange(state, name) %>% 
  
  #normalize name 
  mutate(name = str_to_lower(name),
         name = str_remove_all(name, "-|'"),
         name = str_remove_all(name, "no."),
         name = str_replace_all(name, "\\.", " "),
         name = str_trim(name),
         name = str_squish(name)) -> t1


# list of acfrs whose id are not in dictionary1_2
 sd_db %>% 
  filter(!id %in% dictionary1_2$id) %>% filter(!is.na(id)) %>% 
  select(id, state, name, ncesID) %>% 
  arrange(state, name) %>% 
  
  mutate(name = str_to_lower(name),
         name = str_remove_all(name, "-|'"),
         name = str_remove_all(name, "no."),
         name = str_replace_all(name, "/", " "),
         name = str_squish(name),
         name = str_trim(name)) -> t2


dictionary_3 <- t1 %>% left_join(t2, by = c("name", "state")) %>% drop_na(id) %>% 
  rename(ncesID = ncesID.x) %>% 
  select(-ncesID.y) %>% arrange(state, name)

dictionary123 <- dictionary1_2 %>% rbind(dictionary_3) %>% 
  
  # fixing some duplicated values 
  mutate(ncesID = case_when(id == "87509" ~ "4222620", # get from NCES website
                            id == "190896" ~ "4026010",
                            TRUE ~ as.character(ncesID)
  )) %>% arrange(state, name) %>% drop_na()

#########Dictionary 4##########
#Manual tracking in excel

# acfrs need ncesID
 sd_db %>% filter(!id %in% dictionary123$id) %>% select(state, name, id, ncesID) %>%
  arrange(state, name) #%>% write.csv("acfrs_need_add_ncesID.csv")

#hgarb leftover
sd2020_namematched_Hgarb %>% filter(!ncesID %in% dictionary123$ncesID) %>%
  drop_na(ncesID) %>% select(state, name, ncesID, acfrs_original_name) %>%
  arrange(state, name) #%>% write.csv("hgarb_leftover.csv")

# manually matching the above 2 files --> resulted in dictionary_4
dictionary_4 <- read.csv("data/_dictionary_4.csv") %>% 
  filter(!is.na(ncesID)) %>% 
  select(-ncesName)

dictionary1234 <- dictionary123 %>% rbind(dictionary_4)

##########Dictionary 5 ##########
#manual checking against NCES list
 
# filter nces that are not in dictionary1234
nces_NOT_in_dictionary1234 <- nces %>% filter(!ncesID %in% dictionary1234$ncesID) %>% 
  select(state, nces_original_name, ncesID, students) %>% 
  arrange(state, nces_original_name) #%>% write.csv("nces_NOT_in_dictionary1234.csv")

acfrs_NOT_in_dictionary1234 <-  sd_db %>% filter(!id %in% dictionary1234$id) %>% 
  select(state, name, id) %>% 
  arrange(state, name) #%>% 
#write.csv("acfrs_NOT_in_dictionary1234.csv")

# now use above 2 lists to fill in the remaining acfrs entities manually. 

#Copy acfrs_NOT_in_dictionary1234 into an excel --> search on NCES website to find nces ID & student. 
#If entity not found on NCES, check ACFRs reports on portal to find number of students. 
# Note: ncesID = 99999 indicates an existing ACFR entity without an NCES ID

dictionary5 <- readxl::read_xlsx("data/_dictionary_5_manually_created.xlsx") %>% 
  # this dictionary_5.xls file is manually created in excel
  # this file has number of students, NCES names, and other notes about the entities
  mutate(type = case_when(is.na(type) ~ "regular", 
                          TRUE ~ type)) %>% 
  #filter(type != "non-standard") %>%    
  select(state, name, id, ncesID) %>% filter(id == "88888")

dictionary12345 <- dictionary1234 %>% 
  rbind(dictionary5)

#saveRDS(dictionary12345, "dictionary12345.RDS")

#write.csv(dictionary12345, "dictionary12345.csv")

