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

####School districts####
dictionary <- readRDS("data/dictionary.RDS") %>% 
  #select(name) %>% 
  distinct()

dictionary %>% filter(state.abb == "MN") %>% View()
## NOTE: 
#id 67836 Santa Cruz City Schools (the "District") is a consolidation of 
#Santa Cruz City High School District nces 0635600
#Santa Cruz City Elementary School District nces 0635590 . 
#The Districts have not unified but are consolidated due to the fact that the Districts share a common governing board. These two entities are referred to collectively as Santa Cruz City Schools, 
#and for purposes of these financial statements, will be referred to collectively as the District

school_districts_ <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "School District") %>% 
  mutate(id = as.character(id)) #%>% 
  #select(any_of(fields_to_select)) 

#append URLs
school_districts <- append_url(school_districts_) %>% select(-identifier)

#########TESTING
school_districts %>% left_join(dictionary, by = c("id", "state.abb")) %>% 
  filter(is.na(ncesID)) %>% 
  select(state.abb, id, name, ncesID) %>% 
  select(-ncesID) %>% distinct() %>% arrange(state.abb, name) -> acfrs_sd_NO_ncesID

acfrs_sd_NO_ncesID

nces %>% filter(!ncesID %in% dictionary$ncesID) %>% 
  select(state.abb, name_nces, ncesID, county) %>% 
  arrange(state.abb, name_nces) -> nces_NO_id



#############




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
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() #%>% 
#select(state.abb, ncesID, year, name) %>% 
#add_count(ncesID) %>% filter(n < 3) 


#TODO: check back missing: 
#GA Clayton County Board of education.
#https://www.clayton.k12.ga.us/departments/business-services/financial-reports
# Uploaded: williamson county schools, reported in county acfrs. Uploaded county's acfrs to replace


missing_sd <- top200_school_districts %>% 
  add_count(ncesID) %>% filter(n < 3) %>% 
  select(state.abb, ncesID, year, name, n)

top200_school_districts %>% write.csv("output/top200_sd.csv")
top100_school_districts %>% write.csv("output/top100_sd.csv")
school_districts %>% write.csv("output/all_schooldistricts_3years.csv")
#TODO: ask Geoff about this revenues = expense cases
