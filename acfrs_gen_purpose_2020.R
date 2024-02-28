options(scipen = 999)
library(tidyverse)
library(dplyr)
library(DT)
library(janitor)
source("census.R")
# Entity names in the original Acfrs database are not obvious, i.e., 
# They do not always contain a certain word to indicate its government type (i.e., city, town, county, etc). 

# *ACFRs data has government_id (which is called census_id in the ACFRs portal). 
# This is government unit identifier, and is not geo_id that are often used in many census datasets.
# 
# Two points above constitute challenges when we need to:
# 1) adding population field in acfrs data, and 
# 2) sort out what type of entities are collected.
# 
# * Two main ways to solve the problems: 
# - Use a "middle file" to link geo_id and government_ID
# - Joining by names and states 



##### ACFRs General Purpose 2020

# step 1: get all general purpose entities in acfrs, most contains governmentID
acfrs_general_purpose_20 <- readRDS("data/data_from_dbsite_2020.RDS") %>% 
  filter(category == "General Purpose") %>% 
  select(-c(5:11, category, has_unconfirmed, component_unit_of_id)) %>% 
  
  rename(government_id = census_id,
         state.abb = state) %>% # census_id in Acfrs database is actually government_id
  # some government_id in ACFRs has 13 characters-> need to add 0
  mutate(government_id = ifelse(str_length(government_id) < 14, paste0("0", government_id), government_id)) %>% 
  
# step 2: join with the middle file to get geo_id into acfrs data
  left_join(governmentID_geoID, by = c("state.abb", "government_id")) %>% 
  
  # cleaning to match with names in census
  mutate(name = str_to_lower(name),
         name = str_remove_all(name, "(\\.)|(\\')"),
         # In LA, ACFRs of some parish titled "parish police jury" -> Geoff: these are counties ACFRs
         name = str_remove_all(name, "police jury"), 
         name = str_remove_all(name, " fiscal court"),
         name = str_trim(name)) %>% 
  
  mutate(name = case_when(name == "yakutat borough" & state.abb == "AK" ~ "yakutat city and borough",
                          name == "dona ana county" & state.abb == "NM" ~ "doña ana county", 
                          name == "st marys county" & state.abb == "MD" ~ "st mary's county",
                          name == "athens-clarke county" & state.abb == "GA" ~ "st mary's county",
                          name == "greeneville-greene county" & state.abb == "TN" ~ "greene county",
                          name == "sevierville-sevier county" & state.abb == "TN" ~ "sevier county",
                          name == "lynchburg_moore county" & state.abb == "TN" ~ "moore county",
                          name == "hartsville-trousdale county" & state.abb == "TN" ~ "trousdale county",
                          name == "nashville and davidson county" & state.abb == "TN" ~ "davidson county",
                          name == "lafayette city-parish consolidated government" & state.abb == "LA" ~ "lafayette parish",
                          TRUE ~ name)) %>% 
  
# Step 3: extract geo_id part to map with census
  mutate(geo_id = str_extract(geo_id, "US(.*)"),
         geo_id = str_remove_all(geo_id, "US")) 


####### States

acfrs_state_2020 <- acfrs_general_purpose_20 %>% 
  filter(str_detect(name, "(state of)|(district of columbia)|(commonwealth)")) %>% 
  filter(!str_detect(name, "yap|kosrae")) %>% 
  filter(!str_detect(name, "(iowa single audit)|(puerto rico)")) %>% 
  mutate(name = str_remove_all(name, "(state of)|(commonwealth of)"),
         name = str_trim(name))


# state from census
# census_state <- census_all %>% filter(sumlev == 40) %>% 
#   select(state.abb, geo_id, population)

# Joining acfrs states & census states: state_gov_2020
state_gov_2020 <- acfrs_state_2020 %>% select(-geo_id) %>% 
  left_join(census_state, by = c("state.abb")) %>% 
  select(-government_id)

# create id list to recognize state entities in later years. 
state_acfrs_id <- state_gov_2020 %>% select(state.abb, name, id)




####### Counties

acfrs_county_20 <- acfrs_general_purpose_20 %>% 
  filter(grepl("county|municipality|parish", name)) %>% #In Louisiana, counties are called Parishes.
  filter(!str_detect(name, "\\(")) # not county. Eg: waverly township (van buren county)


# Alaska counties in census
alaska_county_census <- census_county %>% filter(state.abb == "AK")

# Alaska counties in Acfrs:
alaska_county_acfrs_20 <- acfrs_general_purpose_20 %>% 
  filter(state.abb == "AK") %>% 
  filter(str_detect(name, "(borough)|(municipality)$"))

# join acfrs with census population 
county_gov_20 <- acfrs_county_20 %>% 
  rbind(alaska_county_acfrs_20) %>% 
  
  # most acfrs_county do not have geo_id --> must join by state.abb and name
  left_join(census_county, by = c("state.abb", "name" = "name_census")) %>% 
  
  # drop non-county entities
  arrange(desc(population)) %>% 
  drop_na(population) 


# Save county ID to filter in later years
county_acfrs_id_20 <- county_gov_20 %>% select(state.abb, name, id) 

########## Incorporated Place & Minor Civil Division

# ACFRs:
acfrs_place_division_20 <- acfrs_general_purpose_20 %>% 
  # exclude state and county
  filter(!id %in% acfrs_state_2020$id) %>% 
  filter(!id %in% county_gov_20$id) %>% distinct()

# Join Incorporated Place in ACFRs to Census  

place_division_gov_20 <- acfrs_place_division_20 %>% 
  left_join(census_place_division, by= c("geo_id", "state.abb")) %>% 
  drop_na(population)#%>% distinct(state.abb, name) # only 9146 distinct



#### City

city_gov_20 <- place_division_gov_20 %>% 
filter(geo_id %in% census_city$geo_id)

