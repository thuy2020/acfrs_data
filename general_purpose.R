options(scipen = 999)
library(tidyverse)
library(dplyr)
library(DT)
library(janitor)

source("census.R")

#####NOTE###########
#Entity names in the original Acfrs database are not obvious, i.e., 
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


##### ACFRs General Purpose 

# step 1: get all general purpose entities in acfrs, most contains governmentID
acfrs_general_purpose <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "General Purpose") %>% 
  #select(-c(5:11, category, component_unit_of_id)) %>% 
  
  rename(government_id = census_id, # census_id in Acfrs database is actually government_id
         state.abb = state) %>% 
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


#######States########

acfrs_state <- acfrs_general_purpose %>% 
  filter(str_detect(name, "(state of)|(district of columbia)|(commonwealth)")) %>% 
  filter(!str_detect(name, "yap|kosrae")) %>% 
  filter(!str_detect(name, "(iowa single audit)|(puerto rico)")) %>% 
  mutate(name = str_remove_all(name, "(state of)|(commonwealth of)"),
         name = str_trim(name))

# state from census
# census_state <- census_all %>% filter(sumlev == 40) %>% 
#   select(state.abb, geo_id, population)

# Joining acfrs states & census states: state_gov_2020
state_gov <- acfrs_state %>% select(-geo_id) %>% 
  left_join(census_state, by = c("state.abb")) %>% 
  select(-government_id) %>% 
  
  #fix geo_id to join with other data later
  mutate(geo_id = case_when((nchar(geo_id) == 1) ~ paste0("0", geo_id, "000"),
                            (nchar(geo_id) == 2) ~ paste0(geo_id, "000"),
                            (nchar(geo_id) == 3) ~ paste0(geo_id, "00"),
                            (nchar(geo_id) == 4) ~ paste0(geo_id, "0"),
                            TRUE ~ as.character(geo_id)))

# create id list to recognize state entities in later years. 
#state_acfrs_id <- state_gov_2020 %>% select(state.abb, name, id)

####### Counties########

# Special case: Alaska
#Alaska counties in census
alaska_county_census <- census_county %>% filter(state.abb == "AK")

# Alaska counties in Acfrs:
alaska_county_acfrs <- acfrs_general_purpose %>% 
  filter(state.abb == "AK") %>% 
  filter(str_detect(name, "(borough)|(municipality)$"))

acfrs_county <- acfrs_general_purpose %>% 
  filter(grepl("county|municipality|parish", name)) %>% #In Louisiana, counties are called Parishes.
  filter(!str_detect(name, "\\(")) # not county. Eg: waverly township (van buren county)
  rbind(alaska_county_acfrs) 

# join acfrs with census population 
county_gov <- acfrs_county %>% 
  # most acfrs_county do not have geo_id --> must join by state.abb and name
  left_join(census_county, by = c("state.abb", "name" = "name_census")) %>% 
  
  # drop non-county entities
  arrange(desc(population)) %>% 
  drop_na(population) %>% select(-geo_id.x) %>% rename(geo_id = geo_id.y)


# Save county ID to filter in later years
#county_acfrs_id_20 <- county_gov_20 %>% select(state.abb, name, id) 

########## Incorporated Place & Minor Civil Division#########

# ACFRs:
place_division_gov <- acfrs_general_purpose %>% 
  # exclude state and county
  filter(!id %in% acfrs_state$id) %>% 
  filter(!id %in% county_gov$id) %>% distinct() %>% 

# Join Incorporated Place in ACFRs to Census  
  left_join(census_place_division, by= c("geo_id", "state.abb")) 

#### City #########
city_gov <- place_division_gov %>% 
  filter(geo_id %in% census_city$geo_id)

#write.csv(acfrs_county, "output/acfrs_county.csv")
write.csv(county_gov, "output/county_gov.csv")
#write.csv(place_division_gov, "output/place_division_gov.csv")
write.csv(city_gov, "output/city_gov.csv")



#####Population##########
# export population to load in database
state_gov %>% select(state.abb, name, id, geo_id, population) %>% 
  mutate(government_level = "state") -> temp1
  
county_gov %>% select(state.abb, name, id, geo_id, population) %>% 
  mutate(government_level = "county") -> temp2
  
place_division_gov %>% select(state.abb, name, id, geo_id, population) %>% 
  mutate(government_level = "municipal_township") -> temp3

rbind(temp1, temp2, temp3) %>% 
  rename(population_2020 = population) %>% distinct() %>% 
write.csv("output/general_purpose_population_2020.csv")
  















