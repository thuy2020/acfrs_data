options(scipen = 999)
library(tidyverse)
library(dplyr)
library(DT)
library(janitor)

source("census.R")
source("acfrs_gen_purpose_2020.R")
source("acfrs_gen_purpose_2021.R")

acfrs_general_purpose_22 <- readRDS("data/data_from_dbsite_2022.RDS") %>% 
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

### States from ACFRS 2022
acfrs_state_2022 <- acfrs_general_purpose_22 %>% 
  filter(str_detect(name, "(state of)|(district of columbia)|(commonwealth)")) %>% 
  filter(!str_detect(name, "yap|kosrae")) %>% 
  filter(!str_detect(name, "(iowa single audit)|(puerto rico)")) %>% 
  mutate(name = str_remove_all(name, "(state of)|(commonwealth of)"),
         name = str_trim(name))


### Joining acfrs states & census states: state_gov_2020
state_gov_2022 <- acfrs_state_2022 %>% select(-geo_id) %>% 
  left_join(census_state, by = c("state.abb")) %>% select(-government_id)


state_gov_2022 %>% write.csv("output/state_gov_22.csv")


#######County

# bind all counties appeared in 2020 and 2021
county_acfrs_id_2020_2021 <- county_acfrs_id_21 %>% 
  filter(!id %in% county_acfrs_id_20$id) %>% #exclude id already in 2020
  rbind(county_acfrs_id_20)

county_gov_22 <- acfrs_general_purpose_22 %>% 
  filter(id %in% county_acfrs_id_2020_2021$id) %>% 
  left_join(census_county, by = c("state.abb", "name" = "name_census")) %>% 
  arrange((population)) %>% 
  drop_na(population) 

county_gov_22 %>% write.csv("output/county_gov_22.csv")


######### Incorporated Place
# ACFRs:
acfrs_place_division_22 <- acfrs_general_purpose_22 %>% 
  # exclude state and county
  filter(!id %in% acfrs_state_2020$id) %>% 
  filter(!id %in% county_gov_20$id) %>% distinct()

# Join Incorporated Place in ACFRs to Census  

place_division_gov_22 <- acfrs_place_division_22 %>% 
  left_join(census_place_division, by= c("geo_id", "state.abb")) %>% 
  drop_na(population)#%>% distinct(state.abb, name) # only 9146 distinct

place_division_gov_22 %>% write.csv("output/place_division_gov_22.csv")

#### City

city_gov_22 <- place_division_gov_22 %>% 
  filter(geo_id %in% census_city$geo_id)
