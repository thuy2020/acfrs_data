
library(tidyverse)
library(dplyr)
library(janitor)
source("src/data_processing/census.R")
source("src/data_processing/functions.R")
source("src/data_processing/nces.R")
source("src/data_processing/exceptions.R")
options(scipen = 999)

####NOTE##########
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

####General Purpose####
# step 1: get all general purpose entities in acfrs, most contains governmentID
acfrs_general_purpose <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "General Purpose") %>% 
  
  rename(government_id = census_id) %>%  # census_id in Acfrs database is actually government_id
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
  
  mutate(name = case_when(id == "115965" ~ "jefferson county",
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

# only select some fields to display on datatool
fields_to_select <- c("state.abb", "state.name", "id", "geo_id", "year", "name", 
                      "identifier", "category",
                      "total_liabilities", "current_liabilities",
                      "net_pension_liability", "net_pension_assets",
                      "net_opeb_liability", "net_opeb_assets", 
                      "total_assets", "current_assets", "compensated_absences",
                      "expenses", "revenues",
                      "unrestricted",
                      "population", "urban_pop", "pct_urban_pop", "median_hh_income_21")
#######States########
acfrs_state <- acfrs_general_purpose %>% 
  filter(!state.abb %in% c("FM", "PR")) %>% 
  filter(!str_detect(name, "yap|kosrae")) %>% 
  filter(str_detect(name, "(state of)|(commonwealth)")) %>% 
  mutate(name = str_remove_all(name, "(state of)|(commonwealth of)"),
         name = str_trim(name))

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

state_gov_4years <- state_gov %>% 
  left_join(income) %>% 
  select(all_of(fields_to_select)) 

#append url
state_all <- append_url(state_gov_4years) %>% 
  select(-identifier)

#double check missing:
state_all %>% 
  #group_by(year) %>% 
  #summarise(n = n())
  add_count(state.name) %>% filter(n<4) %>% 
  select(state.abb, n) %>% distinct()

state_all %>% write.csv("output/all_states_4years_2020_2023.csv")

####### Counties########
acfrs_county <- acfrs_general_purpose %>% 
  filter(!str_detect(name, "\\(")) %>%   # not county. Eg: waverly township (van buren county)
  #In Louisiana, counties are called Parishes.
  # In Alaska, borough)|municipality
  filter(grepl("county|municipality|parish|borough", name))
  

# join acfrs with census population 
county_gov <- acfrs_county %>% 
  # most acfrs_county do not have geo_id --> must join by state.abb and name
  select(-geo_id) %>% 
  left_join(census_county, by = c("state.abb", "state.name","name" = "name_census")) %>% 
  arrange(desc(population)) 

##Consolidated county-city##
#These counties are consolidated --> have some other name in acfrs list:  
consolidated_city_county <- acfrs_general_purpose %>% 
  filter((name == "city and county of san francisco" & state.abb == "CA") |
           (name == "philadelphia" & state.abb == "PA") |
           (name == "jacksonville" & state.abb == "FL") |
           (name == "the metropolitan government of nashville and davidson county" & state.abb == "TN")  |
           name == "city and county of honolulu") %>% 
  left_join(df_state) %>% 
  
  #Same population, but when it's identified as county, it has different geo_id. 
  #The geo_id as a metro is different and does not contain income & urbanicity info. 
  #Use these geo_id to get income and urbanicity. 
  # get this county geo_id from census_county
  mutate(geo_id = case_when(name == "city and county of san francisco"~ "06075", 
                            name == "philadelphia" ~ "42101",
                            name == "city and county of honolulu" ~ "15003", 
                            name == "jacksonville" ~ "12031", # Duval county geo_id
                            name == "the metropolitan government of nashville and davidson county" ~ "47037",
                            TRUE ~ as.character(geo_id))) 

# get population
consolidated_city_county_population <- census_all %>% filter(geo_id %in% consolidated_city_county$geo_id) %>% 
  select(geo_id, population) 

# get urbanicity
consolidated_city_county_urbanicity <- county_urb %>% filter(geo_id %in% consolidated_city_county$geo_id)

# get income
consolidated_city_county_income <- income %>% filter(geo_id %in% consolidated_city_county$geo_id)

# all
consolidated_city_county_all <- consolidated_city_county %>% 
  left_join(consolidated_city_county_population) %>% 
  left_join(consolidated_city_county_urbanicity) %>% 
  left_join(consolidated_city_county_income) %>% 
  select(all_of(fields_to_select))

##All counties###
county_gov_all <- county_gov %>% 

  left_join(income) %>% 
  select(all_of(fields_to_select)) %>% 
  # bind with consolidated
  rbind(consolidated_city_county_all)

#append URL
county_all <- append_url(county_gov_all) %>% 
  select(-identifier)

#NOTE: The Nashville Metropolitan Statistical Area encompasses the Middle Tennessee counties of 
#Cannon, Cheatham, Davidson, Dickson, Hickman, Macon, Robertson, Rutherford, 
#Smith, Sumner, Trousdale, Williamson, and Wilson.
metropolitan_TN_13counties <- census_all %>% 
  filter(state.abb == "TN" & sumlev == 50) %>% 
  filter(str_detect(name_census,"(?i)Cannon|Cheatham|Davidson|Dickson|Hickman|Macon|Robertson|Rutherford|Smith|Sumner|Trousdale|Williamson|Wilson")) %>% 
  filter(!str_detect(name_census,"(?i)balance of")) 

#write.csv(metropolitan_TN_13counties, "tmp/geo_id_metropolitan_TN_13counties.csv")

metropolitan_TN_13counties_urb <- county_urb %>% 
  filter(geo_id %in% c(metropolitan_TN_13counties$geo_id)) %>% 
  summarise(urban_pop = mean(urban_pop),
            pct_urban_pop = mean(pct_urban_pop))

metropolitan_TN_13counties_income <- income %>% 
  filter(geo_id %in% c(metropolitan_TN_13counties$geo_id)) %>%
  summarise(median_hh_income_21 = round(mean(median_hh_income_21)))

# Find acfrs entities from the list of Top 100 county census 
top100_counties <- county_all %>% 
  filter(geo_id %in% census_county_top100$geo_id) %>% 
  
  #change population of Davidson metropolitan 
  mutate(name = ifelse(state.abb == "TN" & name == "davidson county", 
                       "The Metropolitan Government of Nashville and Davidson County",
                       name)) %>% 
  
  #change population of metropolitan 
  mutate(population = ifelse(name == "The Metropolitan Government of Nashville and Davidson County", 
         sum(metropolitan_TN_13counties$population),
         population
         )) %>% 
  
  #change urbanicity of metropolitan 
  mutate(urban_pop = ifelse(name == "The Metropolitan Government of Nashville and Davidson County", 
                             metropolitan_TN_13counties_urb$urban_pop,
                             urban_pop)) %>% 
  #change percent_urbanicity of metropolitan 
  mutate(pct_urban_pop = ifelse(name == "The Metropolitan Government of Nashville and Davidson County", 
                            metropolitan_TN_13counties_urb$pct_urban_pop,
                            pct_urban_pop
  )) %>% 
  
  # income of metropolitan
  mutate(median_hh_income_21 = ifelse(name == "The Metropolitan Government of Nashville and Davidson County", 
                                metropolitan_TN_13counties_income,
                                median_hh_income_21
  ),
  median_hh_income_21 = as.numeric(median_hh_income_21)) 
  
write.csv(top100_counties, "output/top100_counties.csv")

# Find acfrs entities from the list of Top 200 county census
top200_county_4years <- county_all %>% 
  filter(geo_id %in% census_county_top200$geo_id) 

write.csv(county_all, "output/all_counties_2020_2023.csv")
top200_county_4years %>% write.csv("output/top200_counties.csv")

##########Municipalities#########
# Census calls Incorporated Place & Minor Civil Division
municipality_ <- acfrs_general_purpose %>% 
  # exclude state and county
  filter(!id %in% acfrs_state$id) %>% 
  filter(!id %in% acfrs_county$id) %>% 
  
  # get income
  left_join(city_income) %>% 
# Join Incorporated Place in ACFRs to Census  
  left_join(census_place_division, by= c("geo_id", "state.abb", "state.name")) %>% 
  distinct()

#append ULR
municipality_all <- append_url(municipality_) %>% select(-identifier)


#City&DC
acfrs_city <- municipality_all #%>% 
  #filter((geo_id %in% census_incorporated$geo_id) | name == "district of columbia") 

##Special cities##
special_cities <- acfrs_general_purpose %>% 
  # get geo id in these cities --> from there get income & census data
  filter(id %in% c("101868", "1266697", "1266289", "149470")) %>% 
  mutate(geo_id = case_when(id == "101868" ~ "0668000", # san jose CA
                            id == "1266697" ~ "2255000", # new orleans LA
                            id == "1266289" ~ "0820000",# CO denver county, also denver city
                            id == "149470" ~ "3611000")) %>%  # NY buffalo
  
  left_join(city_income) %>% 
  left_join(df_state) %>% 
  left_join(census_all) %>% 
  mutate(url = NA) %>% 
  select(-identifier)

city_gov <- acfrs_city %>% rbind(special_cities) %>% 
  select(any_of(fields_to_select), url) 

#Top 100
top100_cities <- city_gov %>% 
  filter((geo_id %in% census_city_top100$geo_id) | 
           name == "district of columbia") %>% distinct() %>% 
  mutate(population = ifelse(name == "district of columbia", 689546, population))


#Top 200 cities
top200_cities <- city_gov %>% 
  filter((geo_id %in% census_city_top200$geo_id) | 
           name == "district of columbia") %>% distinct() %>% 
  mutate(population = ifelse(name == "district of columbia", 689546, population))  
  # group_by(year) %>% 
  # summarise(n = n())
  # 

#Top 100 for data tool 
top100_cities %>% write.csv("output/top100_cities.csv")
top200_cities %>% write.csv("output/top200_cities.csv")
city_gov %>% write.csv("output/all_cities_2020_2023.csv")
municipality_all %>% write.csv("output/all_municipalities_2020_2023.csv")

####School districts####
dictionary <- readRDS("data/dictionary.RDS") %>% 
select(-name) %>% distinct()

# filter only top 100
dict_top100_ELSI <- dictionary %>% 
  filter(ncesID %in% top_schools_by_year$ncesID)

school_districts_ <- readRDS("data/acfrs_data.RDS") %>% 
filter(category == "School District") %>% 
  mutate(id = as.character(id)) %>% 
  select(any_of(fields_to_select)) %>% 
  left_join(dictionary) %>% 
  
  #join with nces to get county, city info
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) 
  

#append URLs
school_districts_all <- append_url(school_districts_) %>% select(-identifier)

#Top 100
top100_school_districts <- school_districts_all %>% 
  filter(id %in% dict_top100_ELSI$id) %>% 
  
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) 
  

top100_school_districts_4years <- school_districts_all %>% 
  filter(id %in% dict_top100_ELSI$id) 
  
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) 

# top 200
dict_top200_ELSI <- dictionary %>% 
  filter(ncesID %in% top200_schools_by_year$ncesID) 

top200_school_districts <- school_districts_all %>% 
  filter(id %in% dict_top200_ELSI$id) %>% 
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() 

#top 300
dict_top300_ELSI <- dictionary %>% 
  filter(ncesID %in% top300_schools_by_year$ncesID) 

top300_school_districts <- school_districts_all %>% 
  filter(id %in% dict_top300_ELSI$id) %>% 
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() 

top100_school_districts %>% write.csv("output/top100_sd.csv")
top200_school_districts %>% write.csv("output/top200_sd.csv")
school_districts_all %>% write.csv("output/all_schooldistricts_4years.csv")


####Entity ID####
state_gov %>% select(state.name, state.abb, name, id) %>% distinct() %>% 
  saveRDS("data/stateID.RDS")
county_gov %>% select(state.name, state.abb, name, id) %>% distinct() %>% 
  saveRDS("data/countyID.RDS")
municipality_all %>% select(state.name, state.abb, name, id) %>% distinct() %>% 
  saveRDS("data/place_divisionID.RDS")
city_gov %>% select(state.name, state.abb, name, id) %>% distinct() %>% 
  saveRDS("data/cityID.RDS")



