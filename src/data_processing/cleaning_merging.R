library(tidyverse)
library(dplyr)
library(janitor)
library(writexl)
source("src/data_processing/census.R")
source("src/data_processing/functions.R")
source("src/data_processing/nces.R")
source("src/data_processing/exceptions.R")
options(scipen = 9999)


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
  
  mutate(name = case_when(name == "dona ana county" & state.abb == "NM" ~ "doña ana county", 
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
                      "bonds_outstanding", "loans_outstanding", "notes_outstanding", 
                      "compensated_absences", 
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
  add_count(state.name) %>% filter(n<4) %>% 
  select(state.name, n) %>% distinct()

state_all %>% write.csv("output/all_states_2020_2023.csv")

state_all %>% 
  filter(year == 2023) %>% 
  write.csv(file = paste0("output/all_states_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

compare_latest_csv_versions(
  folder = "output",
  prefix = "all_states_2023",
  output_excel = "output/states_changes_report.xlsx"
)

####### Counties########
acfrs_county <- acfrs_general_purpose %>%
  filter(!str_detect(name, "\\(")) %>%  # Exclude those with parenthetical notes
  filter(
    str_detect(name, "county") |
      (state.abb == "LA" & str_detect(name, "parish")) |  #In Louisiana, counties are called Parishes.
      (state.abb == "AK" & str_detect(name, "borough|municipality")) # In Alaska, borough |municipality
  )

# join acfrs with census population by name
county_gov <- acfrs_county %>% 
  # most acfrs_county do not have geo_id --> must join by state.abb and name
  select(-geo_id) %>% 
  mutate(name_clean = str_remove(name, "commission"),
         name_clean = str_squish(name_clean)) %>% 
  
  # # change some names to match with census
  mutate(name_clean = case_when(name == "city & county of butte silver bow" ~ "butte-silver bow",
                                name == "city and county of broomfield" ~ "broomfield county",
                                name == "city and county of honolulu" ~ "honolulu county",
                                name == "city and county of san francisco" ~ "san francisco county",
                                name == "st marys county" ~ "st mary's county",
                                name == "lafayette city-parish consolidated government" ~ "lafayette parish",
                                name == "cusseta-chattahoochee county" ~ "cusseta-chattahoochee county unified government",
                                name == "consolidated government of columbus-muscogee county" ~ "muscogee county",
                                name == "georgetown-quitman county" ~ "georgetown-quitman county unified government",
                                name == "unified government of webster county" ~ "webster county unified government",
                                TRUE ~ name_clean)) %>%
  
  # most acfrs_county do not have geo_id --> must join by state.abb and name
  left_join(census_all, by = c("state.abb", "state.name","name_clean" = "name_census")) %>% 
  
  #join urbanicity
  left_join(county_urb, by = "geo_id") %>% 
  distinct()


#check any county missing population
county_gov %>% filter(is.na(population)) %>% 
  select(state.abb, name, name_clean) %>% distinct() %>% 
  View()

nrow(county_gov %>% filter(year == 2023))

#####Consolidated government#####
# more on consolidated city, county & county equivalent: https://www.census.gov/programs-surveys/geography/about/glossary.html#par_textimage_12

# Step 1: identify consolidated / unified counties in census
# there are 33 consolidated counties
consolidated_county_census <- census_all %>% 
  filter(sumlev == 50) %>% # sumlev == 50 county level
  filter(funcstat == "C") # funcstat == "C" = consolidated county

nrow(consolidated_county_census)

# Step 2: Identify acfrs that report both city and county. 
#NO special treatment needed if: 
# city and county has their own reports and both exist in census, 
# either city or county does not exist independently in census

#NOTE: 
#1. IN marion county and Indianapolis are component units of 
#the Consolidated City of Indianapolis—Marion County. But they have separate financial reports
#2. KY Lexington-Fayette Urban County - Lexington city does not exist independently in census
#3. KY louisville/jefferson county metro government
#4. KY orleans parish does not have a report
#5. NY 5 counties are reported in NYC - these 5 counties are listed in census
# 6. #MT city & county of butte silver bow, Census does not have Butte city separate
#TN lynchburg, moore county metropolitan government, Census does not have lynchburg 

consolidated_county_city_acfr <- acfrs_general_purpose %>% 
#  county_gov %>% 
filter(id %in% c("1266824", "91930", "95986", "31609", # AK
                 "54175", #City and County of San Francisco
                 "1266589", # City and County of Broomfield
                  "1266289", # City and County of denver 
                 "32292", # the city of Jacksonville and Duval county
                 "99786", #GA macon-bibb county
                 "1266999", #GA cusseta-chattahoochee county
                 "1266998",# GA athens-clarke county unified government
                 "111293", # GA echols county
                 "111473", # GA consolidated government of columbus-muscogee county
                 "96522", #GA georgetown-quitman county
                 "148608",# GAaugusta-richmond county consolidated government
                 "96555", #GA webster county unified government
                 "1267157", #KS greeley county unified government - Tribune city
                 "40839", #KS wyandotte county, KS kansas city consolidated in Wyandotte County 
                 "1267156", #KY Lexington-Fayette Urban County , Lexington city 
                 "115965", #louisville/jefferson county metro government
                 "40777", #anaconda-deer lodge county, deer lodge city 
                 #"81613", #MT city & county of butte silver bow, Census does not have Butte city separate
                 "42714", #PA philadelphia city, also a county
                 "1267141", #TN nashville-davidson metropolitan government
                 #"1268468", #TN lynchburg, moore county metropolitan government, Census does not have lynchburg 
                 "1268161", #TN hartsville/trousdale county
                  "32107", "1266998", "148608", "33244", 
                 "1267157", "115965", "81613", "1267141"
                 )) %>% 
 left_join(df_state) %>% distinct() %>% select(-geo_id) %>% 

  left_join(census_all, by = c("state.abb", "state.name", "name" = "name_census")) 

#TODO:
  #Same population, but when it's identified as county, it has different geo_id. 
  #The geo_id as a metro is different and does not contain income & urbanicity info. 
  #Use these geo_id to get income and urbanicity. 
  # get this county geo_id from census_county
  # mutate(geo_id = case_when(name == "city and county of san francisco"~ "06075", 
  #                           name == "philadelphia" ~ "42101",
  #                           name == "city and county of honolulu" ~ "15003", 
  #                           name == "jacksonville" ~ "12031", # Duval county geo_id
  #                           name == "the metropolitan government of nashville and davidson county" ~ "47037",
  #                           TRUE ~ as.character(geo_id))) 


# get urbanicity
consolidated_city_county_all <- consolidated_county_city_acfr %>% 
  # get urbanicity
  left_join(county_urb, by = "geo_id") %>% 
# get income
 left_join(income, by = "geo_id") %>% 
  select(all_of(fields_to_select))

##All counties###
county_gov_all <- county_gov %>% 

  left_join(income) %>% 
  select(all_of(fields_to_select)) %>% 
  
  # bind with consolidated
  rbind(consolidated_city_county_all) %>% 
  
  #change population of Davidson metropolitan 
  mutate(name = ifelse(state.abb == "TN" & name == "davidson county", 
                       "The Metropolitan Government of Nashville and Davidson County",
                       name)) %>% 
  #change population of metropolitan 
  mutate(population = ifelse(name == "The Metropolitan Government of Nashville and Davidson County", 
                             sum(metropolitan_TN_13counties$population),
                             population)) %>% 
  
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
  median_hh_income_21 = as.numeric(median_hh_income_21)) %>% 
  
  #removing some duplicate
  #TODO: trace this back to initial join to remove dup from there
  filter(!(name %in% c("city and county of honolulu", 
                       "city and county of san francisco") & (is.na(geo_id))))  

  
#append URL
county_all <- append_url(county_gov_all) %>% 
  select(-identifier)
  
#TODO: check those with No geo
 #no_GEO <- county_all %>% filter(is.na(geo_id)) 
#   select(name, state.abb, geo_id, total_liabilities, year) %>%
#   filter(year != 2023)

# Find acfrs entities from the list of Top 100 county census 
top100_counties <- county_all %>% 
  filter(geo_id %in% census_county_top100$geo_id)
  
top100_counties %>% 
  group_by(state.abb) %>% 
  count(name) %>% filter(n<4)

write.csv(top100_counties, "output/top100_counties.csv")

# Find acfrs entities from the list of Top 200 county census
top200_county <- county_all %>% 
  filter(geo_id %in% census_county_top200$geo_id) 

#missing in top 200
top200_county %>% 
  group_by(year) %>% 
  summarise(count = n())

top200_county %>% write.csv("output/top200_counties.csv")

# Find acfrs entities from the list of Top 300 county census
top300_county <- county_all %>% 
  filter(geo_id %in% census_county_top300$geo_id) %>% 
  select(state.abb, id, geo_id, name, population, year) %>% distinct()

#missing in top 300
top300_county %>% 
  group_by(year) %>% 
  summarise(count = n())

top300_county %>% write.csv("tmp/list_top300_counties.csv")


anti_join(census_county_top300, top300_county, by = ("geo_id")) %>% View()

write.csv(county_all, "output/all_counties_2020_2023.csv")

county_all %>% 
  filter(year == 2023) %>% 
  write.csv(file = paste0("output/all_counties_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

compare_latest_csv_versions(
  folder = "output",
  prefix = "all_counties_2023",
  output_excel = "output/counties_changes_report.xlsx"
)

####Municipalities####
# Census calls Incorporated Place & Minor Civil Division
municipality_acfrs_ <- acfrs_general_purpose %>% 
  # exclude state and county
  filter(!id %in% state_all$id) %>% 
  filter(!id %in% county_all$id) %>% 
  
  # get income, only 634 entities has income info
  left_join(city_income) 
  
nrow(municipality_acfrs_ %>% filter(year == 2023))

# Join with census_municipalities using geo_id
municipality_all_ <- municipality_acfrs_ %>% 
  #select(state.abb, state.name, name, id, geo_id, year) %>% 
  left_join(census_all %>% filter(!is.na(geo_id)), by= c("geo_id", "state.abb", "state.name"))  

nrow(municipality_all_ %>% filter(year == 2023))
nrow(municipality_all_ %>% filter(year == 2023) %>% filter(is.na(population)))

#phase 1: join by geo_id
muni_phase1 <- municipality_all_ %>% 
  filter(!is.na(population))

#phase 2: matches those without geo_id using name 
muni_phase2 <- municipality_all_ %>% 
  filter(is.na(population)) %>% # keep remaining part after muni_phase1
  select(-c(population, geo_id)) %>% 
  
  left_join(census_all %>% select(state.abb, state.name, name_census, population, geo_id), 
            by = c("state.abb", "state.name", "name" = "name_census")) %>% 
  filter(!is.na(population)) 


#phase 3: clean up the names to further match with census.  
muni_phase3 <- municipality_all_ %>% 
  filter(is.na(population))  %>% #  remaining part after muni_phase1
  filter(!id %in% muni_phase2$id) %>% # remaining part after muni_phase2

  select(-c(population, geo_id)) %>% 
 
  left_join(census_all %>% select(state.abb, state.name, name_census, population, geo_id), 
            by = c("state.abb", "state.name", "name" = "name_census")) %>% 
  filter(is.na(population)) %>% distinct() %>% #318 missing pop
  
  #clean up names for specific state
  mutate(name = case_when(state.abb == "MI" ~ str_remove(name, "charter"),
                                state.abb == "IL" ~ str_replace(name, "mt", "mount"),
                                TRUE ~ name
                                )) %>% 
  
  mutate(name = str_squish(name)) %>% 
  select(-c(population, geo_id)) %>% 
# join again with census
  left_join(census_all %>% 
              select(state.abb, state.name, name_census, population, geo_id) %>% 
              
              #clean up census for sepecific state
              mutate(name_census = case_when(state.abb == "NJ" ~ str_remove(name_census, "borough"),
                                             TRUE ~ name_census)) %>% 
              mutate(name_census = str_remove(name_census, "city$"),
                     name_census = str_squish(name_census)),
          by = c("state.abb", "state.name", "name" = "name_census")) %>% 
  # check result
   filter(!is.na(population)) %>% distinct() 

muni_phase4_no_pop <- municipality_all_ %>% 
  filter(!id %in% muni_phase1$id) %>% 
  filter(!id %in% muni_phase2$id) %>% 
  filter(!id %in% muni_phase3$id)
  

muni_population <- rbind(
      muni_phase1, 
      muni_phase2, 
      muni_phase3, 
      muni_phase4_no_pop) %>% 

  append_url() %>% select(-identifier) %>% 
#fixing some geo_id
  mutate(geo_id = case_when(id == "138430" ~ "0827425",
                            id == "39880" ~ "5531000", # green bay
                            id == "1265296" ~ "0660620", #CA richmond
                            TRUE ~ geo_id)) 
muni_population %>% 
  group_by(year) %>% 
  summarise(tot = sum(population, na.rm = TRUE))

sum(muni_population$population, na.rm = TRUE)


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

municipality_all <- muni_population %>% rbind(special_cities) %>% 
  #avoid double count - these entities are counted in county_gov
  filter(name != "denver county") %>% 
  select(any_of(fields_to_select), url) %>% 
  
  # create a dummy urban_pop to match the df size with state and counties
  mutate(urban_pop = NA, 
         pct_urban_pop = NA)

#TODO: population in all of municipalities
municipality_all %>% filter(year == 2023) %>% 
  filter(is.na(population)) %>% 
  View()

# check geo_id

municipality_all %>% select(state.abb, id, lat)


#Top 100
top100_cities <- municipality_all %>% 
  filter((geo_id %in% census_city_top100$geo_id) | 
           name == "district of columbia") %>% distinct() %>% 
  mutate(population = ifelse(name == "district of columbia", 689546, population)) #%>% 


#Top 200 cities
top200_cities <- municipality_all %>% 
  filter((geo_id %in% census_city_top200$geo_id) | 
           name == "district of columbia") %>% distinct() %>% 
  mutate(population = ifelse(name == "district of columbia", 689546, population))  

#Top 300 cities
top300_cities <- municipality_all %>% 
  filter((geo_id %in% census_city_top300$geo_id) | 
           name == "district of columbia") %>% distinct() %>% 
  mutate(population = ifelse(name == "district of columbia", 689546, population))  

top300_cities_namelist <- top300_cities %>% select(state.abb, id, geo_id, name) %>% distinct()

#those in top 300 cencus but not collected in acfr: 
#Columbus consolidated in Muscogee county


anti_join(census_city_top300, top300_cities_namelist, by="geo_id")

top300_cities_namelist %>% write.csv("tmp/list_top300_municipalities.csv")


#top100_cities %>% write.csv("output/top100_cities.csv")
#top200_cities %>% write.csv("output/top200_cities.csv")
#city_gov %>% write.csv("output/all_cities_2020_2023.csv")
municipality_all %>% write.csv("output/all_municipalities_2020_2023.csv")


municipality_all %>% 
  filter(year == 2023) %>% View()
  write.csv(file = paste0("output/all_municipalities_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

compare_latest_csv_versions(
  folder = "output",
  prefix = "all_municipalities_2023",
  output_excel = "output/municipalities_changes_report.xlsx"
)

####School districts####
dictionary <- readRDS("data/dictionary.RDS") %>% 
select(-name) %>% distinct()


# filter only top 100
# dict_top100_ELSI <- dictionary %>% 
#   filter(ncesID %in% top100_schools$ncesID)

school_districts_ <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "School District") %>% 
  mutate(id = as.character(id)) %>% 
  select(any_of(fields_to_select)) %>% 
  
  #joint with dictionary to get ncesID
  left_join(dictionary) %>% 
  
  #join with nces to get county, city, long, lat
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) %>% 
 
   #append URLs
  append_url() %>% select(-identifier) 

# Now bind with special cases
school_districts_all <- 
bind_2df_different_size(school_districts_, exceptions) %>% 
  # create a dummy urban_pop to match the df size with state and counties
  mutate(urban_pop = NA, 
         pct_urban_pop = NA, 
         median_hh_income_21 = NA) %>% 
  rename(
    student_enrollment = enrollment_22)

# Save with time stamp 
school_districts_all %>% write.csv("output/school_districts_all_2020_2023.csv")
# 
school_districts_all %>% filter(year == 2023) %>%
  summarise(tot = sum(enrollment_23, na.rm = TRUE))

nces %>%
  summarise(tot = sum(enrollment_23, na.rm = TRUE))


school_districts_all %>%
  filter(year == 2023) %>% 
  write.csv(file = paste0("output/all_schooldistricts_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

school_districts_all %>% 
  filter(year == 2023) %>% 
  filter(ncesID %in% sd_top200_nces$ncesID) %>% View()

# TODO: some MT acfr report include more than 1 school districts. Need to treat MT separately 
school_districts_all %>% 
 # filter(year == 2023)  %>% View()
  count(id) %>%
  filter(n > 1)
  

####Tracking changes####
  

compare_latest_csv_versions(
  folder = "output",
  prefix = "all_schooldistricts_2023",
  output_excel = "output/schooldistrict_changes_report.xlsx"
)


####Entity ID####
state_gov %>% select(state.name, state.abb, name, id) %>% distinct() %>% 
  saveRDS("data/stateID.RDS")
county_gov %>% select(state.name, state.abb, name, id) %>% distinct() %>% 
  saveRDS("data/countyID.RDS")
municipality_all %>% select(state.name, state.abb, name, id) %>% distinct() %>% 
  saveRDS("data/place_divisionID.RDS")
#city_gov %>% select(state.name, state.abb, name, id) %>% distinct() %>% 
 # saveRDS("data/cityID.RDS")

cat("End of script")



