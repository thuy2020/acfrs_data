options(scipen = 999)
library(tidyverse)
library(dplyr)
library(DT)
library(viridis)
library(janitor)

# The key for SUMLEV is as follows (SUB-EST2022.pdf):
#   040 = State
#   050 = County
#   061 = Minor Civil Division
#   071 = Minor Civil Division place part
#   157 = County place part
#   160 = State-place
#   162 = Incorporated place
#   170 = Consolidated city
#   172 = Consolidated city -- place within consolidated city

df_state <- data.frame(state.abb, state.name) %>% 
  add_row(state.abb = "DC", state.name = "District of Columbia")

# Use population estimatesbase2020
census_all <- rio::import(here::here("data/census", "sub-est2022.csv")) %>% 
  clean_names() %>% 
  select(-c(primgeo_flag, popestimate2020, popestimate2021, popestimate2022)) %>% 
  rename (state.name = stname,
          population = estimatesbase2020,
          name_census = name) %>% 
  mutate(name_census = str_to_lower(name_census),
         name_census = str_remove(name_census,"\\.|'|‘"),
         name_census = str_trim(name_census)) %>% 
  left_join(df_state) %>% 
  
  
  mutate(place = as.character(place),
         cousub = as.character(cousub),
         state = as.character(state),
         county = as.character(county)) %>% 
  # adding leading 0 to the FIPS code. Use these to create geo_id 
  mutate(state = case_when(nchar(state) == 1 ~ paste0("0", state),
                           nchar(state) == 2 ~ state),
         county = case_when(nchar(county) == 1 ~ paste0("00", county),
                            nchar(county) == 2 ~ paste0("0", county),
                            nchar(county) == 3 ~ as.character(county)),
         
         place = case_when(nchar(place) == 2 ~ paste0("000", place),
                           nchar(place) == 3 ~ paste0("00", place),
                           nchar(place) == 4 ~ paste0("0", place),
                           nchar(place) == 5 ~ place),
         
         #minor civil division FIPs code
         cousub = case_when(nchar(cousub) == 1 ~ paste0("0000", cousub),
                            nchar(cousub) == 2 ~ paste0("000", cousub),
                            nchar(cousub) == 3 ~ paste0("00", cousub),
                            nchar(cousub) == 4 ~ paste0("0", cousub),
                            nchar(cousub) == 5 ~ cousub)) %>% 
  
  # create geo_id 
  mutate(geo_id = case_when(
    sumlev == 40 ~ state, # state
    sumlev == 50 ~ paste0(state, county), # county
    sumlev == 061 ~ paste0(state, cousub), #minor civil division
    sumlev == 162 ~ paste0(state, place), # incorporated place
    sumlev == 170 ~ paste0(state, concit), #consolidated city
    sumlev == 172 ~ paste0(state, place) # Consolidated city -- place within consolidated city
  )) 
###### Census state #########

state_urb <- rio::import(here::here("data/census", "State_Urban_Rural_Pop_2020_2010.xlsx")) %>% 
  clean_names() %>% 
  rename(state.abb = state_abbrev) %>% 
  rename(urban_pop = x2020_urban_pop,
         pct_urban_pop = x2020_pct_urban_pop) %>% 
  select(state.abb, urban_pop, pct_urban_pop)

census_state <- census_all %>% filter(sumlev == 40) %>% 
  select(state.abb, geo_id, population) %>% 
  left_join(state_urb)

######## Census county#########
# Getting urbanicity data 

#CT has 9 planning region which are not counties
county_CT_urb <- rio::import(here::here("data/census", "2020_UA_COUNTY.xlsx"), sheet = 2) %>% 
  clean_names() %>% 
  mutate(geo_id = paste0(state,county)) %>% 
  select(geo_id, pop_urb, poppct_urb)

county_urb <- rio::import(here::here("data/census", "2020_UA_COUNTY.xlsx")) %>% 
  clean_names() %>% 
  mutate(geo_id = paste0(state,county)) %>% 
  select(geo_id, pop_urb, poppct_urb) %>% 
  #bind with CT
  rbind(county_CT_urb) %>% 
  
  # rename to be consistence with state gov
  rename(urban_pop = pop_urb,
         pct_urban_pop = poppct_urb)

# join with urb data
census_county <- census_all %>% 
  filter(sumlev == 050) %>% 
  left_join(county_urb)

# Check special cases in Census county: 
# cities & district of columbia categorized as county
census_county %>% 
  filter(!str_detect(name_census, "(borough)|(county)|(parish)|(planning regio)|(census area)|(municipality)")) 
# Note: Planning Regio, lacking "n" at last, to include all Planning Region in CT
# Louisiana has 64 entities "Parish"
# Alaska has 30 entities " 17 Borough", "Census Area", "Municipality"
# Connecticut has 9 entities "Planning Region"

###### Top 100 county Census 2021: 
census_county_top100 <- census_county %>% 
  arrange(desc(population)) %>% 
  filter(funcstat %in% c("A", "C")) %>% 
  # does not count 5 counties in NY
  filter(!name_census %in% c("kings county", "queens county", "new york county", "bronx county")) %>% 
  
  slice(1:100) #%>% 
  #select(state.abb, name_census, population, funcstat, geo_id) 

##### Top county 101-200
census_county_top101_200 <- census_county %>% 
  arrange(desc(population)) %>% 
  filter(funcstat %in% c("A", "C")) %>% 
  # does not count 5 counties in NY
  filter(!name_census %in% c("kings county", "queens county", "new york county", "bronx county")) %>% 
  
  slice(101:200) 


#census_county_top100 %>% write_csv("output/census_county_top100.csv")

##### Census Incorporated Place & Minor Civil Division #########
census_place_division <- census_all %>% 
  filter(sumlev %in% c(162, 061, 170, 172)) %>% 
  filter(funcstat %in% c("A", "C")) %>% 
  distinct()

#### City

census_city <- census_all %>% filter(sumlev == 162) %>% 
  filter(str_detect(name_census, "city$")) %>% 
  filter(funcstat %in% c("A", "C")) 

census_city_top100 <- census_city %>% 
  arrange(desc(population)) %>% slice(1:100) %>% 
  mutate(#name_census = str_remove_all(name_census, " city$"),
         name_census = str_trim(name_census)) %>% 
  select(state.abb, name_census, population, geo_id)


census_city_top101_200 <- census_city %>% 
  arrange(desc(population)) %>% slice(101:200) %>% 
  mutate(#name_census = str_remove_all(name_census, " city$"),
    name_census = str_trim(name_census)) %>% 
  select(state.abb, name_census, population, geo_id)

##### Middle file - Use this file to bridge geo_id from census data to into acfrs_general_purpose

# sheet 3 includes all geo_id in sheet 1. 
sheet3 <- rio::import(here::here("data", "City and Town Mapping.xlsx"), sheet = 3) %>%
  clean_names() %>% 
  select(government_id, inferred_geo_id, name, state_ab)  %>% 
  rename(geo_id = inferred_geo_id,  # Marc created INFERRED GEO_ID, which meant to be geoID
         state.abb = state_ab) %>% 
  mutate(name = str_to_lower(name)) %>% 
  
  mutate(#name = str_remove_all(name, "(town of)|(city of)|(village of)"),
         name = str_trim(name))

# sheet 2 has some geo_id that sheet 3 doesn't.   
sheet2 <- rio::import(here::here("data", "City and Town Mapping.xlsx"), sheet = 2) %>%
  clean_names() %>% 
  rename(popestimate2020 = total) %>% 
  mutate(state.name = str_extract(name, ",(.*)"),
         state.name = str_remove_all(state.name, ", "),
         name = str_remove_all(name, ",(.*)")) %>% 
  left_join(df_state) %>% # get state.abb
  
  mutate(government_id = "") %>% 
  select(government_id, geo_id, name, state.abb) %>% 
  
  mutate(name = str_to_lower(name))
         #name = str_remove_all(name, "(city)|(town)$"))

# dictionary linking governmentID and geo ID of cities 
governmentID_geoID <- rbind(sheet2, sheet3) %>% rename(name_midfile = name)


options(scipen = 999)
library(tidyverse)
library(dplyr)
library(janitor)


#########Income###########
# County: Income from Census - County level household income
income <- rio::import(here::here("data", "Unemployment_median income.xlsx"), skip = 4)  %>% 
  select(FIPS_Code, Median_Household_Income_2021) %>% 
  rename(geo_id = FIPS_Code, 
         median_hh_income_21 = Median_Household_Income_2021)

#City:
#S1903 MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS)
#2021: ACS 1-Year Estimates Subject Tables
#https://data.census.gov/table/ACSST1Y2021.S1903?q=median%20household%20income%20by%20city%202021&g=010XX00US$0300000

city_income <- rio::import(here::here("data/ACSST1Y2021.S1903_2023-12-29T105538/ACSST1Y2021.S1903-Data.csv"), skip = 1) %>% 
  select(1, `Geographic Area Name`, `Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households`) %>% 
  rename(median_hh_income_21 = `Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households`
  ) %>% 
  mutate(
    place = substring(Geography, 12, 16),
    state = substring(Geography, 10, 11)) %>% 
  
  mutate(geo_id = paste0(state, place)) %>% 
  select(geo_id, median_hh_income_21)


####### Partisan lean - state ##########

partisan_lean <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/partisan-lean/2021/fivethirtyeight_partisan_lean_STATES.csv")

