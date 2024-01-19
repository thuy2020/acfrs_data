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
census_all <- rio::import(here::here("data", "sub-est2022.csv")) %>% 
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
###### Census state

state_urb <- rio::import(here::here("data", "State_Urban_Rural_Pop_2020_2010.xlsx")) %>% 
  clean_names() %>% 
  rename(state.abb = state_abbrev) %>% 
  select(state.abb, x2020_urban_pop, x2020_pct_urban_pop)

census_state <- census_all %>% filter(sumlev == 40) %>% 
  select(state.abb, geo_id, population) %>% 
  left_join(state_urb)

######## Census county#########
# Getting urbanicity data 

#CT has 9 planning region which are not counties
county_CT_urb <- rio::import(here::here("data", "2020_UA_COUNTY.xlsx"), sheet = 2) %>% 
  clean_names() %>% 
  mutate(geo_id = paste0(state,county)) %>% 
  select(geo_id, pop_urb, poppct_urb)

county_urb <- rio::import(here::here("data", "2020_UA_COUNTY.xlsx")) %>% 
  clean_names() %>% 
  mutate(geo_id = paste0(state,county)) %>% 
  select(geo_id, pop_urb, poppct_urb) %>% rbind(county_CT_urb)

# join with urb data
census_county <- census_all %>% 
  filter(sumlev == 050) %>% left_join(county_urb)

# Check special cases in Census county: 
# 41 cities & district of columbia categorized as county
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
  filter(!name_census %in% c("kings county", "queens county", "new york county", "bronx county")) %>% 
  slice(1:100) #%>% 
  #select(state.abb, name_census, population, funcstat, geo_id) 

#census_county_top100 %>% write_csv("output/census_county_top100.csv")

##### Incorporated Place & Minor Civil Division:
census_place_division <- census_all %>% 
  filter(sumlev %in% c(162, 061, 170, 172)) %>% 
  filter(funcstat %in% c("A", "C")) %>% 
  distinct()

#### City

census_city <- census_all %>% filter(sumlev == 162) %>% 
  filter(str_detect(name_census, "city$")) 

census_city_top100 <- census_city %>% 
  filter(funcstat %in% c("A", "C")) %>% 
  arrange(desc(population)) %>% slice(1:100) %>% 
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

governmentID_geoID <- rbind(sheet2, sheet3) %>% rename(name_midfile = name)

