library(tidyverse)
library(dplyr)
library(janitor)
library(writexl)
source("src/data_processing/functions.R")
source("src/data_processing/census.R")
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
acfrs_data <- readRDS("data/acfrs_data.RDS")
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
         geo_id = str_remove_all(geo_id, "US")) %>% 

# step 4: manually add & fix geo_id for some important cities. The governemnt ID above contains some wrong geo_id
  mutate(geo_id = case_when(id == "101868" ~ "0668000", #CA san jose city
                            id == "1266697" ~ "2255000", #LA 	new orleans city
                            id == "35652" ~ "3451000", #NJ newark city
                            id == "149470" ~ "3611000", #NY buffalo city
                            id == "138430" ~ "0827425", # CO fort collins city
                            id == "35675" ~ "3457000", #NJ paterson city ???
                            id == "34945" ~ "2836000", #???
                            id == "1265296" ~ "0660620", #CA richmond
                            id == "400904" ~ "4853388", #???
                            id == "105189" ~ "1372122",#??
                            id == "39880" ~ "5531025",
                            id == "68810" ~ "1257425", #FL Plantation
                            id == "149880" ~ "1257550",
                            TRUE ~ geo_id
                            )) 


#######States########
acfrs_state <- acfrs_general_purpose %>% 
  filter(!state.abb %in% c("FM", "PR")) %>% 
  filter(!str_detect(name, "yap|kosrae")) %>% 
  filter(str_detect(name, "(state of)|(commonwealth)")) %>% 
  mutate(name = str_remove_all(name, "(state of)|(commonwealth of)"),
         name = str_trim(name))

# Joining acfrs states & census states: state_gov_2020
state_all <- acfrs_state %>% select(-geo_id) %>% 
  left_join(census_state, by = c("state.abb")) %>% 
  select(-government_id) %>% 
  left_join(state_county_income) %>% 
  select(all_of(fields_to_select)) %>% 

#append url
  append_url() %>% 
  select(-identifier) %>% 

# TODO: check if Nevada is available
bind_rows(
tibble(state.abb = "NV", state.name = "Nevada", id = 35504, geo_id = "32",
       name = "nevada", category = "General Purpose", population = 3104624,
       year = 2023)) %>% 

  # add flag
mutate(flg_acfr = ifelse((state.abb == "NV" & year == 2023), 0, 1))  
  
state_all %>% write.csv("output/all_states_2020_2023.csv")

state_all %>% 
  group_by(year) %>% 
  summarise(tot = sum(population))

state_all %>% 
  filter(year == 2023) %>% 
  write.csv(file = paste0("output/all_states_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

compare_latest_csv_versions(
  folder = "output",
  prefix = "all_states_2023",
  output_excel = "output/states_changes_report.xlsx"
)

####### Counties########

####NOTE####
# more on consolidated city, county & county equivalent: 
#https://www.census.gov/programs-surveys/geography/about/glossary.html#par_textimage_12

#NOTE: 
#1. IN marion county and Indianapolis are component units of 
#the Consolidated City of Indianapolis—Marion County. But they have separate financial reports
#2. KY Lexington-Fayette Urban County - Technically is not a county (sumlev = 162). Lexington city does not exist independently in census
#3. KY louisville/jefferson county metro government.  Census does not have louisville city. #jefferson county, geo_id for county 21111
#4. KY orleans parish does not have a report
#5. NY 5 counties are listed in census as counties, but are incorporated NYC ACFR
#6. #MT city & county of butte silver bow, Technically is not a county (sumlev = 170). Census does not have Butte city separate
#7. TN lynchburg, moore county metropolitan government, - Census does not have lynchburg city

#NOTE: 
#1. The Capitol Planning Region in Connecticut 
#encompasses the city of Hartford, the state capital, and the city of New Britain
#2. Massachusetts has 14 counties. While 8 of these once had active county governments, only 5 counties 
#(Barnstable, Bristol, Dukes, Norfolk, and Plymouth) currently have functional county-level local governments
#3. The Western Connecticut Planning Region in Connecticut includes the cities of Danbury, Norwalk, and Stamford
#.The South Central Connecticut Planning Region includes the cities of Meriden, Milford, New Haven, and West Haven, as well as the towns of Bethany, 
#Branford, East Haven, Guilford, Hamden, Madison, North Branford, North Haven, Orange, Wallingford, and Woodbridge. 
# The Naugatuck Valley Planning Region in Connecticut includes the cities of Ansonia, Derby, Naugatuck, Shelton, 
#and Waterbury, along with several other towns

#####Consolidated counties#####
# There are 33 entities in consolidated_county_dictionary
consolidated_county_dictionary <- read.csv("data/_consolidated_county_city_census_acfr_dictionary.csv") %>% 
  select(-c(name, name_municipality, X)) 

#####ACFR counties#####
county_acfrs <- acfrs_general_purpose %>%
  filter(!str_detect(name, "\\(")) %>%  # Exclude those with parenthetical notes
  filter(
    str_detect(name, "county") |
      (state.abb == "LA" & str_detect(name, "parish")) |  #In Louisiana, counties are called Parishes.
      (state.abb == "AK" & str_detect(name, "borough|municipality")) # In Alaska, borough |municipality
  ) %>% 

# clean name to join with census
## NOTE: most acfrs_county do not have geo_id --> must join by state.abb and name
  select(-geo_id) %>% 
  mutate(name_clean = str_remove(name, "commission"),
         name_clean = str_squish(name_clean),
         name = str_squish(name)) 

county_acfrs_2023 <- county_acfrs %>%
  filter(
    # Keep 2023 for all states
    year == 2023 |
      # OR: keep 2022 if SD or OK and entity has no 2023 data
      (
        year == 2022 &
          state.abb %in% c("SD", "OK") & # if no matching 2023 for that id
          !id %in% (county_acfrs %>% filter(year == 2023) %>% pull(id))
      )) 

#####Round 1#####
#join 3144 Census counties Census sumlev = 50
county_census_acfr_2023_round1 <- 
  
  #take counties in census, but not consolidated ones = 3111 entities
  #exclude 33 consolidated counties to avoid double counted - will rbind later
  (census_all %>%  
  filter(sumlev %in% c(50) & funcstat != "C"
         )) %>% 
  
  #join with counties in acfr
  left_join(county_acfrs_2023, 
            by = c("state.abb", "state.name", "name_census" = "name_clean"))  
  
#####Round 2 - consolidated counties #####
#create a subset of consolidated alone: 
consolidated_county_2023 <- consolidated_county_dictionary %>% 
  select(-geo_id) %>% 
  left_join(acfrs_general_purpose %>% filter(year == 2023), 
            by = c("state.abb", "state.name", "id")) 
  
#bind back to round 1
county_census_acfr_2023_round1_2 <- rbind(county_census_acfr_2023_round1,
        consolidated_county_2023)


#####Round 3 - counties eported in municipalities#####
# the following counties are reported in municipalities ACFR because government funcstat = F -> not functional
#--> mark these municipalities in ACFRs as counties too: 
county_acfr_reported_as_muni <- acfrs_general_purpose %>% 
  filter(id %in% c("230476", #"virginia beach", #"230476" VA county in census as "virginia beach"
                   "230389", #norfolk", #VA county in census as: "norfolk city"
                   "230275",#"chesapeake", #VA county in census as "chesapeake city"
                   # MO st louis city whose geo_id is 29510 is independent city, also a county, population 300k. 
                   #This is different from other St Louis county whose geo_id is 29189, population > 1M
                   "44598" ,#"st louis city", #Missouri county by the same name, geo_id = 29510
                   "33980" ,#"baltimore", #MD  baltimore county as in baltimore city
                   "1266545" ,#"baton rouge",# this is LA "east baton rouge parish"
                   "47298", #DC columbia
                   "111562" # HI city and county of honolulu, same geo_id, no need to change
                   )) %>% 
  
  #When identified as cities, they have different geo_id --> need to get geo_id as counties to join back to county_census_acfr_2023_
  #get these geo_id for county in census
  mutate(geo_id = case_when(id == "230476" ~ "51810", #"virginia beach"
                            id =="230389" ~ "51710", #norfolk", #VA county in census as: "norfolk city"
                            id =="230275" ~ "51550",#"chesapeake", #VA county in census as "chesapeake city"
                            # MO st louis city whose geo_id is 29510 is independent city, also a county, population 300k. 
                            #This is different from other St Louis county whose geo_id is 29189, population > 1M
                            id =="44598" ~ "29510",#"st louis city", #Missouri county by the same name, geo_id = 29510
                            id == "33980" ~ "24510", #MD baltimore = Census lists as county by name #MD "baltimore city"
                            id =="1266545" ~ "22033",#"baton rouge",# this is LA "east baton rouge parish"
                            id == "47298" ~ "11001", #DC district of columbia
                            id == "111562" ~"15003",
                            TRUE ~ geo_id
                            )) %>% 
  filter(year == 2023) 

county_census_acfr_2023_round3 <- census_all %>% 
  filter(geo_id %in% county_acfr_reported_as_muni$geo_id) %>% 
  left_join(county_acfr_reported_as_muni) %>% 
  drop_na(id)

#####Binding 3 rounds#### 
county_census_acfr_2023_ <- county_census_acfr_2023_round1_2 %>% 
  #remove some before binding to the counties as muni to avoid double listing
  filter(!geo_id %in% county_acfr_reported_as_muni$geo_id) %>%
  
  #bind with non-fuctional counties
  rbind(county_census_acfr_2023_round3) %>% 
  
  #join urbanicity
  left_join(county_urb, by = "geo_id") %>% 
  
  #
  left_join(state_county_income, by = "geo_id") %>% 
  #select(all_of(fields_to_select))

  #append URL
  append_url() %>% select(-identifier) %>% distinct()

#Total: 
nrow(county_census_acfr_2023_) #3144 entities in county file, including:

county_census_acfr_2023_ %>%
  add_count(id, name = "n") %>%   # count by `id`, name the count column `n`
  filter(n > 1) %>%               # show rows where id occurs more than once
  distinct(id, .keep_all = TRUE) # optional: keep only one row per duplicate id

#####Adding flags####
county_census_acfr_2023 <- county_census_acfr_2023_ %>% 
  mutate(name_census = str_squish(name_census)) %>% 
  
  #flg_backfilled: 13 in OK, 20 in SD
  mutate(flg_backfilled = case_when(
         is.na(id) ~ 0,
         year == 2022 ~ 1,
         TRUE ~ 0)) %>% 
  
  #flg_acfr
  mutate(flg_acfr = ifelse(!is.na(id), 1, 0)) %>%  
  
  #flg_muni
  mutate(flg_muni = case_when(id %in% county_acfr_reported_as_muni$id & !is.na(id) ~ 1, 
                              id %in% consolidated_county_2023$id & !is.na(id) ~ 1, 
                           TRUE ~ 0)) %>% 
  mutate(flg_muni = case_when(name == "marion county" ~ 0, 
                              TRUE ~ flg_muni)) %>%  #consolidated, but report separately from muni)))

  
#Final cleaning
  mutate(name = ifelse(is.na(name), name_census, name)) %>% 
  select(-c(cousub, concit, place, source_year, name_midfile, government_id, nces_district_id)) 

#####Top300, Missing counties#####
#missing top 300:
missing_top300_county <- county_census_acfr_2023 %>% 
  arrange(desc(population)) %>% 
  slice(1:300) %>% 
  filter(is.na(id)) %>%  ## These 26 counties are NOT real missing. See explanation below

  filter(!name_census %in% c("kings county", "queens county", "bronx county", ## 5 NY counties already accounted for in NY city
                             "richmond county", "new york county")) %>%

  #Although listed in Census, these are not functional counties: CT, RI
  filter(!state.abb %in% c("CT", "RI")) %>%

  #not functional: eg. MA"hampden county"
  filter(funcstat != "N") %>%

  select(state.abb, name_census, population, funcstat) %>%
  mutate(category = "county",
         year = 2023)

# 9 counties TRUE missing as of June 13, 2025
missing_top300_county %>% View()


# all counties that do not have acfr:
county_census_acfr_2023 %>% 
  filter(flg_acfr == 0) %>% View() #839 missing, 811 true missing

#not true missing. Just not produced
county_census_acfr_2023_ %>% 
  filter(is.na(id)) %>% 
  
  #does not count these non-functional counties
  filter(!name_census %in% c("kings county", "queens county", "bronx county", ## 5 NY counties already accounted for in NY city
                             "richmond county", "new york county")) %>%

  #Although listed in Census, these are not functional counties: CT, RI
  filter(!state.abb %in% c("CT", "RI")) %>%

  #not functional: eg. MA"hampden county"
  filter(funcstat != "N") %>%

  #writexl::write_xlsx(paste0("tmp/missing_county_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"))
  View()

#count by year
county_acfrs %>% 
  group_by(year) %>% 
  summarise(n = n())

county_census_acfr_2023 %>% 
  summarise(tot = sum(population, na.rm = TRUE))

#####Final result counties####

county_census_acfr_2023 %>% 
  write.csv(file = paste0("output/all_counties_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

compare_latest_csv_versions(
  folder = "output",
  prefix = "all_counties_2023",
  output_excel = "output/counties_changes_report.xlsx"
)

####Municipalities####
# Census calls Incorporated Place & Minor Civil Division

#####Joining ACFRs muni & census#####


#Step 1: get ACFRs municipalities

# IDs to exclude: those in county_acfrs_2023 but NOT in consolidated
ids_to_exclude <- setdiff(county_acfrs_2023$id, consolidated_county_2023$id)

municipality_acfrs_ <- acfrs_general_purpose %>% 
  filter(year == 2023) %>% 
  
  # exclude state 
  filter(!id %in% state_all$id) %>% 
  
  # exclude counties in acfr county section above, 
  filter(!id %in% ids_to_exclude) %>% 

  # get income, only about 500 entities has income info
  left_join(city_income) 

nrow(municipality_acfrs_ %>% filter(year == 2023))

#Step 2: Join with census_municipalities to get population using geo_id

######dictionary######
dictionary_acfrID_censusGEOID <- readxl::read_xlsx("data/_dictionary_acfrID_censusGEOID.xlsx") %>% 
  drop_na() %>%  select(state.abb, id, geo_id) %>% 
  mutate(geo_id = as.character(geo_id), 
         geo_id = str_squish(geo_id), 
         id = str_squish(id)) %>% 
  rename(correct_geoID = geo_id) 


######fixing some geo_id#####
municipality_all_0 <- municipality_acfrs_ %>% 
  mutate(id = as.character(id)) %>% 
  left_join(dictionary_acfrID_censusGEOID, by = c("id", "state.abb")) %>%
  mutate(geo_id = coalesce(correct_geoID, geo_id)) %>%
  select(-correct_geoID)


municipality_all_ <- municipality_all_0 %>%  
  
  # join with Census, exclude some
  left_join(
    (census_all %>%
       # need to get distinct census entity to avoid many-to-many join. Many rows have 
       #the same geo_id b/c enity can be listed in difference sumlev such as 172, 071, 061, 61
       filter(funcstat != "F") %>% 
       filter(!is.na(geo_id)) %>% 
       filter(!sumlev %in% c(071, 40, 50, 172)) %>% #071 = Minor Civil Division place part
       select(sumlev, state.abb, state.name, name_census, geo_id, population) %>% 
       distinct() 
     ), 
            by= c("geo_id", "state.abb", "state.name"))  


#####Get population in those missing#####

#check missing population
nrow(municipality_all_ %>% filter(is.na(population)))


######phase 1:got population after using join by geo_id above#####
muni_phase1 <- municipality_all_ %>% 
  filter(!is.na(population)) %>% 
  
  #TODO: temporary fixed. Already deleted 1 dup on portal 
  filter(!id %in% c("1270328","68389"))

#identify duplicated - NONE in this phase1
muni_phase1 %>% 
  select(geo_id) %>% 
  #select(state.abb, name, id, total_liabilities) %>% 
  filter((duplicated(.) | duplicated(., fromLast = TRUE)))

######phase 2: matches those without geo_id using name#####

#get a subset of census 
census_muni <- census_all %>% 
  filter(!is.na(geo_id)) %>% # some entities in census duplicated: 1 has geo_id, 1 does not
  filter(funcstat != "F") %>% 
  filter(!sumlev %in% c(071, 40, 50, 172)) %>% #071 = Minor Civil Division place part
  select(state.abb, state.name, name_census, population, geo_id) 

# join this subset with the remainder of acfr muni after phase 1
muni_phase2_ <- municipality_all_ %>% 
  filter(!id %in% c(muni_phase1$id)) %>%  # subtract muni_phase1
  select(-c(population, geo_id)) %>% 
  
  left_join(census_muni, 
            by = c("state.abb", "state.name", "name" = "name_census")) %>% 
  
  filter(!is.na(population)) 
  
#check duplicated caused by some entities have same name & state, but different counties

muni_phase2_duplicated <- muni_phase2_ %>% 
  select(state.abb, name, id, total_liabilities) %>% 
  filter((duplicated(.) | duplicated(., fromLast = TRUE)))

#filter out those duplicated
muni_phase2 <- muni_phase2_ %>% 
  filter(!id %in% muni_phase2_duplicated$id) 

#check dup again:
muni_phase2 %>% 
  #select(geo_id) %>% 
  select(state.abb, name, id, total_liabilities) %>% 
  filter((duplicated(.) | duplicated(., fromLast = TRUE)))

######phase 3: clean up names to further match with census.######  
muni_phase3_ <- municipality_all_ %>% 
  filter(!id %in% c(muni_phase1$id, muni_phase2$id))  %>% # remaining part after muni_phase2

  select(-c(population, geo_id)) %>% 
  #clean up names for specific state to map with census
  mutate(name = case_when(state.abb == "MI" ~ str_remove(name, "charter"),
                                state.abb == "IL" ~ str_replace(name, "mt", "mount"),
                                TRUE ~ name
                                )) %>% 
# join again with census
  left_join(census_muni %>% 
              
              #clean up census for specific state
              mutate(name_census = case_when(state.abb == "NJ" ~ str_remove(name_census, "borough"),
                                             TRUE ~ name_census)) %>% 
              mutate(name_census = str_remove(name_census, "city$"),
                     name_census = str_squish(name_census)
                     ),
          by = c("state.abb", "state.name", "name" = "name_census")) %>% 
  
   filter(!is.na(population)) %>% 
  distinct() 

#identify duplicated
muni_phase3_duplicated <- muni_phase3_ %>% 
  select(state.abb, name, id, total_liabilities) %>% 
  filter((duplicated(.) | duplicated(., fromLast = TRUE)))

#filter out those duplicated
muni_phase3 <- muni_phase3_ %>% 
  filter(!id %in% muni_phase3_duplicated$id) 

#check dup again
muni_phase3 %>% 
  #select(geo_id) %>% 
  select(id) %>% 
  filter((duplicated(.) | duplicated(., fromLast = TRUE)))

######phase 4: manually solve the remaining dup######

muni_phase4 <- municipality_all_ %>% 
  mutate(id = as.character(id)) %>% 
  
  filter(!id %in% c(muni_phase1$id, muni_phase2$id, muni_phase3$id)) %>%    # remaining part after muni_phase2
  select(-c(geo_id, population)) %>% 
 
  select(state.abb, state.name, id) %>% 
  left_join(dictionary_acfrID_censusGEOID %>% rename(geo_id = correct_geoID)) %>% 

#select(-name_census) %>% 
  
  left_join(census_muni, by = c("state.abb", "state.name", "geo_id")) %>% 
  filter(!is.na(population)) 

#check dup again
muni_phase4 %>% 
  select(geo_id) %>% 
  #select(id) %>% 
  filter((duplicated(.) | duplicated(., fromLast = TRUE)))

muni_phase4_no_pop <- municipality_all_ %>% 
  filter(!id %in% muni_phase1$id) %>% 
  filter(!id %in% muni_phase2$id) %>% 
  filter(!id %in% muni_phase3$id) %>% 
  filter(!id %in% muni_phase4$id)
  
#check if there's a big entity
muni_phase4_no_pop %>% 
  select(state.abb, name, id, total_liabilities, 
                              total_assets) %>% View()
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
                            id == "71110" ~ "1983145", #webster city city / diff from webster city 1983055
                            id == "1266152" ~ "4254688", # northampton township in Bucks county PA
                            id == "131132" ~ "5508225", #bloomer city WI
                            TRUE ~ geo_id))  


muni_population %>% 
  group_by(year) %>% 
  summarise(tot = sum(population, na.rm = TRUE))

######phase 5: Special cites#####
#need to get some counties that are also cities. But got excluded from above
special_cities <- acfrs_general_purpose %>% 
  filter(year == 2023) %>% 
  # get geo id in these cities --> from there get income & census data
  filter(id %in% c(
                   "111562")) %>% #HI city and county of honolulu
  
  mutate(geo_id = case_when(id == "111562" ~ "15003",
                            TRUE ~ geo_id)) %>% 
  left_join(city_income) %>% 
  left_join(df_state) %>% 
  left_join(census_all) %>% 
  
  mutate(median_hh_income = ifelse(name == "city and county of honolulu", 104300, median_hh_income)) %>% 
  mutate(url = NA) %>% 
  select(-identifier)

municipality_all <- muni_population %>%  
  
  rbind(special_cities %>% 
          select(-c(state, county, place, cousub, concit, funcstat))) %>% 
  #avoid double count - these entities are counted in county_gov
  #filter(name = "denver county") %>% 
  select(any_of(fields_to_select), url) %>% 
  
  # create a dummy urban_pop to match the df size with state and counties
  mutate(urban_pop = NA, 
         pct_urban_pop = NA) 

#####Result muni#####
# municipality_all %>% 
#   write.csv("output/all_municipalities_2020_2023.csv")
 
municipality_all_2023 <- municipality_all %>% filter(year == 2023)

#population coverage of municipalities acfr/ census
(municipality_all_2023 %>% select(state.abb, name, population) %>% distinct() %>% 
    summarise(tot= sum(population, na.rm = TRUE))) /
  
(census_municipalities %>% 
  summarise(tot = sum(population, na.rm = TRUE)))

municipality_all %>% 
  group_by(year) %>% 
  summarise(n = n())


######Flags#####
# need to add these in municipality_all_2023
muni_top300_missing <- census_city_top300 %>% 
  filter(!geo_id %in% municipality_all_2023$geo_id) %>% 
  rename(name = name_census) %>% 
  left_join(df_state)

municipality_all_2023_flag <- municipality_all_2023 %>% 
  bind_2df_different_size(muni_top300_missing) %>% 
  
  #flg_acfr
  mutate(flg_acfr = case_when(
    geo_id %in% muni_top300_missing$geo_id ~ 0,                                              # 5 in top 300 missing ACFR
    
    TRUE ~ 1
  )) %>% 
  
#flg_county
mutate(flg_county = case_when(id %in% consolidated_county_2023$id | id == "111562" ~ 1, #honolulu is not consolidated, but counted as a county too
                              
                              TRUE ~ 0)) %>% 
  mutate(year = 2023,
         category = "General Purpose") %>% 
  filter(!(name == "lexington-fayette urban county" & is.na(geo_id))) 

######Coordinates######

coordinates_muni <- readRDS("output/archive/municipalities_with_coordinates.rds") %>% 
  select(state.abb, id, latitude, longitude, geo_id) %>% 
  drop_na(latitude) %>% 
  drop_na(longitude) %>% 
  distinct() %>% 
  mutate(id = as.character(id))

# get some extra coordinates manually
extra_entities <- readxl::read_xlsx("data/coordinates_extra_entities.xlsx") %>% 
  mutate(name = str_squish(name)) %>% 
  #filter(name == "louisville/jefferson county metro government")
  mutate(longitude_extra = longitude,
         latitude_extra = latitude) %>% 
  select(state.abb, name, latitude_extra, longitude_extra)


#### final
  municipality_final_2023 <- municipality_all_2023_flag %>% 
    
  # join coordinate result 
    left_join(coordinates_muni, by = c("state.abb", "id", "geo_id")) %>% 
  
    # fixing some coordinate 
  mutate(latitude = case_when(geo_id == "3651000" ~ 40.7555, #NYC
                              TRUE ~ latitude)) %>% 
  
  mutate(longitude = case_when(geo_id == "3651000" ~ -73.9739, 
                               TRUE ~ longitude)) %>% 
  
  # add extra coordinate collected manually
    left_join(extra_entities, by = c("state.abb", "name")) %>% 
  
    mutate(
      latitude = ifelse(is.na(latitude), latitude_extra, latitude),
      longitude = ifelse(is.na(longitude), longitude_extra, longitude)
    ) %>%
    select(-latitude_extra, -longitude_extra)
   
  
# check missing coordinates:
  municipality_final_2023 %>% 
    select(state.abb, name, latitude, longitude, geo_id, population) %>% 
    filter(is.na(latitude) | is.na(latitude)) %>% #write.csv("tmp/missing_coordinates.csv")
   View()

######Check duplicates####
municipality_final_2023 %>% select(state.abb, name, id, total_liabilities) %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) 

municipality_final_2023 %>% 
  select(geo_id) %>% 
  filter(!is.na(geo_id)) %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) 

#####Final result muni#####
municipality_final_2023 %>% 
  write.csv(file = paste0("output/all_municipalities_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

# compare_latest_csv_versions(
#   folder = "output",
#   prefix = "all_municipalities_2023",
#   output_excel = "output/municipalities_changes_report.xlsx"
# )

# this part to display in progress report app:
# missing_top300_cities_2023 %>% 
# mutate(year = 2023,
#        category = "municipality") %>% 
# select(-c(geo_id)) -> missing_top300_muni

####School districts####
dictionary <- readRDS("data/dictionary.RDS") 

school_districts_ <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "School District") %>% 
  mutate(id = as.character(id)) %>% 
  select(any_of(fields_to_select)) %>% 
  
  #Exclude some sd in Montana to avoid double count - they will be join later as pairs exceptions
  filter(!id %in% MT_sd_pairs$id) %>% 

  #joint with dictionary to get ncesID
  left_join(dictionary) %>% 
  
  #join with nces to get county, city, long, lat
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) %>% 
 
   #append URLs
  append_url() %>% select(-identifier) 
  

# Now bind with special cases
school_districts_all <- bind_2df_different_size(school_districts_, exceptions) %>% 
  # create a dummy urban_pop to match the df size with state and counties
  mutate(urban_pop = NA, 
         pct_urban_pop = NA, 
         median_hh_income = NA) %>% 
  mutate(enrollment_23 = as.numeric(enrollment_23)) %>% 
  select(-c(enrollment_20, enrollment_21, enrollment_22)) %>% 
  
  # SC: 2023, Barnwell County School District 45, 80 =  #Barnwell County Consolidated School District
 mutate(enrollment_23 = ifelse(id == "38322", sum(1974 + 1194), enrollment_23))


school_districts_all %>% 
  group_by(year) %>% 
  summarise(n = n())

school_districts_all %>% 
  group_by(year) %>% 
  summarise(tot = sum(enrollment_23, na.rm = TRUE))
   
school_districts_2023 <- school_districts_all %>% filter(year == 2023) 

#coverage
(school_districts_2023 %>% 
  summarise(tot = sum(enrollment_23, na.rm = TRUE))) /
(nces %>% 
  summarise(tot = sum(enrollment_23, na.rm = TRUE)))


#####Top300 sd #####

#missing only DC
school_districts_2023 %>% 
  filter(ncesID %in% sd_top300_nces$ncesID) %>% View()

sd_top300_nces %>% 
  filter(!ncesID %in% school_districts_2023$ncesID) %>% View()

#####Flag#####
school_districts_final_2023 <- school_districts_2023 %>% 
  
  #add sd in top 300 but not available in ACFR
  bind_rows(
    tibble(state.abb = "DC", state.name = "District of Columbia", ncesID = "1100030",
           name = "district of columbia public schools", 
           category = "School District",
           year = 2023)) %>% 
  
  mutate(flg_acfr = case_when(ncesID == "1100030" ~ 0, #district of columbia public schools blended in DC
              
                              TRUE ~ 1)) %>% 
  mutate(flg_note = case_when(ncesID %in% c("1100030") ~ 1,
                              TRUE ~ NA_real_)) %>% 
  mutate(note = case_when(ncesID %in% c("1100030") ~ "blended component units",
                          TRUE ~ NA)) 


######Check duplicates####

#no id duplicates
school_districts_final_2023 %>%
  select(id) %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) %>% View()

#some ncesID duplicates because the parsers record an entity twice under different names 
dup_ncesid <- school_districts_final_2023 %>%
  filter(!is.na(ncesID)) %>% 
  select(ncesID) %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) 


dup_fixed <- school_districts_final_2023 %>%
  filter(ncesID %in% dup_ncesid$ncesID) %>%
  arrange(ncesID) %>%
  group_by(ncesID) %>%
  slice(1) %>%
  ungroup() 

school_districts_final_2023_ <- school_districts_final_2023 %>% 
  filter(!id %in% dup_fixed$id) 

#check dup again after fixing:
school_districts_final_2023_ %>% 
filter(!is.na(ncesID)) %>% 
  select(ncesID) %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) %>% View()

#TODO: fill ncesID in these entities
school_districts_final_2023_ %>% filter(is.na(ncesID)) %>% 
  select(state.abb, id, name) %>% View()

#####Final#####
school_districts_final_2023_ %>% 
  write.csv(file = paste0("output/all_schooldistricts_2023_", 
                          format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

typeof(school_districts_final_2023_$pop)

####Tracking changes####
##########
# missing_ncesID <- school_districts_2023 %>% 
#   filter(is.na(ncesID)) 
# 
# missing_ncesID %>% write.csv("tmp/acfr_schools_missing_ncesID.csv")
# 
# unmatched_ncesID <- nces %>% filter(!ncesID %in% school_districts_2023$ncesID)
# 
# unmatched_ncesID %>% write.csv("tmp/nces_schools_unmatched_with_acfr.csv")

compare_latest_csv_versions(
  folder = "output",
  prefix = "all_schooldistricts_2023",
  output_excel = "output/schooldistrict_changes_report.xlsx"
)

####Missing top300####
# this file is for displaying status on progress app
#missing state
state_2023 <- state_all %>% filter(year == 2023)
census_all %>% filter(sumlev == 40) %>% 
  select(state.abb, population, name_census) %>% 
  mutate(category = "state", 
         year = 2023) %>% 
  filter(!state.abb %in% state_2023$state.abb) %>% 
  filter(state.abb != "DC") -> missing_top300_state

#bind all
rbind(missing_top300_state,
      missing_top300_muni,
      missing_top300_county) %>% 
  rename(state_abbreviation = state.abb,
         name = name_census) #%>% write.csv("tmp/missing_top300.csv")

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

