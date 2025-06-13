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

# step 4: manuall add & fix geo_id for some important cities 
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
                            TRUE ~ geo_id
                            )) 

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
                      "population", "urban_pop", "pct_urban_pop", "median_hh_income")


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
  left_join(state_county_income) %>% 
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
                
                                name == "macon-bibb county"~ "bibb county", # GA; Not macon county
                                name == "nashville-davidson county" ~ "nashville-davidson metropolitan government",
                                TRUE ~ name_clean)) 


county_acfrs_2023 <- county_acfrs %>% filter(year == 2023)

#####Round 1#####
#join Census sumlev = 50, some of sumlev = 170 with acfr
county_census_acfr_2023_round1 <- 
  
  #  3150 Census counties + few consolidated cities 
  (census_all %>%  
  filter(sumlev %in% c(50, 170)) %>% 
  filter(!name_census %in% c("milford city", "indianapolis city")) # these are 170, but not a county
)  %>% 
  
  #join with counties in acfr
  left_join(county_acfrs_2023, 
                            by = c("state.abb", "state.name", "name_census" = "name_clean")) 

#####Round 2##### 
#some counties exist in ACFR but do not get matched to census counties
# because Census identifies these as incorporated places (sumlev= 162), not counties
county_acfrs_2023 %>% 
  filter(!id %in% county_census_acfr_2023_round1$id) %>% View()

# What are the corresponding counties of these entities? --> need to copy them to county:
#--->  Solution: get these from Census to add back
incorporated_place_census <- census_all %>% 
  filter(name_census %in% c("hartsville/trousdale county", 
                            "lexington-fayette urban county", 
                     "lynchburg, moore county metropolitan government", 
                     "georgetown-quitman county unified government", 
                     "anaconda-deer lodge county")) %>% drop_na(geo_id)


county_census_acfr_2023_round2 <- incorporated_place_census %>% 
  left_join(county_acfrs_2023, 
            by = c("state.abb", "state.name", "name_census" = "name_clean")) 

#####Round 3#####
# the following counties are reported in municipalities ACFR. 
#--> mark these municipalities in ACFRs as counties too: 
county_acfr_reported_as_muni <- acfrs_general_purpose %>% 
  filter(id %in% c("230476",#"virginia beach", #"230476" VA county in census as "virginia beach"
                   "230389", #norfolk", #VA county in census as: "norfolk city"
                   "230275",#"chesapeake", #VA county in census as "chesapeake city"
                   
                   # MO st louis city whose geo_id is 29510 is independent city, also a county, population 300k. 
                   #This is different from other St Louis county whose geo_id is 29189, population > 1M
                   "44598" ,#"st louis city", #Missouri county by the same name, geo_id = 29510
                   
                   "33980" ,#"baltimore", #MD  baltimore county
                   "1266545" ,#"baton rouge",# this is LA "east baton rouge parish"
                   "1266697", #"new orleans") # this is orleans parish LA
  
                   "32292", #FL jacksonville = FL duval county
                   "42714", #PA philadelphia = PA philadelphia county
                   "33980", #MD baltimore 
                   "115965", #KY louisville/jefferson county metro government
                   "1267141", #TN nashville-davidson county
                   "47298", #DC columbia
                   "1267156" # KY "#lexington-fayette urban county  = KY fayette county geo 21067
                   )) %>% 
  
  #When identify as cities, they have different geo_id --> need to get geo_id as counties to join back to county_census_acfr_2023_
  #get these geo_id for county in census
  
  mutate(geo_id = case_when(id == "230476" ~ "51810", 
                            
                            id =="230389" ~ "51710", #norfolk", #VA county in census as: "norfolk city"
                            id =="230275" ~ "51550",#"chesapeake", #VA county in census as "chesapeake city"
                            
                            # MO st louis city whose geo_id is 29510 is independent city, also a county, population 300k. 
                            #This is different from other St Louis county whose geo_id is 29189, population > 1M
                            id =="44598" ~ "29510",#"st louis city", #Missouri county by the same name, geo_id = 29510
                            
                        
                            id =="1266545" ~ "22033",#"baton rouge",# this is LA "east baton rouge parish"
                            id =="1266697" ~ "22071", #"new orleans") # this is orleans parish LA
                            
                            id == "32292" ~ "12031", #geo_id of FL duval county, equivalent to FL jacksonville
                            id == "42714" ~ "42101", #PA philadelphia
                            id == "33980" ~ "24510", #MD baltimore = Census lists as county by name #MD "baltimore city"
                            id == "115965" ~ "21111", #KY louisville/jefferson county metro government = KY jefferson county
                            id == "1267141"~ "47037", #TN nashville-davidson county = TN davidson county
                            id == "47298" ~ "11001", #DC district of columbia
                            
                            id == "1267156" ~ "21067",# KY "#lexington-fayette urban county  = KY fayette county geo 21067
                            TRUE ~ geo_id
                            )) %>% 
  filter(year == 2023) 

county_census_acfr_2023_round3 <- census_all %>% 
  filter(geo_id %in% county_acfr_reported_as_muni$geo_id) %>% 
  left_join(county_acfr_reported_as_muni) %>% 
  drop_na(id)

#####Bind 3 rounds#### 
county_census_acfr_2023_ <- rbind(county_census_acfr_2023_round1, 
                                       county_census_acfr_2023_round2) %>% 
  
  #remove some before binding to the counties as muni to avoid double listing
  filter(!geo_id %in% county_acfr_reported_as_muni$geo_id) %>% 
  
  # bind back to round 3
  rbind(county_census_acfr_2023_round3) %>% 

  #join urbanicity
  left_join(county_urb, by = "geo_id") %>% 
  
  #
  left_join(state_county_income, by = "geo_id") %>% 
  #select(all_of(fields_to_select))

  #append URL
  append_url() %>% select(-identifier) %>% distinct()


#Total: 
nrow(county_census_acfr_2023_) #3155 entities in county file, including:
nrow(census_all %>% filter(sumlev == 50)) #3144 counties in original census
nrow(incorporated_place_census) # 5 additional incoporated places into county ACFR, sumlev == 162
nrow(census_all %>% filter(sumlev == 170) %>% 
       filter(!name_census %in% c("milford city", "indianapolis city"))) # 6 Consolidated city except 2

#####Consolidated counties#####
# more on consolidated city, county & county equivalent: https://www.census.gov/programs-surveys/geography/about/glossary.html#par_textimage_12

# Step 1: identify consolidated / unified counties in census
# there are 33 consolidated counties, all of which are already in county_census_acfr_2023
census_all %>% 
  filter(sumlev == 50) %>% 
  filter(funcstat == "C") %>% 
  select(state.abb, name_census, funcstat, population) %>% 
  View()

# Are they all county and municipalities ? No.
# Counties are: marion county, echols county, city & county of butte silver bow,
# lynchburg, moore county metropolitan government

# Step 2: Identify acfrs that report both city and county. 
#NO special treatment needed if: 
# city and county has their own reports and both exist in census, 
# either city or county does not exist independently in census

#NOTE: 
#1. IN marion county and Indianapolis are component units of 
#the Consolidated City of Indianapolis—Marion County. But they have separate financial reports
#2. KY Lexington-Fayette Urban County - Technically is not a county (sumlev = 162). Lexington city does not exist independently in census
#3. KY louisville/jefferson county metro government.  Census does not have louisville city. #jefferson county, geo_id for county 21111
#4. KY orleans parish does not have a report
#5. NY 5 counties are listed in census as counties, but are incorporated NYC ACFR
#6. #MT city & county of butte silver bow, Technically is not a county (sumlev = 170). Census does not have Butte city separate
#7. TN lynchburg, moore county metropolitan government, - Technically is not a county (sumlev = 162). Census does not have lynchburg 

consolidated_county_city_acfr <- acfrs_general_purpose %>% 
filter(id %in% c("1266824", #AK
                 "91930", "95986", "31609", # AK
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
                 "40839", #KS wyandotte county, KS kansas city consolidated in Wyandotte County,  
                 "1267156", #KY Lexington-Fayette Urban County , Lexington city 
                 "115965", #louisville/jefferson county metro government
                 "40777", #anaconda-deer lodge county, deer lodge city 
                 #"81613", #MT city & county of butte silver bow, Census does not have Butte city separate
                 "42714", #PA philadelphia city, also a county
                 "1267141", #TN nashville-davidson metropolitan government
                 #"1268468", #TN lynchburg, moore county metropolitan government, Census does not have lynchburg 
                 "1268161", #TN hartsville/trousdale county
                  "1266998", "148608", "33244", 
                 "1267157", "115965", "81613", "1267141"
                 )) %>% 
 left_join(df_state) %>% distinct() %>% select(-geo_id) 

# these consolidated counties already in county_census_acfrs_2023_
consolidated_county_city_acfr %>% filter(year == 2023) %>% 
  filter(id %in% county_census_acfr_2023_$id) %>% 
  View()

# consolidated counties NOT in county_census_acfrs_2023_: 
#1. IN marion county and Indianapolis are component units of 
#the Consolidated City of Indianapolis—Marion County. But they have separate financial reports
consolidated_county_city_acfr %>% filter(year == 2023) %>% 
  filter(!id %in% county_census_acfr_2023_$id) %>% 
  View()

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


#####Adding flags####
county_census_acfr_2023 <- county_census_acfr_2023_ %>% 
  mutate(flg_acfr = ifelse(!is.na(id), 1, 0),
         
         #some listed in Census as muni, but should also counted as county to display in acfr tool
         flg_county = case_when(
           name %in% c("city & county of butte silver bow") ~ 1, #acfr: 
           sumlev == 50 ~ 1,
           TRUE ~ 0
         ),
         
         # identify some counties also as city:
         # but not all sumleve == 50 & funcstat == "C" need to be flagged as city in the tool, 
         #because the cities have their own acfr report
         
         # Census counts as county, but NOT as Municipalities are --> flg_muni = 0 
         flg_muni = case_when(
           name %in% c("marion county", "echols county", "city & county of butte silver bow", 
                       "lynchburg, moore county metropolitan government",
                       "louisville/jefferson county metro government") ~ 0,  # override specific cases first
           sumlev %in% c(170, 162) ~ 1,
           sumlev == 50 & funcstat == "C" ~ 1,
           TRUE ~ 0
         )) 

#county-city-metro government: 
county_census_acfr_2023 %>% filter(flg_muni == 1) %>% View()


#####Top100, Top300, Missing counties#####
#missing top 100 county - none of these are real missing
county_census_acfr_2023 %>% arrange(desc(population)) %>% 
  slice(1:100) %>% 
  filter(is.na(id)) %>% 
  View()

#missing top 300:
missing_top300_county <- county_census_acfr_2023 %>% arrange(desc(population)) %>% 
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

#count by year
county_acfrs %>% 
  group_by(year) %>% 
  summarise(n = n())

county_census_acfr_2023 %>% 
  summarise(tot = sum(population, na.rm = TRUE))

# all counties that do not have acfr:
county_census_acfr_2023_ %>% 
  filter(is.na(id)) %>% 
  #writexl::write_xlsx(paste0("tmp/missing_county_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"))
  View()

#write.csv(top100_counties, "output/top100_counties.csv")
# save file
#write.csv(county_all, "output/all_counties_2020_2023.csv")

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
municipality_acfrs_ <- acfrs_general_purpose %>% 
  
  # exclude state 
  filter(!id %in% state_all$id) %>% 
  
  # exclude counties already accounted for in county section above, but DO NOT exclude those are also muni
  filter(!id %in% county_acfrs$id | id == "54175") %>%  # CA city and county of san francisco
  
  # get income, only about 500 entities has income info
  left_join(city_income) 


#Check those do NOTE have income data
#TODO: 
city_income %>% filter(!geo_id %in% municipality_acfrs_$geo_id) %>% View()

nrow(municipality_acfrs_ %>% filter(year == 2023))

#Step 2: Join with census_municipalities using geo_id

municipality_all_ <- municipality_acfrs_ %>% 
  #select(state.abb, state.name, name, id, geo_id, year) %>% 
  left_join(census_all %>% filter(!is.na(geo_id)), by= c("geo_id", "state.abb", "state.name"))  


#####Get population in those missing#####
#check missing population
nrow(municipality_all_ %>% filter(is.na(population)))
nrow(municipality_all_ %>% filter(year == 2023) %>% filter(is.na(population)))

#phase 1: already got population after using join by geo_id above
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

#phase 4: 

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
  #filter(name = "denver county") %>% 
  select(any_of(fields_to_select), url) %>% 
  
  # create a dummy urban_pop to match the df size with state and counties
  mutate(urban_pop = NA, 
         pct_urban_pop = NA)

#TODO: population in all municipalities
municipality_all %>% filter(year == 2023) %>% 
  filter(is.na(population)) %>% 
  View()

#####Result muni#####
municipality_all_2023 <- municipality_all %>% filter(year == 2023)

#still some muni without geo_id
municipality_all_2023 %>% filter(is.na(geo_id)) %>% View()

#population coverage of municipalities acfr/ census
(municipality_all_2023 %>% select(state.abb, name, population) %>% distinct() %>% 
    summarise(tot= sum(population, na.rm = TRUE))) /
  
(census_municipalities %>% 
  summarise(tot = sum(population, na.rm = TRUE)))

municipality_all %>% 
  group_by(year) %>% 
  summarise(n = n())

municipality_all %>% 
  group_by(year) %>% 
  summarise(tot = sum(population, na.rm = TRUE))

######Top and missing#####
#Top 100 cities
anti_join(census_city_top100, municipality_all_2023, by = "geo_id") %>% 
  
  #consolidated with county, just need to copy to city list
  filter(!name_census %in% c("san francisco city", "philadelphia city", 
                             "jacksonville city", "denver city")) %>% 
  View()

######Top300 cities & flag#####
top300_cities_2023 <- census_city_top300 %>% 
  left_join(acfrs_general_purpose %>% filter(year == 2023), by = c("geo_id", "state.abb")) %>% 
              distinct() %>% 
  
  mutate(flg_acfr = if_else(is.na(id), 0, 1)) %>% 
  mutate(flg_muni = 1) %>% 
  
  #mark those are also county
  mutate(flg_county = case_when(id %in% c("1266289", #CO denver county
                                          "42714", # PA philadelphia
                                          "32292", #FL jacksonville
                                          "54175" # CA city and county of san francisco
                                          ) ~ 1,
                                 TRUE ~ 0)) %>% 
  
  #
  mutate(name = ifelse(is.na(name), name_census, name),
         name = str_remove(name, "city$"))


#missing cities in top 300  - missing 6 as of June 13, 2025
missing_top300_cities_2023 <- top300_cities_2023 %>% 
  filter(is.na(id)) 

#KS kansas city consolidated in Wyandotte County 
anti_join(census_city_top300, municipality_all_2023, by = "geo_id") %>% 
 View()
  
  # this part to display in progress report app:
  # missing_top300_cities_2023 %>% 
  # mutate(year = 2023,
  #        category = "municipality") %>% 
  # select(-c(geo_id)) -> missing_top300_muni

#####Final result muni#####
top300_cities_2023 %>% 
  write.csv("output/top300_cities_2023.csv")

municipality_all %>% 
  filter(year == 2023) %>% 
  write.csv(file = paste0("output/all_municipalities_2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

compare_latest_csv_versions(
  folder = "output",
  prefix = "all_municipalities_2023",
  output_excel = "output/municipalities_changes_report.xlsx"
)


####School districts####
dictionary <- readRDS("data/dictionary.RDS") 
acfrs_data
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
         median_hh_income = NA) %>% 
  mutate(
    enrollment_23 = as.numeric(enrollment_23)) %>% 
  select(-c(enrollment_20, enrollment_21, enrollment_22))

# Save 
school_districts_all %>% write.csv("output/school_districts_all_2020_2023.csv")

school_districts_all %>% 
  group_by(year) %>% 
  summarise(n = n())

school_districts_all %>% 
  group_by(year) %>% 
  summarise(tot = sum(enrollment_23, na.rm = TRUE))
  
# 
school_districts_2023 <- school_districts_all %>% filter(year == 2023) 

#####TOp300 sd #####
school_districts_2023 %>% 
  summarise(tot = sum(enrollment_23, na.rm = TRUE))

nces %>% 
  summarise(tot = sum(enrollment_23, na.rm = TRUE))

school_districts_2023 %>% 
  filter(ncesID %in% sd_top300_nces$ncesID) %>% View()

school_districts_2023 %>% 
  write.csv(file = paste0("output/all_schooldistricts_2023_", 
                          format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

top300_sd_2023 <- sd_top300_nces %>% 
  filter(ncesID %in% school_districts_2023$ncesID) 

#where are these 3?
sd_top300_nces %>% 
  filter(!ncesID %in% school_districts_2023$ncesID) %>% View()

write.csv(file = paste0("output/top300_sd_2023_", 
                        format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

# all of these are uploaded. Hgarb are checking. June 9
sd_top300_nces %>% 
  filter(!ncesID %in% school_districts_2023$ncesID) %>%
  #filter(name_nces != "detroit public schools community district") %>% 
  View()

# TODO: some MT acfr report include more than 1 school districts. Need to treat MT separately 

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



