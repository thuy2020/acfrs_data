
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
options(scipen = 999)

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


####State######
state_gov_4years <- state_gov %>% 
  #join income
  left_join(income) %>% 
  #get a set of variables
  select(all_of(fields_to_select)) 
  
  #append url
state_4years <- append_url(state_gov_4years) %>% 
  select(-identifier)

#double check missing:
state_4years %>% add_count(state.name) %>% filter(n<4) %>% 
  select(state.abb, n)

state_4years %>% filter(year == 2023) %>% select(name, year) %>% View()

state_4years %>% write.csv("output/all_states_4years_2020_2023.csv")

# Missing states: CA, NV, MS, IL, AZ
#https://www.sco.ca.gov/ard_state_acfr.html
#https://www.dfa.ms.gov/publications
#https://controller.nv.gov/FinancialRpts/CAFR/Home/
# https://illinoiscomptroller.gov/financial-reports-data/find-a-report/comprehensive-reporting/annual-comprehensive-financial-report/
# AZ has financial report 2023 but not ACFRs 2023: https://gao.az.gov/financials/acfr

####County####

##Consolidated county-city##

#These counties are consolidated --> have some other name in acfrs list:  
consolidated_city_county <- acfrs_general_purpose %>% 
  filter((name == "city and county of san francisco" & state.abb == "CA") |
           (name == "philadelphia" & state.abb == "PA") |
           (name == "jacksonville" & state.abb == "FL") |
           (name == "the metropolitan government of nashville and davidson county" & state.abb == "TN")  |
           name == "city and county of honolulu") %>% 
  left_join(df_state) %>% 
  
  #Same population, but when it's identified as county, it has different geo_id. The geo_id as a metro is different and does not contain income & urbanicity info. Use these geo_id to get income and urbanicity. 
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
consolidated_city_county_all <- consolidated_city_county %>% left_join(consolidated_city_county_population) %>% 
  left_join(consolidated_city_county_urbanicity) %>% 
  left_join(consolidated_city_county_income) %>% 
  select(all_of(fields_to_select))

## All counties ###

county_gov_all <- county_gov %>% 
  
  #get income
  left_join(income) %>% 
  
  select(all_of(fields_to_select)) %>% 
  # bind with consolidated
  rbind(consolidated_city_county_all)

  #append URL
county_all <- append_url(county_gov_all) %>% 
  select(-identifier)

county_all %>% filter(year == 2023) %>% View()


# Find acfrs entities from the list of Top 100 county census
top100_county_3years <- county_all %>% 
  filter(geo_id %in% census_county_top100$geo_id)

# Find acfrs entities from the list of Top 200 county census
top200_county_4years <- county_all %>% 
  filter(geo_id %in% census_county_top200$geo_id) 

top200_county_4years %>% select(state.abb, name, year, id) %>% 
  add_count(id) %>% select(-year) %>% 
  distinct()%>% filter(n<4) #%>% write.csv("tmp/top200_counties_missing_2023.csv")

# Missing states 2023: CA, NV, MS, IL, AZ

# missing county top100 year 2023
# MI macomb county https://www.macombgov.org/departments/finance-department/financial-transparency/annual-comprehensive-financial
# TX hidalgo county https://www.hidalgocounty.us/1288/Annual-Financial-Report
# NJ middlesex county https://www.middlesexcountynj.gov/government/departments/department-of-finance/financial-information/-folder-157
# PA montgomery county https://www.montgomerycountypa.gov/331/Annual-Financial-Statements-Reports
# OH hamilton county https://www.hamiltoncountyohio.gov/government/departments/budget_and_strategic_initiatives/annual_information_statement
# WA snohomish county https://sao.wa.gov/reports-data/audit-reports?SearchText=snohomish%20county&StartDate=&EndDate=
# OK oklahoma county https://www.sai.ok.gov/audit-reports/?counties=55&years=%2C2023&orgs=
# MA 	norfolk county
# NJ hudson county https://www.hcnj.us/finance/
# CO 	denver county https://www.denvergov.org/Government/Agencies-Departments-Offices/Agencies-Departments-Offices-Directory/Department-of-Finance/Financial-Reports/Annual-Comprehensive-Financial-Report
# OK tulsa county https://countyclerk.tulsacounty.org/Home/Reports
# UT utah county https://www.utahcounty.gov/Dept/ClerkAud/BudgetFinance/FinancialReports.asp
# PA 	bucks county https://www.buckscounty.gov/253/Finance-Division
# NJ monmouth county https://www.visitmonmouth.com/page.aspx?Id=2166
#NJ Essex county https://essexcountynj.org/treasurer/


top100_county_3years %>% select(state.abb, name, id, year, geo_id) %>% add_count(geo_id) %>% 
  filter(n <4) %>% select(state.abb, name, n) %>% distinct() %>%  View()

# missing county top200
missing_county <- top200_county_3years %>% 
  select(state.abb, name, id, year, geo_id) %>% add_count(geo_id) %>% 
    filter(n <3)

#MA Norfolk 2022: not released yet
# MA Bristol: have 2022, 2023 but not 2020, 2021
# Uploaded: Union county 2020, PA montgomery county
# AL Mobile: should be non-standard 

#Lake County’s Chronically Poor Audit Results Continue
#https://comptroller.tn.gov/news/2024/3/5/lake-county-s-chronically-poor-audit-results-continue.html


top100_county_3years %>% add_count(geo_id) %>% filter(n <3 ) %>% arrange(name)
#TODO: Checking on 2 missing - as of June 7, 2024
# PA montgomery county 2022: https://www.montgomerycountypa.gov/Archive.aspx?AMID=45
# MA norfolk county 2022: https://www.norfolkcounty.org/county_commission/about_norfolk_county/annual_reports.php#outer-31

write.csv(top100_county_3years, "output/top100_counties.csv")
write.csv(county_all, "output/all_counties_3years.csv")
top200_county_3years %>% write.csv("output/top200_counties.csv")

####Incorporated Place & Minor Civil Division####
##Special cities##
special_cities <- acfrs_general_purpose %>% 
  #only filter those missing
  filter(id %in% c("101868", "1266697", "1266289", "149470")) %>% 
  mutate(geo_id = case_when(id == "101868" ~ "0668000", # san jose CA
                            id == "1266697" ~ "2255000", # new orleans LA
                            id == "1266289" ~ "0820000",# CO denver county, also denver city
                            id == "149470" ~ "3611000")) %>%  # NY buffalo
  
  left_join(city_income) %>% 
  left_join(df_state) %>% 
  left_join(census_all)

city_gov_ <- city_gov %>% left_join(city_income) %>% 
  rbind(special_cities) %>% 
  select(any_of(fields_to_select)) 

#append ULR

city_gov <- append_url(city_gov_) %>% select(-identifier)

#Top 100
top100_cities <- city_gov %>% 
  filter((geo_id %in% census_city_top100$geo_id) | 
           name == "district of columbia") %>% distinct() %>% 
  mutate(population = ifelse(name == "district of columbia", 689546, population))


top100_cities %>% 
  select(name, year) %>% 
  group_by(year) %>% 
  add_count() %>% filter(n< 4) %>% distinct() %>% 
  View()


top100_cities %>% select(state.abb, name, id, year, geo_id) %>% #filter(year == 2023) %>% View()
  add_count(geo_id) %>% 
  filter(n < 4) %>% select(state.abb, name, n) %>% distinct() %>% View()
#Missing top 100 cities year 2023

# CA bakersfield
# AL huntsville
# CO aurora
# KS wichita
# NJ jersey city
# NJ newark
# NE omaha
# MN 	minneapolis
# OH cleveland
# OH toledo
# MN saint paul
# WA spokane
# WA tacoma
# WI milwaukee
# CO Denver


#Top 200
top200_cities <- city_gov %>% filter(year == 2023) %>% View()
  filter((geo_id %in% census_city_top200$geo_id) | 
           name == "district of columbia") %>% distinct() %>% 
  mutate(population = ifelse(name == "district of columbia", 689546, population)) 
  #filter(year != 2023)

#missing cities 2023
missing_city <- top200_cities %>% add_count(geo_id) %>% filter(n<4) %>% 
  arrange(state.abb, name, year) %>% 
  select(state.name, name, year, id, n) 

read.csv("tmp/top200cities_missing2023.csv") %>% 
  filter(X.1 != "uploaded 2023") %>% 
  filter(!year %in% c(2020, 2021)) %>% 
  select(state.name, name) %>% distinct() %>% write.csv("tmp/missing_cities_2023.csv")

# Cities in top 200 that are missing 2023
# state.name	name
# 1	Alabama	birmingham
# 2	Arkansas	little rock
# 3	California	bakersfield
# 4	California	salinas
# 5	Colorado	lakewood
# 6	Connecticut	new haven
# 7	Connecticut	stamford
# 8	Florida	fort lauderdale
# 9	Georgia	savannah
# 10	Illinois	aurora
# 11	Illinois	joliet
# 12	Louisiana	shreveport
# 13	Minnesota	saint paul
# 14	Nebraska	omaha
# 15	New Jersey	jersey city
# 16	Ohio	akron
# 17	Ohio	toledo
# 18	South Carolina	charleston
# 19	Washington	kent
# 20	Washington	spokane

# missing: 1 as of Jul 8, 2024
#NJ newark 2022
#MS Jackson 2022
#NJ patterson 2022

top200_cities %>% write.csv("output/top200_cities.csv")
top100_cities %>% write.csv("output/top100_cities.csv")
city_gov %>% write.csv("output/all_cities_3years.csv")


####School districts####
dictionary <- readRDS("data/dictionary.RDS")

school_districts_ <- readRDS("data/acfrs_data.RDS") %>% 
  filter(name == "norfolk public schools")
  
  filter(category == "School District") %>% 
  mutate(id = as.character(id)) %>% 
  select(any_of(fields_to_select)) 

#append URLs

school_districts <- append_url(school_districts_) %>% select(-identifier)

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
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() 

#top 300
top300_schools_by_year

dict_top300_ELSI <- dictionary %>% 
  filter(ncesID %in% top300_schools_by_year$ncesID) %>% 
  drop_na(id) %>% select(-name)

top300_school_districts <- school_districts %>% 
  filter(id %in% dict_top300_ELSI$id) %>% 
  left_join(dict_top300_ELSI, by = c("id",  "state.abb")) %>% 
  
  #join with nces to get county, city info
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) %>% 
  
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() 


#TODO: check back missing: 
#GA Clayton County Board of education.
#https://www.clayton.k12.ga.us/departments/business-services/financial-reports
# Uploaded: williamson county schools, reported in county acfrs. Uploaded county's acfrs to replace


missing_sd <- top200_school_districts %>% 
  add_count(ncesID) %>% filter(n < 4) %>% 
  select(state.abb, ncesID, year, name, n, id) 
  
missing_sd_top300 <- top300_school_districts %>% 
  add_count(ncesID) %>% filter(n < 4) %>% 
  select(state.abb, ncesID, year, name, n, id) 

# SD in the Top 200 missing 2023:

# state.abb	name	ncesID
# 1	CA	san francisco unified school district	634410
# 2	GA	clayton county board of education	1301230
# 3	GA	dekalb county board of education	1301740
# 4	MA	boston public schools	2502790
# 5	MN	independent school district no. 625	2733840
# 6	NY	new york city geographic district # 10	3600087
# 7	NY	new york city geographic district # 2	3600077
# 8	NY	new york city geographic district # 20	3600151
# 9	NY	new york city geographic district # 24	3600098
# 10	NY	new york city geographic district # 31	3600103
# 11	TN	clarksville-montgomery county school system	4703030
# 12	TN	clarksville-montgomery county school system	4703030
# 13	VA	chesapeake public schools	5100810
# 14	WA	seattle school district no. 1	5307710
# 15	WI	milwaukee public schools	5509600
# 16-20 NY new york city geographic district # 2, 10, 20, 24, 31

top200_school_districts %>% write.csv("output/top200_sd.csv")
top100_school_districts %>% write.csv("output/top100_sd.csv")
school_districts %>% write.csv("output/all_schooldistricts_3years.csv")
#TODO: ask Geoff about this revenues = expense cases




