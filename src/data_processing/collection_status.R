library(tidyverse)
library(dplyr)
library(janitor)

# count of state by year
state_gov %>% select(state.abb, year, name) %>% 
  group_by(year) %>% 
  summarise(count = n())

# State 2023
state_gov %>% 
  filter(year == 2023) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

# state missing 2023
anti_join(state_gov %>% filter(year == 2022) %>% select(state.abb), 
          state_gov %>% filter(year == 2023) %>% select(state.abb))
# Missing states: CA, NV, IL
#https://www.sco.ca.gov/ard_state_acfr.html
#https://controller.nv.gov/FinancialRpts/CAFR/Home/
# https://illinoiscomptroller.gov/financial-reports-data/find-a-report/comprehensive-reporting/annual-comprehensive-financial-report/


####County####

# count by year
county_gov_all %>% select(state.abb, year, name, population) %>% 
  #filter(year == 2023) %>% View()
  group_by(year) %>% 
  summarise(count = n(), 
            tot = sum(population, na.rm = TRUE))

consolitated_county <- census_county %>% 
  filter(funcstat == "C")

counted_as_city <- c("juneau city and borough", "wrangell city and borough", "san francisco county",
                     "philadelphia county")
  

missing_county <- anti_join(census_county, county_gov, by = "geo_id") %>% 
  arrange(desc(population)) %>% 
  #not missing, just diff name, 
  filter(!str_detect(name_census, "honolulu|philadelphia|san francisco|duval|(orleans parish)")) %>% 
  # check consolidated separately
  filter(funcstat == "A") %>% 
  select(state.abb, state.name, name_census, population) 

missing_county %>% summarise(tot = sum(population, na.rm = TRUE))

# population by year
county_gov_all %>% #select(state, name, population, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

# those missing 2023
anti_join(county_gov %>% filter(year == 2022) %>% select(state.abb, name, id, population), 
          county_gov %>% filter(year == 2023) %>% select(state.abb, name, id)) %>% View()
#write.csv("tmp/missing_counties_fy23_Dec2024.csv")

#NOTE: some have good source here: 
#MS: https://www.osa.ms.gov/reports/audit-reports
#OH: https://ohioauditor.gov/auditsearch/search.aspx
#WA: https://www.sao.wa.gov/reports-data/audit-reports
#AK: https://arklegaudit.gov/reports
#TN: https://comptroller.tn.gov/advanced-search.html
#LA: https://www.lla.la.gov/reports/audit-reports?tab=by-parish&search=p
#AR: https://www.arklegaudit.gov/reports?keyword=lonoke+county


#TODO: missing county top100 year 2023

# PA montgomery county https://www.montgomerycountypa.gov/331/Annual-Financial-Statements-Reports
# OK oklahoma county https://www.sai.ok.gov/audit-reports/?counties=55&years=%2C2023&orgs=
# MA 	norfolk county
#NJ bergen county


top200_county_4years %>% select(state.abb, name, year, id, population) %>% 
  add_count(id) %>% select(-year) %>% 
  distinct()%>% filter(n<4) 

#%>% write.csv("tmp/top200_counties_missing_2023.csv")

top300_county %>% select(state.abb, name, id, population, year) %>% 
  add_count(id) %>% 
  distinct()%>% filter(n<4) %>% 
  filter(year != 2023) %>% View()

####Cities####
city_gov %>% filter(year == 2022) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

# Why so few cities?
anti_join(city_gov %>% filter(year == 2022) %>% select(state.abb, name, id, population), 
          city_gov %>% filter(year == 2023) %>% select(state.abb, name, id, population)) %>%
  arrange(desc(population)) 
  #writexl::write_xlsx("tmp/missing_cities_Dec2024.xlsx")

city_gov %>% select(state.abb, year, name) %>% 
  group_by(year) %>% 
  summarise(count = n())

city_gov %>% select(state.abb, name, population, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE),
            count =n())

#MA Norfolk 2022: not released yet
# MA Bristol: have 2022, 2023 but not 2020, 2021
# Uploaded: Union county 2020, PA montgomery county
# AL Mobile: should be non-standard 

#Lake County’s Chronically Poor Audit Results Continue
#https://comptroller.tn.gov/news/2024/3/5/lake-county-s-chronically-poor-audit-results-continue.html

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
####SD####

# collected sd
school_districts_all %>% select(state.abb, name, enrollment_22, year) %>% 
  group_by(year) %>% 
  summarise(count = n())

school_districts_all %>% select(state.abb,enrollment_22, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(enrollment_22, na.rm = TRUE))

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


anti_join(school_districts_all %>% filter(year == 2022) %>% select(state.abb, name, id, enrollment_22), 
          school_districts_all %>% filter(year == 2023) %>% select(state.abb, name, id, enrollment_22)) %>%
  arrange(desc(enrollment_22)) 
  
#type of school district
## NOTE: 
#id 67836 Santa Cruz City Schools (the "District") is a consolidation of 
#Santa Cruz City High School District nces 0635600
#Santa Cruz City Elementary School District nces 0635590 . 
#The Districts have not unified but are consolidated due to the fact that the Districts share a common governing board. These two entities are referred to collectively as Santa Cruz City Schools, 
#and for purposes of these financial statements, will be referred to collectively as the District

#NE	ewing public schools district no. 29	id = 190272
# Effective June 6, 2020, Holt County School District 45-0029, commonly known as Ewing
# Public Schools; Antelope County Public School District 02-0006, commonly known as
# Clearwater Public Schools, and Antelope County Public School District 02-0049, commonly
# known as Orchard Public Schools, were dissolved and merged to create a new school district, Antelope County School District 02-0115, commonly known as Summerland Public
# Schools.

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