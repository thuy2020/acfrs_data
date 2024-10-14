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
# Missing states: CA, NV, MS, IL, AZ
#https://www.sco.ca.gov/ard_state_acfr.html
#https://www.dfa.ms.gov/publications
#https://controller.nv.gov/FinancialRpts/CAFR/Home/
# https://illinoiscomptroller.gov/financial-reports-data/find-a-report/comprehensive-reporting/annual-comprehensive-financial-report/
# AZ has financial report 2023 but not ACFRs 2023: https://gao.az.gov/financials/acfr


####County####

# count by year
county_gov_all %>% select(state.abb, year, name) %>% 
  #filter(year == 2023) %>% View()
  group_by(year) %>% 
  summarise(count = n())

missing_county <- anti_join(census_county, county_gov, by = "geo_id") %>% 
  arrange(desc(population)) %>% 
  #not missing, just diff name, 
  filter(!str_detect(name_census, "honolulu|philadelphia|san francisco|duval|(orleans parish)"))

# population by year
county_gov_all %>% #select(state, name, population, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

# those missing 2023
anti_join(county_gov %>% filter(year == 2022) %>% select(state.abb, name, id, population), 
          county_gov %>% filter(year == 2023) %>% select(state.abb, name, id)) %>% View()
#write.csv("tmp/missing_counties_23.csv")


top200_county_4years %>% select(state.abb, name, year, id) %>% 
  add_count(id) %>% select(-year) %>% 
  distinct()%>% filter(n<4) %>% write.csv("tmp/top200_counties_missing_2023.csv")

# missing county in top200
missing_county <- top200_county_4years %>% 
  select(state.abb, name, id, year, geo_id) %>% add_count(geo_id) %>% select(-year) %>% 
  filter(n <4) %>% distinct()


#TODO: missing county top100 year 2023
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


####Cities####
city_gov %>% filter(year == 2022) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

View()
# Why so few cities?
anti_join(city_gov %>% filter(year == 2022) %>% select(state.abb, name, population), 
          city_gov %>% filter(year == 2023) %>% select(state.abb, name, population)) %>% View()

city_gov %>% select(state.abb, year, name) %>% 
  group_by(year) %>% 
  summarise(count = n())

city_gov %>% select(state.abb, name, population, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

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