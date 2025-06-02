library(tidyverse)
library(dplyr)
library(janitor)
source("src/data_processing/nces.R")

nrow(acfrs_general_purpose)

acfrs_general_purpose %>% 
  group_by(year) %>%
  summarise(n_entities = n(), .groups = "drop")


# count of state by year
state_gov %>% select(state.abb, year, name) %>% 
  group_by(year) %>% 
  summarise(count = n())

# State 2023
state_gov %>%
  summarise(collected_pop = sum(population, na.rm = TRUE))

# state missing 2023
anti_join(state_gov %>% filter(year == 2022) %>% select(state.abb), 
          state_gov %>% filter(year == 2023) %>% select(state.abb))
# Missing states: NV, IL
#https://controller.nv.gov/FinancialRpts/CAFR/Home/
# https://illinoiscomptroller.gov/financial-reports-data/find-a-report/comprehensive-reporting/annual-comprehensive-financial-report/


####County####

# count by year
county_gov_all %>% select(state.abb, year, name, population) %>% 
  #filter(year == 2023) %>% View()
  group_by(year) %>% 
  summarise(count = n(), 
            tot = sum(population, na.rm = TRUE))



counted_as_city <- c("juneau city and borough", "wrangell city and borough", 
                     "san francisco county",
                     "philadelphia county")
  

top200_county %>% group_by(name) %>% 
  add_count() %>% 
  filter(n <4) %>% View()

#Missing in top 300
top300_county %>% group_by(name) %>% 
  
  add_count() %>% 
  filter(n <4)  %>% View()

missing_county <- anti_join(census_county, county_gov, by = "geo_id") %>% 
  arrange(desc(population)) %>% 
  #not missing, just diff name, 
  filter(!str_detect(name_census, "honolulu|philadelphia|san francisco|duval|(orleans parish)")) %>% 
  # check consolidated separately
  filter(funcstat == "A") %>% 
  select(state.abb, state.name, name_census, population) 

missing_county %>% summarise(tot = sum(population, na.rm = TRUE))

county_23 <- county_gov_all %>% filter(year == 2023)

missing_county_May25 <- read.csv("tmp/missing_counties_fy23_May2025.csv") %>% filter(X.1 != "y")

missing_county_23 <- anti_join(census_county, county_23, by = "geo_id") %>% 
anti_join(missing_county_May25, by = c("state.abb",  "name_census"= "name")) %>% 
  
write.csv(paste0("tmp/missing_county_fy2023_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"))

# population by year
county_gov_all %>% #select(state, name, population, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

# those having 22, missing 23
anti_join(county_gov %>% filter(year == 2021) %>% select(state.abb, name, id, population), 
          county_gov %>% filter(year == 2022) %>% select(state.abb, name, id)) %>% View()

# those missing 2023
anti_join(county_gov %>% filter(year == 2022) %>% select(state.abb, name, id, population), 
          county_gov %>% filter(year == 2023) %>% select(state.abb, name, id)) %>% 
write.csv("tmp/missing_counties_fy23_May2025.csv")

#NOTE: some have good source here: 
#MS: https://www.osa.ms.gov/reports/audit-reports
#OH: https://ohioauditor.gov/auditsearch/search.aspx
#WA: https://www.sao.wa.gov/reports-data/audit-reports
#AK: https://arklegaudit.gov/reports
#TN: https://comptroller.tn.gov/advanced-search.html
#LA: https://www.lla.la.gov/reports/audit-reports?tab=by-parish&search=p
#AR: https://www.arklegaudit.gov/reports?keyword=lonoke+county
#AL: https://alison.legislature.state.al.us/epa-audit-reports
#MO: https://auditor.mo.gov/AuditReport/Reports?SearchLocalState=4
#GA: https://ted.cviog.uga.edu/financial-documents/financial-reports?og_group_ref_target_id%5B%5D=204&field_fiscal_year_value%5Bmin%5D%5Byear%5D=&field_fiscal_year_value%5Bmax%5D%5Byear%5D=&=Apply
#KS: https://admin.ks.gov/offices/accounts-reports/local-government/municipal-services/municipal-audits/categories/f3d8faddac2a48c08823b55d8209245d
#KY: https://www.kaco.org/county-information/county-financials/
#ME: https://www.maine.gov/audit/county/annual-audit-reports.html
#MN: https://www.auditor.state.mn.us/audit-resources/reports/audit-reports/
#ND: https://www.nd.gov/auditor/audit-reports
#NE: https://www.nebraska.gov/auditor/FileSearch/entityresults.cgi?id=Platte%20Township%20Dodge%20County


# OK oklahoma county https://www.sai.ok.gov/audit-reports/?type=3&rpp=50&years=,2023&sort=&counties=55&searchtext=

top200_county_4years %>% select(state.abb, name, year, id, population) %>% 
  add_count(id) %>% select(-year) %>% 
  distinct()%>% filter(n<4) %>%
  View()

#%>% write.csv("tmp/top200_counties_missing_2023.csv")

top300_county %>% select(state.abb, name, year, id, population) %>% 
  add_count(id) %>% select(-year) %>% 
  distinct()%>% filter(n<4) %>% 
  filter(!id %in% c("116575")) %>% 
  View()

####Cities####
municipality_all %>% filter(year == 2022) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

#Missing in top 100
top200_cities %>% 
  group_by(id) %>% 
  add_count() %>% 
  filter(n<4) %>% View()

# Why so few cities?
anti_join(municipality_all %>% filter(year == 2022) %>% select(state.abb, name, id, population), 
          municipality_all %>% filter(year == 2023) %>% select(state.abb, name, id, population)) %>%
  arrange(desc(population)) %>% View()
  #writexl::write_xlsx("tmp/missing_cities_Dec2024.xlsx")

municipality_all %>% select(state.abb, year, name) %>% 
  group_by(year) %>% 
  summarise(count = n())

municipality_all %>% select(state.abb, name, population, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE),
            count =n())

#MA Norfolk 2022: not released yet
# MA Bristol: have 2022, 2023 but not 2020, 2021
# AL Mobile: should be non-standard 

#Lake County’s Chronically Poor Audit Results Continue
#https://comptroller.tn.gov/news/2024/3/5/lake-county-s-chronically-poor-audit-results-continue.html

#Missing top 100 cities year 2023
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
school_districts_all %>% select(state.abb, name, enrollment_23, year) %>% 
  group_by(year) %>% 
  summarise(count = n())

school_districts_all %>% select(state.abb,enrollment_23, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(enrollment_23, na.rm = TRUE))

#TODO: check back missing: 
#GA Clayton County Board of education.
#https://www.clayton.k12.ga.us/departments/business-services/financial-reports
# Uploaded: williamson county schools, reported in county acfrs. Uploaded county's acfrs to replace
sd_top200_nces
missing_sd <- top200_school_districts %>% 
  add_count(ncesID) %>% filter(n < 4) %>% 
  select(state.abb, ncesID, year, name, n, id) 

missing_sd_top300 <- top300_school_districts %>% 
  add_count(ncesID) %>% filter(n < 4) %>% 
  select(state.abb, ncesID, year, name, n, id) 


anti_join(school_districts_all %>% filter(year == 2022) %>% select(state.abb, name, id, enrollment_23), 
          school_districts_all %>% filter(year == 2023) %>% select(state.abb, name, id, enrollment_23)) %>%
  arrange(desc(enrollment_23)) %>% 
  filter(state.abb != "NY") %>% write.csv("tmp/THUY_school_districts_reported_in_county_city.csv")
  View()
  
  
 
  
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

# 2	GA	clayton county board of education	1301230 https://app.fac.gov/dissemination/search/
# 4	MA	boston public schools	2502790

# 6	NY	new york city geographic district # 10	3600087
# 7	NY	new york city geographic district # 2	3600077
# 8	NY	new york city geographic district # 20	3600151
# 9	NY	new york city geographic district # 24	3600098
# 10	NY	new york city geographic district # 31	3600103
# 11	TN	clarksville-montgomery county school system	4703030
# 12	TN	clarksville-montgomery county school system	4703030
# 13	VA	chesapeake public schools	5100810
# 15	WI	milwaukee public schools	5509600
# 16-20 NY new york city geographic district # 2, 10, 20, 24, 31
