library(tidyverse)
library(dplyr)
library(scales)

####State####
# count of state by year
state_gov %>% select(state.abb, year, name) %>% 
  group_by(year) %>% 
  summarise(count = n())

# State 2023
state_gov %>% 
  filter(year == 2023) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))

# state missing 2023
missing_state <- anti_join(state_gov %>% filter(year == 2022) %>% select(state.abb), 
          state_gov %>% filter(year == 2023) %>% select(state.abb, population))
# Missing states: CA, NV, IL
#https://www.sco.ca.gov/ard_state_acfr.html
#https://controller.nv.gov/FinancialRpts/CAFR/Home/
# https://illinoiscomptroller.gov/financial-reports-data/find-a-report/comprehensive-reporting/annual-comprehensive-financial-report/
state_gov %>% filter(year == 2022) %>% 
  filter(!state.abb %in% c("CA", "NV", "IL")) %>% 
  summarise(tot_pop = sum(population))

####County####
missing_county <- anti_join(census_county, county_gov, by = "geo_id") %>% 
  arrange(desc(population)) %>% 
  #not missing, just diff name, 
  filter(!str_detect(name_census, "honolulu|philadelphia|san francisco|duval|(orleans parish)"))

# count by year
county_gov %>% select(state.abb, year, name) %>% 
  group_by(year) %>% 
  summarise(count = n())

# population by year

county_gov %>% select(state, name, population, year) %>% 
  group_by(year) %>% 
  summarise(collected_pop = sum(population, na.rm = TRUE))


# those missing 2023
anti_join(county_gov %>% filter(year == 2022) %>% select(state.abb, name, id, population), 
          county_gov %>% filter(year == 2023) %>% select(state.abb, name, id)) %>% 
View()

# missing county top200
missing_county_top200 <- top200_county_4years %>% select(state.abb, name, year, id) %>% 
  add_count(id) %>% select(-year) %>% 
  distinct()%>% filter(n<4) 

#TODO: Checking on 2 missing - as of June 7, 2024
# PA montgomery county 2022: https://www.montgomerycountypa.gov/Archive.aspx?AMID=45
# MA norfolk county 2022: https://www.norfolkcounty.org/county_commission/about_norfolk_county/annual_reports.php#outer-31

# missing county top100 year 2023

# PA montgomery county https://www.montgomerycountypa.gov/331/Annual-Financial-Statements-Reports
# WA snohomish county https://sao.wa.gov/reports-data/audit-reports?SearchText=snohomish%20county&StartDate=&EndDate=
# OK oklahoma county https://www.sai.ok.gov/audit-reports/?counties=55&years=%2C2023&orgs=
# MA 	norfolk county
# OK tulsa county https://countyclerk.tulsacounty.org/Home/Reports
# PA 	bucks county https://www.buckscounty.gov/Archive.aspx?AMID=40



####Municipalities####
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
# AL Mobile: should be non-standard 

#Lake County’s Chronically Poor Audit Results Continue
#https://comptroller.tn.gov/news/2024/3/5/lake-county-s-chronically-poor-audit-results-continue.html

#Missing top 100 cities year 2023

# CA bakersfield: https://www.bakersfieldcity.us/220/Annual-Comprehensive-Financial-Reports
# NJ newark https://www.newarknj.gov/departments/finance
# NE omaha https://finance.cityofomaha.org/
# MN saint paul https://www.stpaul.gov/ofs/annual-comprehensive-financial-report
# WA spokane https://my.spokanecity.org/opendata/documents/financial-reports/


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
# 14	Nebraska	omaha
# 16	Ohio	akron
# 18	South Carolina	charleston
# 19	Washington	kent

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
  select(state.abb, ncesID, name, n, id) %>% distinct()

missing_sd_top300 <- top300_school_districts %>% 
  add_count(ncesID) %>% filter(n < 4) %>% 
  select(state.abb, ncesID, name, n, id) %>% distinct()

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

####All acfrs sd####
d_app <- school_districts_all %>% 
  select(2:revenues, enrollment_22) %>% 
  #group_by(state.name, year) %>% 
  mutate(across(current_assets:revenues, list(tot = ~ sum(., na.rm = TRUE)), .names = "sum_{col}")) %>% 
  select(state.name, name, year, enrollment_22, contains("sum_")) %>% 
  pivot_longer(cols = 5:29,
               names_to = "category", 
               values_to = "value") %>% 
  pivot_wider(names_from = year, 
              values_from = value) %>% 
  mutate(category = str_remove(category, "sum_")) %>% distinct()

d_app %>% saveRDS("data/school_districts_for_shinyapp.RDS")

all_school_districts <- d_app %>% 
  mutate(across(where(is.numeric) & !all_of("enrollment_22"), ~ comma(.))) 


d_app %>% 
  mutate(across(where(is.numeric), ~ comma(.))) %>% saveRDS("data/state_tot.RDS")


valuebox <- d_app %>%
  filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>%
  select(-c(name, `2023`, enrollment_22)) %>% 
  distinct() %>% 
  pivot_longer(3:5, names_to = "year", values_to = "value") %>% 
  group_by(state.name, category) %>%
  summarise(allyears = sum(value, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~ comma(.))) 

valuebox %>% saveRDS("data/valuebox_data.RDS")

#Top 10 each state
top10_each_state <- d_app %>% 
  select(state.name, name, enrollment_22) %>% 
  distinct() %>% 
  arrange(state.name, desc(enrollment_22)) %>%      
  group_by(state.name) %>%                              
  slice_head(n = 10)

top10_chart_data <- d_app %>%  
  select(-c(enrollment_22, `2023`)) %>% 
  filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>% 
  inner_join(top10_each_state, by = c("statename", "name")) %>% 
  mutate(name = str_to_title(name))

saveRDS(top10_chart_data, "data/top10_chart_data.RDS")


filtered_data <- top10_chart_data %>%
  filter(state.name == "Arizona") 
filter(category == "net_pension_liability") %>%
  select(name, `2022`)

p <- filtered_data %>%
  ggplot(aes(fct_reorder(name, `2022`), `2022`)) +
  geom_col(fill = "#55C5E6") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(x = "", y = "", 
       title = paste("Top 10 School Districts in ", input$selected_state, "in 2022")) +
  theme_minimal()
