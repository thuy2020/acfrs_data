library(dplyr)
library(tidyr)
library(scales)
options(scipen = 999)
source("src/data_processing/processing_for_data_tool.R")

## Get the ID 
stateID <- readRDS("data/stateID.RDS") # excluding DC
countyID <- readRDS("data/countyID.RDS")
place_divisionID <- readRDS("data/place_divisionID.RDS") # including DC

#read in data: acfrs_data.RDS
acfrs_data_22 <-  readRDS("data/acfrs_data.RDS") %>% 
  filter(!state.abb %in% c("MP", "PR", "AS", "GU", "FM")) %>% 
  filter(year == 2022) %>% 
  select(state.abb, state.name, id, name, category,
         net_pension_liability, 
         net_pension_assets,
         net_opeb_liability,
         total_liabilities) %>% 
  filter(category %in% c("General Purpose", "School District"))

# break down by categories 
acfrs_data_22_summary <- acfrs_data_22 %>% 
  mutate(category = case_when(id %in% stateID$id ~ "state_entity",
                              id %in% countyID$id ~ "county",
                              id %in% place_divisionID$id ~ "municipality",
                              TRUE ~ category)) %>% 

  mutate(net_pension_liability = net_pension_liability - net_pension_assets) |>
  group_by(state.abb, state.name, category) %>% 
  summarise(
    net_pension_liability = sum(net_pension_liability, na.rm = TRUE)
    ) %>% 
  ungroup() |>
  pivot_wider(names_from = category, values_from = net_pension_liability) |>
  rename(
    school_district = `School District`,
    state = state.name, 
    state_abb = state.abb
  ) |>
  mutate(across(state_entity:school_district, ~ replace_na(., 0))) |>
  mutate(
    total = state_entity + county + municipality + school_district,
    total_for_pct = ifelse(state_entity < 0, 0, state_entity) + 
      ifelse(county < 0, 0, county) + 
      ifelse(municipality < 0, 0, municipality) + 
      ifelse(school_district < 0, 0, school_district)
  )

####National summary####

# sum columns to create national state
national_summary <- acfrs_data_22_summary %>% 
  summarise(
    state_abb = "US",
    state = "United States",
    state_entity = sum(state_entity),
    county = sum(county),
    municipality = sum(municipality),
    school_district = sum(school_district),
    total = sum(total),
    total_for_pct = sum(total_for_pct)
  )

# add national summary to the data
acfrs_data_22_summary_national <- acfrs_data_22_summary %>% 
  bind_rows(national_summary)

#Notes:
#CT Connecticut does not have county governments. 
#DE we currently don't have data for school district in DE. 
#DC is counted as a Place
#HI does not have Place
#RI does not have county government
#All counties in VT are governed by three assistant judges.
#AZ, CA, IL, MS, NV doesn't have state Acfr 2023 as of Sep 4, 2024

# write the data to js
acfrs_data_2022_summary_json <- jsonlite::toJSON(acfrs_data_22_summary_national, 
                                                 pretty = TRUE)
acfrs_data_2022_summary_json <- paste0("export default ", acfrs_data_2022_summary_json)

write(acfrs_data_2022_summary_json, file = "output/acfrs_data_22_summary_national.js")


####Checking data for APR page#### 

# States:
#U.S. states owed $505.8 billion as net public pension liabilities.
acfrs_data_22_summary_national %>% 
  mutate(across(3:8, ~ comma(.))) %>% View()

us_netnetPL_22 <- acfrs_data_22_summary_national[acfrs_data_22_summary_national$state_abb == "US",6]
us_netnetPL_22


#Illinois and New Jersey account for 42.8% of the total net pension liability held by U.S. states while only being responsible for 6.6% of the U.S. population.
(acfrs_data_22_summary_national[acfrs_data_22_summary_national$state_abb == "IL",6] + 
    acfrs_data_22_summary_national[acfrs_data_22_summary_national$state_abb == "NJ",6]) / 
  us_netnetPL_22  

#while only being responsible for 6.6% of the U.S. population.
(12.58 + 9.262)/ 333

#The ten most indebted states account for 86% of all U.S. states' total net pension liability.

ten_most_indebted <- acfrs_data_22_summary_national %>% arrange(desc(state_entity)) %>% 
  slice(2:11) %>% 
  summarise(tot_top10state = sum(state_entity))

ten_most_indebted/ acfrs_data_22_summary_national[acfrs_data_22_summary_national$state_abb == "US",6]  

#Cities:
#The 100 most populated U.S. cities owed $136.8 billion as net public pension liabilities.

city_data %>% 
  filter(year == 2022) %>% 
  summarise(tot = sum(net_pension_liability, na.rm = TRUE))

#The top ten most indebted cities alone are responsible for 77.15% of the total net pension liability held by all 100 most populous cities.

ten_indebted_cities <- city_data %>% 
  filter(year == 2022) %>% 
  arrange(desc(net_pension_liability)) %>% 
  slice(1:10) %>% 
  summarise(tot = sum(net_pension_liability, na.rm = TRUE))

ten_indebted_cities / 135548999937
  
#New York and Chicago were responsible for 56.8% of 
#the total net pension liability held by all 100 most populous cities.
city_data %>% 
  filter(year == 2022) %>% 
  arrange(desc(net_pension_liability)) %>% 
  slice(1:2) %>% 
  summarise(tot = sum(net_pension_liability, na.rm = TRUE))

77786072000/ 135548999937

#Counties:
#  The 100 most populous U.S. counties owed $72 billion as net public pension liabilities.
top100_populous_counties <- county_data %>% 
  filter(year == 2022) %>% 
  summarise(tot = sum(net_pension_liability, na.rm = TRUE))

#however, if only count for those with real debt, i.e excluding those with asset bigger than liability
county_data %>% 
  filter(year == 2022) %>% 
  filter(net_pension_liability >=0) %>% 
  summarise(tot = sum(net_pension_liability, na.rm = TRUE))


#The top ten most indebted counties alone are responsible for 64.2% of 
#the total net pension liability held by all 100 most populous counties.

ten_most_indebted_counties <- county_data %>% 
  filter(year == 2022) %>% 
    arrange(desc(net_pension_liability)) %>% 
    slice(1:10) %>% 
  summarise(tot = sum(net_pension_liability))

ten_most_indebted_counties / top100_populous_counties
  
#The two most indebted counties were responsible for 24.8% of the 
#total net pension liability held by all 100 most populous counties.
#--> correct
two_most_indebted_counties <- county_data %>% 
  filter(year == 2022) %>% 
  arrange(desc(net_pension_liability)) %>% 
  slice(1:2) %>% 
  summarise(tot = sum(net_pension_liability))

two_most_indebted_counties / top100_populous_counties


#School Districts:
 # The 100 most populated U.S. school districts owed $136.8 billion as net public pension liabilities.
school_data %>% 
  filter(year == 2022) %>% 
  arrange(desc(enrollment_22)) %>% slice(1:100) %>% 
  summarise(tot = sum(net_pension_liability, na.rm = TRUE))

#The top ten most indebted school districts alone are responsible for 59.7% of 
#the total net pension liability held by all 100 most populous school districts.

ten_indebted_sd <- school_data %>% 
  filter(year == 2022) %>% 
  arrange(desc(enrollment_22)) %>% slice(1:100) %>% 
  arrange(desc(net_pension_liability)) %>% slice(1:10) %>% 
  summarise(tot = sum(net_pension_liability, na.rm = TRUE))

ten_indebted_sd/50903746980

#The Chicago and Los Angeles school districts were responsible for 35.8% 
#of the total net pension liability, 
#while only 7.9% percent of the student population of the 100 most populous school districts.
school_data %>% 
  filter(year == 2022) %>% 
  arrange(desc(enrollment_22)) %>% slice(1:100) %>% 
  arrange(desc(net_pension_liability)) %>% slice(1:2) %>% 
  summarise(tot = sum(net_pension_liability, na.rm = TRUE))

19039085000/ 50903746980

school_data %>% 
  filter(year == 2022) %>% 
  arrange(desc(enrollment_22)) %>% slice(1:100) %>% 
  #arrange(desc(net_pension_liability)) %>% slice(1:2) %>%
  summarise(tot = sum(enrollment_22, na.rm = TRUE))

765794/ 9380703

#Figure 19 illustrates the distribution of the total net pension liability, 
#which amounts to $987.8 billion, across different levels of local government: 

national_summary[,7]

#The graph reveals that state-level pension plans bear 
#the largest share of this liability, accounting for 50.8% ($501.8 billion) of the total. 
national_summary[,3] / national_summary[,7]

#City-level plans represent 21.2% ($209.5 billion), 
#--> should be "municipality"
national_summary[,5] / national_summary[,7]

#while county-level plans hold 9.1% ($89.8 billion).
national_summary[,4] / national_summary[,7]

