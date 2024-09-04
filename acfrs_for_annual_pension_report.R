library(dplyr)
library(tidyr)

## Get the ID 
stateID <- readRDS("data/stateID.RDS")
countyID <- readRDS("data/countyID.RDS")
place_divisionID <- readRDS("data/place_divisionID.RDS")

#read in data: acfrs_data.RDS
acfrs_data_22_23 <-  readRDS("data/acfrs_data.RDS") %>% 
  filter(!state.abb %in% c("MP", "PR", "AS", "GU", "FM")) %>% 
  filter(year == 2022 | year == 2023) %>% 
  select(state.abb, state.name, id, name, year, category,
         net_pension_liability, 
         net_pension_assets,
         net_opeb_liability,
         total_liabilities) %>% 
  filter(category %in% c("General Purpose", "School District"))

# filter for only needed categories by ID
acfrs_data_22_23_summary <- acfrs_data_22_23 %>% 
  mutate(category = case_when(id %in% stateID$id ~ "State",
                              id %in% countyID$id ~ "County",
                              id %in% place_divisionID$id ~ "Place",
                              TRUE ~ category)) %>% 
  #  filter(!category %in% c("State", "County", "Place", "School District")) %>% 
  
  group_by(state.abb, state.name, category, year) %>% 
summarise(state_NPL = sum(net_pension_liability, na.rm = TRUE),
          state_OPEB = sum(net_opeb_liability, na.rm = TRUE),
          state_NPA = sum(net_pension_assets, na.rm = TRUE),
          state_total_liabilities = sum(total_liabilities, na.rm = TRUE)) %>% 
  ungroup()


# Double check to make sure each state have 4 categories:
acfrs_data_22_23_summary %>% 
  filter(year == 2022) %>% 
  group_by(state.name, state.abb) %>% 
  summarise(count = n()) %>% 
  filter(count < 4)

acfrs_data_22_23_summary %>% 
  filter(year == 2023) %>% View()
  group_by(state.name, state.abb) %>% 
  summarise(count = n()) %>% 
  filter(count < 4)

# Explanation:
#CT Connecticut does not have county governments. 
#DE we currently don't have data for school district in DE. 
#DC is counted as a Place
#HI does not have Place
#RI does not have county government
#All counties in VT are governed by three assistant judges.

#AZ, CA, IL, MS, NV doesn't have state Acfr 2023

# View of all school districts. 
acfrs_data_22_23_summary %>% 
  group_by(year) %>% 
  select(where(is.numeric)) %>%  
  summarise(across(everything(), sum, na.rm = TRUE)) %>% View()

####Calculate percentage only for year 2022 #######

acfrs_data_2022_pct <- acfrs_data_22_23_summary %>% filter(year == 2022) %>% 
                            pivot_longer(4:7, 
                                         names_to = "acfr_field",
                                         values_to = "value") %>% 
  pivot_wider(names_from = category, 
              values_from = value) %>% 
  
  # replace all Nas value by 1 to avoid Na at the summary
  mutate(across(County:State, ~ replace_na(., 1))) %>% 
  
  rowwise() %>% 
  mutate(tot_state = sum(c_across(County:State))) %>% 
  mutate(across(County:State, ~ . / tot_state * 100,
                .names = "{col}_pct")) %>% 
  mutate(across(ends_with("_pct"), ~ round(., 2))) 
  
acfrs_data_2022_pct %>% write.csv("output/acfrs_data_2022_for_annual_pension_report.csv")
  
