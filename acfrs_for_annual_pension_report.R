library(dplyr)

## Get the ID 
stateID <- readRDS("data/stateID.RDS")
countyID <- readRDS("data/countyID.RDS")
cityID <- readRDS("data/cityID.RDS")

#need to read in this data: acfrs_data.RDS
acfrs_data_2022 <- readRDS("data/acfrs_data.RDS") %>% 
  filter(!state.abb %in% c("MP", "PR", "AS", "GU")) %>% 
  filter(year == 2022) %>% 
  select(state.abb, state.name, id, name, year, category,
         net_pension_liability, 
         net_pension_assets,
         total_liabilities) %>% 
  filter(category %in% c("General Purpose", "School District"))

# filter for only needed categories by ID
acfrs_data_2022_summary <- acfrs_data_2022 %>% filter(id %in% c(stateID$id, countyID$id, cityID$id)) %>%
  mutate(category = case_when(id %in% stateID$id ~ "State",
                              id %in% countyID$id ~ "County",
                              id %in% cityID$id ~ "City",
                              TRUE ~ category))%>% 
  group_by(state.abb, state.name, category) %>% 
summarise(state_NPL = sum(net_pension_liability, na.rm = TRUE),
          state_NPA = sum(net_pension_assets, na.rm = TRUE),
          state_total_liabilities = sum(total_liabilities, na.rm = TRUE)) 

#Why some does not have county summary? 
#All counties in VT are governed by three assistant judges.
#CT Connecticut does not have county governments. 

####Calculate percentage#######

acfrs_data_2022_pct <- acfrs_data_2022_summary %>% pivot_longer(4:6, 
                                         names_to = "acfr_field",
                                         values_to = "value") %>% 
  pivot_wider(names_from = category, 
              values_from = value) %>% 
  rowwise() %>% 
  mutate(tot_state = sum(c_across(City:State))) %>% 
  mutate(across(City:State, ~ . / tot_state * 100,
                .names = "{col}_pct")) %>% 
  mutate(across(ends_with("_pct"), ~ round(., 2)))
  
acfrs_data_2022_pct %>% write.csv("output/acfrs_data_2022_for_annual_pension_report.csv")
  
