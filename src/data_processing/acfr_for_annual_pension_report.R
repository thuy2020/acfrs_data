library(dplyr)
library(tidyr)
options(scipen = 999)

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
