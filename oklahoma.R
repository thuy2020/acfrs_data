library(dplyr)
library(stringr)
library(janitor)
library(scales)
library(knitr)
source("src/data_processing/census.R")

fields_to_export <- c("state.abb", "state.name", "id", "year", "name", 
                      "category",
                      "net_pension_liability",
                      "total_liabilities", 
                      "net_pension_assets",
                      "net_opeb_liability", "net_opeb_assets", 
                      "total_assets", 
                      "population")
####State####
OK_state <- state_gov %>% 
  filter(state.abb == "OK") %>% 
  filter(year == 2022) %>% 
  select(all_of(fields_to_export)) %>% 
  mutate(category = "State")

OK_acfr_state_pop <- OK_state %>% 
  filter(year == 2022) %>% 
  summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)

OK_census_state_pop <- census_state %>% filter(state.abb == "OK") %>% 
  select(population) %>% pull(population)

####County####

#NOTE: We collected financial report FY 22 for 24 counties in OK. 
#Only 2 of those provide pension and OPEB liability information are Tulsa county and Oklahoma county.

OK_acfr_counties <- county_gov %>% 
  filter(state.abb == "OK") %>% filter(year == 2022) %>% 
  select(all_of(fields_to_export)) %>% 
  arrange(desc(net_pension_liability)) %>% 
  mutate(category = "Counties")

OK_acfr_counties_pop <- OK_acfr_counties %>% 
summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)

OK_census_counties <- census_county %>% 
  filter(state.abb == "OK")

OK_census_counties_pop <- OK_census_counties %>% 
  summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)

#comparing census and acfrs collected
anti_join(OKcounty_census,
          acfrs_county %>% 
            filter(state.abb == "OK") %>% filter(year == 2022) %>% 
            select(state.abb, name),
          by = c("state.abb", "name_census" = "name")) %>% 
  # writexl::write_xlsx("tmp/OK_counties_22_NO_acfrs.xlsx")
  View()

#checking counties having 2021 but not 2022
anti_join(acfrs_county %>% 
            filter(state.abb == "OK") %>% filter(year == 2021) %>% 
            select(state.abb, name),
          
          acfrs_county %>% 
            filter(state.abb == "OK") %>% filter(year == 2022) %>% 
            select(state.abb, name)) %>% View()

####Municipality####
OK_census_municipality <- census_incorporated %>% 
  filter(state.abb == "OK") %>% 
  select(state.abb, name_census, population) %>% 
  mutate(name_census = str_remove(name_census, "city$"),
         name_census = str_squish(name_census)) 

OK_census_municipality_pop <- OK_census_municipality %>% 
summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)

# population covered in the current acfrs
OK_acfrs_municipalities <- municipality_all %>% 
  filter(state.abb == "OK") %>% filter(year == 2022) %>% 
  select(all_of(fields_to_export)) %>% 
  arrange(desc(net_pension_liability)) %>% 
  mutate(category = "Municipalities") 

#population collected in acfr
OK_acfr_municipalities_pop <- OK_acfrs_municipalities %>% 
  summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)

anti_join(
  OK_census_incorporated, 
  OK_acfrs_municipality_22,
  by = c("name_census" = "name", "state.abb")) %>% View()

####School districts####
OK_nces <- nces %>% filter(state.abb == "OK") %>% 
  select(name_nces, enrollment_22, ncesID) 

OK_nces_pop <- OK_nces %>% 
  summarise(pop = sum(enrollment_22, na.rm = TRUE)) %>% pull(pop)

OK_acfr_sd <- school_districts_all %>% 
  filter(state.abb == "OK") %>% filter(year == 2022) %>% 
  select(any_of(fields_to_export), enrollment_22, ncesID) %>% 
  arrange(desc(net_pension_liability)) %>% distinct() %>% 
  mutate(category = "School Districts") %>% 
  rename(population = enrollment_22) #%>% 

#number of student collected
OK_acfr_sd_pop <- OK_acfr_sd %>% 
  summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)

#check which one acfr does not have
anti_join(
  OK_nces, 
  OK_sd_22,
  by = c("ncesID")) %>% View()

####Result#### 

total_collected <- data.frame(
  public_employers = c("State", "Counties", "Municipalities", "School Districts"),
  total_population = c(OK_census_state_pop, 
                       OK_census_counties_pop, 
                       OK_census_municipality_pop, 
                       OK_nces_pop),
  collected_population = c(OK_acfr_state_pop, 
                           1465839, #only have pention liability for 2 counties: Tulsa, 
                           #Oklahoma OK_acfr_counties_pop, # this is pop for all financial report collected, only have rev and expense info
                          OK_acfr_municipalities_pop, 
                           OK_acfr_sd_pop),
  total_count = c(1, 
                  nrow(OK_census_counties),
                  nrow(OK_census_municipality),
                  nrow(OK_nces)),
  collected_count = c(1, 2,
                      nrow(OK_acfrs_municipalities),
                      nrow(OK_acfr_sd))) %>% 
  mutate(`% of Population Captured` = paste0(round(collected_population / total_population * 100), "%"),
         `% of Number of Entity Captured` = paste0(round(collected_count / total_count * 100), "%")) %>% 
  rename(`Public Employers` = public_employers) 



OK_aggregate_by_government_type <- rbind(OK_state, 
                    OK_acfr_counties, 
                    OK_acfrs_municipalities, 
                    OK_acfr_sd %>% select(-ncesID)) %>% 
                    select(-year) %>% 
                    group_by(category) %>% 
  
  summarise(`Net Pension Liability` = sum(net_pension_liability, na.rm = TRUE),
            `Net OPEB Liability` = sum(net_opeb_liability, na.rm = TRUE),
            `Total Liabilities` = sum(total_liabilities, na.rm = TRUE),
            
            `Net Pension + OPEB Liability` = (`Net Pension Liability` + `Net OPEB Liability`)) %>% 
  bind_rows(summarise(., category = "Total", across(where(is.numeric), sum))) %>% 
  
  mutate(`Net Pension + OPEB Liability Share of Total` = round(`Net Pension + OPEB Liability` /`Total Liabilities`*100), 
          `Net Pension + OPEB Liability Share of Total` = paste0(`Net Pension + OPEB Liability Share of Total`, "%")) %>% 

  rename(`Public Employers` = category) %>% 
  # join to get population captured
  left_join(total_collected %>% 
              select(`Public Employers`, `% of Population Captured`))
  

data_list <- list(
  "OK aggregate by government type" = OK_aggregate_by_government_type,
  "OK State" = OK_state,
  "OK Counties" = OK_counties,
  "OK Municipalities" = OK_municipalities,
  "OK School Districts" = OK_sd_22,
  "Total vs collected" = total_collected)
rio::export(data_list, "output/OK ACFRs data 2022.xlsx")
