library(dplyr)
library(stringr)
library(janitor)

fields_to_export <- c("state.abb", "state.name", "id", "year", "name", 
                      "category",
                      "net_pension_liability",
                      "total_liabilities", 
                      "net_pension_assets",
                      "net_opeb_liability", "net_opeb_assets", 
                      "total_assets", 
                      "population")
####State####
AZ_state <- state_gov %>% 
  filter(state.abb == "AZ") %>% 
  filter(year == 2022) %>% 
  select(all_of(fields_to_export)) %>% 
  mutate(category = "state")

####County####

state_county_census <- census_county %>% filter(state.abb == "AZ") %>% 
  select(state.abb, name_census, population) 

AZ_counties_acfrs <- county_gov %>% 
  filter(state.abb == "AZ") %>% filter(year == 2022) %>% 
  select(all_of(fields_to_export)) %>% 
  arrange(desc(net_pension_liability)) %>% 
  mutate(category = "county")

#comparing census and acfrs collected
anti_join(AZcounty_census,
          acfrs_county %>% 
            filter(state.abb == "AZ") %>% filter(year == 2022) %>% 
            select(state.abb, name),
          by = c("state.abb", "name_census" = "name")) %>% 
  # writexl::write_xlsx("tmp/AZ_counties_22_NO_acfrs.xlsx")
  View()

#checking counties between 21 - 22
anti_join(acfrs_county %>% 
            filter(state.abb == "AZ") %>% filter(year == 2022) %>% 
            select(state.abb, name),
          
          acfrs_county %>% 
            filter(state.abb == "AZ") %>% filter(year == 2023) %>% 
            select(state.abb, name)) %>% View()

####Municipality####

AZ_place_division_census <- census_place_division %>% filter(state.abb == "AZ") %>% 
  select(state.abb, name_census, population) %>% 
  mutate(name_census = str_remove(name_census, "city$"),
         name_census = str_squish(name_census)) 

AZ_acfrs_municipality_22 <- place_division_gov %>% 
  filter(state.abb == "AZ") %>% filter(year == 2022) %>% 
  select(all_of(fields_to_export)) %>% 
  arrange(desc(net_pension_liability)) %>% 
  mutate(category = "municipality") 

sum(AZ_acfrs_municipality_22$population)
anti_join(
  AZ_place_division_census, 
  AZ_acfrs_municipality_22,
  by = c("name_census" = "name", "state.abb")
) %>% View()

  
####School districts####
AZ_nces <- nces %>% filter(state.abb == "AZ") %>% 
  select(name_nces, enrollment_22, ncesID) 


AZ_sd_22 <- school_districts %>% 
  filter(state.abb == "AZ") %>% filter(year == 2022) %>% 
  select(any_of(fields_to_export), enrollment_22, ncesID) %>% 
  arrange(desc(net_pension_liability)) %>% distinct() %>% 
  mutate(category = "school district") %>% 
  rename(population = enrollment_22) %>% 
  mutate(population = case_when(ncesID == "5900052" ~ 92,
                                ncesID == "5900192" ~ 367,
                                ncesID == "5900131" ~ 98,
                                ncesID == "5900148" ~ 234,
                                TRUE ~ population
                                ))
                         

#number of student: 
sum(AZ_sd_22$population, na.rm = TRUE)

anti_join(
  AZ_nces, 
  AZ_sd_22,
  by = c("ncesID")
) %>% View()

####Result#### 

total_collected <- data.frame(
  type_of_government = c("state",
                         "county", 
                         "municipality", 
                         "school district"),
  total_population = c(7151507, 
                       7151507, 
                       5729817, 
                       899657), 
  
  collected_population = c(7151507,
                           7085483,
                           5668946, 
                           893575),
  total_count     = c(1, 
                     15,
                     91,
                     248),
  collected_count = c(1,
                      14,
                      67,
                      162
                      )) %>% 
mutate(pct_collected_population = round((collected_population/total_population)*100,2),
         pct_collected_count = round((collected_count/total_count)*100, 2)) %>% 
  mutate(
    pct_collected_population = paste0(pct_collected_population, "%"),  
    pct_collected_count = paste0(pct_collected_count, "%")  
  )

AZ_acfr_22 <- rbind(AZ_state, AZ_counties_acfrs, AZ_acfrs_municipality_22, AZ_sd_22 %>% 
                      select(-ncesID)) %>% 
  select(-year) %>% 
  group_by(category) 

AZ_aggregate_by_government_type <- AZ_acfr_22 %>% 
  summarise(sum_net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
            sum_net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE),
            sum_net_pension_assets = sum(net_pension_assets, na.rm = TRUE),
            sum_net_opeb_assets = sum(net_opeb_assets, na.rm = TRUE),
            sum_total_liabilities = sum(total_liabilities, na.rm = TRUE),
            sum_total_assets = sum(total_assets, na.rm = TRUE),
            
  ) 

data_list <- list(
  "AZ aggregate by government type" = AZ_aggregate_by_government_type,
  "AZ State" = AZ_state,
  "AZ counties" = AZ_counties_acfrs,
  "AZ municipalities" = AZ_acfrs_municipality_22,
  "AZ School districts" = AZ_sd_22,
  "Total vs collected" = total_collected
)
rio::export(data_list, "output/AZ ACFRs data 2022_corrected_Sep13.xlsx")
