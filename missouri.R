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
MO_state <- state_gov %>% 
  filter(state.abb == "MO") %>% 
  filter(year != 2023) %>% 
  select(all_of(fields_to_export)) %>% 
  mutate(category = "state")

MO_state %>% 
  filter(year == 2022) %>% 
  summarise(pop = sum(population, na.rm = TRUE)) %>% View()

####County####
#Christian County: does not have net pension liability
#Having 2021, not 2022, but does not follow standard GAAP report: atchison county

MOcounty_census <- census_county %>% filter(state.abb == "MO") %>% 
  select(state.abb, name_census, population)

MO_counties <- county_gov %>% 
  filter(state.abb == "MO") %>% filter(year == 2022) %>% 
  select(all_of(fields_to_export)) %>% 
  arrange(desc(net_pension_liability)) %>% 
  mutate(category = "county")
#summarise(pop = sum(population, na.rm = TRUE)) %>% View()


#comparing census and acfrs collected
anti_join(MOcounty_census,
          acfrs_county %>% 
            filter(state.abb == "MO") %>% filter(year == 2022) %>% 
            select(state.abb, name),
          by = c("state.abb", "name_census" = "name")) %>% 
 # writexl::write_xlsx("tmp/MO_counties_22_NO_acfrs.xlsx")
  View()

#checking counties between 21 - 22
anti_join(acfrs_county %>% 
            filter(state.abb == "MO") %>% filter(year == 2021) %>% 
            select(state.abb, name),
          
          acfrs_county %>% 
            filter(state.abb == "MO") %>% filter(year == 2022) %>% 
            select(state.abb, name)) %>% View()

####Municipality####

MO_place_division_census <- census_place_division %>% filter(state.abb == "MO") %>% 
  select(state.abb, name_census, population) %>% 
  mutate(name_census = str_remove(name_census, "city$"),
         name_census = str_squish(name_census)) #%>% 
  #summarise(tot = sum(population, na.rm = TRUE)) 


MO_acfrs_municipality_22 <- place_division_gov %>% 
  filter(state.abb == "MO") %>% filter(year == 2022) %>% 
  select(state.abb, name) 

MO_acfrs_municipality_22 %>% View()

anti_join(
  MO_place_division_census, 
  MO_acfrs_municipality_22,
  by = c("name_census" = "name", "state.abb")
) %>% View()

# population covered in the current acfrs
MO_municipalities <- place_division_gov %>% 
  filter(state.abb == "MO") %>% filter(year == 2022) %>% 
  select(all_of(fields_to_export)) %>% 
  arrange(desc(net_pension_liability)) %>% 
  mutate(category = "municipality") %>% 
  mutate(population = case_when(name == "st john" ~ 6643,
                                name == "crystal city" ~ 4710,
                                name == "saint robert" ~ 5242,
                                TRUE ~ population)) 

#View()

#population collect for municipalities
MO_municipalities %>% 
summarise(pop = sum(population, na.rm = TRUE)) %>% 
View()


####School districts####
MO_nces <- nces %>% filter(state.abb == "MO") %>% 
  select(name_nces, enrollment_22, ncesID) %>% 
  summarise(tot = sum(enrollment_22, na.rm = TRUE)) 

MO_sd_22 <- school_districts %>% 
  filter(state.abb == "MO") %>% filter(year == 2022) %>% 
  select(any_of(fields_to_export), enrollment_22, ncesID) %>% 
  arrange(desc(net_pension_liability)) %>% distinct() %>% 
 # select(name, ncesID, enrollment_22) %>% 
    mutate(category = "school district") %>% 
    rename(population = enrollment_22) #%>% 

  
  #number of student: 805579 Total: 863852
MO_sd_22 %>% 
  summarise(tot_student = sum(population, na.rm = TRUE)) %>% 
  View()
  
anti_join(
  MO_nces, 
  MO_sd_22,
  by = c("ncesID")
) %>% View()


####Result#### 

total_collected <- data.frame(
  type_of_government = c("state", "county", "municipality", "school district"),
  total_population = c(  6154920, 6154920, 4368405, 863852),
  collected_population = c(6154920, 4961199, 3364061, 805579),
  total_count = c(1, 144, 1221, 517),
  collected_count = c(1, 61, 167, 314)
) %>% 
  mutate(pct_collected_population = collected_population/total_population,
         pct_collected_count = collected_count/total_count)

MO_acfr_22 <- rbind(MO_state, MO_counties, MO_municipalities, MO_sd_22 %>% select(-ncesID)) %>% 
  select(-year) %>% 
  group_by(category) 

MO_aggregate_by_government_type <- MO_acfr_22 %>% 
  summarise(sum_net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
            sum_net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE),
            sum_net_pension_assets = sum(net_pension_assets, na.rm = TRUE),
            sum_net_opeb_assets = sum(net_opeb_assets, na.rm = TRUE),
            sum_total_liabilities = sum(total_liabilities, na.rm = TRUE),
            sum_total_assets = sum(total_assets, na.rm = TRUE),
            
            ) 

data_list <- list(
  "MO aggregate by government type" = MO_aggregate_by_government_type,
  "MO State" = MO_state,
  "MO counties" = MO_counties,
  "MO municipalities" = MO_municipalities,
  "MO School districts" = MO_sd_22,
  "Total vs collected" = total_collected
)
rio::export(data_list, "output/MO ACFRs data 2022.xlsx")
