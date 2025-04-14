library(tidyverse)
library(dplyr)
library(janitor)
options(scipen = 999)


#break down by state
breakdown_state <- school_districts_all %>% 
  filter(year %in% c(2022, 2023)) %>% 
  select(state.name, , year, enrollment_22) %>% 
  group_by(state.name, year) %>% 
  # add_count() %>% 
  # rename(number_sd_collected = n) %>% 
  # distinct() %>% 
  # arrange(state.name)
  summarize(sd_collected = n(),
            enrollment_collected = sum(enrollment_22, na.rm = TRUE))

nces_by_state <- nces %>% 
  select(state.name, enrollment_22) %>% 
  group_by(state.name) %>% 
  
  # number of students and sd by each state
  summarize(
    total_enrollment_22 = sum(enrollment_22, na.rm = TRUE),
    total_sd = n()  
  ) 

breakdown_state_pct_collected <- breakdown_state %>% 
  filter(year == 2023) %>% 
  #TODO: chech why MI has too many enrollment
  mutate(enrollment_collected = ifelse(state.name == "Michigan", enrollment_collected*.95, enrollment_collected)) %>% 
  left_join(nces_by_state, by = "state.name") %>% 
  mutate(pct_enrollment_collected = round(enrollment_collected/total_enrollment_22*100,2),
         pct_sd_collected = round(sd_collected/total_sd*100,2)) %>% 

# Carter checked: MT, MI, CO, OK
  filter(!`state.name` %in% c("Michigan", "Montana", "Colorado", "Oklahoma"))


####Michigan####
# Michigan has 57 ISDs, which are also known as Regional Educational Service Agencies (RESAs). 
michigan_nces %>% write.csv("tmp/michigan_schoodistricts_NCES.csv")

#all NCES school district - Michigan
michigan_nces <- nces %>% filter(state.name == "Michigan") 

michigan_acfr_22 <- school_districts_all %>% filter(state.name == "Michigan" & year == 2022) 

# in nces but Not in acfr: 
inNCES_NOT_in_acfr <- anti_join(michigan_nces, michigan_acfr_22, by = "ncesID") 

# in acfr but not in NCES: 
inACFR_NOT_in_nces <- anti_join(michigan_acfr_22, michigan_nces,  by = "ncesID") 


writexl::write_xlsx(
  list(
    "michigan_nces" = michigan_nces,
    "michigan_acfr_22" = michigan_acfr_22,
    "inNCES_NOT_in_acfr" = inNCES_NOT_in_acfr,
    "inACFR_NOT_in_nces" = inACFR_NOT_in_nces
    
  ),
  path = "tmp/michigan_school_districts.xlsx"
)


##
compare_school_districts <- function(state_name) {
  # Filter NCES and ACFR data for the specified state
  state_nces <- nces %>% filter(state.name == state_name)
  state_acfr_22 <- school_districts_all %>% filter(state.name == state_name & year == 2022)
  
  # Find districts in NCES but not in ACFR
  inNCES_NOT_in_acfr <- anti_join(state_nces, state_acfr_22, by = "ncesID")
  
  # Find districts in ACFR but not in NCES
  inACFR_NOT_in_nces <- anti_join(state_acfr_22, state_nces, by = "ncesID")
  
  # Find ACFR already in 2022 but not in 2023
  inACFR2022_NOT_inACFR2023 <- anti_join(school_districts_all %>% filter(state.name == state_name & year == 2022),
            school_districts_all %>% filter(state.name == state_name & year == 2023), by = "id") %>% 
    select(state.abb, year, id, name, name_nces, ncesID, county, city)
  
  # Define output file path
  output_path <- paste0("tmp/", tolower(gsub(" ", "_", state_name)), "_school_districts.xlsx")
  
  # Create a named list for writing to Excel
  data_list <- setNames(
    list(state_nces, state_acfr_22, inNCES_NOT_in_acfr, inACFR_NOT_in_nces, inACFR2022_NOT_inACFR2023),
    c(paste0(state_name, "_nces"), paste0(state_name, "_acfr_22"), "inNCES_NOT_in_acfr", "inACFR_NOT_in_nces", 
      "inACFR2022_NOT_inACFR2023")
  )
  
  # Write results to an Excel file
  writexl::write_xlsx(data_list, path = output_path)
  
  # Return file path for reference
  return(output_path)
}

# Example usage
compare_school_districts("Illinois")

anti_join(school_districts_all %>% filter(state.name == "Montana" & year == 2022),
          school_districts_all %>% filter(state.name == "Montana" & year == 2023), by = "id") %>% View()


anti_join(school_districts_all %>% filter(state.name == "Montana" & year == 2023),
          school_districts_all %>% filter(state.name == "Montana" & year == 2022), by = "id") %>% View()

# school in acr 2022 but not in nces
anti_join(
  school_districts_all %>% filter(state.name == "Colorado" & year == 2022),
 
   nces %>% filter(state.name == "Colorado"), by = "ncesID") %>% View()

# school in nces but not acr 2022 
anti_join(
  nces %>% filter(state.name == "Montana"),
  school_districts_all %>% filter(state.name == "Montana" & year == 2022),
  by = "ncesID") %>% View()

nces %>% filter(state.name == "Montana") %>% 
  summarise(tot_students = sum(enrollment_22, na.rm = TRUE))
  View()
  
  school_districts_all %>% filter(state.name == "Montana" & year == 2023) %>% View()
    summarise(tot_students = sum(enrollment_22, na.rm = TRUE))

####Result####
writexl::write_xlsx(
  list(
    "Aggregated Pension & OPEB debts" = result_sd_aggregated,
    "SD collected by state" = breakdown_state_pct_collected),
  path = "tmp/all_school_districts_22_23.xlsx")
