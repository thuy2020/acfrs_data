library(dplyr)
library(stringr)
library(rio)
library(tidyr)
source("src/data_processing/census.R")
source("src/data_processing/nces.R")
source("src/data_processing/functions.R")

####Counties####


####Schools####
#####New York City#######

# NYC DOE reports total numbers of 32 school districts. https://infohub.nyced.org/docs/default-source/default-document-library/2021-annual-financial-statements.pdf Page 26, 33
# 
# FY FY2FY0FY2FY2FY: https://infohub.nyced.org/docs/default-source/default-document-library/2022-annual-financial-statements.pdf
# 
# Method: Apportion Acfrs data for School Districts
# - Calculate the students share of each school district. 
# - Multiply this share to the DOE total.
# data for Department of education, reported here:
#NYC DOE reports total numbers of 32 school districts. 
#https://infohub.nyced.org/docs/default-source/default-document-library/2021-annual-financial-statements.pdf Page 26, 33
#FY 2021 - 2022: https://infohub.nyced.org/docs/default-source/default-document-library/2022-annual-financial-statements.pdf
#FY 2020-2021: https://infohub.nyced.org/docs/default-source/default-document-library/2021-annual-financial-statements.pdf
#FY 2022-2023: https://infohub.nyced.org/docs/default-source/default-document-library/2023-annual-financial-statements.pdf

financial_columns <- c(
  "total_liabilities", "current_liabilities", "net_opeb_liability", 
  "net_pension_liability", "net_pension_assets", "net_opeb_assets",
  "current_assets", "total_assets", "expenses", "compensated_absences",
  "charges_for_services", "operating_grants", "general_revenue", "revenues"
)

nyc_doe <- import("data/_apportion values.xlsx", sheet = 1, skip = 6) %>% slice(1:4) %>% 
  select(2:18)

# Take data of 32 school districts in NY from NCES 
nyc_sd <- nces %>% filter(str_detect(name_nces, "(?i)NEW YORK CITY GEOGRAPHIC DISTRICT")) %>% 
  mutate(name_nces = str_to_lower(name_nces)) %>% 
  select(state.abb, ncesID, name_nces, 
         enrollment_20, enrollment_21, enrollment_22, enrollment_23) %>% 
  
  #percentage of enrollment 
  mutate(
    pct_enrollment_20 = enrollment_20/sum(enrollment_20),
    pct_enrollment_21 = enrollment_21/sum(enrollment_21),
    pct_enrollment_22 = enrollment_22/sum(enrollment_22),
    pct_enrollment_23 = enrollment_23/sum(enrollment_23)) %>% # 
  pivot_longer(cols = c(pct_enrollment_20:pct_enrollment_23), 
               names_to = "year", 
               values_to = "pct_enrollment") %>% 
  mutate(year = case_when(year == "pct_enrollment_20" ~ "2020",
                          year == "pct_enrollment_21" ~ "2021",
                          year == "pct_enrollment_22" ~ "2022",
                          year == "pct_enrollment_23" ~ "2023")) %>% # 
  mutate(year = as.double(year)) %>% 
  
  #join with DOE data
  left_join(nyc_doe, by  = c('year', 'state.abb')) %>% 
  
  #calculate financial number for each school district
  mutate(across(all_of(financial_columns), ~ as.numeric(.x) * pct_enrollment)) %>% 
  select(-c(charges_for_services, operating_grants, general_revenue)) %>% 
  select(-starts_with("pct")) %>% 
  arrange(name_nces) %>% 
  mutate(id = paste0("nycsd", row_number()))

# 4 NY schools in top 100 school nation-wide
# nyc_top <- nyc_sd %>% 
#   filter(ncesID %in% top100_schools$ncesID) 
# 
# nyc_top %>% select(ncesID) %>% distinct()

#####Boston Public Schools#####

boston_public_schools <- import("data/_apportion values.xlsx", sheet = 2, skip = 1) %>% 
  filter(name == "Boston Public Schools") %>% 
  select(-3, -c(general_revenue,charges_for_services, operating_grants, capital_grants)) %>% 
  mutate(ncesID = as.character(ncesID)) %>% 
  mutate(id = "boston")


exceptions <-  bind_2df_different_size(nyc_sd, boston_public_schools) %>% 
  
  #join all exceptions with ncesID
  left_join(nces, by = c("ncesID", "state.name", "state.abb"),
                         suffix = c("", "")) %>% 
  mutate(name = ifelse(state.abb == "NY", name_nces, name))
  
