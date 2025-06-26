library(dplyr)
library(stringr)
library(rio)
library(tidyr)
library(purrr)

source("src/data_processing/functions.R")
source("src/data_processing/census.R")
source("src/data_processing/nces.R")


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


NYC_Boston <-  bind_2df_different_size(nyc_sd, boston_public_schools) %>% 
  
  #join all exceptions with ncesID
  left_join(nces, by = c("ncesID", "state.name", "state.abb"),
                         suffix = c("", "")) %>% 
  mutate(name = ifelse(state.abb == "NY", name_nces, name))
  
#####Delaware#####
#p.53-54
#State of Delaware are financially responsible for Delaware Charter schools

delaware_schools <- readxl::read_xlsx("data/_delaware_schooldistricts.xlsx")

# Compute percent enrollment and apply it to all relevant columns
nces_de <- nces %>%
  filter(state.abb == "DE") %>%
  filter(!ncesID %in% c("1000022", "1000020", "1000021")) %>%
  mutate(percent_enrollment = enrollment_23 / sum(enrollment_23)) 

# Get the relevant columns to be distributed (columns 2 to 10)
columns_to_distribute <- names(delaware_schools)[5:17]

# Multiply each column by percent_enrollment
weighted_data <- map_dfc(columns_to_distribute, function(col) {
  weighted_col <- delaware_schools[[col]] * nces_de$percent_enrollment
  tibble(!!col := weighted_col)
})

# Combine the result
final_data_DE <- bind_cols(nces_de, weighted_data) %>% 
  mutate(across(10:27, as.numeric)) %>% 
  rowwise() %>% 
  mutate(
    revenues = sum(charge_services + grant_capital + general_revenues)) %>% 
  select(-c(charge_services, grant_capital, general_revenues)) %>% 
  # create dummy id
  mutate(id = state_agency_id) %>% 
  
  mutate(year = 2023,
         name = name_nces) %>% 
  ungroup()


#####Montana####
#Problem: NCES lists some schools in MT as school districts, while ACFR report includes 
#several ones.  
#--> sum up enrollment of these NCES school districts to correspond to ACFR report
MT_sd_pairs <- nces %>% filter(state.abb == "MT") %>% 
  #Pair of school districts in NCES but every 2 of them are covered in one ACFR
filter(str_detect(name_nces, "billings|missoula|(great falls)|(flathead)|
                  (butte)|bozeman|helena|whitefish|browning|polson|belgrade")) %>% 
 
   #these are not in any pair
filter(!name_nces %in% c("helena flats elem", "east helena k-12", 
                         "flathead h s")) %>% #this already in Kalispell Public Schools
drop_na(enrollment_23) %>% 

#sum up enrollment for each pair, only keep one NCESID, long and lat

  # Assign a group id to each pair
  mutate(pair_id = (row_number() + 1) %/% 2) %>%     #integer division by 2: divides by 2 and discards the remainder
  group_by(pair_id) %>%
  mutate(
    enrollment_20 = sum(enrollment_20),
    enrollment_21 = sum(enrollment_21),
    enrollment_22 = sum(enrollment_22),
    enrollment_23 = sum(enrollment_23)) %>%       # Sum enrollment per pair
  slice(1) %>%                                       # Keep only the first row in each pair
  ungroup() %>%
  select(-pair_id) %>% 
  
  
  #create id column --> map these pairs to their acfr entities 
  #--> use this id to join back to get acfr data
  mutate(id = case_when(ncesID == "3003290" ~ "43827", 
                        ncesID == "3003870" ~ "35425",
                        ncesID == "3004560" ~ "35403",
                        ncesID == "3005140" ~ "42164",
                        ncesID == "3013040" ~ "81617",
                        ncesID == "3000005" ~ "35411",
                        ncesID == "3018570" ~ "1266224",
                        ncesID == "3021060" ~ "43285",
                        ncesID == "3027740" ~ "35402"
                        ))


MT_sd_acfr <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "School District") %>% 
  mutate(id = as.character(id)) %>% 
  select(any_of(fields_to_select)) %>% 
  filter(state.abb == "MT")


montana_sd <- MT_sd_pairs %>% 
  left_join(MT_sd_acfr, by = c("state.abb", "state.name", "id")) %>% 
  select(-c(identifier, category))


#####Final result exceptions#####

nyc_boston_de <- bind_2df_different_size(NYC_Boston, final_data_DE) 

exceptions <- bind_2df_different_size(montana_sd, nyc_boston_de) %>% 
  mutate(category = "School District")

