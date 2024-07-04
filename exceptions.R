
library(dplyr)
library(stringr)
library(rio)
library(tidyr)
source("nces.R")

#####New York City#######

# NYC DOE reports total numbers of 32 school districts. https://infohub.nyced.org/docs/default-source/default-document-library/2021-annual-financial-statements.pdf Page 26, 33
# 
# FY FY2FY0FY2FY2FY: https://infohub.nyced.org/docs/default-source/default-document-library/2022-annual-financial-statements.pdf
# 
# Method: Apportion Acfrs data for School Districts
# - Calculate the students share of each school district. 
# - Multiply this share to the DOE total.
# 
# Geoff:Just make all the pension liabilities 0, as the note states. 

nyc_nces <- nces %>% filter(str_detect(name_nces, "(?i)NEW YORK CITY GEOGRAPHIC DISTRICT")) %>% 
  mutate(id = NA) %>% mutate(name_nces = str_to_lower(name_nces))
sum(nyc_nces$enrollment_20)

nyc_sd <- import("data/_apportion values.xlsx", sheet = 1, skip = 5) %>% 
  filter(name != "DOE") %>% 
  mutate(name_nces = NA,
         category = "School District",
         url = NA) %>% 
  select(-c(charges_services, operating_grant, general_rev)) 

# only get those in top100 largest
nyc_top5 <- nyc_sd %>% 
  filter(ncesID %in% top_schools_by_year$ncesID)  



######### Others##########
#These values will be imported to the database later. 

# currently there's no revenues field in database

#Metropolitan Nashville Public Schools TN
# revenues = case_when(id == "107203" & year == "2021"~ 1333733574, # 982397940 + 213091395 + 138244239) %>% # page B-10
#                      id == "107203" & year == "2020" ~ 1135186800,   #883724814 + 138389571 + 113072415      
#                      TRUE ~ revenues)


  
