
library(dplyr)
library(stringr)
library(rio)
library(tidyr)
source("nces.R")
## New York City

# NYC DOE reports total numbers of 32 school districts. https://infohub.nyced.org/docs/default-source/default-document-library/2021-annual-financial-statements.pdf Page 26, 33
# 
# FY FY2FY0FY2FY2FY: https://infohub.nyced.org/docs/default-source/default-document-library/2022-annual-financial-statements.pdf
# 
# Method: Apportion Acfrs data for School Districts
# - Calculate the students share of each school district. 
# - Multiply this share to the DOE total.
# 
# Geoff:Just make all the pension liabilities 0, as the note states. 
nyc_nces <- nces %>% filter(str_detect(name, "(?i)NEW YORK CITY GEOGRAPHIC DISTRICT")) %>% 
  mutate(id = NA) %>% mutate(name = str_to_lower(name))

nyc_sd <- import("data/_apportion values.xlsx", sheet = 1, skip = 5) %>% 
  filter(name != "DOE") %>% 
  select(-c(charges_services, operating_grant, general_rev)) 

# only get those in top100 largest
nyc_top5 <- nyc_sd %>% 
  filter(ncesID %in% top_schools_by_year$ncesID)  
  
