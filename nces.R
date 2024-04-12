# Top School Districts in NCES
library(dplyr)
library(rio)

#NCES list
#data from here: https://nces.ed.gov/ccd/districtsearch/ 
# Filter number of student >=1
#NCES data set has 13,713 school districts across 50 states and 5 US territories, accounts for 48,420,226 students. 
#This analysis only concerns 13,707 school districts in 50 states, accounts for a total of 48,026,570 students.

#NOTE: This list contains all school districts that have 1 or more students. 
#Problem: Some ACFRs entities that are categorized as school districts have 0 student.

nces <- rio::import(here::here("data", "ncesdata_DBBFFFC.xlsx"), skip = 14) %>% 
  clean_names() %>% 
  select(nces_district_id, district_name, county_name, city, state,locale, students) %>% 
  rename(nces_original_name = district_name,
         ncesID = nces_district_id) %>% 
  drop_na(nces_original_name) %>% 
  # take out 5 territories "AS" "DC" "GU" "PR" "VI"
  # filter(!state %in% c("AS", "DC", "GU", "PR", "VI")) %>% 
  
  # for NCES ID that has only 6 digits, adding a leading 0
  mutate(ncesID = ifelse(str_length(ncesID) < 7, paste0("0", ncesID), ncesID)) %>% 
  mutate(students = as.numeric(students)) 

nces %>% saveRDS("nces.RDS")


###########
# NCES new each year

nces_20 <- import("data/ELSI_school year 2019 2020.csv")
nces_21 <- import("data/ELSI_school year 2020 2021.csv")
nces_22 <- import("data/ELSI_school year 2021 2022.csv")




#############





top_sd_nces <- nces %>% 
  arrange(desc(students)) %>% 
  filter(!nces_original_name %in% c("PUERTO RICO DEPARTMENT OF EDUCATION", "District of Columbia Public Schools")) %>% select(-c(county_name, city)) %>% 
  slice(1:100) 

top_sd_nces_102_200 <- nces %>% 
  arrange(desc(students)) %>% 
  filter(!nces_original_name %in% c("PUERTO RICO DEPARTMENT OF EDUCATION", "District of Columbia Public Schools")) %>% select(-c(county_name, city)) %>% 
  slice(101:200) 
