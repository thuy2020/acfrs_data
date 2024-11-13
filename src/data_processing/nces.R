library(dplyr)
library(stringr)
library(rio)
library(tidyr)

#####Glossary#####
#https://nces.ed.gov/ccd/elsi/glossary.aspx?app=tableGenerator&term=11020,9558,13403,13392,21783,21784,21785,21786,21779,21780,21778,21781,21777,21776,21546,21545,21789,21544,21569,21553,21572,21554,21571,21875&level=PublicSchool&groupby=0

#Total Students, All Grades (Excludes AE) [Public School]: 
# This count excludes adult education students, 
#which is the number of students enrolled in adult education courses provided by the public elementary/secondary school system.

#NOTE: Enrollment data year 2022 is for school year 2021 - 2022
#download data: https://nces.ed.gov/ccd/elsi/tableGenerator.aspx?savedTableID=649124

# HSD = High School District
# CHSD = Community High School District
# CUSD = Community Unit School District
# 
# CCSD = Community Consolidated School District
# Spec Educ Coop = Special Education Cooperative
# ROE = Regional Office of Education
# sd = public schools district
# USD = School District
# UD =  Consolidated School District

# old data: Marc emailed 8/23/2022

# old_sd <- rio::import("data/ncesdata_DBBFFFC.xlsx", skip = 14) %>% select(1, 3, 11) %>% 
#   mutate(student = as.double(`Students*`))
# sum(old_sd$student)

####Input####
d_20 <- import(here::here("data", "ELSI_school year 2019 2020.csv")) 
#%>% 
#filter(str_detect(`Agency Type [District] 2019-20`, "Regular local school district"))
d_21 <- import(here::here("data", "ELSI_school year 2020 2021.csv")) 
#%>% 
# filter(str_detect(`Agency Type [District] 2020-21`, "Regular local school district"))
d_22 <- import(here::here("data", "ELSI_school year 2021 2022_cordinate.csv")) %>% 
  # filter(str_detect(`Agency Type [District] 2021-22`, "Regular local school district"))
  
  select(-c(`State Name [District] 2021-22`, 
            `Location State Abbr [District] 2021-22`,
            `Agency Name [District] 2021-22`, 
            `Total Staff [District] 2021-22`)) %>% 
  #filter(`Agency Type [District] 2021-22` == "1-Regular local school district that is NOT a component of a supervisory union")
  rename(
    county = `County Name [District] 2021-22`,
    city = `Location City [District] 2021-22`,
    zip = `Location ZIP [District] 2021-22`,
    zip4 = `Location ZIP4 [District] 2021-22`, 
    locale = `Locale [District] 2021-22`, 
    latitude = `Latitude [District] 2021-22`,
    longitude = `Longitude [District] 2021-22`, 
    congresstional_code = `Congressional Code [District] 2021-22`,
    metro_code = `Metro Micro Area Code [District] 2021-22`)

#NCES agency type

#unique(d_20$`Agency Type [District] 2019-20`)
# [1] "7-Independent Charter District"                                                                        
# [2] "†"                                                                                                     
# [3] "9-Specialized public school district"                                                                  
# [4] "1-Regular local school district that is NOT a component of a supervisory union"                        
# [5] "8-Other education agencies"                                                                            
# [6] "4-Regional Education Service Agency (RESA)"                                                            
# [7] "3-Supervisory union administrative center (or county superintendent's office serving the same purpose)"
# [8] "2-Local school district that is a component of a supervisory union"                                    
# [9] "5-State agency providing elementary and/or secondary level instruction"                                
# [10] "6-Federal agency providing elementary and/or secondary level instruction"


nces <- d_20 %>% left_join(d_21) %>% left_join(d_22) %>% 
  rename(name_nces = 1,
         state.name = 2, 
         state.abb = 3,
         ncesID = 4,
         
         agency_type_20 = `Agency Type [District] 2019-20`,
         agency_type_21 = `Agency Type [District] 2020-21`,
         agency_type_22 = `Agency Type [District] 2021-22`,
         
         enrollment_20 = `Total Students All Grades (Excludes AE) [District] 2019-20`,
         enrollment_21 = `Total Students All Grades (Excludes AE) [District] 2020-21`,
         enrollment_22 = `Total Students All Grades (Excludes AE) [District] 2021-22`,
         
         
         fte_20 = `Full-Time Equivalent (FTE) Teachers [District] 2019-20`,
         fte_21 = `Full-Time Equivalent (FTE) Teachers [District] 2020-21`,
         fte_22 = `Full-Time Equivalent (FTE) Teachers [District] 2021-22`,
         
         
         revenue_nces_20 = `Total Revenue (TOTALREV) [District Finance] 2019-20`,
         revenue_nces_21 = `Total Revenue (TOTALREV) [District Finance] 2020-21`,
         
         expenditure_nces_20 = `Total Expenditures (TOTALEXP) [District Finance] 2019-20`,
         expenditure_nces_21 = `Total Expenditures (TOTALEXP) [District Finance] 2020-21`
         
  ) %>% 
  filter(state.abb != "PR") %>% 
  
  # excluding school districts that belong to one of these categories for at least 1 year
  filter(!(agency_type_20 %in% c("7-Independent Charter District", "†", "6-Federal agency providing elementary and/or secondary level instruction")) |
         !(agency_type_21 %in% c("7-Independent Charter District", "†", "6-Federal agency providing elementary and/or secondary level instruction")) | 
         !(agency_type_22 %in% c("7-Independent Charter District", "†", "6-Federal agency providing elementary and/or secondary level instruction"))) %>% 
  
  
  # excluding school districts who has 0 or NA enrollment for any school year. 
  filter(!is.na(enrollment_20) & !is.na(enrollment_21) & !is.na(enrollment_22)) %>% 
  filter(enrollment_20 != 0 & enrollment_21 != 0 & enrollment_22 != 0) %>% 
  
  # year 20 & 21 have revenue by source local, state, fed
  select(-contains("Source")) %>% 
  
  # assuming agency type does not change in the 3 years
  select(-contains("agency_type")) %>% 
  
  # some ncesID missing leading 0
  mutate(ncesID = as.character(ncesID),
         ncesID = ifelse(
                  nchar(ncesID) == 6, paste0("0", ncesID), ncesID),
         name_nces = str_to_lower(name_nces)) %>% 
  
  mutate(across(.cols = c(5:12, 22:23), as.double)) %>% 
  mutate(state.name = str_to_title(state.name)) %>% 

# adding some enrollments
  mutate(enrollment_22 = case_when(ncesID == "0405030" ~ 933,
                                   ncesID == "5900192"~ 367,
                                   
                                   ncesID == "5900052" ~ 92,
                                   ncesID == "5900148" ~ 234,
                                   ncesID == "5900131" ~ 98,
                                   TRUE ~ enrollment_22
                                   ))



####Count total enrollment each year####

nrow(nces %>% select(state.abb, ncesID, enrollment_20) %>% distinct())

sum(nces$enrollment_20, na.rm = TRUE)
sum(nces$enrollment_21, na.rm = TRUE)
sum(nces$enrollment_22, na.rm = TRUE)


####Top 100 schools in each of the 3 years####
top_schools_by_year <- nces %>% 
  select(name_nces, ncesID, enrollment_20, enrollment_21, enrollment_22) %>% 
  pivot_longer(cols = 3:5, 
               names_to = "year",
               values_to = "value") %>% 
  group_by(year) %>% 
  top_n(100, value) %>% 
  ungroup() %>% 
  arrange(year, desc(value))
####Top 200 schools in each of the 3 years####
top200_schools_by_year <- nces %>% 
  select(name_nces, ncesID, enrollment_20, enrollment_21, enrollment_22) %>% 
  pivot_longer(cols = 3:5, 
               names_to = "year",
               values_to = "value") %>% 
  group_by(year) %>% 
  top_n(200, value) %>% 
  ungroup() %>% 
  arrange(year, desc(value))


####Top 300 schools in each of the 3 years####
top300_schools_by_year <- nces %>% 
  select(name_nces, ncesID, enrollment_20, enrollment_21, enrollment_22) %>% 
  pivot_longer(cols = 3:5, 
               names_to = "year",
               values_to = "value") %>% 
  group_by(year) %>% 
  top_n(300, value) %>% 
  ungroup() %>% 
  arrange(year, desc(value))

# split each year

top100_2020 <- top_schools_by_year %>% 
  filter(year == "enrollment_20")

top100_2021 <- top_schools_by_year %>% 
  filter(year == "enrollment_21")

top100_2022 <- top_schools_by_year %>% 
  filter(year == "enrollment_22")


# lists of top 100 sd for each year are slightly different

# 2020 and 2021 are the same
anti_join(top100_2020, top100_2021, by = "ncesID")
anti_join(top100_2021, top100_2020, by = "ncesID")

# 4 sd in 2021 but not 2022
anti_join(top100_2021, top100_2022, by = "ncesID") %>% 
  mutate(name_nces = str_to_lower(name_nces))

# 4 sd in 2022 but not 2021
anti_join(top100_2022, top100_2021, by = "ncesID")

nces %>% saveRDS("data/nces.RDS")

# pop break down by state 
nces_pop_bystate <- nces %>%
  group_by(state.abb) %>%
  summarise(population = sum(enrollment_22,na.rm = TRUE)) %>%
  mutate(category = "School Districts")


cat("End of script")

