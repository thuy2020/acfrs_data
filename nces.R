# Top School Districts in NCES
library(dplyr)
library(stringr)
library(rio)
library(tidyr)

# Glossary
#https://nces.ed.gov/ccd/elsi/glossary.aspx?app=tableGenerator&term=11020,9558,13403,13392,21783,21784,21785,21786,21779,21780,21778,21781,21777,21776,21546,21545,21789,21544,21569,21553,21572,21554,21571,21875&level=PublicSchool&groupby=0

#Total Students, All Grades (Excludes AE) [Public School]: 
# This count excludes adult education students, 
#which is the number of students enrolled in adult education courses provided by the public elementary/secondary school system.

#NOTE: Enrollment data year 2022 is for school year 2021 - 2022
#download data: https://nces.ed.gov/ccd/elsi/tableGenerator.aspx?savedTableID=649124


nces_20 <- import(here::here("data", "ELSI_school year 2019 2020.csv")) 
#%>% 
  #filter(str_detect(`Agency Type [District] 2019-20`, "Regular local school district"))
nces_21 <- import(here::here("data", "ELSI_school year 2020 2021.csv")) 
#%>% 
 # filter(str_detect(`Agency Type [District] 2020-21`, "Regular local school district"))
nces_22 <- import(here::here("data", "ELSI_school year 2021 2022_cordinate.csv")) %>% 
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


nces <- nces_20 %>% left_join(nces_21) %>% left_join(nces_22) %>% 
  rename(name = 1,
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
  
  filter(!agency_type_20 %in% c("7-Independent Charter District", "†")) %>% 
  
  # year 20 & 21 have revenue by source local, state, fed
  select(-contains("Source")) %>% 
  
  # assuming agency type does not change in the 3 years
  select(-contains("agency_type")) %>% 
  
  # some ncesID missing leading 0
  mutate(ncesID = as.character(ncesID),
         ncesID = ifelse(
           nchar(ncesID) == 6, paste0("0", ncesID), ncesID)) %>% 
  
  mutate(across(.cols = c(5:12, 22:23), as.double)) %>% 
  
  mutate(state.name = str_to_title(state.name))


# Top 100 schools in each of the 3 years:
top_schools_by_year <- nces %>% 
  select(name, ncesID, enrollment_20, enrollment_21, enrollment_22) %>% 
  pivot_longer(cols = 3:5, 
               names_to = "year",
               values_to = "value") %>% 
  group_by(year) %>% 
  top_n(100, value) %>% 
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
anti_join(top100_2021, top100_2022, by = "ncesID")

# 4 sd in 2022 but not 2021
anti_join(top100_2022, top100_2021, by = "ncesID")

nces %>% saveRDS("nces.RDS")


###########

