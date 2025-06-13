library(dplyr)
library(stringr)
library(rio)
library(tidyr)
library(janitor)
df_state <- data.frame(state.abb, state.name)
####School year 2023####
#NOTE: Enrollment data year 2022 is for school year 2021 - 2022
#download data: https://nces.ed.gov/ccd/elsi/tableGenerator.aspx?savedTableID=649124

####Read in data####
# Note that these might vary by year: district names, agency type. 
# There are 870 rows showing this differences. But there is only one ncesID. 
# So we can safely ignore the variations in district names. 

# d_checking_differences <- import(here::here("data/ELSI_csv_export_6388358946254740384252.csv")) %>% 
#   clean_names() %>% 
#   rowwise() %>%
#   filter(!all(c_across(state_name_district_2022_23:state_name_district_2019_20) == first(c_across(state_name_district_2022_23:state_name_district_2019_20)))) %>%
#   ungroup()


nces <- import(here::here("data/ELSI_csv_export_6388370796396691352320.csv")) %>% 
  clean_names() %>% 
  select(agency_name, 
       state_name_district_2022_23,
       state_abbr_district_latest_available_year,
       agency_name_district_2022_23,
       agency_id_nces_assigned_district_latest_available_year,
       
       agency_type_district_2022_23,
       state_agency_id_district_2022_23,
       county_name_district_2022_23,
       
       latitude_district_2022_23,
       longitude_district_2022_23,
       
       total_students_all_grades_excludes_ae_district_2019_20,
       total_students_all_grades_excludes_ae_district_2020_21,
       total_students_all_grades_excludes_ae_district_2021_22,
       total_students_all_grades_excludes_ae_district_2022_23) %>% 
  rename(state.name = state_name_district_2022_23,
         state.abb = state_abbr_district_latest_available_year,
         name_nces = agency_name_district_2022_23,
         
         ncesID = agency_id_nces_assigned_district_latest_available_year,
         state_agency_id = state_agency_id_district_2022_23,
         county_nces = county_name_district_2022_23,
         agency_type = agency_type_district_2022_23,
         
         
         latitude = latitude_district_2022_23,
         longitude = longitude_district_2022_23,
         
         enrollment_20 = total_students_all_grades_excludes_ae_district_2019_20,
         enrollment_21 = total_students_all_grades_excludes_ae_district_2020_21,
         enrollment_22 = total_students_all_grades_excludes_ae_district_2021_22,
         enrollment_23 = total_students_all_grades_excludes_ae_district_2022_23
         ) %>% 
  select(-1) %>% 
       
  # excluding school districts that belong to one of these categories 
  filter(!(agency_type %in% c("7-Independent Charter District", "†", 
                                 "6-Federal agency providing elementary and/or secondary level instruction"))) %>% 
  #filter(!enrollment_23 %in% c("†")) %>% 
  mutate(ncesID = as.character(ncesID),
         ncesID = str_squish(ncesID),
         
         
         
         state.name = str_to_title(state.name),
         state.name = str_squish(state.name)) %>% 
  
  mutate(name_nces = str_to_lower(name_nces), 
         name_nces = str_squish(name_nces)) %>% 
  
  mutate(across(enrollment_20:enrollment_23, as.numeric))

nrow(nces)

#note: 1372 districts do not have enrollment
nrow(nces %>% filter(enrollment_23 < 1))


# dictionary %>% 
#   filter(ncesID %in% nces_23$ncesID)
# nces_23 %>% 
#   filter(ncesID %in% dictionary$ncesID) %>% View()
# 
# nces_23 %>% left_join(dictionary, by = "ncesID") %>% 
#   drop_na(id) %>% 
#   View()


# adding some enrollments
  # mutate(enrollment_22 = case_when(ncesID == "0405030" ~ 933,
  #                                  ncesID == "5900192"~ 367,
  # 
  #                                  ncesID == "5900052" ~ 92,
  #                                  ncesID == "5900148" ~ 234,
  #                                  ncesID == "5900131" ~ 98,
  #                                  TRUE ~ enrollment_22
  #                                  ))


####Top 100 schools in 2022 enrollment ####
top100_schools <- nces %>%
  select(name_nces, ncesID, enrollment_23) %>%
  arrange(desc(enrollment_23)) %>%
 slice(1:100)
# 
# # pop break down by state 
# nces_pop_bystate <- nces %>%
#   group_by(state.abb) %>%
#   summarise(population = sum(enrollment_22,na.rm = TRUE)) %>%
#   mutate(category = "School Districts")
# 

sd_top100_nces <- nces %>% select(state.abb, name_nces, ncesID, enrollment_23) %>%
  arrange(desc(enrollment_23)) %>%
  slice(1:100)

sd_top200_nces <- nces %>% select(state.abb, name_nces, ncesID, enrollment_23) %>%
  arrange(desc(enrollment_23)) %>%
  slice(1:200)

sd_top300_nces <- nces %>% select(state.abb, name_nces, ncesID, enrollment_23) %>%
  arrange(desc(enrollment_23)) %>%
  slice(1:300)

cat("End of script")


#####Glossary#####
#https://nces.ed.gov/ccd/elsi/glossary.aspx?app=tableGenerator&term=11020,9558,13403,13392,21783,21784,21785,21786,21779,21780,21778,21781,21777,21776,21546,21545,21789,21544,21569,21553,21572,21554,21571,21875&level=PublicSchool&groupby=0

#Total Students, All Grades (Excludes AE) [Public School]: 
# This count excludes adult education students, 
#which is the number of students enrolled in adult education courses provided by the public elementary/secondary school system.



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

#NCES agency type

#unique(d_20$`Agency Type [District] 2019-20`)
#The education Agency Type code specifies the classifications within the geographic 
#boundaries of a state according to the level of administrative and operational control. 
#Agency types have changed over the years of the CCD collection. 
#Not all the types shown above will be found in the earlier, annual data files published here, 
#https://nces.ed.gov/ccd/ccddata.asp. ElSi has been programmed to reconcile these differences 
#so that filtering by agency type and displays of agency type are consistent with the above list.

#The classifications are:

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

