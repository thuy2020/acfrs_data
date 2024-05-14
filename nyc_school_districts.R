
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
# Note: previously, Geoff suggested to apportion liabilities based on employee count 
#(email "NYC School districts",Feb 1). 
#However, since we have the number of students above, there's no need to take into account employee count. 


# input number from DOE report 
nyc_2020 <- nces %>% filter(str_detect(name, "(?i)NEW YORK CITY GEOGRAPHIC DISTRICT")) %>% 
#year 2020 - Department of Education NYC
  mutate(year = 2020, 
         doe_total_liabilities = 59391179000, #page 33
         doe_net_opeb = 35457858000, #page 33
         doe_net_pension = 0, #(Geoff checked, no net pension)
         doe_expenses = 30782114000,# page 26
         doe_total_asset = 66355469000, #page 33
         doe_pension_asset = NA,
         doe_opeb_asset = NA,
         doe_charges_services = 103098000,
         doe_operating_grant = 13505234000,
         doe_general_rev =  14444455000 + 51432000) %>% # page 26, report FY 2021

# apportion financial data for all 32 sd
mutate(
share_student = enrollment_20/sum(enrollment_20),
total_liabilities = doe_total_liabilities*share_student,
net_opeb_liability = doe_net_opeb*share_student,
net_pension_liability = doe_net_pension*share_student,
net_pension_assets = doe_pension_asset*share_student,
net_opeb_assets = doe_opeb_asset*share_student,
expenses = doe_expenses*share_student,
charges_services = doe_charges_services*share_student ,
operating_grant = doe_operating_grant*share_student ,
general_rev = doe_general_rev*share_student) 




###############
  
nyc_2021 <- nces %>% filter(str_detect(name, "(?i)NEW YORK CITY GEOGRAPHIC DISTRICT")) %>% 
#year 2021 - Department of Education NYC
  mutate(year = 2021,
         doe_total_liabilities = 48619741000, #page 33
         doe_net_opeb = 38982221000,#page 33
         doe_net_pension = 0, # page 33 (Geoff: Just make all the pension liabilities 0, as the note states.) 
         doe_expenses = 29278093000,# page 24
         doe_total_asset = 66152289000, #page 22
         
         
         #### TODO: note this in portal 
         doe_pension_asset = 1137314000, # Geoff: Feb 5, 2024, instead of input net pension liability as negative number. Should input net pension asset for the same number. 
         doe_opeb_asset = NA,
         
         #####
         
         doe_charges_services = 41776000,
        doe_operating_grant = 13458922000,
        doe_general_rev = 15412901000 + 62773000) %>%  # page 26

# apportion financial data for all 32 sd
mutate(
  share_student = enrollment_21/sum(enrollment_21),
  total_liabilities = doe_total_liabilities*share_student,
  net_opeb_liability = doe_net_opeb*share_student,
  net_pension_liability = doe_net_pension*share_student,
  net_pension_assets = doe_pension_asset*share_student,
  net_opeb_assets = doe_opeb_asset*share_student,
  expenses = doe_expenses*share_student,
  charges_services = doe_charges_services*share_student ,
  operating_grant = doe_operating_grant*share_student ,
  general_rev = doe_general_rev*share_student)  


###################


nyc_2022 <- nces %>% filter(str_detect(name, "(?i)NEW YORK CITY GEOGRAPHIC DISTRICT")) %>% 
#year 2021 - Department of Education NYC
  mutate(year = 2022,
         doe_total_liabilities = 56398529000, #page 33
         doe_net_opeb = 29427659000,#page 33
         doe_net_pension = 0, # Geoff:Just make all the pension liabilities 0, as the note states. 
         doe_pension_asset = NA,
         doe_opeb_asset = NA,
         doe_expenses = 33529494000,# page 24
         doe_total_asset = 66678975000, #page 22
         
    
# from report 2022
doe_charges_services = 66519000,
doe_operating_grant = 17238018000,
doe_general_rev = 16144530000 + 80427000) %>%  # page 24

# apportion financial data for all 32 sd
mutate(
  share_student = enrollment_22/sum(enrollment_22),
  total_liabilities = doe_total_liabilities*share_student,
  net_opeb_liability = doe_net_opeb*share_student,
  net_pension_liability = doe_net_pension*share_student,
  net_pension_assets = doe_pension_asset*share_student,
  net_opeb_assets = doe_opeb_asset*share_student,
  expenses = doe_expenses*share_student,
  charges_services = doe_charges_services*share_student ,
  operating_grant = doe_operating_grant*share_student ,
  general_rev = doe_general_rev*share_student)  

################
################

nyc_20_21_22 <- rbind(nyc_2020, nyc_2021, nyc_2022) %>% 
  mutate(id = NA,
         total_assets = NA,
         current_liabilities = NA, 
         unrestricted = NA, 
         current_assets = NA,
         revenues = charges_services + operating_grant + general_rev) %>% 
  select(state.abb, name, id, year, 
         total_liabilities, current_liabilities, current_assets,
         net_opeb_liability, net_pension_liability,
         net_pension_assets, net_opeb_assets,total_assets,
         unrestricted,
         expenses,revenues, ncesID) 


# only get those in top100 largest
nyc_top5_20_21_22 <- nyc_20_21_22 %>% 
  filter(ncesID %in% top_schools_by_year$ncesID)  
  
