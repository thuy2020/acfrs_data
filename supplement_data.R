options(scipen = 999)
library(tidyverse)
library(dplyr)
library(janitor)



#########Income###########
# County: Income from Census - County level household income
income <- rio::import(here::here("data", "Unemployment_median income.xlsx"), skip = 4)  %>% 
  select(FIPS_Code, Median_Household_Income_2021) %>% 
  rename(geo_id = FIPS_Code, 
         median_hh_income_21 = Median_Household_Income_2021)



#City:
#S1903 MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS)
#2021: ACS 1-Year Estimates Subject Tables
#https://data.census.gov/table/ACSST1Y2021.S1903?q=median%20household%20income%20by%20city%202021&g=010XX00US$0300000

city_income <- rio::import(here::here("data/ACSST1Y2021.S1903_2023-12-29T105538/ACSST1Y2021.S1903-Data.csv"), skip = 1) %>% 
  select(1, `Geographic Area Name`, `Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households`) %>% 
  rename(median_hh_income_21 = `Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households`
  ) %>% 
  mutate(
    place = substring(Geography, 12, 16),
    state = substring(Geography, 10, 11)) %>% 
  
  mutate(geo_id = paste0(state, place)) %>% 
  select(geo_id, median_hh_income_21)


# State - partisan lean

partisan_lean <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/partisan-lean/2021/fivethirtyeight_partisan_lean_STATES.csv")
