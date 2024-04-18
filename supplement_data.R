options(scipen = 999)
library(tidyverse)
library(dplyr)
library(janitor)



# Income from Census - County level household income
income <- rio::import(here::here("data", "Unemployment_median income.xlsx"), skip = 4)  %>% 
  select(FIPS_Code, Median_Household_Income_2021) %>% 
  rename(geo_id = FIPS_Code, 
         median_hh_income_21 = Median_Household_Income_2021)

# State - partisan lean

partisan_lean <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/partisan-lean/2021/fivethirtyeight_partisan_lean_STATES.csv")
