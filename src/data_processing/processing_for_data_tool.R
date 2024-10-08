library(tidyverse)
library(scales)
library(jsonlite)
source("src/data_processing/census.R")

# read in data
state_data_temp <- read_csv("output/all_states_4years_2020_2023.csv") %>% 
  filter(year != 2023) 
county_data_temp <- read_csv("output/top100_counties.csv") %>% 
  filter(year != 2023) 
city_data_temp <- read_csv("output/top100_cities.csv") %>% 
  filter(year != 2023) 
school_data_temp <- read_csv("output/top100_sd.csv")%>% 
  filter(year != 2023) 

city_data_temp %>% 
  group_by(name) %>% 
  summarise(n = n()) %>% filter(n <3)

#function to clean up data
modify_data <- function(data_temp){
  data_temp|>
    mutate(name = str_to_title(name)) |>
    mutate(
      modified_revenues = revenues - (expenses + current_liabilities),
      net_pension_liab =  net_pension_liability - net_pension_assets,
      net_opeb_liab = net_opeb_liability - net_opeb_assets
    ) |>
    select(-net_pension_liability, -net_opeb_liability, -net_pension_assets, -net_opeb_assets) |>
    rename(
      net_pension_liability = net_pension_liab,
      net_opeb_liability = net_opeb_liab
    ) |>
    mutate(
      assets_liab_ratio = total_liabilities / total_assets
    ) |>
    arrange(year, state.name) |>
    select(-`...1`)
}
############################
####### State Data ########
############################

state_data <- modify_data(state_data_temp) 

# Filter 2022 data 
state_data_22 <- state_data |>
  filter(year == 2022) |>
  rename_with(~ paste0(., "_22"))


state_data_20 <- state_data |>
  filter(year == 2020) |>
  rename_with(~ paste0(., "_20"))

#growth function
data_growth <- function(data_22, data_20) {

data_22 |>
  left_join(data_20, by = c("name_22" = "name_20", "state.name_22" = "state.name_20")) |>
  mutate(
    total_liabilities_growth = case_when(
      total_liabilities_22 == 0 & total_liabilities_20 == 0 ~ 0,
      TRUE ~ (total_liabilities_22 - total_liabilities_20) / abs(total_liabilities_20)
    ),
    current_liabilities_growth = case_when(
      current_liabilities_22 == 0 & current_liabilities_20 == 0 ~ 0,
      TRUE ~ (current_liabilities_22 - current_liabilities_20) / abs(current_liabilities_20)
    ),
    expenses_growth = case_when(
      expenses_22 == 0 & expenses_20 == 0 ~ 0,
      TRUE ~ (expenses_22 - expenses_20) / abs(expenses_20)
    ),
    net_opeb_liability_growth = case_when(
      net_opeb_liability_22 == 0 & net_opeb_liability_20 == 0 ~ 0,
      TRUE ~ (net_opeb_liability_22 - net_opeb_liability_20) / abs(net_opeb_liability_20)
    ),
    net_pension_liability_growth = case_when(
      net_pension_liability_22 == 0 & net_pension_liability_20 == 0 ~ 0,
      TRUE ~ (net_pension_liability_22 - net_pension_liability_20) / abs(net_pension_liability_20)
    ),
    revenues_growth = case_when(
      revenues_22 == 0 & revenues_20 == 0 ~ 0,
      TRUE ~ (revenues_22 - revenues_20) / abs(revenues_20)
    ),
    total_assets_growth = case_when(
      total_assets_22 == 0 & total_assets_20 == 0 ~ 0,
      TRUE ~ (total_assets_22 - total_assets_20) / abs(total_assets_20)
    ),
    assets_liab_ratio_growth = case_when(
      assets_liab_ratio_22 == 0 & assets_liab_ratio_20 == 0 ~ 0,
      TRUE ~ (assets_liab_ratio_22 - assets_liab_ratio_20) / abs(assets_liab_ratio_20)
    ),
    modified_revenues_20 = revenues_20 - (expenses_20 + current_liabilities_20),
    modified_revenues_22 = revenues_22 - (expenses_22 + current_liabilities_22),
    modified_revenues_growth = case_when(
      modified_revenues_22 == 0 & modified_revenues_20 == 0 ~ 0,
      TRUE ~ (modified_revenues_22 - modified_revenues_20) / abs(modified_revenues_20)
    )
  ) |>
  rename(
    name = name_22,
    geo_id = geo_id_22,
    state.name = state.name_22,
    state.abb = state.abb_22
  ) |>
  select(-geo_id_20)  

}

state_data_growth <- data_growth(state_data_22, state_data_20) %>% 

# join partisan lean
  left_join(partisan_lean, by = c("name" = "state")) 

state_data_growth <- state_data_growth |>
  mutate(id = as.numeric(geo_id)) |>
  mutate(id = str_sub(id, 1, str_length(id)-3)) |>
  mutate(
    geo_id = str_pad(as.numeric(id), width = 2, pad = "0"),
    partisan_lean = `2022`
  ) |>
  mutate(pct_urban_pop_22_normalized = rescale(pct_urban_pop_22, to = c(-50, 50))) |>
  mutate(median_hh_income_normalized = rescale(median_hh_income_21_22, to = c(-50, 50)))


state_data_growth_json <- toJSON(state_data_growth, auto_unbox = TRUE, pretty = TRUE)
write(state_data_growth_json, file="output/state_data_growth.json")


state_data_json <- toJSON(state_data, auto_unbox = TRUE, pretty = TRUE)
write(state_data_json, file="output/state_data.json")



############################
####### County Data ########
############################

# clean up data
county_data <- modify_data(county_data_temp) %>% 
mutate(name = paste(name, `state.abb`, sep = ", "))

county_data_22 <- county_data |>
  filter(year == 2022) |>
  rename_with(~ paste0(., "_22"))

county_data_20 <- county_data |>
  filter(year == 2020) |>
  rename_with(~ paste0(., "_20"))


county_data_growth <- data_growth(county_data_22, county_data_20) %>% 
  mutate(pct_urban_pop_22_normalized = rescale(pct_urban_pop_22, to = c(-50, 50))) |>
  mutate(median_hh_income_normalized = rescale(median_hh_income_21_22, to = c(-50, 50)))


county_data_growth_json <- toJSON(county_data_growth, auto_unbox = TRUE, pretty = TRUE)
write(county_data_growth_json, file="output/county_data_growth.json")

county_data_growth <- county_data_growth |>
  mutate(
    total_liabilities_growth = total_liabilities_growth * 100,
    net_pension_liability_growth = net_pension_liability_growth * 100,
    expenses_growth = expenses_growth * 100
  )

write_csv(county_data_growth, "output/county_data_growth.csv")

county_data_json <- toJSON(county_data, auto_unbox = TRUE, pretty = TRUE)
write(county_data_json, file="output/county_data.json")


############################
######## City Data #########
############################

# clean up data
city_data <- modify_data(city_data_temp) %>% 
  mutate(name_state = paste(name, `state.name`, sep = ", ")) |>
  mutate(name = paste(name, `state.abb`, sep = ", ")) 

city_data_22 <- city_data |>
  filter(year == 2022) |>
  rename_with(~ paste0(., "_22"))

city_data_20 <- city_data |>
  filter(year == 2020) |>
  rename_with(~ paste0(., "_20"))


city_data_growth <- data_growth(city_data_22, city_data_20) %>% 
  rename(name_state = name_state_22)
  

cityCoordinates <- readRDS("data/cityCoordinates.RDS")

cityCoordinates <- cityCoordinates[, (3:5)]

city_data_growth <- city_data_growth |>
  mutate(name_state = ifelse(name_state == "City And County Of San Francisco, California",
                             "San Francisco, California",
                             name_state)) |>
  left_join(cityCoordinates, by = c("name_state" = "name_state"))|>
  mutate(median_hh_income_normalized = rescale(median_hh_income_21_22, to = c(-50, 50)))


city_data_growth_json <- toJSON(city_data_growth, auto_unbox = TRUE, pretty = TRUE)
write(city_data_growth_json, file="output/city_data_growth.json")

city_data_growth <- city_data_growth |>
  mutate(
    total_liabilities_growth = total_liabilities_growth * 100,
    net_pension_liability_growth = net_pension_liability_growth * 100,
    expenses_growth = expenses_growth * 100
  )

write_csv(city_data_growth, "output/city_data_growth.csv")

city_data_json <- toJSON(city_data, auto_unbox = TRUE, pretty = TRUE)
write(city_data_json, file="output/city_data.json")


############################
####### School Data ########
############################

# clean up data
school_data <- modify_data(school_data_temp) %>% 
  mutate(city = str_to_title(tolower(city))) |>
  mutate(name_state = paste(city, `state.abb`, sep = ", ")) 


school_data_22 <- school_data |>
  filter(year == 2022) |>
  rename_with(~ paste0(., "_22"))

school_data_20 <- school_data |>
  filter(year == 2020) |>
  rename_with(~ paste0(., "_20"))


school_data_growth <- school_data_22 |>
  left_join(school_data_20, by = c("ncesID_22" = "ncesID_20")) |>
  mutate(
    total_liabilities_growth = case_when(
      total_liabilities_22 == 0 & total_liabilities_20 == 0 ~ 0,
      TRUE ~ (total_liabilities_22 - total_liabilities_20) / abs(total_liabilities_20)
    ),
    expenses_growth = case_when(
      expenses_22 == 0 & expenses_20 == 0 ~ 0,
      TRUE ~ (expenses_22 - expenses_20) / abs(expenses_20)
    ),
    net_opeb_liability_growth = case_when(
      net_opeb_liability_22 == 0 & net_opeb_liability_20 == 0 ~ 0,
      TRUE ~ (net_opeb_liability_22 - net_opeb_liability_20) / abs(net_opeb_liability_20)
    ),
    net_pension_liability_growth = case_when(
      net_pension_liability_22 == 0 & net_pension_liability_20 == 0 ~ 0,
      TRUE ~ (net_pension_liability_22 - net_pension_liability_20) / abs(net_pension_liability_20)
    ),
    revenues_growth = case_when(
      revenues_22 == 0 & revenues_20 == 0 ~ 0,
      TRUE ~ (revenues_22 - revenues_20) / abs(revenues_20)
    ),
    total_assets_growth = case_when(
      total_assets_22 == 0 & total_assets_20 == 0 ~ 0,
      TRUE ~ (total_assets_22 - total_assets_20) / abs(total_assets_20)
    ),
    assets_liab_ratio_growth = case_when(
      assets_liab_ratio_22 == 0 & assets_liab_ratio_20 == 0 ~ 0,
      TRUE ~ (assets_liab_ratio_22 - assets_liab_ratio_20) / abs(assets_liab_ratio_20)
    )
  ) |>
  rename(
    name = name_22,
    ncesID = ncesID_22,
    state.name = state.name_22,
    state.abb = state.abb_22,
    name_state = name_state_22
  )


school_data_growth_json <- toJSON(school_data_growth, auto_unbox = TRUE, pretty = TRUE)
write(school_data_growth_json, file="output/school_data_growth.json")

school_data_growth <- school_data_growth |>
  mutate(
    total_liabilities_growth = total_liabilities_growth * 100,
    net_pension_liability_growth = net_pension_liability_growth * 100,
    expenses_growth = expenses_growth * 100
  )

write_csv(school_data_growth, "output/school_data_growth.csv")

school_data_json <- toJSON(school_data, auto_unbox = TRUE, pretty = TRUE)
write(school_data_json, file="output/school_data.json")




