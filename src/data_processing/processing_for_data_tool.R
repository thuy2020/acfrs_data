library(tidyverse)
library(scales)
library(jsonlite)
source("src/data_processing/census.R")

# read in data
state_data_temp <- read_csv("output/all_states_4years_2020_2023.csv")
county_data_temp <- read_csv("output/top100_counties.csv")
city_data_temp <- read_csv("output/top100_cities.csv")
school_data_temp <- read_csv("output/top100_sd.csv")


############################
####### State Data ########
############################

# clean up data
state_data <- state_data_temp |>
  #select(-1) |>
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

# Filter 2022 data 
state_data_22 <- state_data |>
  filter(year == 2022) |>
  rename_with(~ paste0(., "_22"))


state_data_20 <- state_data |>
  filter(year == 2020) |>
  rename_with(~ paste0(., "_20"))


state_data_growth <- state_data_22 |>
  left_join(state_data_20, by = c("name_22" = "name_20", "state.name_22" = "state.name_20")) |>
  mutate(
    total_liabilities_growth = total_liabilities_22 / total_liabilities_20 - 1,
    current_liabilities_growth = current_liabilities_22 / current_liabilities_20 - 1,
    expenses_growth = expenses_22 / expenses_20 - 1,
    net_opeb_liability_growth = net_opeb_liability_22 / net_opeb_liability_20 - 1,
    net_pension_liability_growth = net_pension_liability_22 / net_pension_liability_20 - 1,
    revenues_growth = revenues_22 / revenues_20 - 1,
    total_assets_growth = total_assets_22 / total_assets_20 - 1,
    assets_liab_ratio_growth = assets_liab_ratio_22 / assets_liab_ratio_20 - 1
  ) |>
  mutate(
    modified_revenues_20 = revenues_20 - (expenses_20 + current_liabilities_20),
    modified_revenues_22 = revenues_22 - (expenses_22 + current_liabilities_22),
  ) |>
  mutate(
    modified_revenues_growth = modified_revenues_22 / modified_revenues_20 - 1
  ) |>
  rename(
    name = name_22,
    geo_id = geo_id_22,
    state.name = state.name_22,
    state.abb = state.abb_22
  ) |>
  select(-geo_id_20) 


# join partisan lean
state_data_growth <- state_data_growth |>
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
county_data <- county_data_temp |>
  #select(-1) |>
  mutate(name = str_to_title(name)) |>
  mutate(name = paste(name, `state.abb`, sep = ", ")) |>
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

county_data_22 <- county_data |>
  filter(year == 2022) |>
  rename_with(~ paste0(., "_22"))

county_data_20 <- county_data |>
  filter(year == 2020) |>
  rename_with(~ paste0(., "_20"))


county_data_growth <- county_data_22 |>
  left_join(county_data_20, by = c("name_22" = "name_20", "state.name_22" = "state.name_20")) |>
  mutate(
    total_liabilities_growth = total_liabilities_22 / total_liabilities_20 - 1,
    current_liabilities_growth = current_liabilities_22 / current_liabilities_20 - 1,
    expenses_growth = expenses_22 / expenses_20 - 1,
    net_opeb_liability_growth = net_opeb_liability_22 / net_opeb_liability_20 - 1,
    net_pension_liability_growth = net_pension_liability_22 / net_pension_liability_20 - 1,
    revenues_growth = revenues_22 / revenues_20 - 1,
    total_assets_growth = total_assets_22 / total_assets_20 - 1,
    assets_liab_ratio_growth = assets_liab_ratio_22 / assets_liab_ratio_20 - 1
  ) |>
  mutate(
    modified_revenues_20 = revenues_20 - (expenses_20 + current_liabilities_20),
    modified_revenues_22 = revenues_22 - (expenses_22 + current_liabilities_22),
  ) |>
  mutate(
    modified_revenues_growth = modified_revenues_22 / modified_revenues_20 - 1
  ) |>
  rename(
    name = name_22,
    geo_id = geo_id_22,
    state.name = state.name_22,
    state.abb = state.abb_22
  ) |>
  select(-geo_id_20) |>
  mutate(pct_urban_pop_22_normalized = rescale(pct_urban_pop_22, to = c(-50, 50))) |>
  mutate(median_hh_income_normalized = rescale(median_hh_income_21_22, to = c(-50, 50)))


county_data_growth_json <- toJSON(county_data_growth, auto_unbox = TRUE, pretty = TRUE)
write(county_data_growth_json, file="output/county_data_growth.json")

county_data_json <- toJSON(county_data, auto_unbox = TRUE, pretty = TRUE)
write(county_data_json, file="output/county_data.json")


############################
######## City Data #########
############################

# clean up data
city_data <- city_data_temp |>
  #select(-1) |>
  mutate(name = str_to_title(name)) |>
  mutate(name_state = paste(name, `state.name`, sep = ", ")) |>
  mutate(name = paste(name, `state.abb`, sep = ", ")) |>
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


city_data_22 <- city_data |>
  filter(year == 2022) |>
  rename_with(~ paste0(., "_22"))

city_data_20 <- city_data |>
  filter(year == 2020) |>
  rename_with(~ paste0(., "_20"))


city_data_growth <- city_data_22 |>
  left_join(city_data_20, by = c("name_22" = "name_20", "state.name_22" = "state.name_20")) |>
  mutate(
    total_liabilities_growth = total_liabilities_22 / total_liabilities_20 - 1,
    current_liabilities_growth = current_liabilities_22 / current_liabilities_20 - 1,
    expenses_growth = expenses_22 / expenses_20 - 1,
    net_opeb_liability_growth = net_opeb_liability_22 / net_opeb_liability_20 - 1,
    net_pension_liability_growth = net_pension_liability_22 / net_pension_liability_20 - 1,
    revenues_growth = revenues_22 / revenues_20 - 1,
    total_assets_growth = total_assets_22 / total_assets_20 - 1,
    assets_liab_ratio_growth = assets_liab_ratio_22 / assets_liab_ratio_20 - 1
  ) |>
  mutate(
    modified_revenues_20 = revenues_20 - (expenses_20 + current_liabilities_20),
    modified_revenues_22 = revenues_22 - (expenses_22 + current_liabilities_22),
  ) |>
  mutate(
    modified_revenues_growth = modified_revenues_22 / modified_revenues_20 - 1
  ) |>
  rename(
    name = name_22,
    geo_id = geo_id_22,
    state.name = state.name_22,
    state.abb = state.abb_22,
    name_state = name_state_22
  ) |>
  select(-geo_id_20)

cityCoordinates <- readRDS("data/cityCoordinates.RDS")

cityCoordinates <- cityCoordinates[, (3:5)]

city_data_growth <- city_data_growth |>
  mutate(name_state = ifelse(name_state == "City And County Of San Francisco, California",
                             "San Francisco, California",
                             name_state)) |>
  left_join(cityCoordinates, by = c("name_state" = "name_state"))|>
  # mutate(pct_urban_pop_22_normalized = rescale(pct_urban_pop_22, to = c(-50, 50))) |>
  mutate(median_hh_income_normalized = rescale(median_hh_income_21_22, to = c(-50, 50)))


city_data_growth_json <- toJSON(city_data_growth, auto_unbox = TRUE, pretty = TRUE)
write(city_data_growth_json, file="output/city_data_growth.json")

city_data_json <- toJSON(city_data, auto_unbox = TRUE, pretty = TRUE)
write(city_data_json, file="output/city_data.json")


############################
####### School Data ########
############################

# clean up data
school_data <- school_data_temp |>
  #select(-1) |>
  mutate(name = str_to_title(name)) |>
  mutate(city = str_to_title(tolower(city))) |>
  mutate(name_state = paste(city, `state.abb`, sep = ", ")) |>
  mutate(
    net_pension_liab = net_pension_liability - net_pension_assets,
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

school_data_22 <- school_data |>
  filter(year == 2022) |>
  rename_with(~ paste0(., "_22"))

school_data_20 <- school_data |>
  filter(year == 2020) |>
  rename_with(~ paste0(., "_20"))


school_data_growth <- school_data_22 |>
  left_join(school_data_20, by = c("ncesID_22" = "ncesID_20")) |>
  mutate(
    total_liabilities_growth = total_liabilities_22 / total_liabilities_20 - 1,
    expenses_growth = expenses_22 / expenses_20 - 1,
    net_opeb_liability_growth = net_opeb_liability_22 / net_opeb_liability_20 - 1,
    net_pension_liability_growth = net_pension_liability_22 / net_pension_liability_20 - 1,
    revenues_growth = revenues_22 / revenues_20 - 1,
    total_assets_growth = total_assets_22 / total_assets_20 - 1,
    assets_liab_ratio_growth = assets_liab_ratio_22 / assets_liab_ratio_20 - 1
  ) |>
  rename(
    name = name_22,
    ncesID = ncesID_22,
    state.name = state.name_22,
    state.abb = state.abb_22,
    name_state = name_state_22
  )

#schoolCoordinates <- readRDS("schoolCoordinates.RDS")
#schoolCoordinates <- schoolCoordinates[, 3:5]
#school_data_growth <- school_data_growth |>
# left_join(schoolCoordinates, by = c("name_state" = "name_state"))

school_data_growth_json <- toJSON(school_data_growth, auto_unbox = TRUE, pretty = TRUE)
write(school_data_growth_json, file="output/school_data_growth.json")

school_data_json <- toJSON(school_data, auto_unbox = TRUE, pretty = TRUE)
write(school_data_json, file="output/school_data.json")
