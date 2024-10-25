# Load libraries
library(tidyverse)
library(writexl)

# Read the data
states <- read_csv("output/all_states_4years_2020_2023.csv")
counties <- read_csv("output/top100_counties.csv")
cities <- read_csv("output/top200_cities.csv") %>% 
  filter(year != 2023) |>
  filter(name != "denver county") |>
  group_by(year) %>%                      
  arrange(desc(population)) %>%           
  slice_head(n = 100) %>%                 
  ungroup()
schools <- read_csv("output/top100_sd.csv") %>% 
  filter(year != 2023) |>
  # top 100 by enrollment_20
  group_by(year) %>%
  arrange(desc(enrollment_20)) %>%
  slice_head(n = 100)


# Modify the states data
states_mod <- states |>
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
  select(-`...1`) |>
  filter(year == 2022)

# Modify the counties data
counties_mod <- counties |>
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
  arrange(year, name) |>
  select(-`...1`) |>
  filter(year == 2022)

# Modify the cities data
cities_mod <- cities |>
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
  arrange(year, name) |>
  select(-`...1`) |>
  filter(year == 2022)

# Modify the schools data
schools_mod <- schools |>
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
  arrange(year, name) |>
  select(-`...1`) |>
  filter(year == 2022)

################################################################################
#################################### STATES ####################################
################################################################################

# Find the top 10 states by net pension liability
top_10_states <- states_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(desc(total_pension_liability)) |>
  top_n(10, total_pension_liability) |>
  select(state.name, total_pension_liability, total_pension_liability_pc) |>
  arrange(-total_pension_liability)

# Find the top 10 states by net pension liability per capita
top_10_states_pc <- states_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(desc(total_pension_liability_pc)) |>
  top_n(10, total_pension_liability_pc) |>
  select(state.name, total_pension_liability_pc, total_pension_liability) |>
  arrange(-total_pension_liability_pc)

# Find the top 10 states by least net pension liability
top_10_states_least_pension_debt <- states_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(total_pension_liability) |>
  top_n(10, -total_pension_liability) |>
  select(state.name, total_pension_liability, total_pension_liability_pc)

################################################################################
#################################### CITIES ####################################
################################################################################

# Find the top 10 cities by net pension liability
top_10_cities <- cities_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(desc(total_pension_liability)) |>
  top_n(10, total_pension_liability) |>
  select(name, state.name, total_pension_liability, total_pension_liability_pc) |>
  arrange(-total_pension_liability)

# Find the top 10 cities by net pension liability per capita
top_10_cities_pc <- cities_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(desc(total_pension_liability_pc)) |>
  top_n(10, total_pension_liability_pc) |>
  select(name, state.name, total_pension_liability_pc, total_pension_liability) |>
  arrange(-total_pension_liability_pc)

# Find the top 10 cities by least net pension liability
top_10_cities_least_pension_debt <- cities_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(total_pension_liability) |>
  top_n(10, -total_pension_liability) |>
  select(name, state.name, total_pension_liability, total_pension_liability_pc)

################################################################################
################################## COUNTIES ####################################
################################################################################

# Find the top 10 counties by net pension liability
top_10_counties <- counties_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(desc(total_pension_liability)) |>
  top_n(10, total_pension_liability) |>
  select(name, state.name, total_pension_liability, total_pension_liability_pc) |>
  arrange(-total_pension_liability)

# Find the top 10 counties by net pension liability per capita
top_10_counties_pc <- counties_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(desc(total_pension_liability_pc)) |>
  top_n(10, total_pension_liability_pc) |>
  select(name, state.name, total_pension_liability_pc, total_pension_liability) |>
  arrange(-total_pension_liability_pc)

# Find the top 10 counties by least net pension liability
top_10_counties_least_pension_debt <- counties_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / population) |>
  arrange(total_pension_liability) |>
  top_n(10, -total_pension_liability) |>
  select(name, state.name, total_pension_liability, total_pension_liability_pc)

################################################################################
############################### SCHOOL DISTRICTS ###############################
################################################################################

# Find the top 10 schools by net pension liability
top_10_schools <- schools_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / enrollment_22) |>
  arrange(desc(total_pension_liability)) |>
  top_n(10, total_pension_liability) |>
  select(name, state.name, total_pension_liability, total_pension_liability_pc) |>
  arrange(-total_pension_liability)

# Find the top 10 schools by net pension liability per capita
top_10_schools_pc <- schools_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / enrollment_22) |>
  arrange(desc(total_pension_liability_pc)) |>
  top_n(10, total_pension_liability_pc) |>
  select(name, state.name, total_pension_liability_pc, total_pension_liability) |>
  arrange(-total_pension_liability_pc)

# Find the top 10 schools by least net pension liability
top_10_schools_least_pension_debt <- schools_mod |>
  mutate(total_pension_liability = net_pension_liability + net_opeb_liability) |>
  mutate(total_pension_liability_pc = total_pension_liability / enrollment_22) |>
  arrange(total_pension_liability) |>
  top_n(10, -total_pension_liability) |>
  select(name, state.name, total_pension_liability, total_pension_liability_pc)

# Write all the data frames to an Excel file
write_xlsx(list(
  "Top 10 States" = top_10_states,
  "Top 10 States PC" = top_10_states_pc,
  "Top 10 States Least Debt" = top_10_states_least_pension_debt,
  "Top 10 Cities" = top_10_cities,
  "Top 10 Cities PC" = top_10_cities_pc,
  "Top 10 Cities Least Debt" = top_10_cities_least_pension_debt,
  "Top 10 Counties" = top_10_counties,
  "Top 10 Counties PC" = top_10_counties_pc,
  "Top 10 Counties Least Debt" = top_10_counties_least_pension_debt,
  "Top 10 Schools" = top_10_schools,
  "Top 10 Schools PC" = top_10_schools_pc,
  "Top 10 Schools Least Debt" = top_10_schools_least_pension_debt
), "output/commentary_muni_debt_ranking.xlsx")
