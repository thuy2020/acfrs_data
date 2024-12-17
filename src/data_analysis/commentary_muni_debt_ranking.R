# Load libraries
library(tidyverse)
library(writexl)

#NOTE: This is the script for Mariana's commentary: 
#https://app.asana.com/0/1202764950551818/1206186736602232

#This file consolidated file "commentary_opeb_debt.R"

# Read the data
states <- read_csv("output/all_states_4years_2020_2023.csv") %>% 
  filter(year != 2023)
counties <- read_csv("output/top100_counties.csv") %>% 
  filter(year != 2023)
cities <- read_csv("output/top100_cities.csv") %>% 
  filter(year != 2023)

schools <- read_csv("output/top100_sd.csv") %>% 
  filter(year != 2023) %>%
  # top 100 by enrollment_20
  arrange(desc(enrollment_20)) %>%
  slice_head(n = 100)


# Modify the states data
modify_dataset <- function(data, group_col) {
  data |>
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
    arrange(year, {{ group_col }}) |>
    select(-`...1`) |>
    filter(year == 2022)
}

states_mod <- modify_dataset(states, state.name)
counties_mod <- modify_dataset(counties, name)
cities_mod <- modify_dataset(cities, name)
schools_mod <- modify_dataset(schools, name)

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



################################################################################
###################################### Figure 1 ################################
################################################################################

# Calculate the total pension and OPEB liabilities for each entity type,
#  then sum the entity types

# Summarize the data for each entity type
states_pension_opeb <- states_mod |>
  group_by(year) |>
  summarise(
    net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
    net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE)
  ) |>
  mutate(entity_type = "States")

counties_pension_opeb <- counties_mod |>
  group_by(year) |>
  summarise(
    net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
    net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE)
  ) |>
  mutate(entity_type = "Counties")

cities_pension_opeb <- cities_mod |>
  group_by(year) |>
  summarise(
    net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
    net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE)
  ) |>
  mutate(entity_type = "Cities")

schools_pension_opeb <- schools_mod |>
  group_by(year) |>
  summarise(
    net_pension_liability = sum(net_pension_liability, na.rm = TRUE),
    net_opeb_liability = sum(net_opeb_liability, na.rm = TRUE)
  ) |>
  mutate(entity_type = "Schools")

# Combine the data for all entity types
combined_data <- bind_rows(states_pension_opeb, counties_pension_opeb, cities_pension_opeb, schools_pension_opeb)

# Add a row for the total sum
total_pension_opeb <- combined_data |>
  summarise(
    net_pension_liability = sum(net_pension_liability),
    net_opeb_liability = sum(net_opeb_liability)
  ) |>
  mutate(entity_type = "Total")

# Combine all data into one final dataframe
final_data <- bind_rows(combined_data, total_pension_opeb)

# Modify the final_data to include a 'liability_type' column for ggplot
final_data_long <- final_data %>%
  pivot_longer(cols = c("net_pension_liability", "net_opeb_liability"), 
               names_to = "liability_type", 
               values_to = "liability_value") 

# Ensure entity_type is treated as a factor for proper dodging
final_data_long$entity_type <- as.factor(final_data_long$entity_type)

# Create the bar chart using ggplot2 with explicit dodge position adjustment
ggplot(final_data_long, aes(x = entity_type, y = liability_value, fill = liability_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    title = "Total Pension and OPEB Liabilities by Entity Type",
    x = "Entity Type",
    y = "Liability (in billions)",
    fill = "Liability Type"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("net_pension_liability" = "#ff6633", "net_opeb_liability" = "#2879cb"))


