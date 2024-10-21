# Load libraries
library(tidyverse)
library(ggplot2)
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

