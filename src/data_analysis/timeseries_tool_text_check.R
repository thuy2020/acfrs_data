library(tidyverse)
library(scales)
library(jsonlite)
# source("census.R")

####read in data####
state_data_temp <- read_csv("output/all_states_4years_2020_2023.csv") %>% 
  filter(year != 2023)
county_data_temp <- read_csv("output/top100_counties.csv") %>% 
  filter(year != 2023)
city_data_temp <- read_csv("output/top200_cities.csv") %>% 
  filter(year != 2023) |>
  filter(name != "denver county") |>
  group_by(year) %>%                      
  arrange(desc(population)) %>%           
  slice_head(n = 100) %>%                 
  ungroup()
school_data_temp <- read_csv("output/top100_sd.csv") %>% 
  filter(year != 2023) |>
  # top 100 by enrollment_20
  group_by(year) %>%
  arrange(desc(enrollment_20)) %>%
  slice_head(n = 100)

####Summary data####
#function to summary data each category
create_entity_summary <- function(data) {
  data %>% 
    filter(year != 2023) |>
    mutate(
      debt_ratio = total_liabilities / total_assets,
      net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
      net_net_pension_liability = net_pension_liability - net_pension_assets,
      free_cash_flow = revenues - (expenses + current_liabilities)
    ) |>
    group_by(year) |>
    summarise(
      current_liabilities_sum = sum(current_liabilities, na.rm = TRUE),
      net_net_opeb_liability_sum = sum(net_net_opeb_liability, na.rm = TRUE),
      net_net_pension_liability_sum = sum(net_net_pension_liability, na.rm = TRUE),
      expenses_sum = sum(expenses, na.rm = TRUE),
      total_liabilities_sum = sum(total_liabilities, na.rm = TRUE),
      free_cash_flow_sum = sum(free_cash_flow, na.rm = TRUE),
      revenues_sum = sum(revenues, na.rm = TRUE),
      total_assets_sum = sum(total_assets, na.rm = TRUE),
      debt_ratio = total_liabilities_sum / total_assets_sum,
      current_liabilities_pc = sum(current_liabilities, na.rm = TRUE) / sum(population[!is.na(current_liabilities)], na.rm = TRUE),
      net_net_opeb_liability_pc = sum(net_net_opeb_liability, na.rm = TRUE) / sum(population[!is.na(net_net_opeb_liability)], na.rm = TRUE),
      net_net_pension_liability_pc = sum(net_net_pension_liability, na.rm = TRUE) / sum(population[!is.na(net_net_pension_liability)], na.rm = TRUE),
      expenses_pc = sum(expenses, na.rm = TRUE) / sum(population[!is.na(expenses)], na.rm = TRUE),
      total_liabilities_pc = sum(total_liabilities_sum, na.rm = TRUE) / sum(population[!is.na(total_liabilities)], na.rm = TRUE),
      free_cash_flow_pc = sum(free_cash_flow_sum, na.rm = TRUE) / sum(population[!is.na(free_cash_flow)], na.rm = TRUE),
      revenues_pc = sum(revenues_sum, na.rm = TRUE) / sum(population[!is.na(revenues)], na.rm = TRUE),
      total_assets_pc = sum(total_assets_sum, na.rm = TRUE) / sum(population[!is.na(total_assets)], na.rm = TRUE)
    ) |>
    select(
      year,
      current_liabilities_sum,
      current_liabilities_pc,
      debt_ratio,
      net_net_opeb_liability_sum,
      net_net_opeb_liability_pc,
      net_net_pension_liability_sum,
      net_net_pension_liability_pc,
      expenses_sum,
      expenses_pc,
      total_liabilities_sum,
      total_liabilities_pc,
      free_cash_flow_sum,
      free_cash_flow_pc,
      revenues_sum,
      revenues_pc,
      total_assets_sum,
      total_assets_pc
    ) |>
    rename_with(~ str_replace(., "_sum", ""), ends_with("_sum"))}

#state summary#
state_data_summary <- create_entity_summary(state_data_temp)


write_csv(state_data_summary, "output/data_validation/state_data_summary.csv")

county_data_summary <- create_entity_summary(county_data_temp)

write_csv(county_data_temp, "output/data_validation/county_data_temp.csv")
write_csv(county_data_summary, "output/data_validation/county_data_summary.csv")

#city
city_data_summary <- create_entity_summary(city_data_temp)
write_csv(city_data_temp, "output/data_validation/city_data_temp.csv")
write_csv(city_data_summary, "output/data_validation/city_data_summary.csv")

#School Data Summary
school_data_summary <- school_data_temp |>
  filter(year != 2023) |>
  mutate(enrollment = case_when(
    year == 2020 ~ enrollment_20,
    year == 2021 ~ enrollment_21,
    year == 2022 ~ enrollment_22,
    TRUE ~ NA_real_  # Assign NA for any other years, if applicable
  )) |> 
  mutate(
    debt_ratio = total_liabilities / total_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
    net_net_pension_liability = net_pension_liability - net_pension_assets,
  ) |>
  group_by(year) |>
  summarise(
    net_net_pension_liability_sum = sum(net_net_pension_liability, na.rm = TRUE),
    net_net_opeb_liability_sum = sum(net_net_opeb_liability, na.rm = TRUE),
    expenses_sum = sum(expenses, na.rm = TRUE),
    total_liabilities_sum = sum(total_liabilities, na.rm = TRUE),
    revenues_sum = sum(revenues, na.rm = TRUE),
    total_assets_sum = sum(total_assets, na.rm = TRUE),
    debt_ratio = total_liabilities_sum / total_assets_sum,
    net_net_pension_liability_pc = net_net_pension_liability_sum / sum(enrollment[!is.na(net_net_pension_liability)], na.rm = TRUE),
    net_net_opeb_liability_pc = net_net_opeb_liability_sum / sum(enrollment[!is.na(net_net_opeb_liability)], na.rm = TRUE),
    expenses_pc = expenses_sum / sum(enrollment[!is.na(expenses)], na.rm = TRUE),
    total_liabilities_pc = total_liabilities_sum / sum(enrollment[!is.na(total_liabilities)], na.rm = TRUE),
    revenues_pc = revenues_sum / sum(enrollment[!is.na(revenues)], na.rm = TRUE),
    total_assets_pc = total_assets_sum / sum(enrollment[!is.na(total_assets)], na.rm = TRUE)
  ) |>
  select(
    year,
    debt_ratio,
    net_net_pension_liability_sum,
    net_net_pension_liability_pc,
    net_net_opeb_liability_sum,
    net_net_opeb_liability_pc,
    expenses_sum,
    expenses_pc,
    total_liabilities_sum,
    total_liabilities_pc,
    revenues_sum,
    revenues_pc,
    total_assets_sum,
    total_assets_pc
  ) |>
  rename_with(~ str_replace(., "_sum", ""), ends_with("_sum"))


#####

write_csv(school_data_temp, "output/data_validation/school_data_temp.csv")
write_csv(school_data_summary, "output/data_validation/school_data_summary.csv")

#####Top 10 in each category#####

# Get the top 10 for total_liabilities
state_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_liabilities)) |>
  head(10) |>
  select(state.name, total_liabilities)

# bottom 10 for total_liabilities
state_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_liabilities)) |>
  tail(10) |>
  select(state.name, total_liabilities)

# top 10 total liabilities per cap
state_data_temp  %>% 
  filter(year == 2022)  %>% 
  mutate(total_liabilities_pc = total_liabilities/population) %>% 
  arrange(desc(total_liabilities_pc)) %>% 
  select(state.name, total_liabilities_pc) %>% 
  head(10)

# bottom 10 total liabilities per cap
state_data_temp  %>% 
  filter(year == 2022)  %>% 
  mutate(total_liabilities_pc = total_liabilities/population) %>% 
  arrange(desc(total_liabilities_pc)) %>% 
  select(state.name, total_liabilities_pc) %>% 
  tail(10)
county_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_liabilities)) |>
  head(10) |>
  select(name, state.name, total_liabilities)


city_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_liabilities)) |>
  head(10) |>
  select(name, state.name, total_liabilities)

school_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_liabilities)) |>
  head(10) |>
  select(name, state.name, total_liabilities)


# Get the top 10 for total_assets
state_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_assets)) |>
  head(10) |>
  select(state.name, total_assets)

county_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_assets)) |>
  head(10) |>
  select(name, state.name, total_assets)

city_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_assets)) |>
  head(10) |>
  select(name, state.name, total_assets)

school_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_assets)) |>
  head(10) |>
  select(name, state.name, total_assets)


# Get the top 10 for debt_ratio
state_data_temp |>
  filter(year == 2022) |>
  mutate(debt_ratio = total_liabilities / total_assets) |>
  arrange(desc(debt_ratio)) |>
  head(10) |>
  select(state.name, debt_ratio)

#top 10 county debt ratios
county_data_temp |>
  filter(year == 2022) |>
  mutate(debt_ratio = total_liabilities / total_assets) |>
  arrange(desc(debt_ratio)) |>
  head(10) |>
  select(name, state.name, debt_ratio)

city_data_temp |>
  filter(year == 2022) |>
  mutate(debt_ratio = total_liabilities / total_assets) |>
  arrange(desc(debt_ratio)) |>
  head(10) |>
  select(name, state.name, debt_ratio)

school_data_temp |>
  filter(year == 2022) |>
  mutate(debt_ratio = total_liabilities / total_assets) |>
  # filter out Inf values
  filter(!is.infinite(debt_ratio)) |>
  arrange(desc(debt_ratio)) |>
  head(10) |>
  select(name, state.name, debt_ratio)


# Get the top 10 for free_cash_flow
state_data_temp |>
  filter(year == 2022) |>
  mutate(free_cash_flow = revenues - (expenses + current_liabilities)) |>
  arrange(desc(free_cash_flow)) |>
  head(10) |>
  select(state.name, free_cash_flow)

county_data_temp |>
  filter(year == 2022) |>
  mutate(free_cash_flow = revenues - (expenses + current_liabilities)) |>
  arrange(desc(free_cash_flow)) |>
  head(10) |>
  select(name, state.name, free_cash_flow)

city_data_temp |>
  filter(year == 2022) |>
  mutate(free_cash_flow = revenues - (expenses + current_liabilities)) |>
  arrange(desc(free_cash_flow)) |>
  head(10) |>
  select(name, state.name, free_cash_flow)



# Get the top 10 for net_net_opeb_liability

state_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets) |>
  arrange(desc(net_net_opeb_liability)) |>
  head(10) |>
  select(state.name, net_net_opeb_liability)

county_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets) |>
  arrange(desc(net_net_opeb_liability)) |>
  head(10) |>
  select(name, state.name, net_net_opeb_liability)

city_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets) |>
  arrange(desc(net_net_opeb_liability)) |>
  head(10) |>
  select(name, state.name, net_net_opeb_liability)

school_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets) |>
  arrange(desc(net_net_opeb_liability)) |>
  head(10) |>
  select(name, state.name, net_net_opeb_liability)



# Get the top 10 for net_net_pension_liability
state_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  arrange(desc(net_net_pension_liability)) |>
  head(10) |>
  select(state.name, net_net_pension_liability)


county_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  arrange(desc(net_net_pension_liability)) |>
  head(10) |>
  select(name, state.name, net_net_pension_liability)


city_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  arrange(desc(net_net_pension_liability)) |>
  head(10) |>
  select(name, state.name, net_net_pension_liability)


school_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  arrange(desc(net_net_pension_liability)) |>
  head(10) |>
  select(name, state.name, net_net_pension_liability)



####State Page####

# From FY 2020 through 2022, 47 states saw increases in revenues. 
# Alaska, Michigan, and Wyoming were the three states that did not
# increase revenues.
state_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  select(state.name, year, revenues) |>
  pivot_wider(names_from = "year", values_from = "revenues")|>
  mutate(revenue_change = `2022` - `2020`) |>
  arrange(desc(revenue_change)) |>
  select(state.name, `2020`, `2022`, revenue_change) |>
  filter(revenue_change < 0)


# During the same time period, total assets, such as growth in cash, investments,
# receivables, land, buildings, and infrastructure, increased for all 50 states.
state_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  select(state.name, year, total_assets) |>
  pivot_wider(names_from = "year", values_from = "total_assets")|>
  mutate(asset_change = `2022` - `2020`) |>
  arrange(desc(asset_change)) |>
  select(state.name, `2020`, `2022`, asset_change) |>
  filter(asset_change < 0)

# The increase in assets helped 49 states, with the exception of North Dakota, 
# reduce its state debt ratio, defined as the proportion of total liabilities to
# total assets from FY 2020 to FY 2022.
state_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  select(state.name, year, total_liabilities, total_assets) |>
  pivot_wider(names_from = "year", values_from = c("total_liabilities", "total_assets")) |>
  mutate(debt_ratio_2020 = total_liabilities_2020 / total_assets_2020,
         debt_ratio_2022 = total_liabilities_2022 / total_assets_2022,
         debt_ratio_change = debt_ratio_2020 - debt_ratio_2022) |>
  arrange(debt_ratio_change) |>
  select(state.name, debt_ratio_2020, debt_ratio_2022, debt_ratio_change) |>
  filter(debt_ratio_change < 0)


# From 2020 to 2022, 42 states improved their free cash flows, which is 
# calculated subtracting expenses and current liabilities from revenues.
state_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  select(state.name, year, revenues, expenses, current_liabilities) |>
  pivot_wider(names_from = "year", values_from = c("revenues", "expenses", "current_liabilities")) |>
  mutate(free_cash_flow_2020 = revenues_2020 - (expenses_2020 + current_liabilities_2020),
         free_cash_flow_2022 = revenues_2022 - (expenses_2022 + current_liabilities_2022),
         free_cash_flow_change = free_cash_flow_2022 - free_cash_flow_2020) |>
  arrange(desc(free_cash_flow_change)) |>
  select(state.name, free_cash_flow_2020, free_cash_flow_2022, free_cash_flow_change) |>
  filter(free_cash_flow_change > 0) 


# At the end of the 2022 fiscal year, the 50 states held an aggregate of 
# $1.03 trillion in employee-related debt, including $502 billion in net public 
# pension liabilities and $524 billion in net other post-employment benefit 
# liabilities, such as promised medical benefits for retirees.
state_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  summarise(
    total_net_pension_liabilities = sum(net_net_pension_liability),
    total_net_opeb_liabilities = sum(net_net_opeb_liability),
    total_net_liabilities = sum(total_net_liabilities)
  )


# As of 2022, the states with the most total liabilities are California, 
# Illinois, New York, New Jersey, and Texas. The states with the least total 
# liabilities are South Dakota, Idaho, Nebraska, Montana, and New Hampshire.
state_data_temp |>
  filter(year == 2022) |>
  arrange(desc(total_liabilities)) |>
  select(state.name, total_liabilities) |>
  head(5)

state_data_temp |>
  filter(year == 2022) |>
  arrange(total_liabilities) |>
  select(state.name, total_liabilities) |>
  head(5)


# On a per capita basis, Connecticut, New Jersey, Hawaii, Illinois, and Wyoming,
# had the most total liabilities at the end of 2022. Tennessee, Utah, Oklahoma, 
# Nebraska, and Idaho, had the lowest liabilities on a per capita basis.
state_data_temp |>
  filter(year == 2022) |>
  mutate(total_liabilities_pc = total_liabilities / population) |>
  arrange(desc(total_liabilities_pc)) |>
  select(state.name, total_liabilities_pc) |>
  head(5)

state_data_temp |>
  filter(year == 2022) |>
  mutate(total_liabilities_pc = total_liabilities / population) |>
  arrange(total_liabilities_pc) |>
  select(state.name, total_liabilities_pc) |>
  head(5)


# Ten states—Illinois, New Jersey, California, Texas, New York, Connecticut, 
# Massachusetts, Pennsylvania, Kentucky, and Maryland—account for 84.7% of the 
# total employee-related debt among all U.S. states.
net_liab_state <- state_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  summarise(
    total_net_pension_liabilities = sum(net_net_pension_liability),
    total_net_opeb_liabilities = sum(net_net_opeb_liability),
    total_net_liabilities = sum(total_net_liabilities)
  ) |>
  mutate(
    total_net_pension_liabilities_pc = total_net_pension_liabilities / sum(total_net_liabilities),
    total_net_opeb_liabilities_pc = total_net_opeb_liabilities / sum(total_net_liabilities)
  )

net_liab_state_10 <- state_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  arrange(desc(total_net_liabilities)) |>
  select(state.name, total_net_liabilities) |>
  head(10) |>
  summarise(
    total_net_liabilities = sum(total_net_liabilities)
  )

net_liab_state_10/net_liab_state[3]


####County Page####

# Of the 100 most populous counties in the United States, all counties increased 
# their total assets betwee 2020 and 2022, the city and county of San Francisco 
# and Denver County, CO did so dramatically. 

county_takeaway_1 <- county_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  mutate(name = paste0(name,  ", ",state.name)) |>
  select(name, year, total_assets, population) |>
  mutate(total_assets_pc = total_assets / population) |>
  select(-total_assets, -population) |>
  pivot_wider(names_from = "year", values_from = "total_assets_pc") |>
  mutate(asset_change = `2022` - `2020`) |>
  arrange(desc(asset_change)) |>
  select(name, `2020`, `2022`, asset_change)


# Cook County, Illinois, and Essex County, New Jersey, had the highest debt 
# ratios, defined as the proportion of a government's total assets that are 
# financed by outstanding debt. Cook and Essex were the only counties with debt 
# ratios above 250% in 2022.

county_takeaway_2 <- county_data_temp |>
  filter(year == 2022) |>
  mutate(debt_ratio = total_liabilities / total_assets) |>
  arrange(desc(debt_ratio)) |>
  select(name, state.name, debt_ratio)


# While the City and County of San Francisco saw the largest increase in total 
# assets, it had the lowest free cash flow per capita at -$3,971 in 2022. Free 
# cash flow is revenues less expenses and current liabilities. The only other 
# county with a free cash flow lower than -$1,000 per capita in 2022 was 
# Philadelphia (-$1,289).

county_takeaway_3 <- county_data_temp |>
  filter(year == 2022) |>
  mutate(free_cash_flow = revenues - (expenses + current_liabilities),
         free_cash_flow_pc = free_cash_flow / population) |>
  arrange(free_cash_flow_pc) |> 
  select(name, state.name, free_cash_flow_pc)


# At the end of 2022, the governments representing the 100 most populous counties across America 
# owed $448.6 billion in total debt, including an aggregate of $151.2 billion in employee-related 
# benefits debt—$71.3 billion as net public pension liabilities and $79.9 as net other 
# post-employment benefit liabilities, such as medical benefits promised to retirees.

county_takeaway_4_0 <- county_data_temp |>
  filter(year == 2022) |>
  summarise(tot = sum(total_liabilities)/1e8)

county_takeaway_4 <- county_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(name, state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  summarise(
    total_net_pension_liabilities = sum(net_net_pension_liability, na.rm=T),
    total_net_opeb_liabilities = sum(net_net_opeb_liability, na.rm=T),
    total_net_liabilities = sum(total_net_liabilities, na.rm=T)
  )


# The 10 most indebted counties, in terms of total liabilities, were responsible
# for more than half (56.5%) of the total liabilities held by the 100 most populous
# counties.

county_takeaway_5 <- county_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(name, state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  arrange(desc(total_net_liabilities)) |>
  select(name, total_net_liabilities) |>
  head(10) |>
  summarise(
    total_liabilities = sum(total_net_liabilities)
  )

county_takeaway_5/county_takeaway_4[3]



# On a per capita basis, the City and County of San Francisco, Denver County, CO,
# and the city and county of Philadelphia, PA had the most total liabilities at
# the end of 2022. And Middlesex County, NJ, Monmouth County, NJ, Bergen County, NJ,
# Hudson County, NJ, and Tulsa County, OK had the least debt.

county_takeaway_6a <- county_data_temp |>
  filter(year == 2022) |>
  mutate(total_liabilities_pc = total_liabilities / population) |>
  arrange(desc(total_liabilities_pc)) |>
  select(name, state.name, total_liabilities_pc) |>
  head(5)

county_takeaway_6b <- county_data_temp |>
  filter(year == 2022) |>
  mutate(total_liabilities_pc = total_liabilities / population) |>
  arrange(total_liabilities_pc) |>
  select(name, state.name, total_liabilities_pc) |>
  head(5)




####City Page####

# In FY 2022, all but one (Long Beach, CA) of the top 100 most populous municipalities 
# increased total assets in 2022.

city_takeaway_1 <- city_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  mutate(name = paste0(name,  ", ",state.name)) |>
  select(name, year, total_assets, population) |> 
  mutate(total_assets_pc = total_assets / population) |>
  select(-total_assets, -population) |>
  pivot_wider(names_from = "year", values_from = "total_assets_pc") |>
  mutate(asset_change = `2022` - `2020`) |>
  arrange(desc(asset_change)) |>
  select(name, `2020`, `2022`, asset_change)



# Most municipal governments’ debt ratios, defined as the proportion of total 
# liabilities to total assets, decreased from 2020 to 2022. Twelve municipalities 
# had modest increases in their debt ratios during that span: 
#Santa Ana, CA, Boise, ID, Saint Paul, MN, Minneapolis, MN, 
#Long Beach, CA, Tampa, FL, Fayetteville, NC, Irving, TX, Garland, TX, 
#Aurora, CO, and Kansas City, MO, and Glendale, AZ.


city_takeaway_2 <- city_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  mutate(name = paste0(name,  ", ",state.name)) |>
  mutate(debt_ratio = total_liabilities / total_assets) |>
  select(name, year, debt_ratio) |>
  pivot_wider(names_from = "year", values_from = "debt_ratio") |>
  mutate(debt_ratio_change = `2022` - `2020`) |>
  arrange(debt_ratio_change) |>
  select(name, `2020`, `2022`, debt_ratio_change) 


# A positive free cash flow indicates the municipal government has the capacity 
# to improve its net financial position and maintain fiscal health without relying
# on debt rollovers. Free cash flow was more mixed for municipalities. The 
# largest decline was in Long Beach, where free cash flow went from -$523 to 
# -$3,362. The largest increase was the City and County of San Francisco which 
# went from -$5,851 to -$3,971.

city_takeaway_3 <- city_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  mutate(name = paste0(name,  ", ",state.name)) |>
  mutate(free_cash_flow = revenues - (expenses + current_liabilities),
         free_cash_flow_pc = free_cash_flow / population) |>
  select(name, year, free_cash_flow_pc) |>
  pivot_wider(names_from = "year", values_from = "free_cash_flow_pc") |>
  mutate(free_cash_flow_change = `2022` - `2020`) |>
  arrange(free_cash_flow_change) |>
  select(name, `2020`, `2022`, free_cash_flow_change)



# On a per capita basis, the City and County of San Francisco, CA, New York, NY,
# the District of Columbia, Chicago, IL, and Atlanta, GA, had the most total 
# liabilities at the end of 2022. Irvine, CA, Santa Clarita, CA, Jersey City, NJ,
# Moreno Valley, CA, and Bakersfield, CA had the fewest total liabilities.

city_takeaway_4a <- city_data_temp |>
  filter(year == 2022) |>
  mutate(total_liabilities_pc = total_liabilities / population) |>
  arrange(desc(total_liabilities_pc)) |>
  select(name, state.name, total_liabilities_pc) |>
  head(5)

city_takeaway_4b <- city_data_temp |>
  filter(year == 2022) |>
  mutate(total_liabilities_pc = total_liabilities / population) |>
  arrange(total_liabilities_pc) |>
  select(name, state.name, total_liabilities_pc) |>
  head(5)


# In 2022, governments representing the 100 most populous municipalities across 
# America owed $266.9 billion in employee-related debt, including $136.8 billion
# as net public pension liabilities and $130.10 as net other post-employment benefit
# liabilities, such as medical benefits promised to retirees.

city_takeaway_5 <- city_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(name, state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  summarise(
    total_net_pension_liabilities = sum(net_net_pension_liability, na.rm=T),
    total_net_opeb_liabilities = sum(net_net_opeb_liability, na.rm=T),
    total_net_liabilities = sum(total_net_liabilities, na.rm=T)
  )

city_takeaway_5[3]/1e9
city_takeaway_5[2]/1e9
city_takeaway_5[1]/1e9


# Ten municipalities—New York, Chicago, Philadelphia, Austin, Phoenix, Houston, 
# Portland, Dallas, Boston, and Jacksonville—account for 77.7% of the total 
# employee-related debt among the 100 most populous municipalities.

city_takeaway_6 <- city_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(name, state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  arrange(desc(total_net_liabilities)) |>
  select(name, total_net_liabilities) |>
  head(10) |>
  summarise(
    total_net_liabilities = sum(total_net_liabilities)
  )

city_takeaway_6/city_takeaway_5[3]


# In 2022, the cities of New York and Chicago were responsible for 63.4% of the 
# total employee debt held by the country’s 100 largest municipalities.

city_takeaway_7 <- city_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(name, state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  arrange(desc(total_net_liabilities)) |>
  select(name, total_net_liabilities) |>
  head(2) |>
  summarise(
    total_net_liabilities = sum(total_net_liabilities)
  )


city_takeaway_7/city_takeaway_5[3] * 100


####School Page####

# Most school districts increased their total assets, such as Los Angeles Unified
# School District, CA, San Diego Unified School District, CA, and Dallas 
# Independent School District, TX from FY 2020 to 2022. 

# raw change
school_data_temp %>% select(name, total_assets) %>% 
  filter(year %in% c(2020, 2022)) %>% 
  pivot_wider(names_from = year, values_from = total_assets) %>% 
  mutate(increase = `2022` - `2020`) #%>% View()

# per capita change
school_takeaway_1 <- school_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  mutate(enrollment = case_when(
    year == 2020 ~ enrollment_20,
    year == 2021 ~ enrollment_21,
    year == 2022 ~ enrollment_22,
    TRUE ~ NA_real_  # Assign NA for any other years, if applicable
  )) |>
  select(name, year, total_assets, enrollment) |>
  mutate(total_assets_pc = total_assets / enrollment) |>
  select(-total_assets, -enrollment) |>
  pivot_wider(names_from = "year", values_from = "total_assets_pc") |>
  mutate(asset_change = `2022` - `2020`) |>
  arrange(desc(asset_change)) |>
  select(name, `2020`, `2022`, asset_change)



# From 2020 to 2022, only 3 school districts reported an increase in debt 
# ratio, the ratio of total liabilities to total assets, over the period: 
# Howard County Board of Education in Maryland, Knox County Schools in Kentucky, 
# Board of Education of Anne Arundel County in Maryland, and Shelby County Board 
# of Education in Tennessee.

school_takeaway_2 <- school_data_temp |>
  filter(year %in% c(2020, 2022)) |>
  mutate(enrollment = case_when(
    year == 2020 ~ enrollment_20,
    year == 2021 ~ enrollment_21,
    year == 2022 ~ enrollment_22,
    TRUE ~ NA_real_  # Assign NA for any other years, if applicable
  )) |>
  mutate(debt_ratio = total_liabilities / total_assets) |>
  select(name, year, debt_ratio) |>
  pivot_wider(names_from = "year", values_from = "debt_ratio") |>
  mutate(debt_ratio_change = `2022` - `2020`) |>
  arrange(debt_ratio_change) |>
  select(name, `2020`, `2022`, debt_ratio_change)


# At the end of 2022, the Chicago Board of Education, IL, Los Angeles Unified School
# District, CA, and School District of Philadelphia, PA had the most total liabilities.

school_takeaway_3 <- school_data_temp |>
  filter(year == 2022) |>
  select(name, state.name, total_liabilities) |>
  arrange(desc(total_liabilities)) |>
  select(name, total_liabilities) |>
  head(3)


# On a per student basis, the Chicago Board of Education, IL, San Diego Unified School
# District, CA, and School District of Philadelphia, PA, had the most total liabilities at
# the end of 2022.

school_takeaway_4 <- school_data_temp |>
  filter(year == 2022) |>
  mutate(enrollment = case_when(
    year == 2020 ~ enrollment_20,
    year == 2021 ~ enrollment_21,
    year == 2022 ~ enrollment_22,
    TRUE ~ NA_real_  # Assign NA for any other years, if applicable
  )) |>
  mutate(total_liabilities_pc = total_liabilities / enrollment) |>
  arrange(desc(total_liabilities_pc)) |>
  select(name, state.name, total_liabilities_pc) |>
  head(3)


# In 2022, the 100 school districts with the most students owed $111.6 billion in
# employee-related debt, including $53.7 billion as net public pension liabilities and $57.9
# as net other post-employment benefit liabilities.

school_takeaway_5 <- school_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(name, state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  summarise(
    total_net_pension_liabilities = sum(net_net_pension_liability, na.rm=T),
    total_net_opeb_liabilities = sum(net_net_opeb_liability, na.rm=T),
    total_net_liabilities = sum(total_net_liabilities, na.rm=T)
  )

school_takeaway_5[4]/1e9
school_takeaway_5[3]/1e9
school_takeaway_5[2]/1e9


# Chicago and Los Angeles’s school districts accounted for 29.2% of the employee-related
# debt held by the 100 school districts with the greatest enrollment in 2022. 

school_takeaway_6 <- school_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(name, state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  arrange(desc(total_net_liabilities)) |>
  select(name, total_net_liabilities) |>
  head(2) |>
  summarise(
    total_net_liabilities = sum(total_net_liabilities)
  )

school_takeaway_6[2]/school_takeaway_5[4]



# They account for 7.9% percent of the students in the top 100 school districts.
school_takeaway_6b <- school_data_temp |>
  filter(year == 2022) |>
  summarise(
    total_students = sum(enrollment_22)
  )


school_takeaway_6c <- school_data_temp |>
  filter(year == 2022) |>
  mutate(enrollment = case_when(
    year == 2020 ~ enrollment_20,
    year == 2021 ~ enrollment_21,
    year == 2022 ~ enrollment_22,
    TRUE ~ NA_real_  # Assign NA for any other years, if applicable
  )) |>
  select(name, enrollment) |>
  head(2) |>
  summarise(
    total_students = sum(enrollment)
  )

school_takeaway_6c[2] / school_takeaway_6b[2]


# Ten school districts account for 49.6% of the total employee-related debt among the 100
# most populous municipalities.

school_takeaway_7 <- school_data_temp |>
  filter(year == 2022) |>
  mutate(net_net_opeb_liability = net_opeb_liability - net_opeb_assets,
         net_net_pension_liability = net_pension_liability - net_pension_assets) |>
  select(name, state.name, net_net_pension_liability, net_net_opeb_liability) |>
  mutate(total_net_liabilities = net_net_pension_liability + net_net_opeb_liability) |>
  arrange(desc(total_net_liabilities)) |>
  select(name, total_net_liabilities) |>
  head(10) |>
  summarise(
    total_net_liabilities = sum(total_net_liabilities)
  )

school_takeaway_7[2]/school_takeaway_5[4]
