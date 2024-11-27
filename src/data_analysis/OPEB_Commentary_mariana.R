library(tidyverse)
options(scipen = 99999)

State <- read_csv("output/all_states_4years_2020_2023.csv") %>%
  mutate(
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets, 
    non_current_liabilities = total_liabilities - current_liabilities) %>% 
    mutate(Entity_Type = "State")

County <- read_csv("output/top100_counties.csv") %>%
  mutate(
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets, 
    non_current_liabilities = total_liabilities - current_liabilities) %>% 
  mutate(Entity_Type = "County")

City <- read_csv("output/top100_cities.csv") %>%
  mutate(
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets, 
    non_current_liabilities = total_liabilities - current_liabilities) %>% 
  mutate(Entity_Type = "City")

SD <- read_csv("output/top100_sd.csv") %>%
  mutate(
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets, 
    non_current_liabilities = total_liabilities - current_liabilities) %>% 
  mutate(Entity_Type = "School District")


# Figure 1: Total NPL v OPEB
#In2022, net OPEB liabilities for the largest governments reached $786billion, 
#surpassing $756billion in unfunded pension liabilities.”

by_year_data <- bind_rows(
  State %>% select(year, net_net_pension_liability, net_net_opeb_liability),
  County %>% select(year, net_net_pension_liability, net_net_opeb_liability),
  City %>% select(year, net_net_pension_liability, net_net_opeb_liability),
  SD %>% select(year, net_net_pension_liability, net_net_opeb_liability)
)

# Group by year and sum the liabilities
summary_by_year_data <- by_year_data %>%
  group_by(year) %>%
  summarise(
    total_net_net_pension_liability = sum(net_net_pension_liability, na.rm = TRUE),
    total_net_net_opeb_liability = sum(net_net_opeb_liability, na.rm = TRUE)
  ) %>% 
  filter(year != 2023)

#Summed together, unfunded public employee pensions plus the healthcare benefits of
#America’s largest municipal governments totaled $1.5 trillion, equivalent to around
#4.5 % of thenational federal debt, or 5.4% of the U.S. gross domestic product in 2022

#National debt here: https://fiscaldata.treasury.gov/americas-finance-guide/national-debt/
summary_by_year_data_2022 <- summary_by_year_data %>% filter(year == 2022)
((summary_by_year_data_2022$total_net_net_pension_liability) + (summary_by_year_data_2022$total_net_net_opeb_liability))/30900000000000

#GDP here: https://www.bea.gov/news/2023/gross-domestic-product-fourth-quarter-and-year-2022-advance-estimate: $25.46 trillion
((summary_by_year_data_2022$total_net_net_pension_liability) + (summary_by_year_data_2022$total_net_net_opeb_liability))/ 25460000000000


#unfunded OPEB falling: 2020 - 2022
100- ((summary_by_year_data[3,3]/summary_by_year_data[1,3])*100)

#unfunded PENSION falling 2020 - 2022
100 - ((summary_by_year_data[3,2]/summary_by_year_data[1,2])*100)

#write_xlsx(list(summary_by_year_data = summary_by_year_data), "total_liabilities_by_year.xlsx"

# Figure 2: Share that NPL v OPEB occupy of total debt by entity 

by_entity_data <- bind_rows(
  State %>% select(year, net_net_pension_liability, net_net_opeb_liability, non_current_liabilities,Entity_Type),
  County %>% select(year, net_net_pension_liability, net_net_opeb_liability, non_current_liabilities,Entity_Type),
  City %>% select(year, net_net_pension_liability, net_net_opeb_liability, non_current_liabilities,Entity_Type),
  SD %>% select(year, net_net_pension_liability, net_net_opeb_liability, non_current_liabilities,Entity_Type)
) %>%
  filter(year == 2022) %>%
  group_by(Entity_Type) %>%
  summarise(
    total_net_net_pension_liability = sum(net_net_pension_liability, na.rm = TRUE),
    total_net_net_opeb_liability = sum(net_net_opeb_liability, na.rm = TRUE),
    total_other_non_current_liabilities = sum(non_current_liabilities, na.rm = TRUE) - total_net_net_opeb_liability + total_net_net_pension_liability
  ) %>% 
  rowwise() %>% 
  mutate(tot = sum(c_across(total_net_net_pension_liability:total_other_non_current_liabilities), na.rm = TRUE))

by_entity_data[1, 2]/by_entity_data[1, 5] 
by_entity_data[1, 3]/by_entity_data[1, 5] 
#county
by_entity_data[2, 2]/by_entity_data[2, 5] 
by_entity_data[2, 3]/by_entity_data[2, 5] 

#school
by_entity_data[3, 2]/by_entity_data[3, 5] 
by_entity_data[3, 3]/by_entity_data[3, 5] 
#state
by_entity_data[4, 2]/by_entity_data[4, 5] 
by_entity_data[4, 3]/by_entity_data[4, 5] 


# Write the result to an Excel file
#write_xlsx(list(summary_by_year_data2 = by_entity_data), "total_liabilities_entity.xlsx")
