library(dplyr)

#need to read in this data: acfrs_data.RDS

acfrs_data <- readRDS("data/acfrs_data.RDS") %>% 
  select(state.abb, state.name, name, year, net_pension_liability, net_pension_assets,
         total_liabilities)

acfrs_data_2022 <- acfrs_data %>% filter(year == 2022) %>% 
  filter(!state.abb %in% c("MP", "PR", "AS", "GU")) %>% 
  group_by(state.abb, state.name) %>% 
summarise(tot_state_NPL = sum(net_pension_liability, na.rm = TRUE),
          tot_state_NPA = sum(net_pension_assets, na.rm = TRUE),
          tot_state_total_liabilities = sum(total_liabilities, na.rm = TRUE))

acfrs_data_2022 %>% write.csv("state_tot_NPL_totalLianiliites_2022.csv")