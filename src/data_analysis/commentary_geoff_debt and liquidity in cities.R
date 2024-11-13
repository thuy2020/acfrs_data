

#An entity’s debt ratio is calculated by dividing its total liabilities 
#by total assets. It reveals roughly what proportion of the entity’s 
#assets are debt-financed versus owned outright. 
#As with a business or household, a heavily indebted city must devote a 
#larger share of its future cash flow toward debt service and has less 
#financial flexibility with which to consider new priorities.

top10cities <- city_data_temp %>% 
  filter(year == 2022) %>% 
  arrange(desc(population)) %>% slice(1:10) 

top10cities %>% 
  select(name, state.name, total_liabilities, total_assets) %>% 
  mutate(debt_ratio = round(total_liabilities/total_assets*100, 1)) %>% 
 # arrange(desc(debt_ratio)) %>% 
  mutate(debt_ratio = paste0(debt_ratio, "%")) 
  
#Unrestricted net position
top10cities %>% 
  select(name, state.name, unrestricted)


#Revenues less expenditures

top10cities %>% 
  select(name, state.name, revenues, expenses, population) %>% 
  mutate(rev_exp = revenues - expenses,
         rev_exp_percap = rev_exp/population) %>% 
  select(-population)

#Quick ratio
top10cities %>% 
  select(name, state.name, current_liabilities)

#Cash assets
top10cities %>% 
  select(name, state.name, total_assets, revenues)

city_data_temp %>% 
  filter(year == 2022) %>% 
  select(name, state.name, unrestricted) %>% View()
