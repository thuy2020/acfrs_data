
#####Population##########
# export population to load in database
state_gov %>% select(state.abb, name, id, geo_id, population) %>% 
  mutate(government_level = "state") -> temp1

county_gov %>% select(state.abb, name, id, geo_id, population) %>% 
  mutate(government_level = "county") -> temp2

place_division_gov %>% select(state.abb, name, id, geo_id, population) %>% 
  mutate(government_level = "municipal_township") -> temp3

rbind(temp1, temp2, temp3) %>% 
  rename(population_2020 = population) %>% distinct() 
#write.csv("output/general_purpose_population_2020.csv")