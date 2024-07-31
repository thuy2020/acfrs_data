source("nces.R")
source("functions.R")

fields_to_select <- c("state.abb", "state.name", "id", "geo_id", "year", "name", 
                      "identifier", "category",
                      "total_liabilities", "current_liabilities",
                      "net_pension_liability", "net_pension_assets",
                      "net_opeb_liability", "net_opeb_assets", 
                      "total_assets", "current_assets", "compensated_absences",
                      "expenses", "revenues",
                      "population", "urban_pop", "pct_urban_pop", "median_hh_income_21")


####Dictionary#### 
dictionary <- readRDS("data/dictionary.RDS")

dictionary_oh <- dictionary %>% filter(state.abb == "OH")

dictionary_oh %>% add_count(ncesID) %>% filter(n >1) %>% arrange(ncesID)

####ACFRs - OH school districts####
school_districts_ <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "School District") %>% 
  mutate(id = as.character(id)) %>% 
  select(any_of(fields_to_select)) 

school_districts <- append_url(school_districts_) %>% select(-identifier)

acfrs_oh <- school_districts %>% filter(state.abb == "OH") 

# count by each year
acfrs_oh %>% group_by(year) %>% add_count() %>% 
  select(year, n) %>% distinct()

####NCES OH####
nces_oh <- nces %>% filter(state.abb == "OH") 

###

oh_acfr_dic_nces <- acfrs_oh %>% 
  #Joining with dictionary to get ncesID into acfr data
  left_join(dictionary_oh) %>% 
  
  
  #Joining with nces to get enrollment
  left_join(nces_oh, by = "ncesID") %>% drop_na(enrollment_20) 

oh_acfr_dic_nces %>% write.csv("analysis/OH_school_districts_all4years.csv")


#break down by year
oh_acfr_dic_nces %>% #filter(is.na(enrollment_20)) %>%  #-> test
  group_by(year) %>% 
  drop_na(enrollment_22) %>% 
  add_count(year) %>% select(year, n) %>% distinct()

#which sd not collected? 
anti_join(nces_oh, oh_acfr_dic_nces, by = "ncesID") %>% 
  arrange(desc(enrollment_20)) -> test

#2022

oh_22 <- oh_acfr_dic_nces %>% filter(year == 2022) %>% 
drop_na(enrollment_20)

sum(oh_22$total_liabilities, na.rm = TRUE)
sum(oh_22$net_opeb_liability, na.rm = TRUE)
sum(oh_22$net_pension_liability, na.rm = TRUE)
