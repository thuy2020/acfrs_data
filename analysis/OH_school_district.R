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
  mutate(id = as.character(id)) #%>% 
  #select(any_of(fields_to_select)) 

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
  left_join(nces_oh, by = "ncesID") %>% 
  drop_na(enrollment_20) 

# Year 2022 only
oh_acfr_dic_nces %>% 
  filter(year == 2022) %>% 
  select(name, id, ncesID, enrollment_22, 
         total_liabilities, 
         net_pension_liability, net_opeb_liability,
         bonds_outstanding, notes_outstanding, loans_outstanding,
         current_liabilities,
         compensated_absences, url) %>% 
  arrange(desc(enrollment_22))-> oh_2022
  
oh_2022 %>% write.csv("analysis/OH_schooldistricts_2022.csv")
  write.csv("analysis/OH_school_districts_all4years.csv")


#break down by year
oh_acfr_dic_nces %>% 
  group_by(year) %>% 
  drop_na(enrollment_22) %>% 
  add_count(year) %>% 
  mutate(number_student_acfrs = sum(enrollment_20)) %>% 
  select(year, n, number_student_acfrs) %>% 
  rename(number_schooldistricts_acfrs = n) %>% 
  distinct() %>% 
  add_column("number_student_NCES" = c(sum(nces_oh$enrollment_20),
                                        sum(nces_oh$enrollment_21),
                                        sum(nces_oh$enrollment_22),
                                        sum(nces_oh$enrollment_22)
                                        )) %>% 
  
  mutate(collected_percent = number_student_acfrs/number_student_NCES) %>% 
  add_column("note" = c("  ",
                        "  ",
                        "  ",
                        "using 2022 headcount")) %>% 
  mutate(number_student_acfrs = format(number_student_acfrs, big.mark = ","),
         number_student_NCES = format(number_student_NCES, big.mark = ",")) 

#  write.csv("tmp/oh_sd_headcount_collected.csv")
  
  

#which sd not collected? 
anti_join(nces_oh, oh_acfr_dic_nces, by = "ncesID") %>% 
  arrange(desc(enrollment_20)) -> test

#2022

oh_22 <- oh_acfr_dic_nces %>% filter(year == 2022) %>% 
drop_na(enrollment_20)
sum(oh_22$enrollment_20)
sum(oh_22$total_liabilities, na.rm = TRUE)
sum(oh_22$net_opeb_liability, na.rm = TRUE)
sum(oh_22$net_pension_liability, na.rm = TRUE)


sum(nces_oh$enrollment_22)

nces_oh %>% select(enrollment_20, enrollment_21, enrollment_22)%>% 
  summarise_all(sum) %>% 
  pivot_longer(1:3, 
               names_to = "year_enroll", 
               values_to = "number_students_NCES") 
  add_row(year)
