# NCES data set has 13,713 school districts across 50 states and 5 US territories, accounts for 48,420,226 students. 
#This analysis only concerns 13,707 school districts in 50 states, accounts for a total of 48,026,570 students.
# 
# NOTE: This list contains all school districts that have 1 or more students. 
# Potential Problem: Some ACFRs entities that are categorized as school districts have 0 student.

nces <- rio::import(here::here("./data/ncesdata_DBBFFFC.xlsx"), skip = 14) %>% 
  clean_names() %>% 
  rename(nces_original_name = district_name,
         ncesID = nces_district_id) %>% 
  
  # for NCES ID that has only 6 digits, adding a leading 0
  mutate(ncesID = ifelse(str_length(ncesID) < 7, paste0("0", ncesID), ncesID)) %>% 
  mutate(students = as.numeric(students)) %>% 
  
  drop_na(nces_original_name) %>% 
  # take out 5 territories "AS" "DC" "GU" "PR" "VI"
  filter(!state %in% c("AS", "DC", "GU", "PR", "VI"))
  
saveRDS(nces, "nces.RDS")
