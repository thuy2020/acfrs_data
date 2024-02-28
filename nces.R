# Top School Districts in NCES
nces <- readRDS("nces.RDS")

top_sd_nces <- nces %>% 
  arrange(desc(students)) %>% 
  filter(!nces_original_name %in% c("PUERTO RICO DEPARTMENT OF EDUCATION", "District of Columbia Public Schools")) %>% select(-c(county_name, city)) %>% 
  slice(1:100) 