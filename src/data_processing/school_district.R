library(tidyverse)
library(dplyr)
library(janitor)
library(jsonlite)
library(stringr)
library(forcats)
library(scales)
source("src/data_processing/census.R")
source("src/data_processing/general_purpose.R")
source("src/data_processing/nces.R")
source("src/data_processing/exceptions.R")
source("src/data_processing/functions.R")
options(scipen = 9999)

####School districts####
dictionary <- readRDS("data/dictionary.RDS") %>% 
  distinct()

#type of school district
## NOTE: 
#id 67836 Santa Cruz City Schools (the "District") is a consolidation of 
#Santa Cruz City High School District nces 0635600
#Santa Cruz City Elementary School District nces 0635590 . 
#The Districts have not unified but are consolidated due to the fact that the Districts share a common governing board. These two entities are referred to collectively as Santa Cruz City Schools, 
#and for purposes of these financial statements, will be referred to collectively as the District

#NE	ewing public schools district no. 29	id = 190272
# Effective June 6, 2020, Holt County School District 45-0029, commonly known as Ewing
# Public Schools; Antelope County Public School District 02-0006, commonly known as
# Clearwater Public Schools, and Antelope County Public School District 02-0049, commonly
# known as Orchard Public Schools, were dissolved and merged to create a new school district, Antelope County School District 02-0115, commonly known as Summerland Public
# Schools.

school_districts_ <- readRDS("data/acfrs_data.RDS") %>% 
  filter(category == "School District") %>% 
  mutate(id = as.character(id)) %>% 
  select(-c(source_year, census_id, nces_district_id))

#append URLs
school_districts <- append_url(school_districts_) %>% 
  select(-c(identifier, category)) %>% 
  filter(!state.name %in% c("Guam", "Puerto Rico")) %>% 
  left_join(dictionary, by = c("id", "state.abb")) %>% 
  
  filter(!is.na(ncesID)) %>% 
  filter(ncesID != "099999") %>% 
  rename(name = name.x) %>% 
  #test - why this can't get student? 5900148
 # select(state.abb, state.name, ncesID, id) %>% 

#  filter(ncesID == "5900148") %>% 

  left_join(nces, by = c("state.abb", "state.name","ncesID")) %>% 
  arrange(state.name)



#########Sum of all school districts#########

school_districts %>% filter(year == 2022) %>% #View()
  select(net_pension_liability,
        net_opeb_liability, 
        total_liabilities) %>% 
  summarise(all_NPL = sum(net_pension_liability, na.rm = TRUE),
           all_opeb = sum(net_opeb_liability, na.rm = TRUE), 
           all_total_liabilities = sum(total_liabilities, na.rm = TRUE)) %>%
  mutate(across(everything(), comma)) 

school_districts %>% filter(year == 2023) %>% #View()
  select(net_pension_liability,
         net_opeb_liability, 
         total_liabilities) %>% 
  summarise(all_NPL = sum(net_pension_liability, na.rm = TRUE),
            all_opeb = sum(net_opeb_liability, na.rm = TRUE), 
            all_total_liabilities = sum(total_liabilities, na.rm = TRUE)) %>%
  mutate(across(everything(), comma)) 

#############

####NO_ncesID####
# school_districts %>% 
#   left_join(dictionary, by = c("id", "state.abb")) %>% 
#   filter(is.na(ncesID)) %>% 
#   select(state.abb, id, name, ncesID, year, total_liabilities) %>% 
#   add_count(id) %>% filter(n > 1) %>% 
#   distinct() %>% arrange(name) -> acfrs_sd_NO_ncesID
 # write.csv("data/_acfrs_without_ncesID_Aug2024.csv")

####All acfrs sd####
d_app <- school_districts %>% 
  select(2:revenues, enrollment_22) %>% 
  #group_by(state.name, year) %>% 
  mutate(across(current_assets:revenues, list(tot = ~ sum(., na.rm = TRUE)), .names = "sum_{col}")) %>% 
  select(state.name, name, year, enrollment_22, contains("sum_")) %>% 
  pivot_longer(cols = 5:29,
               names_to = "category", 
               values_to = "value") %>% 
  pivot_wider(names_from = year, 
              values_from = value) %>% 
  mutate(category = str_remove(category, "sum_")) %>% distinct()
  
d_app %>% saveRDS("data/school_districts_for_shinyapp.RDS")

all_school_districts <- d_app %>% 
  mutate(across(where(is.numeric) & !all_of("enrollment_22"), ~ comma(.))) 


d_app %>% 
mutate(across(where(is.numeric), ~ comma(.))) %>% saveRDS("data/state_tot.RDS")


valuebox <- d_app %>%
  filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>%
  select(-c(name, `2023`, enrollment_22)) %>% 
  distinct() %>% 
  pivot_longer(3:5, names_to = "year", values_to = "value") %>% 
  group_by(state.name, category) %>%
  summarise(allyears = sum(value, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~ comma(.))) 
  
  valuebox %>% saveRDS("data/valuebox_data.RDS")

  #Top 10 each state
  top10_each_state <- d_app %>% 
    select(state.name, name, enrollment_22) %>% 
    distinct() %>% 
    arrange(state.name, desc(enrollment_22)) %>%      
    group_by(state.name) %>%                              
    slice_head(n = 10)
  
  top10_chart_data <- d_app %>%  
    select(-c(enrollment_22, `2023`)) %>% 
    filter(category %in% c("net_pension_liability", "net_opeb_liability", "total_liabilities")) %>% 
    inner_join(top10_each_state, by = c("statename", "name")) %>% 
    mutate(name = str_to_title(name))

saveRDS(top10_chart_data, "data/top10_chart_data.RDS")


filtered_data <- top10_chart_data %>%
  filter(state.name == "Arizona") 
  filter(category == "net_pension_liability") %>%
  select(name, `2022`)

p <- filtered_data %>%
  ggplot(aes(fct_reorder(name, `2022`), `2022`)) +
  geom_col(fill = "#55C5E6") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(x = "", y = "", 
       title = paste("Top 10 School Districts in ", input$selected_state, "in 2022")) +
  theme_minimal()


####Top 100####
dict_top100_ELSI <- dictionary %>% 
  filter(ncesID %in% top_schools_by_year$ncesID) %>% 
  drop_na(id) %>% select(-name)

top100_school_districts <- school_districts %>% 
  filter(id %in% dict_top100_ELSI$id) %>% 
  left_join(dict_top100_ELSI, by = c("id",  "state.abb")) %>% 
  
  #join with nces to get county, city info
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) %>% 
  
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() 

####Top 200####

dict_top200_ELSI <- dictionary %>% 
  filter(ncesID %in% top200_schools_by_year$ncesID) %>% 
  drop_na(id) %>% select(-name)

top200_school_districts <- school_districts %>% 
  filter(id %in% dict_top200_ELSI$id) %>% 
  left_join(dict_top200_ELSI, by = c("id",  "state.abb")) %>% 
  
  #join with nces to get county, city info
  left_join(nces, by = c("ncesID", "state.abb", "state.name")) %>% 
  
  #bind with NYC
  rbind(nyc_top5) %>% arrange(state.abb, name) %>% distinct() #%>% 
#select(state.abb, ncesID, year, name) %>% 
#add_count(ncesID) %>% filter(n < 3) 


#TODO: check back missing: 
#GA Clayton County Board of education.
#https://www.clayton.k12.ga.us/departments/business-services/financial-reports
# Uploaded: williamson county schools, reported in county acfrs. Uploaded county's acfrs to replace

missing_sd <- top200_school_districts %>% 
  add_count(ncesID) %>% filter(n < 3) %>% 
  select(state.abb, ncesID, year, name, n)

top200_school_districts %>% write.csv("output/top200_sd.csv")
top100_school_districts %>% write.csv("output/top100_sd.csv")
school_districts %>% write.csv("output/all_schooldistricts_3years.csv")
#TODO: ask Geoff about this revenues = expense cases
