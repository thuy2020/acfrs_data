library(DBI)  
library(dplyr)
library(tidyr)
library(stringr)
source("src/data_ingestion/db_connection.R")

##########NOTE######
# the file "db_connection.R" - database connection is not tracked by git. 
# To run this script, use your own personal credential to connect to database

#######Function to fetch data from database############
fetch_data <- function(years, con){
  data_list <- list()
  
  for (year in years){
    statement <- sprintf("SELECT cafrs_state.abbreviation as state_abb,
                         cafrs_state.name as state_name,
                         cafrs_entity.name, 
                         cafrs_entity.id, cafrs_entity.nces_district_id, 
                         year, category,
                         
                         cafrs_acfr.created, cafrs_acfr.collected_by
                         
                         
                         FROM cafrs_acfr
                         INNER JOIN cafrs_entity on (cafrs_acfr.entity_id = cafrs_entity.id)
                         INNER JOIN cafrs_state on (cafrs_state.id = cafrs_entity.state_id)
                      
                         INNER JOIN cafrs_acfrvalue on (cafrs_acfrvalue.acfr_id = cafrs_acfr.id)
                         
                         WHERE year = %d
                         AND category != 'Non-Profit'
                         AND is_valid
                        
                         AND not is_nonstandard", year)
    
    
    data_from_db <- dbGetQuery(con, statement) %>%
      as.data.frame()
    
    # Append the data frame to the list
    data_list[[as.character(year)]] <- data_from_db
    
  }
  
  # Combine all data frames 
  acfrs_data_created <- bind_rows(data_list, .id = "source_year") 
  
  saveRDS(acfrs_data_created, "data/acfrs_data_created.RDS")
  
}
#Call function

fetch_data(c(2020, 2021, 2022, 2023), con)

# make sure to close all connections 
dbDisconnect(con)

nrow(readRDS("data/acfrs_data_created.RDS"))
acfrs_data_created <- readRDS("data/acfrs_data_created.RDS")
acfrs_data_created %>% filter(year == 2023) %>% 
  filter(category %in% c("General Purpose", "School District")) %>% 
  distinct() %>% 
  View()

