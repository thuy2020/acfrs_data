library(DBI)  
library(dplyr)
library(tidyr)
library(stringr)
source("src/data_ingestion/db_connection.R")

##########NOTE######
# the file "db_connection.R" - database connection is not tracked by git. 
# To run this script, use your own personal credential to connect to database below:

# Insert your credentials to connect with the database
# con <- dbConnect(dbDriver("PostgreSQL"), 
#                  host = "",
#                  port=5432, 
#                  dbname = "",
#                  user = "",
#                  password = ""
#                  )


#######Function to fetch data from database############
fetch_data <- function(years, con){
  data_list <- list()
  
  for (year in years){
    statement <- sprintf("SELECT cafrs_state.abbreviation as state_abb,
                         cafrs_state.name as state_name,
                         cafrs_entity.name, CAST(cafrs_entity.census_id as varchar),
                    
                         cafrs_entity.id, cafrs_entity.nces_district_id, 
                         year, category,
                         
                         cafrs_acfr.identifier, cafrs_acfr.unable_to_review,
                         
                         cafrs_acfrvalue.reason_value, cafrs_acfrvalue.name as field_name
                         
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
  acfrs_data <- bind_rows(data_list, .id = "source_year") %>% 
    
    #TODO: fix this in database
    mutate(census_id = case_when(state_abb == "NY" & name == "Rochester" ~ "33202800800000",
                                 state_abb == "WI" & name == "Madison" ~ "50201301100000",
                                 TRUE ~ as.character(census_id))) %>% 
  
    # pivot to get all fields as columns 
  pivot_wider(names_from = field_name, 
                values_from = reason_value) %>% 
    
    # some cleaning to be compatible with other data
  mutate(name = str_to_lower(name)) %>% 
  rename(state.abb = state_abb,
         state.name = state_name) %>% 
    
    # exclude acfrs unable to review due to various reason
    filter(!unable_to_review != "") %>% 
    
    # calculate revenues. NOTE: when applying pmax(), na.rm does not work
  rowwise() %>% 
    mutate(sum1 = sum(charges_for_services, operating_grants,  capital_grants, general_revenue, na.rm = TRUE),
           sum2 = sum(activities_change_in_net_position,  expenses, na.rm = TRUE),
           revenues = max(sum1, sum2)) %>% ungroup() %>% 
    select(-c(sum1, sum2, unable_to_review))
    

  saveRDS(acfrs_data, "data/acfrs_data.RDS")
  
}
#Call function
fetch_data(c(2020, 2021, 2022, 2023), con)

# make sure to close all connections 
all_cons <- dbListConnections(drv)
for (con in all_cons) {
  dbDisconnect(con)
}

# Ensure all connections are closed
all_cons <- dbListConnections(drv)
print(all_cons)  


