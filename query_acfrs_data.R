library(DBI)  
library(dplyr)
source("db_connection.R")
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
    statement <- sprintf("SELECT cafrs_state.abbreviation as state,
                         cafrs_entity.name, CAST(cafrs_entity.census_id as varchar),
                         cafrs_entity.id, cafrs_entity.nces_district_id,
                         cafrs_entity.is_city, 
                         cafrs_entity.is_county, 
                         cafrs_entity.is_intergovernmental, 
                         cafrs_entity.is_special_district, 
                         cafrs_entity.is_state, 
                         cafrs_entity.is_tribal,
                         year, category,
                         
                         total_liabilities, current_liabilities, 
                         net_pension_liability, net_pension_assets, 
                         net_opeb_liability, net_opeb_assets, 
                         
                         leases, loans_outstanding, notes_outstanding,bonds_outstanding,compensated_absences,
                         
                         charges_for_services, operating_grants, capital_grants, general_revenue, 
                         cafrs_activities.activities_change_in_net_position, expenses, 
                         total_operating_revenues, non_operating_revenues, capital_contributions,
                         
                         greatest(charges_for_services + operating_grants + capital_grants + general_revenue,
                                  cafrs_activities.activities_change_in_net_position + expenses,
                                  total_operating_revenues + non_operating_revenues) as revenues,
                         
                         component_unit_of_id
                         
                         FROM cafrs_acfr
                         INNER JOIN cafrs_entity on (cafrs_acfr.entity_id = cafrs_entity.id)
                         INNER JOIN cafrs_state on (cafrs_state.id = cafrs_entity.state_id)
                         INNER JOIN cafrs_netposition on (cafrs_netposition.cafr_id = cafrs_acfr.id)
                         INNER JOIN cafrs_activities on (cafrs_activities.cafr_id = cafrs_acfr.id)
                         INNER JOIN cafrs_proprietaryrevenues on (cafrs_proprietaryrevenues.cafr_id = cafrs_acfr.id)
                         WHERE year = %d
                         AND category != 'Non-Profit'
                         AND is_valid
                         AND reviewed_date is not null
                         AND not is_nonstandard", year)
    
    
    data_from_db <- dbGetQuery(con, statement) %>%
      as.data.frame()
    
    # Append the data frame to the list
    data_list[[as.character(year)]] <- data_from_db
    
  }
  
  # Combine all data frames 
  acfrs_data <- bind_rows(data_list, .id = "source_year") %>% 
    
    #TODO: fix this in database
    mutate(census_id = case_when(state == "NY" & name == "Rochester" ~ "33202800800000", 
                                 state == "WI" & name == "Madison" ~ "50201301100000",
                                 TRUE ~ as.character(census_id)))
  
  saveRDS(acfrs_data, "data/acfrs_data.RDS")
  
  # return the combined data frame
  return(acfrs_data)
 
}


########Call function###########
fetch_data(c(2020, 2021, 2022), con)


