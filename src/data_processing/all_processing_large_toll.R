library(tidyverse)
library(jsonlite)
source("src/data_processing/cleaning_merging.R")

####Functions, common process####
entity_type_data <- function(data, year = 2023) {
  data %>% 
    filter(year == !!year) %>% 
  rename(
      state_abbr = state.abb,
      state_name = state.name,
      entity_id = id,
      document_url = url,
      entity_name = name,
      entity_type = category,
      total_revenues = revenues,
      total_expenses = expenses,
      urban_population = urban_pop,
      pct_urban_population = pct_urban_pop,
      median_household_income = median_hh_income_21
    ) %>% 
    mutate(across(
      c(total_assets, total_liabilities, current_assets, current_liabilities,
        bonds_outstanding, loans_outstanding, notes_outstanding,
        
        net_pension_assets, net_opeb_assets, net_pension_liability, net_opeb_liability, 
        total_revenues, total_expenses, urban_population, median_household_income),
      as.numeric)) %>% 
    # create net net pension and opeb liabilities
    mutate(
      net_pension_assets = replace_na(net_pension_assets, 0),
      net_opeb_assets = replace_na(net_opeb_assets, 0),
      net_pension_liability = replace_na(net_pension_liability, 0),
      net_opeb_liability = replace_na(net_opeb_liability, 0),
      
      net_net_pension_liability = net_pension_liability - net_pension_assets,
      net_net_opeb_liability = net_opeb_liability - net_opeb_assets
    ) %>% 
    
    # rename instead of using net-net for clean names
    mutate(
      pension_liability = net_net_pension_liability,
      opeb_liability = net_net_opeb_liability
    )
}

# Process each entity type data
state_data <- entity_type_data(state_all, 2023)
county_data <- entity_type_data(county_all, 2023)
municipal_data <- entity_type_data(municipality_all, 2023)
school_district_data <- entity_type_data(school_districts_all, 2023)
  

# Extract demographic data from state data to use later
state_demographics <- state_data  %>% 
  select(
    state_name,
    state_abbr,
    population,
    urban_population,
    pct_urban_population,
    median_household_income
  )  %>% 
  distinct()

# Prepare financial data of each entity type for aggregation
entity_type_financial <- function(data){
    data  %>% 
    select(
    state_name,
    state_abbr,
    total_assets,
    current_assets,
    total_liabilities,
    current_liabilities,
    pension_liability,
    opeb_liability,
    bonds_outstanding,
    loans_outstanding,
    notes_outstanding,
    total_revenues,
    total_expenses
  )}

state_financial <- entity_type_financial(state_data)
county_financial <- entity_type_financial(county_data)
municipal_financial <- entity_type_financial(municipal_data)
school_district_financial <- entity_type_financial(school_district_data)

# Combine all datasets
all_financial_data <- bind_rows(
  state_financial,
  county_financial,
  municipal_financial,
  school_district_financial
)

final_columns <- c(
  "entity_id", "entity_name", "entity_type", "year",
  "state_name", "state_abbr",
  
  "total_assets",
  "current_assets",
  "total_liabilities",
  "current_liabilities",
  "pension_liability",
  "opeb_liability",
  
  "bonds_outstanding",
  "loans_outstanding",
  "notes_outstanding",
  "bond_loans_notes",
  
  "total_revenues",
  "total_expenses",
  
  "net_position",
  "debt_ratio",
  "current_ratio",
  "free_cash_flow",
  
  "population",
  "urban_population",
  "pct_urban_population",
  "median_household_income"
)

####state_aggregate_data####
state_aggregate_data <- all_financial_data |>
  group_by(state_name, state_abbr) |>
  summarize(
    total_assets = sum(total_assets, na.rm = TRUE),
    current_assets = sum(current_assets, na.rm = TRUE),
    total_liabilities = sum(total_liabilities, na.rm = TRUE),
    current_liabilities = sum(current_liabilities, na.rm = TRUE),
    pension_liability = sum(pension_liability, na.rm = TRUE),
    opeb_liability = sum(opeb_liability, na.rm = TRUE),
    bonds_outstanding = sum(bonds_outstanding, na.rm = TRUE),
    loans_outstanding = sum(loans_outstanding, na.rm = TRUE),
    notes_outstanding = sum(notes_outstanding, na.rm = TRUE),
    
    # Calculate bond_loans_notes as the sum of the three debt types
    bond_loans_notes = sum(
        replace_na(bonds_outstanding, 0) +
        replace_na(loans_outstanding, 0) +
        replace_na(notes_outstanding, 0),
        na.rm = TRUE),  
    total_revenues = sum(total_revenues, na.rm = TRUE),
    total_expenses = sum(total_expenses, na.rm = TRUE),
    .groups = "drop"
  ) %>% 

# Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities, 
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities), 
    current_ratio = current_assets / current_liabilities) %>% 
  
  #TODO: add non_current_liability

# Join with demographic data from state entities
  left_join(state_demographics, by = c("state_name", "state_abbr")) %>% 
  # Add year and entity_type
  mutate(
    year = 2023,
    entity_type = "state_aggregate", 
    entity_id = paste0("agg_", tolower(state_abbr)),
    entity_name = paste(state_name, "Aggregate")) %>% 

# Select and order final columns
  select(all_of(final_columns))

# Write to JSON
state_aggregate_json <- toJSON(state_aggregate_data, pretty = TRUE)
state_aggregate_json <- paste0("export default ", state_aggregate_json)
write(state_aggregate_json, "output/state_aggregated.js")

# Print summary to console
cat("State aggregate data created with", nrow(state_aggregate_data), "states\n")


####state_data_final####
state_data_final <- state_data %>% 

mutate(
  net_position = total_assets - total_liabilities,
  debt_ratio = total_liabilities / total_assets,
  free_cash_flow = total_revenues - (total_expenses + current_liabilities),
  current_ratio = current_assets / current_liabilities
) %>% 
  # Add sum of bonds, loans, and notes
  mutate(
    bond_loans_notes = sum(
        replace_na(bonds_outstanding, 0) +
        replace_na(loans_outstanding, 0) +
        replace_na(notes_outstanding, 0),
      na.rm = TRUE)) %>% 
  
  # Select and order final columns
  select(all_of(final_columns), document_url)


# write to json
state_json <- toJSON(state_data_final, pretty = TRUE)
state_json <- paste0("export default ", state_json)
write(state_json, "output/state_data.js")

####county_data_final####

county_data_final <- county_data %>% 
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities
  ) %>% 
  # Add sum of bonds, loans, and notes
  mutate(
    bond_loans_notes = sum(
      replace_na(bonds_outstanding, 0) +
        replace_na(loans_outstanding, 0) +
        replace_na(notes_outstanding, 0),
      na.rm = TRUE)) %>% 
  
  # Select and order final columns
  select(all_of(final_columns), document_url)

# write to json
county_json <- toJSON(county_data_final, pretty = TRUE)
county_json <- paste0("export default ", county_json)
write(county_json, "output/county_data.js")


####municipality_data_final####
#coordinates data - R library
coordinates_data <- readRDS("src/data_processing/coordinates_data/geocoded_final_20250315_182544.rds")
# Prepare coordinates data for merging
coordinates_data <- coordinates_data |>
  rename(
    entity_name = clean_name,
    state_name = state.name
  ) |>
  # Create a merged key for joining
  mutate(
    entity_state_key = tolower(paste(entity_name, state_name, sep = "_"))
  ) |>
  select(entity_name, state_name, entity_state_key, latitude, longitude)

# filter for only 2023 data, standardize names
municipal_data_final <- municipal_data  %>% 
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities
  ) %>% 
  # Add sum of bonds, loans, and notes
  mutate(
    bond_loans_notes = sum(
        replace_na(bonds_outstanding, 0) +
        replace_na(loans_outstanding, 0) +
        replace_na(notes_outstanding, 0),
      na.rm = TRUE)) %>% 
  
  # Add the same merged key to municipal data
  mutate(
    entity_state_key = tolower(paste(entity_name, state_name, sep = "_"))) %>% 
  
  left_join(coordinates_data, by = "entity_state_key") |>
  # Use original entity_name and state_name from municipal_data
  # and only keep latitude and longitude from coordinates_data
  select(-entity_name.y, -state_name.y) |>
  rename(entity_name = entity_name.x, state_name = state_name.x) %>% 
  
  # Select final columns including new latitude and longitude
  select(all_of(final_columns), 
         latitude,
         longitude,
         document_url
  ) %>% 
  select(-urban_population)


###

municipal_data_to_latlong <- municipal_data %>% 
  mutate(entity_name = str_remove_all(entity_name, "city|township"),
         entity_name = str_squish(entity_name))


# write to json
municipal_json <- toJSON(municipal_data_final, pretty = TRUE)
municipal_json <- paste0("export default ", municipal_json)
write(municipal_json, "output/municipal_data.js")


####school_district_data_final####

school_district_data_final <- school_district_data %>% 
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities
  ) %>% 
  # Add sum of bonds, loans, and notes
  mutate(
    bond_loans_notes = sum(
      replace_na(bonds_outstanding, 0) +
        replace_na(loans_outstanding, 0) +
        replace_na(notes_outstanding, 0),
      na.rm = TRUE)) %>% 
  
  # Select final columns including latitude and longitude, but no population
  select(all_of(setdiff(final_columns, "population")), 
         latitude,
         longitude,
         document_url) 

# Write to json/js
school_district_json <- toJSON(school_district_data_final, pretty = TRUE)
school_district_json <- paste0("export default ", school_district_json)
write_json(school_district_data, "output/school_district_data.js", pretty = TRUE, auto_unbox = TRUE)
