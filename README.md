# Processing ACFRs data

* Identify & Categorize acfrs entity
* Joining with external data from census, nces
* Exporting result for other purposes: analysis, data tool, update database

Run the following main files for replication. 
 
# Query data from database
query_acfrs_data.R

# Genereal purpose entity

general_purpose.R

top100_state_county_city.Rmd

# School District

top100sd.Rmd

# External data

These files are sourced to the above files. No need to run separately. 

supplement_data.R

nyc_school_districts.R

nces.R

census.R

# Result

processing_for_data_tool.R
