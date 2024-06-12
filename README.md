# Run `main.R` to run all scripts. 

# Processing ACFRs data

* Identify & Categorize acfrs entity
* Joining with external data from census, nces.
* Exporting result for other purposes: analysis, data tool, update database

Run the following main files for replication. 
 
# Query data from database
query_acfrs_data.R

# Top 100 entities in 4 categories: state, county, city, school districts

top100.R

# Others

## Mapping school district collected in ACFRs and NCES school district

dictionary_ncesID_acfrsID.R


# External data

These files are sourced to the above files. No need to run separately. 

census.R

exceptions.R

nces.R

# Result & checking

processing_for_data_tool.R
database_tool_check.R
