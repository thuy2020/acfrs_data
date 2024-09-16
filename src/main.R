
cat("Starting the main script...\n")

source("src/data_ingestion/query_acfrs_data.R")
source("src/data_processing/cleaning_merging.R")

source("src/data_processing/processing_for_data_tool.R")

#check NA values for data tool
source("src/data_processing/database_tool_check.R")


source("src/data_processing/acfr_for_annual_pension_report.R")

cat("All scripts have been run.\n")
