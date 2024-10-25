library(openxlsx)
source("src/data_processing/functions.R")


####Changes across years####

save_metric_changes_to_excel(state_all, "state_all")
save_metric_changes_to_excel(county_all, "county_all")
save_metric_changes_to_excel(municipality_all, "municipality_all")
save_metric_changes_to_excel(school_districts_all, "school_districts_all")

