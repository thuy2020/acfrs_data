library(openxlsx)
source("src/data_processing/functions.R")

####Changes across years####

save_metric_changes_to_excel(state_all, "state_all")
save_metric_changes_to_excel(top100_county_4years, "top100_counties")
save_metric_changes_to_excel(top100_cities, "top100_cities")
save_metric_changes_to_excel(top100_school_districts_4years, "top100_sd")

