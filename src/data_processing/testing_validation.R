library(openxlsx)
source("src/data_processing/functions.R")


####Changes across years####

save_metric_changes_to_excel(state_all, "state_all")
save_metric_changes_to_excel(county_all, "county_all")
save_metric_changes_to_excel(municipality_all, "municipality_all")
save_metric_changes_to_excel(school_districts_all, "school_districts_all")

######

school_districts_all %>% select(state.abb, name, year, total_liabilities) %>% 
    pivot_wider(names_from = year, values_from = total_liabilities) %>% 
    rowwise() %>% 
    mutate(diff_20_21 = `2021`/`2020`,
           diff_21_22 = `2022`/`2021`,
           diff_23_22 = `2023`/`2022`) 


# Function to save the changes of metrics across years to Excel
save_metric_changes_to_excel <- function(data, dataset_name, 
                                         output_dir = "output/testing/") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)  # Creates the directory, including any parent directories
  }
  
  wb <- createWorkbook()
  # List of metrics
  list_metric <- c("total_liabilities", 
                   "net_pension_liability", "net_pension_assets",
                   "net_opeb_liability", "net_opeb_assets", 
                   "total_assets", "current_assets", "compensated_absences",
                   "current_liabilities",
                   "expenses", "revenues",
                   "unrestricted")  
  
  for (metric in list_metric) {
    if(metric %in% colnames(data)){
      result <- changes_across_years(data, metric)  
      addWorksheet(wb, sheetName = metric)
      writeData(wb, sheet = metric, result)
      
    } else{
      warning(paste("Metric", metric, "not found in the data."))
    }
  }
  
  output_file <- paste0(output_dir, 
                        dataset_name, "_metrics_change_across_years.xlsx")
  saveWorkbook(wb, file = output_file, overwrite = TRUE)
  return(output_file)
}