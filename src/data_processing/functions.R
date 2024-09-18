# Load necessary library
library(dplyr)

# Function to process the data frame and add URLs
append_url <- function(data) {
  # Function to create URL from input string and category
  create_url <- function(input_string, category) {
    # Split the input string into components
    components <- strsplit(input_string, " ")[[1]]
    
    # Extract the components
    state_abbr <- components[1]
    entity_name <- paste(components[-c(1, length(components))], collapse = " ")
    year <- components[length(components)]
    
    # Create a safe filename
    safe_filename <- gsub(" ", "%20", entity_name)
    
    # Replace spaces with %20 in category
    safe_category <- gsub(" ", "%20", category)
    
    # Construct the URL
    url <- sprintf("https://acfrs-docs.s3.us-east-2.amazonaws.com/acfrs/%s/%s/%s/%s%%20%s%%20%s.pdf",
                   safe_category, state_abbr, safe_filename, state_abbr, safe_filename, year)
    
    return(url)
  }
  
  # Check if the necessary columns exist
  if (!all(c("identifier", "category") %in% colnames(data))) {
    stop("The 'identifier' and/or 'category' column does not exist in the data frame.")
  }
  
  # Apply the create_url function to each row
  data <- data %>%
    rowwise() %>%
    mutate(url = create_url(identifier, category)) %>%
    ungroup()
  
  return(data)
}


#Function to calculate % changes across years 
changes_across_years <- function(data, metric) {
  data %>% select(state.abb, name, year, metric) %>% 
    pivot_wider(names_from = year, values_from = metric) %>% 
    rowwise() %>% 
    mutate(diff_20_21 = `2021`/`2020`,
           diff_21_22 = `2022`/`2021`,
           diff_23_22 = `2023`/`2022`) 
}

# Function to save the changes of metrict across years to Excel
save_metric_changes_to_excel <- function(data, dataset_name, 
                                         output_dir = "output/testing_validation/") {
  
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
