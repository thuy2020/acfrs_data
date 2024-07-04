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


