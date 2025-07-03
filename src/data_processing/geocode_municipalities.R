# Geocode Municipalities Script
# This script adds latitude and longitude coordinates to municipality data
# for visualization purposes in the large_acfr_tool
# 
# FEATURES:
# - Incremental processing with checkpoints
# - Resume capability after interruptions
# - Robust error handling
# - Multiple geocoding methods

library(tidyverse)
library(tidygeocoder)  
library(stringr)       
library(readr)        
library(dplyr)     

# Configuration
CONFIG <- list(
  # File paths
  log_file = "geocoding_log.txt",
  checkpoint_dir = "geocoding_checkpoints",
  output_dir = "output",
  
  # Processing parameters
  batch_size = 25,  # Smaller batches for more frequent checkpoints
  pause_sec = 1,    # Pause between batches
  
  # Checkpoint frequency (save after processing this many municipalities)
  checkpoint_frequency = 100
)

# Create directories if they don't exist
if (!dir.exists(CONFIG$checkpoint_dir)) {
  dir.create(CONFIG$checkpoint_dir, recursive = TRUE)
  cat("Created checkpoint directory:", CONFIG$checkpoint_dir, "\n")
}

if (!dir.exists(CONFIG$output_dir)) {
  dir.create(CONFIG$output_dir, recursive = TRUE)
  cat("Created output directory:", CONFIG$output_dir, "\n")
}

# Set up logging
log_message <- function(message, append = TRUE) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  log_entry <- paste(timestamp, message)
  write(log_entry, CONFIG$log_file, append = append)
  cat(log_entry, "\n")
}

# Initialize log
log_message("Geocoding process started", append = FALSE)

# Function to save checkpoint
save_checkpoint <- function(data, checkpoint_name) {
  checkpoint_file <- file.path(CONFIG$checkpoint_dir, paste0(checkpoint_name, ".rds"))
  saveRDS(data, checkpoint_file)
  log_message(paste("Checkpoint saved:", checkpoint_file))
  return(checkpoint_file)
}

# Function to load the most recent checkpoint
load_latest_checkpoint <- function(checkpoint_prefix) {
  checkpoint_pattern <- file.path(CONFIG$checkpoint_dir, paste0(checkpoint_prefix, "*.rds"))
  checkpoint_files <- list.files(path = CONFIG$checkpoint_dir, 
                                pattern = paste0("^", checkpoint_prefix, ".*\\.rds$"), 
                                full.names = TRUE)
  
  if (length(checkpoint_files) == 0) {
    log_message(paste("No checkpoints found for prefix:", checkpoint_prefix))
    return(NULL)
  }
  
  # Get file info and sort by modification time (most recent first)
  file_info <- file.info(checkpoint_files)
  file_info$filename <- rownames(file_info)
  file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]
  
  latest_checkpoint <- file_info$filename[1]
  log_message(paste("Loading checkpoint:", latest_checkpoint))
  
  return(readRDS(latest_checkpoint))
}

# Function to safely geocode with error handling and checkpointing
safe_geocode <- function(locations, already_processed = NULL) {
  # Check if we have already processed some locations
  if (!is.null(already_processed)) {
    log_message(paste("Resuming from checkpoint with", nrow(already_processed), "already processed locations"))
    
    # Identify which locations have already been processed
    processed_ids <- already_processed %>%
      mutate(id = paste(state.name, name, sep = "|||")) %>%
      pull(id)
    
    locations <- locations %>%
      mutate(id = paste(state.name, name, sep = "|||")) %>%
      filter(!(id %in% processed_ids)) %>%
      select(-id)
    
    results <- already_processed
    log_message(paste(nrow(locations), "locations remaining to be processed"))
  } else {
    results <- data.frame()
    log_message(paste("Starting to process", nrow(locations), "locations"))
  }
  
  if (nrow(locations) == 0) {
    log_message("No new locations to process")
    return(results)
  }
  
  # Process in batches
  total <- nrow(locations)
  batches <- split(locations, ceiling(seq_len(total) / CONFIG$batch_size))
  
  for (i in seq_along(batches)) {
    batch <- batches[[i]]
    
    log_message(paste("Processing batch", i, "of", length(batches), 
                     "- Records", (i-1)*CONFIG$batch_size + 1, "to", 
                     min((i)*CONFIG$batch_size, total)))
    
    # Try to geocode with error handling
    tryCatch({
      batch_results <- batch %>%
        geocode(address = address, method = 'osm', lat = latitude, long = longitude)
      
      results <- bind_rows(results, batch_results)
      
      log_message(paste("Successfully geocoded batch", i))
    }, 
    error = function(e) {
      log_message(paste("Error in batch", i, ":", e$message))
      
      # Try one by one for failed batch
      for (j in 1:nrow(batch)) {
        single_location <- batch[j, , drop = FALSE]
        tryCatch({
          log_message(paste("Attempting individual geocode for:", single_location$address))
          single_result <- single_location %>%
            geocode(address = address, method = 'osm', lat = latitude, long = longitude)
          results <<- bind_rows(results, single_result)
          
          # Brief pause to avoid overwhelming the service
          Sys.sleep(0.5)
        }, 
        error = function(e) {
          log_message(paste("Failed to geocode:", single_location$address, "-", e$message))
          
          # Return the original data with NA coordinates
          empty_result <- single_location
          empty_result$latitude <- NA
          empty_result$longitude <- NA
          results <<- bind_rows(results, empty_result)
        })
      }
    })
    
    # Save checkpoint periodically
    if (i %% ceiling(CONFIG$checkpoint_frequency / CONFIG$batch_size) == 0 || i == length(batches)) {
      checkpoint_name <- paste0("geocoded_batch_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      save_checkpoint(results, checkpoint_name)
    }
    
    # Pause between batches to respect rate limits
    Sys.sleep(CONFIG$pause_sec)
  }
  
  return(results)
}

# Function to try alternative geocoding methods for failed attempts
try_alternative_geocoding <- function(failed_data, municipal_data) {
  log_message(paste("Attempting alternative geocoding for", nrow(failed_data), "failed municipalities"))
  
  # Try with state abbreviation instead
  failed_with_abbr <- failed_data %>%
    left_join(municipal_data %>% 
                select(state.name, state.abb) %>% 
                distinct(), 
              by = "state.name") %>%
    mutate(address = paste(clean_name, state.abb, "USA"))
  
  # Load checkpoint if exists
  alt_checkpoint <- load_latest_checkpoint("alt_geocoded")
  
  # Process with alternative method
  alternative_results <- safe_geocode(failed_with_abbr, alt_checkpoint)
  
  # Save final alternative results
  save_checkpoint(alternative_results, paste0("alt_geocoded_final_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  
  return(alternative_results)
}

# Main process
main <- function() {
  log_message("Starting main geocoding process")
  
  # Check if we have a final results file already
  final_results_path <- file.path(CONFIG$output_dir, "municipalities_with_coordinates.rds")
  if (file.exists(final_results_path)) {
    log_message("Final results file already exists. To reprocess, please rename or remove the file.")
    log_message(paste("File path:", final_results_path))
    
    # Ask if user wants to use existing results
    cat("\nDo you want to use the existing results? (y/n): ")
    user_response <- tolower(readline())
    
    if (user_response == "y") {
      log_message("Using existing results")
      municipal_data_with_coords <- readRDS(final_results_path)
      
      # Skip to output generation
      generate_outputs(municipal_data_with_coords)
      return(invisible(NULL))
    } else {
      log_message("Will reprocess data")
    }
  }
  
  
  
  # Read the municipality data
  log_message("Reading municipality data")
  municipal_data_temp <- read_csv("output/all_municipalities_2020_2023.csv")
  municipal_data <- municipal_data_temp [, -1]
  
  # Filter for the most recent year to reduce geocoding load
  recent_year <- max(municipal_data$year, na.rm = TRUE)
  log_message(paste("Using data from year:", recent_year))
  
  # Create a dataframe with unique municipalities for geocoding
  municipalities_to_geocode <- municipal_data %>%
    filter(year == recent_year) %>%
    select(state.name, name) %>%
    distinct() %>%
    # Clean up municipality names
    mutate(
      # Remove common prefixes like "city of", "town of", etc.
      clean_name = str_replace_all(name, "^(city of|town of|village of|township of|borough of) ", ""),
      # Create full address for geocoding
      address = paste(clean_name, state.name, "USA")
    )
  
  log_message(paste("Total unique municipalities to geocode:", nrow(municipalities_to_geocode)))
  
  # Check for existing checkpoint
  geocoded_checkpoint <- load_latest_checkpoint("geocoded_batch")
  
  # Geocode the municipalities (resuming from checkpoint if available)
  geocoded_municipalities <- safe_geocode(municipalities_to_geocode, geocoded_checkpoint)
  
  # Save final results
  final_checkpoint_name <- paste0("geocoded_final_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  save_checkpoint(geocoded_municipalities, final_checkpoint_name)
  
  # Check for failed geocoding attempts
  failed_geocoding <- geocoded_municipalities %>%
    filter(is.na(latitude) | is.na(longitude))
  
  if (nrow(failed_geocoding) > 0) {
    log_message(paste("Failed to geocode", nrow(failed_geocoding), "municipalities"))
    
    # Try alternative geocoding for failed attempts
    alternative_results <- try_alternative_geocoding(failed_geocoding, municipal_data)
    
    # Update the main results with alternative geocoding
    geocoded_municipalities <- geocoded_municipalities %>%
      filter(!(is.na(latitude) | is.na(longitude))) %>%
      bind_rows(alternative_results)
    
    # Save updated final results
    final_alt_name <- paste0("geocoded_final_with_alternatives_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    save_checkpoint(geocoded_municipalities, final_alt_name)
  }
  
  # Prepare the final dataset
  log_message("Joining geocoded data with original dataset")
  municipal_data_with_coords <- municipal_data %>%
    left_join(
      geocoded_municipalities %>%
        select(state.name, name, latitude, longitude),
      by = c("state.name", "name")
    )
  
  # Save the complete dataset as RDS for future use
  saveRDS(municipal_data_with_coords, final_results_path)
  log_message(paste("Complete dataset saved to:", final_results_path))
  
  # Generate outputs
  generate_outputs(municipal_data_with_coords, recent_year)
  
  # Final statistics
  log_message("Geocoding process completed")
  log_message(paste("Total municipalities processed:", nrow(municipal_data_with_coords)))
  log_message(paste("Unique municipalities geocoded:", nrow(geocoded_municipalities)))
  log_message(paste("Successfully geocoded:", sum(!is.na(geocoded_municipalities$latitude))))
  log_message(paste("Failed to geocode:", sum(is.na(geocoded_municipalities$latitude))))
}

# Function to generate output files
generate_outputs <- function(municipal_data_with_coords, recent_year = NULL) {
  log_message("Generating output files")
  
  # If recent_year is not provided, determine it from the data
  if (is.null(recent_year)) {
    recent_year <- max(municipal_data_with_coords$year, na.rm = TRUE)
    log_message(paste("Using most recent year from data:", recent_year))
  }
  
  # Save the enhanced dataset as CSV
  output_file <- file.path(CONFIG$output_dir, "municipalities_with_coordinates.csv")
  
  write_csv(municipal_data_with_coords, output_file)
  log_message(paste("Enhanced dataset saved to:", output_file))
  
  # Create a simplified version with just essential fields for mapping
  mapping_data <- municipal_data_with_coords %>%
    filter(year == recent_year) %>%
    select(
      state.abb, state.name, name, 
      latitude, longitude, 
      population, 
      total_assets, total_liabilities, 
      revenues, expenses,
      net_net_pension_liability, net_net_opeb_liability
    ) %>%
    # Calculate some additional metrics for visualization
    mutate(
      net_position = total_assets - total_liabilities,
      assets_per_capita = if_else(population > 0, total_assets / population, NA_real_),
      liabilities_per_capita = if_else(population > 0, total_liabilities / population, NA_real_),
      net_position_per_capita = if_else(population > 0, net_position / population, NA_real_)
    )
  
  # Save the mapping data as RDS and CSV
  mapping_rds <- file.path(CONFIG$output_dir, "municipality_mapping_data.rds")
  saveRDS(mapping_data, mapping_rds)
  log_message(paste("Mapping data saved as RDS to:", mapping_rds))
  
  mapping_file <- file.path(CONFIG$output_dir, "municipality_mapping_data.csv")
  write_csv(mapping_data, mapping_file)
  log_message(paste("Mapping data saved as CSV to:", mapping_file))
  
  # Convert to JSON for web visualization
  json_file <- file.path(CONFIG$output_dir, "municipality_mapping_data.js")
  mapping_data_json <- jsonlite::toJSON(mapping_data, pretty = TRUE)
  mapping_data_json <- paste0("export default ", mapping_data_json)
  write(mapping_data_json, json_file)
  log_message(paste("JSON mapping data saved to:", json_file))
  
  # Print summary
  cat("\nOutput Generation Complete\n")
  cat("Total municipalities with coordinates:", nrow(mapping_data), "\n")
  cat("Successfully geocoded:", sum(!is.na(mapping_data$latitude)), "\n")
  cat("Failed to geocode:", sum(is.na(mapping_data$latitude)), "\n")
}

# Run the main function
main()


#check result

#df_municipalities_with_coordinates <- readRDS("output/municipalities_with_coordinates.rds")




