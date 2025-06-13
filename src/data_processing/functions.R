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


bind_2df_different_size <- function(df1, df2) {
  # Get all column names from both dataframes
  all_cols <- union(names(df1), names(df2))
  
  # Add missing columns to df1
  for (col in setdiff(all_cols, names(df1))) {
    df1[[col]] <- NA
  }
  
  # Add missing columns to df2
  for (col in setdiff(all_cols, names(df2))) {
    df2[[col]] <- NA
  }
  
  # Reorder columns to be the same in both
  df1 <- df1[, all_cols]
  df2 <- df2[, all_cols]
  
  # Bind the rows
  result <- rbind(df1, df2)
  
  return(result)
}



compare_excel_files <- function(file1, file2, id_col = "id", output_csv = NULL) {
  # Load and clean
  df1 <- read_xlsx(file1) %>% clean_names()
  df2 <- read_xlsx(file2) %>% clean_names()
  
  # Print shapes
  cat("📄 File 1:", basename(file1), "\n  ➤ Rows:", nrow(df1), "Cols:", ncol(df1), "\n")
  cat("📄 File 2:", basename(file2), "\n  ➤ Rows:", nrow(df2), "Cols:", ncol(df2), "\n\n")
  
  # Compare IDs
  added_ids <- setdiff(df2[[id_col]], df1[[id_col]])
  removed_ids <- setdiff(df1[[id_col]], df2[[id_col]])
  
  cat("➕ Added IDs:", length(added_ids), "\n")
  cat("➖ Removed IDs:", length(removed_ids), "\n\n")
  
  # Inner join for value comparison
  joined <- inner_join(df1, df2, by = id_col, suffix = c("_old", "_new"))
  
  # Compare each column except ID
  common_cols <- intersect(names(df1), names(df2))
  common_cols <- setdiff(common_cols, id_col)
  
  changed_values <- map_dfr(common_cols, function(col) {
    old_col <- paste0(col, "_old")
    new_col <- paste0(col, "_new")
    
    joined %>%
      filter(!is.na(.data[[old_col]]) & !is.na(.data[[new_col]]) & .data[[old_col]] != .data[[new_col]]) %>%
      select(!!id_col, all_of(old_col), all_of(new_col)) %>%
      mutate(column = col)
  })
  
  # Output
  if (nrow(changed_values) == 0) {
    cat("✅ No value changes found for matching IDs.\n")
  } else {
    cat("🔄 Value changes found for", nrow(changed_values), "cells.\n")
    
    if (!is.null(output_csv)) {
      write.csv(changed_values, output_csv, row.names = FALSE)
      cat("📁 Changes written to:", output_csv, "\n")
    }
  }
  
  return(list(
    shape_diff = list(
      file1_rows = nrow(df1), file2_rows = nrow(df2),
      file1_cols = ncol(df1), file2_cols = ncol(df2)
    ),
    added_ids = added_ids,
    removed_ids = removed_ids,
    changed_values = changed_values
  ))
}


compare_latest_csv_versions <- function(folder = "output", 
                                        prefix = NULL, 
                                        id_col = "id", 
                                        output_excel = NULL) {
  # List matching CSV files
  files <- list.files(folder, pattern = paste0("^", prefix, "_\\d{8}_\\d{4}\\.csv$"), full.names = TRUE)
  
  # Sort files by timestamp
  files_sorted <- files[order(files, decreasing = TRUE)]
  
  if (length(files_sorted) < 2) {
    stop("❌ Not enough versions found to compare (need at least 2).")
  }
  
  file_new <- files_sorted[1]
  file_old <- files_sorted[2]
  
  cat("🔍 Comparing:\n")
  cat("  🆕 New file: ", file_new, "\n")
  cat("  🪧 Old file: ", file_old, "\n\n")
  
  df_new <- read_csv(file_new, show_col_types = FALSE) %>% janitor::clean_names()
  df_old <- read_csv(file_old, show_col_types = FALSE) %>% janitor::clean_names()
  
  # Compare shapes
  cat("📐 Rows & columns:\n")
  cat("  ➤ Old: ", nrow(df_old), "rows,", ncol(df_old), "columns\n")
  cat("  ➤ New: ", nrow(df_new), "rows,", ncol(df_new), "columns\n\n")
  
  # Compare ID presence
  added_ids <- setdiff(df_new[[id_col]], df_old[[id_col]])
  removed_ids <- setdiff(df_old[[id_col]], df_new[[id_col]])
  
  cat("➕ Added IDs: ", length(added_ids), "\n")
  cat("➖ Removed IDs: ", length(removed_ids), "\n\n")
  
  # Join on ID
  joined <- inner_join(df_old, df_new, by = id_col, suffix = c("_old", "_new"))
  common_cols <- intersect(names(df_old), names(df_new)) |> setdiff(id_col)
  
  # Track value-level changes
  changed_values <- purrr::map_dfr(common_cols, function(col) {
    old_col <- paste0(col, "_old")
    new_col <- paste0(col, "_new")
    joined %>%
      filter(!is.na(.data[[old_col]]) & !is.na(.data[[new_col]]) & .data[[old_col]] != .data[[new_col]]) %>%
      select(!!id_col, all_of(old_col), all_of(new_col)) %>%
      mutate(column = col)
  })
  
  # Prepare Excel output
  # Always create Excel report
  df_added <- tibble(!!id_col := added_ids)
  df_removed <- tibble(!!id_col := removed_ids)
  
  write_xlsx(
    list(
      "Changed Values" = changed_values,
      "Added IDs" = df_added,
      "Removed IDs" = df_removed
    ),
    path = output_excel
  )
  
  if (nrow(changed_values) > 0 || length(added_ids) > 0 || length(removed_ids) > 0) {
    cat("🔄 Found", nrow(changed_values), "changed values across columns.\n")
    cat("📁 Report written to:", output_excel, "\n")
  } else {
    cat("✅ No changes found between files.\n")
    cat("📁 Empty report written to:", output_excel, "\n")
  }
  
  return(list(
    file_old = file_old,
    file_new = file_new,
    added_ids = added_ids,
    removed_ids = removed_ids,
    changed_values = changed_values
  ))
}


####Fuzzy match dictionary####
normalize_name <- function(name) {
  name %>%
    tolower() %>%
    gsub("school district", "", .) %>%
    gsub("elem", "elementary", .) %>%
    gsub("no\\.?|districts?|schools?|public|and", "", .) %>%
    gsub("[^a-z0-9 ]", "", .) %>%
    trimws()
}

acfr_as_missing_ncesID <- school_districts_all %>% 
  select(state.abb, state.name, name, id, ncesID) %>% 
  filter(is.na(ncesID)) %>% distinct() 

acfr_as_missing_ncesID_normalized <- acfr_as_missing_ncesID %>%
  mutate(name_clean = normalize_name(name))

nces_normalized <- nces %>%
  #padding a leading 0, ncesID should have 7 digit
  mutate(ncesID = sprintf("%07s", as.character(ncesID))) %>% 
  
  mutate(name_clean = normalize_name(name_nces))

# Match within state to improve accuracy ---
match_within_state <- function(state_df, nces_df) {
  dist_matrix <- stringdistmatrix(state_df$name_clean, nces_df$name_clean, method = "jw")
  best_match_index <- apply(dist_matrix, 1, which.min)
  
  matched_nces <- nces_df[best_match_index, ]
  
  state_df %>%
    mutate(
      filled_ncesID = nces_df$ncesID[best_match_index],
      matched_name_nces = nces_df$name_nces[best_match_index],
      state_agency_id = matched_nces$state_agency_id,
      county_nces = matched_nces$county_nces,
      match_score = mapply(function(i, j) dist_matrix[i, j], 
                           seq_along(best_match_index), best_match_index)
    )
}

# --- 5. Apply fuzzy match by state ---
matched_all_states <- acfr_as_missing_ncesID_normalized %>%
  group_split(state.abb) %>%
  purrr::map_dfr(function(state_group) {
    state_abbr <- unique(state_group$state.abb)
    
    nces_state <- filter(nces_normalized, state.abb == state_abbr)
    match_within_state(state_group, nces_state)
  })


# matched_all_states %>% 
#   mutate(ncesID = filled_ncesID) %>% 
#   select(state.abb, state.name, id, ncesID, name, name_clean, matched_name_nces, county_nces, match_score, state_agency_id) %>% 
#   #exclusing Montatna - need to treat separately because school district acfr  include mutiple school districts as define in NCES. 
#   # example: whitehall public schools id = 296174 includes both whitehall elem (ncesID 3027810) + whitehall h s (ncesID 3027840)
#   
#   filter(state.abb == "MT") %>% 
#   
#   arrange(match_score)
  #write_xlsx("tmp/sd_montana.xlsx")
  
  # write.csv("tmp/dictionary_fuzzy_match1.csv")
  
  
  # keep only new matches and strong ones ---
#   strong_matches <- matched_all_states %>%
#   filter(match_score < 0.15) 
# !is.na(ncesID) & 
#   #need to treat Montana separately
#   filter(state.abb != "MT") %>% 
#   
#   # fix some error
#   filter(name != "oakes public schools") %>% 
#   mutate(filled_ncesID = case_when(name == "fremont county school district no. 25"
#                                    ~ "5605220",
#                                    
#                                    TRUE ~ as.character(filled_ncesID))) %>% 
#   select(-ncesID) %>% 
#   rename(ncesID = filled_ncesID) %>% 
#   select(id, ncesID, state.abb, name)
