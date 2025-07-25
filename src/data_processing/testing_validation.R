library(tidyverse)
library(dplyr)
library(janitor)
library(writexl)
library(readxl)
source("src/data_processing/functions.R")
source("src/data_processing/cleaning_merging.R")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

####Tests 1####
#   (bonds_outstanding, leases,
#    loans_outstanding, notes_outstanding,
#    net_pension_liability, net_opeb_liability)  CAN'T BE GREATER THAN total_liabilities

#NOTE: we no longer collect leases
# can't include current_liabilities because they may include some debts already. 

test1 <- function(df, year) {
  result <- df %>% 
    filter(year == !!year) %>% 
    select(state.abb, name, id, year, population,
           bonds_outstanding, 
           loans_outstanding, notes_outstanding,
           net_pension_liability, net_opeb_liability,
           total_liabilities) %>% 
    mutate(across(c(bonds_outstanding,  
                    loans_outstanding, notes_outstanding,
                    net_pension_liability, net_opeb_liability),
                  ~ replace_na(., 0))) %>%
    mutate(debt_components = rowSums(across(c(bonds_outstanding,  
                                              loans_outstanding, notes_outstanding,
                                              net_pension_liability, net_opeb_liability)))) %>%
    # exclude cases where total_liabilities are not captured
    filter(total_liabilities != 0) %>% 
    
    #filter failed cases
    filter(debt_components > total_liabilities)
  
  return(result)
}

# state
state <- do.call(rbind, lapply(2020:2023, function(year) test1(state_all, year)))


# county
test1_county <- do.call(rbind, lapply(2020:2023, function(year) test1(county_census_acfr_2023, year))) %>% 
  arrange(desc(total_liabilities))%>% 
  filter(state.abb != "NJ")

# municipality
test1_municipality <- do.call(rbind, lapply(2020:2023, function(year) test1(municipality_final_2023, year))) %>% 
  arrange(desc(total_liabilities))

# school 
school_districts <- school_districts_final_2023_ %>% rename(population = enrollment_23)

test1_school_districts <- do.call(rbind, lapply(2020:2023, function(year) test1(school_districts, year))) %>% 
  arrange(desc(total_liabilities)) 

# All years
test1_result_allyears <- rbind(test1_county, 
                      test1_municipality, test1_school_districts) 

test1_result_allyears %>% write.csv("tmp/test1_result_allyears.csv")

test1_2023 <- test1_result_allyears %>% filter(year == 2023) 
View(test1_2023)

test1_2023 %>% write.csv("tmp/test1.csv")

####Test 2####
# total_liabilities GREATER THAN total_assets
# total liabilities can sometimes be greater than total assets, but those cases are generally anomalies, 
# so it's worth paying attention to as it could indicate we got something wrong.
# 

test2 <- function(df,year){
  result <- df %>% 
    filter(year == !!year) %>% 
    select(state.abb, name, id, year, population,
           total_liabilities,
           total_assets
           ) %>% 
    filter(total_liabilities > total_assets) %>% 
    mutate(ratio= total_liabilities/total_assets) %>% 
    filter(ratio > 1.2)
  
  return(result)
}

## States
test2_state_all <- do.call(rbind, lapply(2020:2023, function(year) test2(state_all, year))) %>% 
  arrange(desc(total_liabilities))


## Counties
test2_county_all <- do.call(rbind, lapply(2023, function(year) test2(county_census_acfr_2023, year))) %>% 
  arrange(desc(total_liabilities))


## Municipalities
test2_municipality_all <- do.call(rbind, lapply(2023, function(year) test2(municipality_final_2023, year))) %>% 
  arrange(desc(total_liabilities))


## Schools
test2_school_districts_all <- do.call(rbind, lapply(2023, function(year) test2(school_districts, year))) %>% 
  arrange(desc(total_liabilities))


test2_2023 <- rbind(test2_state_all,
                    test2_county_all,
                    test2_municipality_all,
                    test2_school_districts_all) %>% filter(year == 2023)

View(test2_2023)
#test2_2023 %>% write.csv("tmp/test2.csv")

####Test 3####
# Are revenues +/- 20% of expenditures? Generally, a government will spend up to their revenues but they can't 
# spend significantly more than revenues. 
# Again, it's possible revenues could be +/- >20% of expenditures, but it's very unlikely.

test3 <- function(df,year){
  result <- df %>% 
    filter(year == !!year) %>% 
    select(state.abb, name, year, population,
           expenses, revenues
    ) %>% 
    mutate(
      expenses = as.numeric(gsub("[^0-9.-]", "", expenses)),
      revenues = as.numeric(gsub("[^0-9.-]", "", revenues))
    ) %>% 

    mutate(expenses_revenues_ratio = (expenses/revenues)) %>% 
    filter(expenses_revenues_ratio >= 1.2)
  
  return(result)
}


test3_state_all <- do.call(rbind, lapply(2020:2023, function(year) test3(state_all, year))) 
View(test3_state_all)

test3_county_all <- do.call(rbind, lapply(2020:2023, function(year) test3(county_census_acfr_2023, year))) 


test3_municipality_all <- do.call(rbind, lapply(2020:2023, function(year) test3(municipality_all, year))) 


test3_school_districts_all <- do.call(rbind, lapply(2020:2023, function(year) test3(school_districts, year))) 


test3_2023 <- rbind(test3_state_all,
                    test3_county_all,
                    test3_municipality_all,
                    test3_school_districts_all) %>% filter(year == 2023)

test3_2023 %>% View()

# writexl::write_xlsx(
#   list(
#     "test1_2023" = test1_2023,
#     "test2_2023" = test2_2023,
#     "test3_2023" = test3_2023
#   ),
#   path = "tmp/test_1_2_3.xlsx"
# )


#test3_2023 %>% write.csv("tmp/test3.csv")

####Test4####
#Calculate percentage changes between years in all data fields and flag out big values. 
columns <- c("total_liabilities", "current_liabilities",
                    "net_pension_liability", "net_pension_assets",
                    "net_opeb_liability", "net_opeb_assets", 
                    "total_assets", "current_assets", "compensated_absences",
                    "expenses", "revenues",
                    "unrestricted",
                    "bonds_outstanding", "loans_outstanding", "notes_outstanding", 
                    "compensated_absences")


test_changes_single_column <- function(df, columns, threshold = 10) {
  
  df <- df %>%
    arrange(year, id) %>%
    group_by(id, state.abb, name)
  
  for (col in columns) {
    df <- df %>%
      mutate(
        !!paste0("change_ratio_", col) := {
          current_col <- as.numeric(.data[[col]])
          prev_col <- lag(current_col)
          safe_prev_col <- dplyr::if_else(is.na(prev_col) | prev_col == 0, NA_real_, prev_col)
          ratio_col <- round(current_col / safe_prev_col, 2)
          ratio_col[!is.finite(ratio_col)] <- NA
          ratio_col
        }
      )
  }
  
  df %>%
    ungroup() %>%
    filter(if_any(starts_with("change_ratio_"), ~ !is.na(.) & . != 0 & (. >= threshold | . <= 1/threshold))) %>%
    select(state.abb, state.name, id, year, name, all_of(columns), starts_with("change_ratio_"))
}



test_changes_single_column(state_all, "total_liabilities") 

test_changes_single_column(county_all, "total_liabilities") %>% View()

columns_to_test <- c(
  "bonds_outstanding", 
  "loans_outstanding", 
  "notes_outstanding",
  
  "net_pension_liability",
  "net_pension_assets",
  "net_opeb_liability",
  "net_opeb_assets",
  
  "total_liabilities",
  "current_liabilities",
  
  "total_assets",
  "current_assets",
  "compensated_absenses",
  
  "revenues",
  "expenses"
)


# Loop through each column and collect results in a list

## State
results_list_state <- lapply(columns_to_test, function(col) {
  test_changes_single_column(state_all, col)
})
results_list_state 
names(results_list_state) <- columns_to_test

# Write each result as a sheet in one Excel file
writexl::write_xlsx(results_list_state, path = "tmp/YOY_changes_10_states.xlsx")

# Save the file
writexl::write_xlsx(results_list_state, path = paste0("tmp/YOY_changes_10_states_", timestamp, ".xlsx"))
newfile_path <- paste0("tmp/YOY_changes_10_states_", timestamp, ".xlsx")


## County
results_list_county <- lapply(columns_to_test, function(col) {
  test_changes_single_column(county_all, col)
})

names(results_list_county) <- columns_to_test
# Write each result as a sheet in one Excel file
writexl::write_xlsx(results_list_county, path = "tmp/YOY_changes_10_counties.xlsx")

## Municipality
results_list_municipality <- lapply(columns_to_test, function(col) {
  test_changes_single_column(municipality_all, col)
})

names(results_list_municipality) <- columns_to_test
# Write each result as a sheet in one Excel file
writexl::write_xlsx(results_list_municipality, path = "tmp/YOY_changes_10_municipalities.xlsx")


## School
results_list_school <- lapply(columns_to_test, function(col) {
  test_changes_single_column(school_districts_all, col)
})

names(results_list_school) <- columns_to_test
# Write each result as a sheet in one Excel file
writexl::write_xlsx(results_list_school, path = "tmp/YOY_changes_10_school.xlsx")


compare_excel_files("tmp/YOY_changes_10_states.xlsx", newfile_path)

####Test 5 - per capital####

per_capita_columns <- c(
  "net_pension_liability_per_capita",
  "net_pension_assets_per_capita",
  "net_opeb_liability_per_capita",
  "net_opeb_assets_per_capita",
  "total_liabilities_per_capita",
  "current_liabilities_per_capita",
  "total_assets_per_capita",
  "current_assets_per_capita",
  "revenues_per_capita",
  "expenses_per_capita",
  "bonds_notes_loans_per_capita"
)

######state######
state_23_percapita <- state_all %>% 
  filter(year == 2023) %>% 
  select(state.abb, name, id, population,
         any_of(columns_to_test)) %>% 
  rowwise() %>% 
  mutate(bonds_notes_loans = sum(c_across(c(bonds_outstanding,
                                            notes_outstanding, 
                                            loans_outstanding)), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(across(any_of(columns_to_test), ~ .x /population, .names = "{.col}_per_capita")) %>% 
  mutate(bonds_notes_loans_per_capita = bonds_notes_loans / population) %>% 
  select(state.abb, name, id, contains("_per_capita")) %>% 
  select(-c(4:6)) 
#%>% write_xlsx("tmp/state_percapita_validation.xlsx")
 # View()
  
  
  # Create a named list of dataframes for each sheet
  state_sheets_list <- lapply(per_capita_columns, function(col) {
    state_23_percapita %>%
      select(state.abb, name, id, all_of(col))
  })
  
  # Set sheet names
  names(state_sheets_list) <- per_capita_columns
  
  write_xlsx(state_sheets_list, path = "tmp/state_per_capita_metrics.xlsx")

  
#####municipalities#####
  
  #muni_23_percapita <- municipality_all_2023 
  muni_23_percapita <- municipality_final_2023 %>% 
    select(state.abb, name, id, population,
           any_of(columns_to_test)) %>% 
    rowwise() %>% 
    mutate(bonds_notes_loans = sum(c_across(c(bonds_outstanding,
                                              notes_outstanding, 
                                              loans_outstanding)), na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(across(any_of(columns_to_test), ~ .x /population, .names = "{.col}_per_capita")) %>% 
    mutate(bonds_notes_loans_per_capita = bonds_notes_loans / population) %>% 
    select(state.abb, name, id, population, contains("_per_capita")) %>% 
    select(-c(5:7)) 
  #%>% write_xlsx("tmp/state_percapita_validation.xlsx")
  # View()
  
  per_capita_columns <- c(
    "net_pension_liability_per_capita",
    "net_pension_assets_per_capita",
    "net_opeb_liability_per_capita",
    "net_opeb_assets_per_capita",
    "total_liabilities_per_capita",
    "current_liabilities_per_capita",
    "total_assets_per_capita",
    "current_assets_per_capita",
    "revenues_per_capita",
    "expenses_per_capita",
    "bonds_notes_loans_per_capita"
  )
  
 # Filter for outliers and create a list of dataframes
  muni_sheets_list <- lapply(per_capita_columns, function(col) {
    x <- muni_23_percapita[[col]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    threshold <- q3 + 1.5 * iqr
    
    muni_23_percapita %>%
      filter(.data[[col]] >= threshold) %>%
      select(state.abb, name, id, population, all_of(col))
  })
  
  # Step 4: Set sheet names and export
  names(muni_sheets_list) <- per_capita_columns
  
  write_xlsx(muni_sheets_list, path = "tmp/muni_per_capita_outliers_with_population_Jul21.xlsx")
  
  
  
  #####counties#####
  
  county_23_percapita <- county_census_acfr_2023 %>% 
    select(state.abb, name, id, population,
           any_of(columns_to_test)) %>% 
    rowwise() %>% 
    mutate(bonds_notes_loans = sum(c_across(c(bonds_outstanding,
                                              notes_outstanding, 
                                              loans_outstanding)), na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(across(any_of(columns_to_test), ~ .x /population, .names = "{.col}_per_capita")) %>% 
    mutate(bonds_notes_loans_per_capita = bonds_notes_loans / population) %>% 
    select(state.abb, name, id, population, contains("_per_capita")) %>% 
    select(-c(5:7)) 

  
  per_capita_columns <- c(
    "net_pension_liability_per_capita",
    "net_pension_assets_per_capita",
    "net_opeb_liability_per_capita",
    "net_opeb_assets_per_capita",
    "total_liabilities_per_capita",
    "current_liabilities_per_capita",
    "total_assets_per_capita",
    "current_assets_per_capita",
    "revenues_per_capita",
    "expenses_per_capita",
    "bonds_notes_loans_per_capita"
  )
  
  # Filter for outliers and create a list of dataframes
  county_sheets_list <- lapply(per_capita_columns, function(col) {
    x <- county_23_percapita[[col]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    threshold <- q3 + 1.5 * iqr
    
    county_23_percapita %>%
      filter(.data[[col]] >= threshold) %>%
      select(state.abb, name, id, population, all_of(col))
  })
  
  # Step 4: Set sheet names and export
  names(county_sheets_list) <- per_capita_columns
  
  write_xlsx(county_sheets_list, path = "tmp/county_per_capita_outliers.xlsx")
  
  
  
  
######schools#####
  
  school_23_percapita <- school_districts_final_2023_ %>% 
    rename(population = enrollment_23) %>% 
    select(state.abb, name, id, ncesID, population,
           any_of(columns_to_test)) %>% 
    rowwise() %>% 
    mutate(bonds_notes_loans = sum(c_across(c(bonds_outstanding,
                                              notes_outstanding, 
                                              loans_outstanding)), na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(across(any_of(columns_to_test), ~ .x /population, .names = "{.col}_per_capita")) %>% 
    mutate(bonds_notes_loans_per_capita = bonds_notes_loans / population) %>% 
    select(state.abb, name, id, ncesID, population, contains("_per_capita")) %>% 
    select(-c(bonds_outstanding_per_capita, 
              loans_outstanding_per_capita,
              notes_outstanding_per_capita )) 
  
  
  per_capita_columns <- c(
    "net_pension_liability_per_capita",
    "net_pension_assets_per_capita",
    "net_opeb_liability_per_capita",
    "net_opeb_assets_per_capita",
    "total_liabilities_per_capita",
    "current_liabilities_per_capita",
    "total_assets_per_capita",
    "current_assets_per_capita",
    "revenues_per_capita",
    "expenses_per_capita",
    "bonds_notes_loans_per_capita"
  )
  
  # Filter for outliers and create a list of dataframes
  school_sheets_list <- lapply(per_capita_columns, function(col) {
    x <- school_23_percapita[[col]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    threshold <- q3 + 1.5 * iqr
    
    school_23_percapita %>%
      filter(.data[[col]] >= threshold) %>%
      select(state.abb, name, id, ncesID, population, all_of(col))
  })
  
  # Step 4: Set sheet names and export
  names(school_sheets_list) <- per_capita_columns
  
  write_xlsx(school_sheets_list, path = "tmp/school_per_capita_outliers_ncesID_July22.xlsx")
  
  
  