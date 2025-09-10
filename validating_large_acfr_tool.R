library(tidyverse)
library(dplyr)
library(janitor)
library(writexl)

"all_counties_2023_20250721_1045.csv"
"all_counties_2023_20250903_1351.cs"
"all_municipalities_2023_20250721_1133.csv"
"all_schooldistricts_2023_20250703_1109.csv"
"all_schooldistricts_2023_20250722_2023.csv"

# Load required libraries
library(readr)
library(dplyr)

# Step 1: Read the two CSV files
df1 <- read_csv("output/all_schooldistricts_2023_20250722_2023.csv")
df2 <- read_csv("output/all_schooldistricts_2023_20250703_1109.csv")

# Step 2: Optional – drop any unnamed index columns
df1 <- df1 %>% select(-matches("^Unnamed"))
df2 <- df2 %>% select(-matches("^Unnamed"))

# Step 3: Convert all values to character to avoid type mismatch
df1_char <- df1 %>% mutate(across(everything(), as.character))
df2_char <- df2 %>% mutate(across(everything(), as.character))

# Step 4: Reorder columns alphabetically and sort rows
df1_sorted <- df1_char %>% select(sort(names(.))) %>% arrange(across(everything()))
df2_sorted <- df2_char %>% select(sort(names(.))) %>% arrange(across(everything()))

# Step 5: Compare content
if (identical(df1_sorted, df2_sorted)) {
  message("✅ The files are identical in content.")
} else {
  message("❌ The files are different in content.")
  
  # Step 6: Show differences
  df1_not_in_df2 <- anti_join(df1_sorted, df2_sorted)
  df2_not_in_df1 <- anti_join(df2_sorted, df1_sorted)
  
  # Summary
  cat("Rows in file 1 but not in file 2:", nrow(df1_not_in_df2), "\n")
  cat("Rows in file 2 but not in file 1:", nrow(df2_not_in_df1), "\n")
  
  # Step 7 (Optional): Write differences to CSV
  write_csv(df1_not_in_df2, "rows_only_in_file1.csv")
  write_csv(df2_not_in_df1, "rows_only_in_file2.csv")
}
