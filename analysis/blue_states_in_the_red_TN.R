# Load necessary libraries
library(tidyverse)
library(Hmisc)

##NOTE: Review Jordan's script as of Aug 6, 2024.

# Read the data

partisan_lean_22 <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/partisan-lean/fivethirtyeight_partisan_lean_STATES.csv")
partisan_lean_21 <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/partisan-lean/2021/fivethirtyeight_partisan_lean_STATES.csv")

d_20 <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/partisan-lean/2020/fivethirtyeight_partisan_lean_STATES.csv")

convert_partisan_lean <- function(lean) {
  sign <- str_sub(lean, 1, 1)
  value <- as.numeric(str_sub(lean, 3))
  
  if (sign == "R") {
    return(-value)
  } else if (sign == "D") {
    return(value)
  } else {
    return(NA) # Handle unexpected format
  }
}

partisan_lean_20 <- d_20 %>%
  mutate(`2020` = sapply(`2020`, convert_partisan_lean))

partisan_lean_3years <- partisan_lean_20 %>% 
  left_join(partisan_lean_21) %>% 
  left_join(partisan_lean_22) %>% 
  pivot_longer(cols = 2:4, names_to = "year", values_to = "lean")
  

# Clean and prepare the data
states <- read_csv("output/all_states_3years.csv") %>% 
  mutate(
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = (revenues - (expenses + current_liabilities)) / population
  ) %>% 
  select(state.name, year, debt_ratio, free_cash_flow) %>% 
  mutate(year = as.character(year))
  
cor_data <- states %>% left_join(partisan_lean_3years, by = c("year", "state.name" = "state")) 

cor.test(cor_data$free_cash_flow, cor_data$lean, method = "pearson")
cor.test(cor_data$free_cash_flow, cor_data$lean, method = "spearman")
plot(cor_data$free_cash_flow, cor_data$lean)

cor.test(cor_data$debt_ratio, cor_data$lean, method = "pearson")
cor.test(cor_data$debt_ratio, cor_data$lean, method = "spearman")
#pooled data for 3 years, there's pearson correlation is not significant & and negative spearman correlation. 
# 2020 & 2021 not significant in peason cor; negative spearman cor
# 2021: no longer significant in spearman cor

############

# List of variables to analyze
variables <- c("partisan_lean", "debt_ratio", "free_cash_flow", "median_hh_income_21", "pct_urban_pop")

# Calculate correlations for all years combined
data_combined <- states %>%
  select(all_of(variables)) %>%
  filter(complete.cases(.))  # Ensure no NA values

pearson_corr_combined <- rcorr(as.matrix(data_combined), type = "pearson")
spearman_corr_combined <- rcorr(as.matrix(data_combined), type = "spearman")

# Function to create a readable summary for combined years
create_summary_combined <- function(correlations, method) {
  corr_matrix <- correlations$r
  p_values <- correlations$P
  
  pairs <- list(
    c("partisan_lean", "debt_ratio"),
    c("partisan_lean", "free_cash_flow"),
    c("median_hh_income_21", "debt_ratio"),
    c("median_hh_income_21", "free_cash_flow"),
    c("pct_urban_pop", "debt_ratio"),
    c("pct_urban_pop", "free_cash_flow")
  )
  
  summary_list <- list()
  
  for (pair in pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    corr_value <- corr_matrix[var1, var2]
    p_value <- p_values[var1, var2]
    if (!is.na(corr_value) && !is.na(p_value)) {
      significance <- ifelse(p_value < 0.05, "significant", "not significant")
      summary_list[[paste(var1, var2, sep = " - ")]] <- paste("The", method, "correlation between", var1, "and", var2, "is", round(corr_value, 3), "and is", significance, "with a p-value of", round(p_value, 3), ".")
    } else {
      summary_list[[paste(var1, var2, sep = " - ")]] <- paste("The", method, "correlation between", var1, "and", var2, "is not available due to missing values.")
    }
  }
  
  return(summary_list)
}

# Create summaries for combined years
summary_combined_pearson <- create_summary_combined(pearson_corr_combined, "Pearson")
summary_combined_spearman <- create_summary_combined(spearman_corr_combined, "Spearman")

# Print summaries for combined years and methods
print_summary_list <- function(summary_list) {
  for (summary in summary_list) {
    cat(summary, "\n")
  }
}

cat("Combined Years Pearson Correlation Summary:\n")
print_summary_list(summary_combined_pearson)
cat("\nCombined Years Spearman Correlation Summary:\n")
print_summary_list(summary_combined_spearman)

# Function to calculate correlations for a given year
calculate_correlations <- function(data, year) {
  data_subset <- data %>%
    filter(year == !!year) %>%
    select(all_of(variables))
  
  pearson_corr <- rcorr(as.matrix(data_subset), type = "pearson")
  spearman_corr <- rcorr(as.matrix(data_subset), type = "spearman")
  
  list(
    year = year,
    pearson_corr = pearson_corr,
    spearman_corr = spearman_corr
  )
}

# Function to create a readable summary for individual years
create_summary <- function(correlations, method) {
  corr_matrix <- correlations[[paste0(tolower(method), "_corr")]]$r
  p_values <- correlations[[paste0(tolower(method), "_corr")]]$P
  
  pairs <- list(
    c("partisan_lean", "debt_ratio"),
    c("partisan_lean", "free_cash_flow"),
    c("median_hh_income_21", "debt_ratio"),
    c("median_hh_income_21", "free_cash_flow"),
    c("pct_urban_pop", "debt_ratio"),
    c("pct_urban_pop", "free_cash_flow")
  )
  
  summary_list <- list()
  
  for (pair in pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    corr_value <- corr_matrix[var1, var2]
    p_value <- p_values[var1, var2]
    if (!is.na(corr_value) && !is.na(p_value)) {
      significance <- ifelse(p_value < 0.05, "significant", "not significant")
      summary_list[[paste(var1, var2, sep = " - ")]] <- paste("The", method, "correlation between", var1, "and", var2, "in", correlations$year, "is", round(corr_value, 3), "and is", significance, "with a p-value of", round(p_value, 3), ".")
    } else {
      summary_list[[paste(var1, var2, sep = " - ")]] <- paste("The", method, "correlation between", var1, "and", var2, "in", correlations$year, "is not available due to missing values.")
    }
  }
  
  return(summary_list)
}

# Calculate correlations for 2020, 2021, and 2022
correlations_2020 <- calculate_correlations(states, 2020)
correlations_2021 <- calculate_correlations(states, 2021)
correlations_2022 <- calculate_correlations(states, 2022)


# Create summaries for 2020, 2021, and 2022
summary_2020_pearson <- create_summary(correlations_2020, "Pearson")
summary_2020_spearman <- create_summary(correlations_2020, "Spearman")
summary_2021_pearson <- create_summary(correlations_2021, "Pearson")
summary_2021_spearman <- create_summary(correlations_2021, "Spearman")
summary_2022_pearson <- create_summary(correlations_2022, "Pearson")
summary_2022_spearman <- create_summary(correlations_2022, "Spearman")

# Print summaries for each year and method
cat("2020 Pearson Correlation Summary:\n")
print_summary_list(summary_2020_pearson)
cat("\n2020 Spearman Correlation Summary:\n")
print_summary_list(summary_2020_spearman)

cat("\n2021 Pearson Correlation Summary:\n")
print_summary_list(summary_2021_pearson)
cat("\n2021 Spearman Correlation Summary:\n")
print_summary_list(summary_2021_spearman)

cat("\n2022 Pearson Correlation Summary:\n")
print_summary_list(summary_2022_pearson)
cat("\n2022 Spearman Correlation Summary:\n")
print_summary_list(summary_2022_spearman)

# Fact-check the specific paragraph for 2022
# States with debt ratio over 100%
high_debt_ratio_states <- states %>%
  filter(year == 2022) %>%
  filter(debt_ratio > 1) %>%
  select(state.name, partisan_lean, debt_ratio)

# Democratic-leaning states with debt ratio over 100%
dem_high_debt_ratio <- high_debt_ratio_states %>%
  filter(partisan_lean > 0)

# Republican-leaning states with debt ratio over 100%
rep_high_debt_ratio <- high_debt_ratio_states %>%
  filter(partisan_lean < 0)

# Specific states with high debt ratios
specific_states <- states %>%
  filter(year == 2022) %>%
  filter(state.name %in% c("Illinois", "New Jersey")) %>%
  select(state.name, debt_ratio)

# States with debt ratio under 50%
low_debt_ratio_states <- states %>%
  filter(year == 2022) %>%
  filter(debt_ratio < 0.5) %>%
  select(state.name, partisan_lean, debt_ratio)

# Dem states with debt ratio under 50%
deb_low_debt_ratio_states <- states %>%
  filter(year == 2022) %>%
  filter(debt_ratio < 0.5) %>%
  filter(partisan_lean > 0) %>%
  select(state.name, partisan_lean, debt_ratio)

# Republican-leaning states with debt ratio above 50%
rep_above_50_debt_ratio <- states %>%
  filter(year == 2022) %>%
  filter(partisan_lean < 0 & debt_ratio > 0.5) %>%
  select(state.name, debt_ratio)

# Print results for verification
cat("High Debt Ratio States (Debt Ratio > 100%) in 2022:\n")
print(high_debt_ratio_states)

cat("\nDemocratic-Leaning States with High Debt Ratio in 2022:\n")
print(dem_high_debt_ratio)

cat("\nRepublican-Leaning States with High Debt Ratio in 2022:\n")
print(rep_high_debt_ratio)

cat("\nSpecific States (Illinois, New Jersey) with High Debt Ratios in 2022:\n")
print(specific_states)

cat("\nLow Debt Ratio States (Debt Ratio < 50%) in 2022:\n")
print(low_debt_ratio_states)

cat("\nRepublican-Leaning States with Debt Ratio Above 50% in 2022:\n")
print(rep_above_50_debt_ratio)

cat("\nDemocratic-Leaning States with Debt Ratio Below 50% in 2022:\n")
print(deb_low_debt_ratio_states)


# Check if North Dakota was the only state to see an increase in debt ratio
all_states_debt_ratio <- states %>%
  filter(year %in% c(2020, 2022)) %>%
  select(state.name, year, debt_ratio) %>%
  spread(key = year, value = debt_ratio) %>%
  mutate(increase = `2022` > `2020`)

only_nd_increase <- all_states_debt_ratio %>%
  filter(increase == TRUE)

cat("\nStates with an increase in debt ratio from 2020 to 2022:\n")
print(only_nd_increase)

# Calculate Free Cash Flow correlation significance in 2022
free_cash_flow_corr_2022 <- correlations_2022$pearson_corr$r["free_cash_flow", ]
free_cash_flow_pval_2022 <- correlations_2022$pearson_corr$P["free_cash_flow", ]

free_cash_flow_correlation_significance <- data.frame(
  variable = names(free_cash_flow_corr_2022),
  correlation = free_cash_flow_corr_2022,
  p_value = free_cash_flow_pval_2022,
  significance = ifelse(free_cash_flow_pval_2022 < 0.05, "significant", "not significant")
)

cat("\nFree Cash Flow Correlation Significance in 2022:\n")
print(free_cash_flow_correlation_significance)

# Alaska and North Dakota per capita liabilities check
ak_nd_liabilities <- states %>%
  filter(state.name %in% c("Alaska", "North Dakota")) %>%
  mutate(current_liabilities_pc = current_liabilities/population) %>%
  select(state.name, year, current_liabilities_pc) %>%
  filter(year %in% c(2020, 2022))

cat("\nAlaska and North Dakota Per Capita Liabilities:\n")
print(ak_nd_liabilities)

# California per capita liabilities check
ca_liabilities <- states %>%
  filter(state.name == "California") %>%
  mutate(current_liabilities_pc = current_liabilities/population) %>%
  select(state.name, year, current_liabilities_pc) %>%
  filter(year %in% c(2020, 2022))

cat("\nCalifornia Per Capita Liabilities:\n")
print(ca_liabilities)

# Revenue declines between 2020 and 2022
all_states_revenue <- states %>%
  filter(year %in% c(2020, 2022)) %>%
  select(state.name, year, revenues) %>%
  spread(key = year, value = revenues) %>%
  mutate(decline = `2022` < `2020`)

revenue_declines <- all_states_revenue %>%
  filter(decline == TRUE)

cat("\nStates with Revenue Declines from 2020 to 2022:\n")
print(revenue_declines)

# Revenue declines between 2020 and 2022
revenue_declines <- states %>%
  filter(state.name %in% c("Alaska", "Michigan", "Wyoming")) %>%
  select(state.name, year, revenues) %>%
  filter(year %in% c(2020, 2022))

cat("\nRevenue Declines for Alaska, Michigan, and Wyoming:\n")
print(revenue_declines)

# Find and print the per capita and total revenues for 2020 and 2022 for Michigan, Alaska, and Wyoming
revenues_per_capita <- states %>%
  filter(state.name %in% c("Michigan", "Alaska", "Wyoming") & year %in% c(2020, 2022)) %>%
  mutate(revenue_per_capita = revenues / population) %>%
  select(state.name, year, revenues, revenue_per_capita)

cat("\nPer Capita and Total Revenues for Michigan, Alaska, and Wyoming in 2020 and 2022:\n")
print(revenues_per_capita)

# Save data for 2022
states_partisan_lean_debt_ratio_22 <- states %>%
  filter(year == 2022) %>%
  select(state.name, partisan_lean, debt_ratio)

write_csv(states_partisan_lean_debt_ratio_22, "states_partisan_lean_debt_ratio_22.csv")

# Extract and print debt ratios for 2020 and 2022 by state
debt_ratios_2020_2022 <- states %>%
  filter(year %in% c(2020, 2022)) %>%
  select(state.name, year, debt_ratio) %>%
  spread(key = year, value = debt_ratio) %>%
  rename(debt_ratio_2020 = `2020`, debt_ratio_2022 = `2022`) %>%
  mutate(difference = debt_ratio_2020 - debt_ratio_2022)

cat("\nDebt Ratios for 2020 and 2022 by State:\n")
print(debt_ratios_2020_2022)

# Save the debt ratios for 2020 and 2022 by state
write_csv(debt_ratios_2020_2022, "debt_ratios_2020_2022.csv")

