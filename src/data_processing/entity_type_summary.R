state_data_final
county_data_final
municipal_data_final
school_district_data_final
# Create entity type summary - using a safer approach with explicit checks
# Function to safely summarize numeric columns
library(dplyr)

# Safe sum
safe_sum <- function(data, column_name) {
  if (column_name %in% names(data)) {
    return(sum(data[[column_name]], na.rm = TRUE))
  } else {
    return(NA)
  }
}

# Safe ratio
safe_ratio <- function(data, numerator, denominator) {
  num_sum <- safe_sum(data, numerator)
  denom_sum <- safe_sum(data, denominator)
  if (!is.na(num_sum) && !is.na(denom_sum) && denom_sum != 0) {
    return(round(num_sum / denom_sum, 3))
  } else {
    return(NA)
  }
}

# Generalized summary function
entity_summary <- function(data, entity_type) {
  data |> 
    summarize(
      entity_type = entity_type,
      count = n(),
      total_population = safe_sum(data, "population"),
      total_assets = safe_sum(data, "total_assets"),
      total_current_assets = safe_sum(data, "current_assets"),
      total_liabilities = safe_sum(data, "total_liabilities"),
      total_current_liabilities = safe_sum(data, "current_liabilities"),
      total_revenues = safe_sum(data, "total_revenues"),
      total_expenses = safe_sum(data, "total_expenses"),
      total_pension_liability = safe_sum(data, "pension_liability"),
      total_opeb_liability = safe_sum(data, "opeb_liability"),
      total_bonds_outstanding = safe_sum(data, "bonds_outstanding"),
      total_loans_outstanding = safe_sum(data, "loans_outstanding"),
      total_notes_outstanding = safe_sum(data, "notes_outstanding"),
      total_bond_loans_notes = safe_sum(data, "bond_loans_notes"),
      net_position = safe_sum(data, "total_assets") - safe_sum(data, "total_liabilities"),
      debt_ratio = safe_ratio(data, "total_liabilities", "total_assets"),
      free_cash_flow = safe_sum(data, "total_revenues") - 
        (safe_sum(data, "total_expenses") + safe_sum(data, "current_liabilities"))
    )
}

state_summary <- entity_summary(state_data_final, "State")



# Combine all summaries
entity_type_summary <- bind_rows(
  state_summary,
  county_summary,
  municipal_summary,
  school_district_summary
)

# Create the overall summary object
overall_summary <- list(
  overall_summary = entity_type_summary
)

# Convert to JSON
entity_type_summary_json <- toJSON(overall_summary, pretty = TRUE, auto_unbox = TRUE)
entity_type_summary_json <- paste0("export default ", entity_type_summary_json)
write(entity_type_summary_json, "output/summary_data.js")

