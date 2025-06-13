library(tidyverse)
library(jsonlite)

# Read all data files
state_data <- read_csv("input/all_states_2020_2023.csv")
state_data <- state_data[, -1]

county_data <- read_csv("input/all_counties_2020_2023.csv")
county_data <- county_data[, -1]

municipal_data <- read_csv("input/all_municipalities_2020_2023.csv")
municipal_data <- municipal_data[, -1]

school_district_data <- read_csv("input/all_schooldistricts_2020_2023.csv")
school_district_data <- school_district_data[, -1]

# Process state data
state_data <- state_data |>
  filter(year == 2023) |>
  rename(
    state_abbr = state.abb,
    state_name = state.name,
    entity_id = id,
    document_url = url,
    entity_name = name,
    entity_type = category,
    total_revenues = revenues,
    total_expenses = expenses,
    urban_population = urban_pop,
    pct_urban_population = pct_urban_pop,
    median_household_income = median_hh_income
  ) |>
  # create net net pension and opeb liabilities
  mutate(
    net_pension_assets = ifelse(is.na(net_pension_assets), 0, net_pension_assets),
    net_opeb_assets = ifelse(is.na(net_opeb_assets), 0, net_opeb_assets),
    net_pension_liability = ifelse(is.na(net_pension_liability), 0, net_pension_liability),
    net_opeb_liability = ifelse(is.na(net_opeb_liability), 0, net_opeb_liability),
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets
  ) |>
  mutate(
    pension_liability = net_net_pension_liability,
    opeb_liability = net_net_opeb_liability
  ) |>
  # Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities,
    # Add sum of bonds, loans, and notes
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process county data
county_data <- county_data |>
  filter(year == 2023) |>
  rename(
    state_abbr = state.abb,
    state_name = state.name,
    entity_id = id,
    document_url = url,
    entity_name = name,
    entity_type = category,
    total_revenues = revenues,
    total_expenses = expenses,
    urban_population = urban_pop,
    pct_urban_population = pct_urban_pop,
    median_household_income = median_hh_income
  ) |>
  # create net net pension and opeb liabilities
  mutate(
    net_pension_assets = ifelse(is.na(net_pension_assets), 0, net_pension_assets),
    net_opeb_assets = ifelse(is.na(net_opeb_assets), 0, net_opeb_assets),
    net_pension_liability = ifelse(is.na(net_pension_liability), 0, net_pension_liability),
    net_opeb_liability = ifelse(is.na(net_opeb_liability), 0, net_opeb_liability),
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets
  ) |>
  mutate(
    pension_liability = net_net_pension_liability,
    opeb_liability = net_net_opeb_liability
  ) |>
  # Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities,
    # Add sum of bonds, loans, and notes
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process municipal data
municipal_data <- municipal_data |>
  filter(year == 2023) |>
  rename(
    state_abbr = state.abb,
    state_name = state.name,
    entity_id = id,
    document_url = url,
    entity_name = name,
    entity_type = category,
    total_revenues = revenues,
    total_expenses = expenses,
    median_household_income = median_hh_income
  ) |>
  # create net net pension and opeb liabilities
  mutate(
    net_pension_assets = ifelse(is.na(net_pension_assets), 0, net_pension_assets),
    net_opeb_assets = ifelse(is.na(net_opeb_assets), 0, net_opeb_assets),
    net_pension_liability = ifelse(is.na(net_pension_liability), 0, net_pension_liability),
    net_opeb_liability = ifelse(is.na(net_opeb_liability), 0, net_opeb_liability),
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets
  ) |>
  mutate(
    pension_liability = net_net_pension_liability,
    opeb_liability = net_net_opeb_liability
  ) |>
  # Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities,
    # Add sum of bonds, loans, and notes
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process school district data
school_district_data <- school_district_data |>
  filter(year == 2023) |>
  rename(
    state_abbr = state.abb,
    state_name = state.name,
    entity_id = id,
    document_url = url,
    entity_name = name,
    entity_type = category,
    total_revenues = revenues,
    total_expenses = expenses
  ) |>
  # create net net pension and opeb liabilities
  mutate(
    net_pension_assets = ifelse(is.na(net_pension_assets), 0, net_pension_assets),
    net_opeb_assets = ifelse(is.na(net_opeb_assets), 0, net_opeb_assets),
    net_pension_liability = ifelse(is.na(net_pension_liability), 0, net_pension_liability),
    net_opeb_liability = ifelse(is.na(net_opeb_liability), 0, net_opeb_liability),
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets
  ) |>
  mutate(
    pension_liability = net_net_pension_liability,
    opeb_liability = net_net_opeb_liability
  ) |>
  # Use enrollment_22 as population for school districts
  mutate(
    population = enrollment_22
  ) |>
  # Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities,
    # Add sum of bonds, loans, and notes
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Create entity type summary - using a safer approach with explicit checks
# Function to safely summarize numeric columns
safe_sum <- function(data, column_name) {
  if (column_name %in% names(data)) {
    return(sum(data[[column_name]], na.rm = TRUE))
  } else {
    return(NA)
  }
}

# Function to safely calculate ratios
safe_ratio <- function(data, numerator, denominator) {
  num_sum <- safe_sum(data, numerator)
  denom_sum <- safe_sum(data, denominator)
  
  if (!is.na(num_sum) && !is.na(denom_sum) && denom_sum != 0) {
    return(round(num_sum / denom_sum, 3))
  } else {
    return(NA)
  }
}

# Create summaries for each entity type
state_summary <- state_data |>
  summarize(
    entity_type = "State",
    count = n(),
    total_population = safe_sum(state_data, "population"),
    total_assets = safe_sum(state_data, "total_assets"),
    total_current_assets = safe_sum(state_data, "current_assets"),
    total_liabilities = safe_sum(state_data, "total_liabilities"),
    total_current_liabilities = safe_sum(state_data, "current_liabilities"),
    total_revenues = safe_sum(state_data, "total_revenues"),
    total_expenses = safe_sum(state_data, "total_expenses"),
    total_pension_liability = safe_sum(state_data, "pension_liability"),
    total_opeb_liability = safe_sum(state_data, "opeb_liability"),
    total_bonds_outstanding = safe_sum(state_data, "bonds_outstanding"),
    total_loans_outstanding = safe_sum(state_data, "loans_outstanding"),
    total_notes_outstanding = safe_sum(state_data, "notes_outstanding"),
    total_bond_loans_notes = safe_sum(state_data, "bond_loans_notes"),
    net_position = safe_sum(state_data, "total_assets") - safe_sum(state_data, "total_liabilities"),
    debt_ratio = safe_ratio(state_data, "total_liabilities", "total_assets"),
    free_cash_flow = safe_sum(state_data, "total_revenues") - (safe_sum(state_data, "total_expenses") + safe_sum(state_data, "current_liabilities"))
  )

county_summary <- county_data |>
  summarize(
    entity_type = "County",
    count = n(),
    total_population = safe_sum(county_data, "population"),
    total_assets = safe_sum(county_data, "total_assets"),
    total_current_assets = safe_sum(county_data, "current_assets"),
    total_liabilities = safe_sum(county_data, "total_liabilities"),
    total_current_liabilities = safe_sum(county_data, "current_liabilities"),
    total_revenues = safe_sum(county_data, "total_revenues"),
    total_expenses = safe_sum(county_data, "total_expenses"),
    total_pension_liability = safe_sum(county_data, "pension_liability"),
    total_opeb_liability = safe_sum(county_data, "opeb_liability"),
    total_bonds_outstanding = safe_sum(county_data, "bonds_outstanding"),
    total_loans_outstanding = safe_sum(county_data, "loans_outstanding"),
    total_notes_outstanding = safe_sum(county_data, "notes_outstanding"),
    total_bond_loans_notes = safe_sum(county_data, "bond_loans_notes"),
    net_position = safe_sum(county_data, "total_assets") - safe_sum(county_data, "total_liabilities"),
    debt_ratio = safe_ratio(county_data, "total_liabilities", "total_assets"),
    free_cash_flow = safe_sum(county_data, "total_revenues") - (safe_sum(county_data, "total_expenses") + safe_sum(county_data, "current_liabilities"))
  )

municipal_summary <- municipal_data |>
  summarize(
    entity_type = "Municipality",
    count = n(),
    total_population = safe_sum(municipal_data, "population"),
    total_assets = safe_sum(municipal_data, "total_assets"),
    total_current_assets = safe_sum(municipal_data, "current_assets"),
    total_liabilities = safe_sum(municipal_data, "total_liabilities"),
    total_current_liabilities = safe_sum(municipal_data, "current_liabilities"),
    total_revenues = safe_sum(municipal_data, "total_revenues"),
    total_expenses = safe_sum(municipal_data, "total_expenses"),
    total_pension_liability = safe_sum(municipal_data, "pension_liability"),
    total_opeb_liability = safe_sum(municipal_data, "opeb_liability"),
    total_bonds_outstanding = safe_sum(municipal_data, "bonds_outstanding"),
    total_loans_outstanding = safe_sum(municipal_data, "loans_outstanding"),
    total_notes_outstanding = safe_sum(municipal_data, "notes_outstanding"),
    total_bond_loans_notes = safe_sum(municipal_data, "bond_loans_notes"),
    net_position = safe_sum(municipal_data, "total_assets") - safe_sum(municipal_data, "total_liabilities"),
    debt_ratio = safe_ratio(municipal_data, "total_liabilities", "total_assets"),
    free_cash_flow = safe_sum(municipal_data, "total_revenues") - (safe_sum(municipal_data, "total_expenses") + safe_sum(municipal_data, "current_liabilities"))
  )

school_district_summary <- school_district_data |>
  summarize(
    entity_type = "School District",
    count = n(),
    total_population = safe_sum(school_district_data, "population"), # This will use enrollment_22 as we renamed it above
    total_assets = safe_sum(school_district_data, "total_assets"),
    total_current_assets = safe_sum(school_district_data, "current_assets"),
    total_liabilities = safe_sum(school_district_data, "total_liabilities"),
    total_current_liabilities = safe_sum(school_district_data, "current_liabilities"),
    total_revenues = safe_sum(school_district_data, "total_revenues"),
    total_expenses = safe_sum(school_district_data, "total_expenses"),
    total_pension_liability = safe_sum(school_district_data, "pension_liability"),
    total_opeb_liability = safe_sum(school_district_data, "opeb_liability"),
    total_bonds_outstanding = safe_sum(school_district_data, "bonds_outstanding"),
    total_loans_outstanding = safe_sum(school_district_data, "loans_outstanding"),
    total_notes_outstanding = safe_sum(school_district_data, "notes_outstanding"),
    total_bond_loans_notes = safe_sum(school_district_data, "bond_loans_notes"),
    net_position = safe_sum(school_district_data, "total_assets") - safe_sum(school_district_data, "total_liabilities"),
    debt_ratio = safe_ratio(school_district_data, "total_liabilities", "total_assets"),
    free_cash_flow = safe_sum(school_district_data, "total_revenues") - (safe_sum(school_district_data, "total_expenses") + safe_sum(school_district_data, "current_liabilities"))
  )

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
