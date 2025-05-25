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
    median_household_income = median_hh_income_21
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
    median_household_income = median_hh_income_21
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
    median_household_income = median_hh_income_21
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
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                     ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                     ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Function to safely sum numeric columns
safe_sum <- function(data, column_name) {
  if (column_name %in% names(data)) {
    return(sum(data[[column_name]], na.rm = TRUE))
  } else {
    return(NA)
  }
}

# Calculate entity counts
state_count <- nrow(state_data)
county_count <- nrow(county_data)
municipality_count <- nrow(municipal_data)
school_district_count <- nrow(school_district_data)
total_count <- state_count + county_count + municipality_count + school_district_count

# Calculate population totals
general_population <- safe_sum(state_data, "population") 
student_enrollment <- safe_sum(school_district_data, "population") # This is enrollment_22

# Calculate financial totals
total_assets <- safe_sum(state_data, "total_assets") + 
                safe_sum(county_data, "total_assets") + 
                safe_sum(municipal_data, "total_assets") + 
                safe_sum(school_district_data, "total_assets")

total_current_assets <- safe_sum(state_data, "current_assets") + 
                        safe_sum(county_data, "current_assets") + 
                        safe_sum(municipal_data, "current_assets") + 
                        safe_sum(school_district_data, "current_assets")

total_liabilities <- safe_sum(state_data, "total_liabilities") + 
                     safe_sum(county_data, "total_liabilities") + 
                     safe_sum(municipal_data, "total_liabilities") + 
                     safe_sum(school_district_data, "total_liabilities")

total_current_liabilities <- safe_sum(state_data, "current_liabilities") + 
                             safe_sum(county_data, "current_liabilities") + 
                             safe_sum(municipal_data, "current_liabilities") + 
                             safe_sum(school_district_data, "current_liabilities")

total_pension_liability <- safe_sum(state_data, "pension_liability") + 
                           safe_sum(county_data, "pension_liability") + 
                           safe_sum(municipal_data, "pension_liability") + 
                           safe_sum(school_district_data, "pension_liability")

total_opeb_liability <- safe_sum(state_data, "opeb_liability") + 
                         safe_sum(county_data, "opeb_liability") + 
                         safe_sum(municipal_data, "opeb_liability") + 
                         safe_sum(school_district_data, "opeb_liability")

total_bonds_outstanding <- safe_sum(state_data, "bonds_outstanding") + 
                           safe_sum(county_data, "bonds_outstanding") + 
                           safe_sum(municipal_data, "bonds_outstanding") + 
                           safe_sum(school_district_data, "bonds_outstanding")

total_loans_outstanding <- safe_sum(state_data, "loans_outstanding") + 
                           safe_sum(county_data, "loans_outstanding") + 
                           safe_sum(municipal_data, "loans_outstanding") + 
                           safe_sum(school_district_data, "loans_outstanding")

total_notes_outstanding <- safe_sum(state_data, "notes_outstanding") + 
                           safe_sum(county_data, "notes_outstanding") + 
                           safe_sum(municipal_data, "notes_outstanding") + 
                           safe_sum(school_district_data, "notes_outstanding")

total_bond_loans_notes <- safe_sum(state_data, "bond_loans_notes") + 
                          safe_sum(county_data, "bond_loans_notes") + 
                          safe_sum(municipal_data, "bond_loans_notes") + 
                          safe_sum(school_district_data, "bond_loans_notes")

total_revenues <- safe_sum(state_data, "total_revenues") + 
                  safe_sum(county_data, "total_revenues") + 
                  safe_sum(municipal_data, "total_revenues") + 
                  safe_sum(school_district_data, "total_revenues")

total_expenses <- safe_sum(state_data, "total_expenses") + 
                  safe_sum(county_data, "total_expenses") + 
                  safe_sum(municipal_data, "total_expenses") + 
                  safe_sum(school_district_data, "total_expenses")

net_position <- total_assets - total_liabilities
debt_ratio <- round(total_liabilities / total_assets, 3)
free_cash_flow <- total_revenues - (total_expenses + total_current_liabilities)
current_ratio <- round(total_current_assets / total_current_liabilities, 2)

# Create the overall totals summary object
overall_totals <- list(
  entity_counts = list(
    states = state_count,
    counties = county_count,
    municipalities = municipality_count,
    school_districts = school_district_count,
    total = total_count
  ),
  total_population = list(
    general_population = general_population,
    student_enrollment = student_enrollment
  ),
  total_assets = total_assets,
  total_current_assets = total_current_assets,
  total_liabilities = total_liabilities,
  total_current_liabilities = total_current_liabilities,
  total_pension_liability = total_pension_liability,
  total_opeb_liability = total_opeb_liability,
  total_bonds_outstanding = total_bonds_outstanding,
  total_loans_outstanding = total_loans_outstanding,
  total_notes_outstanding = total_notes_outstanding,
  total_bond_loans_notes = total_bond_loans_notes,
  total_revenues = total_revenues,
  total_expenses = total_expenses,
  net_position = net_position,
  debt_ratio = debt_ratio,
  free_cash_flow = free_cash_flow,
  current_ratio = current_ratio
)

# Convert to JSON
overall_totals_json <- toJSON(overall_totals, pretty = TRUE, auto_unbox = TRUE)
overall_totals_json <- paste0("export default ", overall_totals_json)
write(overall_totals_json, "output/simplified_summary.js")
