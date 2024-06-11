library(tidyverse)

acfrsData <- readRDS("data/acfrs_data.RDS")

# Check Alabama and Wyoming across the board
acfrsData <- acfrsData  |>
  # rename(`state.abb` = state) |>
  left_join(census_state) |>
  mutate(current_liabilities_pc = current_liabilities / population) |>
  mutate(debt_ratio = total_liabilities / total_assets) |>
  mutate(net_opeb_liability = net_opeb_liability - net_opeb_assets) |>
  mutate(net_opeb_liability_pc = net_opeb_liability / population) |>
  mutate(net_pension_liability = net_pension_liability - net_pension_assets) |>
  mutate(net_pension_liability_pc = net_pension_liability / population) |>
  mutate(expenses_pc = expenses / population) |>
  mutate(total_liabilities_pc = total_liabilities / population) |>
  mutate(current_assets_pc = current_assets / population) |>
  mutate(modified_revenues = revenues - (expenses + current_liabilities)) |>
  mutate(modified_revenues_pc = modified_revenues / population) |>
  mutate(revenues_pc = revenues / population) |>
  mutate(total_assets_pc = total_assets / population) |>
  select(
    year, name, `state.abb`,
    current_liabilities, current_liabilities_pc,
    debt_ratio,
    net_opeb_liability, net_opeb_liability_pc,
    net_pension_liability, net_pension_liability_pc,
    expenses, expenses_pc,
    total_liabilities, total_liabilities_pc,
    current_assets, current_assets_pc,
    modified_revenues, modified_revenues_pc,
    revenues, revenues_pc,
    total_assets, total_assets_pc,
    population
  )


alabama <- acfrsData |>
  filter(name == "state of alabama")

wyoming <- acfrsData |>
  filter(name == "state of wyoming")


# Get the average of states
states <- acfrsData |>
  filter(
    name == "district of columbia" |
      name == "state of alabama" |
      name == "state of alaska" |
      name == "state of arizona" |
      name == "state of arkansas" |
      name == "state of california" |
      name == "state of colorado" |
      name == "state of connecticut" |
      name == "state of delaware" |
      name == "state of florida" |
      name == "state of georgia" |
      name == "state of hawaii" |
      name == "state of idaho" |
      name == "state of illinois" |
      name == "state of indiana" |
      name == "state of iowa" |
      name == "state of kansas" |
      name == "commonwealth of kentucky" |
      name == "state of louisiana" |
      name == "state of maine" |
      name == "state of maryland" |
      name == "commonwealth of massachusetts" |
      name == "state of michigan" |
      name == "state of minnesota" |
      name == "state of mississippi" |
      name == "state of missouri" |
      name == "state of montana" |
      name == "state of nebraska" |
      name == "state of nevada" |
      name == "state of new hampshire" |
      name == "state of new jersey" |
      name == "state of new mexico" |
      name == "state of new york" |
      name == "state of north carolina" |
      name == "state of north dakota" |
      name == "state of ohio" |
      name == "state of oklahoma" |
      name == "state of oregon" |
      name == "commonwealth of pennsylvania" |
      name == "state of rhode island" |
      name == "state of south carolina" |
      name == "state of south dakota" |
      name == "state of tennessee" |
      name == "state of texas" |
      name == "state of utah" |
      name == "state of vermont" |
      name == "commonwealth of virginia" |
      name == "state of washington" |
      name == "state of west virginia" |
      name == "state of wisconsin" |
      name == "state of wyoming"
  ) |>
  arrange(name)


# Identify NA values
na_states <- states |>
  pivot_longer(cols = -c(year, name, `state.abb`), names_to = "variable", values_to = "value") |>
  filter(is.na(value)) |>
  select(year, name, variable)


# Count unique states per year
unique_states_per_year <- states |>
  group_by(year) |>
  summarise(unique_states = n_distinct(name))

average_states <- states |>
  # get average
  group_by(year) |>
  summarise(
    current_liabilities = sum(current_liabilities),
    debt_ratio = mean(debt_ratio),
    net_opeb_liability = sum(net_opeb_liability),
    net_pension_liability = sum(net_pension_liability),
    expenses = sum(expenses),
    total_liabilities = sum(total_liabilities),
    current_assets = sum(current_assets),
    modified_revenues = sum(modified_revenues),
    revenues = sum(revenues),
    total_assets = sum(total_assets),
    population = sum(population)
  ) |>
  mutate(
    current_liabilities_pc = current_liabilities / population,
    net_opeb_liability_pc = net_opeb_liability / population,
    net_pension_liability_pc = net_pension_liability / population,
    expenses_pc = expenses / population,
    total_liabilities_pc = total_liabilities / population,
    current_assets_pc = current_assets / population,
    modified_revenues_pc = modified_revenues / population,
    revenues_pc = revenues / population,
    total_assets_pc = total_assets / population
  )




# 
# Get the average of states
counties <- acfrsData |>
  filter(
    name == "jefferson county" & `state.abb` == "AL" |
      name == "maricopa county" & `state.abb` == "AZ" |
      name == "pima county" & `state.abb` == "AZ" |
      name == "alameda county" & `state.abb` == "CA" |
      name == "city and county of san francisco" |
      name == "contra costa county" & `state.abb` == "CA" |
      name == "fresno county" & `state.abb` == "CA" |
      name == "kern county" & `state.abb` == "CA" |
      name == "los angeles county" & `state.abb` == "CA" |
      name == "orange county" & `state.abb` == "CA" |
      name == "riverside county" & `state.abb` == "CA" |
      name == "sacramento county" & `state.abb` == "CA" |
      name == "san bernardino county" & `state.abb` == "CA" |
      name == "san diego county" & `state.abb` == "CA" |
      name == "san joaquin county" & `state.abb` == "CA" |
      name == "san mateo county" & `state.abb` == "CA" |
      name == "santa clara county" & `state.abb` == "CA" |
      name == "ventura county" & `state.abb` == "CA" |
      name == "arapahoe county" & `state.abb` == "CO" |
      name == "denver county" & `state.abb` == "CO" |
      name == "el paso county" & `state.abb` == "CO" |
      name == "brevard county" & `state.abb` == "FL" |
      name == "broward county" & `state.abb` == "FL" |
      name == "hillsborough county" & `state.abb` == "FL" |
      name == "jacksonville" & `state.abb` == "FL" |
      name == "lee county" & `state.abb` == "FL" |
      name == "miami-dade county" & `state.abb` == "FL" |
      name == "orange county" & `state.abb` == "FL" |
      name == "palm beach county" & `state.abb` == "FL" |
      name == "pinellas county" & `state.abb` == "FL" |
      name == "polk county" & `state.abb` == "FL" |
      name == "cobb county" & `state.abb` == "GA" |
      name == "dekalb county" & `state.abb` == "GA" |
      name == "fulton county" & `state.abb` == "GA" |
      name == "gwinnett county" & `state.abb` == "GA" |
      name == "city and county of honolulu" & `state.abb` == "HI" |
      name == "cook county" & `state.abb` == "IL" |
      name == "dupage county" & `state.abb` == "IL" |
      name == "lake county" & `state.abb` == "IL" |
      name == "will county" & `state.abb` == "IL" |
      name == "marion county" & `state.abb` == "IN" |
      name == "johnson county" & `state.abb` == "KS" |
      name == "jefferson county" & `state.abb` == "KY" |
      name == "baltimore county" & `state.abb` == "MD" |
      name == "montgomery county" & `state.abb` == "MD" |
      name == "prince georges county" & `state.abb` == "MD" |
      name == "norfolk county" & `state.abb` == "MA" |
      name == "kent county" & `state.abb` == "MI" |
      name == "macomb county" & `state.abb` == "MI" |
      name == "oakland county" & `state.abb` == "MI" |
      name == "hennepin county" & `state.abb` == "MN" |
      name == "jackson county" & `state.abb` == "MO" |
      name == "st. louis county" & `state.abb` == "MO" |
      name == "clark county" & `state.abb` == "NV" |
      name == "bergen county" & `state.abb` == "NJ" |
      name == "essex county" & `state.abb` == "NJ" |
      name == "hudson county" & `state.abb` == "NJ" |
      name == "middlesex county" & `state.abb` == "NJ" |
      name == "monmouth county" & `state.abb` == "NJ" |
      name == "ocean county" & `state.abb` == "NJ" |
      name == "bernalillo county" & `state.abb` == "NM" |
      name == "erie county" & `state.abb` == "NY" |
      name == "monroe county" & `state.abb` == "NY" |
      name == "nassau county" & `state.abb` == "NY" |
      name == "suffolk county" & `state.abb` == "NY" |
      name == "westchester county" & `state.abb` == "NY" |
      name == "mecklenburg county" & `state.abb` == "NC" |
      name == "wake county" & `state.abb` == "NC" |
      name == "cuyahoga county" & `state.abb` == "OH" |
      name == "franklin county" & `state.abb` == "OH" |
      name == "hamilton county" & `state.abb` == "OH" |
      name == "oklahoma county" & `state.abb` == "OK" |
      name == "tulsa county" & `state.abb` == "OK" |
      name == "multnomah county" & `state.abb` == "OR" |
      name == "allegheny county" & `state.abb` == "PA" |
      name == "bucks county" & `state.abb` == "PA" |
      name == "montgomery county" & `state.abb` == "PA" |
      name == "philadelphia" & `state.abb` == "PA" |
      name == "shelby county" & `state.abb` == "TN" |
      name == "the metropolitan government of nashville and davidson county" & `state.abb` == "TN" |
      name == "bexar county" & `state.abb` == "TX" |
      name == "collin county" & `state.abb` == "TX" |
      name == "dallas county" & `state.abb` == "TX" |
      name == "denton county" & `state.abb` == "TX" |
      name == "el paso county" & `state.abb` == "TX" |
      name == "fort bend county" & `state.abb` == "TX" |
      name == "harris county" & `state.abb` == "TX" |
      name == "hidalgo county" & `state.abb` == "TX" |
      name == "montgomery county" & `state.abb` == "TX" |
      name == "tarrant county" & `state.abb` == "TX" |
      name == "travis county" & `state.abb` == "TX" |
      name == "williamson county" & `state.abb` == "TX" |
      name == "salt lake county" & `state.abb` == "UT" |
      name == "utah county" & `state.abb` == "UT" |
      name == "fairfax county" & `state.abb` == "VA" |
      name == "king county" & `state.abb` == "WA" |
      name == "pierce county" & `state.abb` == "WA" |
      name == "snohomish county" & `state.abb` == "WA" |
      name == "milwaukee county" & `state.abb` == "WI" 
  )


# Find NA data
na_counties <- counties |>
  pivot_longer(cols = -c(year, name, `state.abb`), names_to = "variable") |>
  filter(is.na(value)) |>
  select(year, name, `state.abb`, variable) |>
  arrange(year, name, variable)

# Count unique states per year
unique_counties_per_year <- counties |>
  mutate(name = paste(name, `state.abb`)) |>
  group_by(year) |>
  summarise(unique_states = n_distinct(name))

# Create separate data frames for each year
counties_2020 <- counties %>%
  filter(year == 2020) %>%
  mutate(county = paste(name, state.abb, sep = "_")) %>%
  mutate(year_2020 = county) |>
  select(year_2020, county)
  

counties_2021 <- counties %>%
  filter(year == 2021) %>%
  mutate(county = paste(name, state.abb, sep = "_")) %>%
  mutate(year_2021 = county) |>
  select(year_2021, county)

counties_2022 <- counties %>%
  filter(year == 2022) %>%
  mutate(county = paste(name, state.abb, sep = "_")) %>%
  mutate(year_2022 = county) |>
  select(year_2022, county)


# Left join the separate data frames on the unique list of all counties
unique_counties <- counties_2020  %>%
  left_join(counties_2021, by = "county") %>%
  left_join(counties_2022, by = "county") |>
  select(year_2022, year_2021, year_2020)

write_csv(unique_counties, "unique_counties_by_year.csv")

unique(counties[,2:4])

average_counties <- counties |>
  # get average
  group_by(year) |>
  summarise(
    current_liabilities = sum(current_liabilities, na.rm = T),
    debt_ratio = mean(debt_ratio, na.rm = T),
    net_opeb_liability = sum(net_opeb_liability, na.rm = T),
    net_pension_liability = sum(net_pension_liability, na.rm = T),
    expenses = sum(expenses, na.rm = T),
    total_liabilities = sum(total_liabilities, na.rm = T),
    current_assets = sum(current_assets, na.rm = T),
    modified_revenues = sum(modified_revenues, na.rm = T),
    revenues = sum(revenues, na.rm = T),
    total_assets = sum(total_assets, na.rm = T),
    population = sum(population, na.rm = T)
  ) |>
  mutate(
    current_liabilities_pc = current_liabilities / population,
    net_opeb_liability_pc = net_opeb_liability / population,
    net_pension_liability_pc = net_pension_liability / population,
    expenses_pc = expenses / population,
    total_liabilities_pc = total_liabilities / population,
    current_assets_pc = current_assets / population,
    modified_revenues_pc = modified_revenues / population,
    revenues_pc = revenues / population,
    total_assets_pc = total_assets / population
  )




# Cities
# Get the average of states
cities <- acfrsData |>
  filter(
    name == "huntsville" & `state.abb` == "AL"  |
      name == "chandler" & `state.abb` == "AZ"  |  
      name == "glendale" & `state.abb` == "AZ"  |  
      name == "mesa" & `state.abb` == "AZ"  |  
      name == "phoenix" & `state.abb` == "AZ"  |  
      name == "scottsdale" & `state.abb` == "AZ"  |  
      name == "tucson" & `state.abb` == "AZ"  |  
      name == "anaheim" & `state.abb` == "CA"  |  
      name == "bakersfield" & `state.abb` == "CA"  |  
      name == "chula vista" & `state.abb` == "CA"  |  
      name == "city and county of san francisco" & `state.abb` == "CA"  |  
      name == "fremont" & `state.abb` == "CA"  |  
      name == "fresno" & `state.abb` == "CA"  |  
      name == "irvine" & `state.abb` == "CA"  |  
      name == "long beach" & `state.abb` == "CA"  |  
      name == "los angeles" & `state.abb` == "CA"  |  
      name == "modesto" & `state.abb` == "CA"  |  
      name == "moreno valley" & `state.abb` == "CA"  |  
      name == "oakland" & `state.abb` == "CA"  |  
      name == "riverside" & `state.abb` == "CA"  |  
      name == "sacramento" & `state.abb` == "CA"  |  
      name == "san bernardino" & `state.abb` == "CA"  |  
      name == "san diego" & `state.abb` == "CA"  |  
      name == "san jose" & `state.abb` == "CA"  |  
      name == "santa ana" & `state.abb` == "CA"  |  
      name == "santa clarita" & `state.abb` == "CA"  |  
      name == "stockton" & `state.abb` == "CA"  |  
      name == "aurora" & `state.abb` == "CO"  |  
      name == "colorado springs" & `state.abb` == "CO"  |  
      name == "denver county" & `state.abb` == "CO"  |  
      name == "hialeah" & `state.abb` == "FL"  |  
      name == "jacksonville" & `state.abb` == "FL"  |  
      name == "miami" & `state.abb` == "FL"  |  
      name == "orlando" & `state.abb` == "FL"  |  
      name == "st. petersburg" & `state.abb` == "FL"  |  
      name == "tampa" & `state.abb` == "FL"  |  
      name == "atlanta" & `state.abb` == "GA"  |  
      name == "boise" & `state.abb` == "ID"  |  
      name == "chicago" & `state.abb` == "IL"  |  
      name == "fort wayne" & `state.abb` == "IN"  |  
      name == "des moines" & `state.abb` == "IA"  |  
      name == "wichita" & `state.abb` == "KS"  |  
      name == "new orleans" & `state.abb` == "LA"  |  
      name == "baltimore" & `state.abb` == ""  |  
      name == "boston" & `state.abb` == "MA"  |  
      name == "baltimore" & `state.abb` == "MD" |
      name == "detroit" & `state.abb` == "MI"  |  
      name == "minneapolis" & `state.abb` == "MN"  |  
      name == "saint paul" & `state.abb` == "MN"  |  
      name == "kansas city" & `state.abb` == "MO"  |  
      name == "st. louis" & `state.abb` == "MO"  |  
      name == "lincoln" & `state.abb` == "NE"  |  
      name == "omaha" & `state.abb` == "NE"  |  
      name == "las vegas" & `state.abb` == "NV"  |  
      name == "north las vegas" & `state.abb` == "NV"  |  
      name == "reno" & `state.abb` == "NV"  |  
      name == "henderson" & `state.abb` == "NV" |
      name == "jersey city" & `state.abb` == "NJ"  |  
      name == "newark" & `state.abb` == "NJ"  |  
      name == "albuquerque" & `state.abb` == "NM"  |  
      name == "buffalo" & `state.abb` == "NY"  |  
      name == "new york" & `state.abb` == "NY"  |  
      name == "rochester" & `state.abb` == "NY"  |  
      name == "yonkers" & `state.abb` == "NY"  |  
      name == "charlotte" & `state.abb` == "NC"  |  
      name == "durham" & `state.abb` == "NC"  |  
      name == "greensboro" & `state.abb` == "NC"  |  
      name == "raleigh" & `state.abb` == "NC"  |  
      name == "winston-salem" & `state.abb` == "NC"  |  
      name == "cincinnati" & `state.abb` == "OH"  |  
      name == "cleveland" & `state.abb` == "OH"  |  
      name == "columbus" & `state.abb` == "OH"  |  
      name == "toledo" & `state.abb` == "OH"  |  
      name == "oklahoma city" & `state.abb` == "OK"  |  
      name == "tulsa" & `state.abb` == "OK"  |  
      name == "portland" & `state.abb` == "OR"  |  
      name == "philadelphia" & `state.abb` == "PA"  |  
      name == "pittsburgh" & `state.abb` == "PA"  |  
      name == "memphis" & `state.abb` == "TN"  |  
      name == "arlington" & `state.abb` == "TX"  |  
      name == "austin" & `state.abb` == "TX"  |  
      name == "corpus christi" & `state.abb` == "TX"  |  
      name == "dallas" & `state.abb` == "TX"  |  
      name == "el paso" & `state.abb` == "TX"  |  
      name == "fort worth" & `state.abb` == "TX"  |  
      name == "garland" & `state.abb` == "TX"  |  
      name == "houston" & `state.abb` == "TX"  |  
      name == "irving" & `state.abb` == "TX"  |  
      name == "laredo" & `state.abb` == "TX"  |  
      name == "lubbock" & `state.abb` == "TX"  |  
      name == "plano" & `state.abb` == "TX"  |  
      name == "san antonio" & `state.abb` == "TX"  |  
      name == "chesapeake" & `state.abb` == "VA"  |  
      name == "richmond" & `state.abb` == "VA"  |  
      name == "norfolk" & `state.abb` == "VA" |
      name == "virginia beach" & `state.abb` == "VA"  |  
      name == "seattle" & `state.abb` == "WA"  |  
      name == "spokane" & `state.abb` == "WA"  |  
      name == "tacoma" & `state.abb` == "WA"  |  
      name == "madison" & `state.abb` == "WI"  |  
      name == "milwaukee" & `state.abb` == "WI"
  )


# Find NA data
na_cities <- cities %>%
  pivot_longer(cols = -c(year, name, `state.abb`), names_to = "variable") %>%
  filter(is.na(value)) %>%
  select(year, name, `state.abb`, variable) %>%
  arrange(year, name, variable)

# Count unique states per year
unique_cities_per_year <- cities %>%
  mutate(name = paste(name, `state.abb`)) %>%
  group_by(year) %>%
  summarise(unique_states = n_distinct(name))

# Create separate data frames for each year
cities_2020 <- cities %>%
  filter(year == 2020) %>%
  mutate(city = paste(name, `state.abb`, sep = "_")) %>%
  mutate(year_2020 = city) %>%
  select(year_2020, city)

cities_2021 <- cities %>%
  filter(year == 2021) %>%
  mutate(city = paste(name, `state.abb`, sep = "_")) %>%
  mutate(year_2021 = city) %>%
  select(year_2021, city)

cities_2022 <- cities %>%
  filter(year == 2022) %>%
  mutate(city = paste(name, `state.abb`, sep = "_")) %>%
  mutate(year_2022 = city) %>%
  select(year_2022, city)

unique_cites <- cities_2020  %>%
  left_join(cities_2021, by = "city") %>%
  left_join(cities_2022, by = "city") |>
  select(year_2022, year_2021, year_2020)




# School districts

schools <- acfrsData %>%
  filter(
    (name == "mobile county board of school commissioners" & state.abb == "AL") |
           (name == "mesa unified school district no. 4" & state.abb == "AZ") |
           (name == "corona-norco unified school district" & state.abb == "CA") |
           (name == "elk grove unified school district" & state.abb == "CA") |
           (name == "fresno unified school district" & state.abb == "CA") |
           (name == "long beach unified school district" & state.abb == "CA") |
           (name == "los angeles unified school district" & state.abb == "CA") |
           (name == "san diego unified school district" & state.abb == "CA") |
           (name == "san francisco unified school district" & state.abb == "CA") |
           (name == "cherry creek school district no. 5" & state.abb == "CO") |
           (name == "city and county of denver school district no. 1" & state.abb == "CO") |
           (name == "douglas county school district re. 1" & state.abb == "CO") |
           (name == "jefferson county school district" & state.abb == "CO") |
           (name == "brevard district school board county" & state.abb == "FL") |
           (name == "collier county district school board" & state.abb == "FL") |
           (name == "district school board of pinellas county" & state.abb == "FL") |
           (name == "district school board of seminole county" & state.abb == "FL") |
           (name == "duval county public schools" & state.abb == "FL") |
           (name == "manatee county district school board" & state.abb == "FL") |
           (name == "orange county district school board" & state.abb == "FL") |
           (name == "osceola county district school board" & state.abb == "FL") |
           (name == "pasco county district school board" & state.abb == "FL") |
           (name == "school district of hillsborough countynty" & state.abb == "FL") |
           (name == "school district of polk county" & state.abb == "FL") |
           (name == "st. johns county school district" & state.abb == "FL") |
           (name == "the school board of broward county" & state.abb == "FL") |
           (name == "the school board of miami-dade county" & state.abb == "FL") |
           (name == "the school district of lee county" & state.abb == "FL") |
           (name == "the school district of palm beach county" & state.abb == "FL") |
           (name == "volusia county district school board" & state.abb == "FL") |
           (name == "clayton county board of education" & state.abb == "GA") |
           (name == "cobb county school district" & state.abb == "GA") |
           (name == "dekalb county board of education" & state.abb == "GA") |
           (name == "forsyth county board of education" & state.abb == "GA") |
           (name == "fulton county board of education" & state.abb == "GA") |
           (name == "gwinnett county board of education" & state.abb == "GA") |
           (name == "school district atlanta public schools" & state.abb == "GA") |
           (name == "hawaii department of education" & state.abb == "HI") |
           (name == "chicago board of education" & state.abb == "IL") |
           (name == "jefferson county board of education" & state.abb == "KY") |
           (name == "jefferson parish public school system" & state.abb == "LA") |
           (name == "boston public schools" & state.abb == "MA") |
           (name == "baltimore city public school system" & state.abb == "MD") |
           (name == "board of education of anne arundel county" & state.abb == "MD") |
           (name == "board of education of baltimore county" & state.abb == "MD") |
           (name == "howard county board of education" & state.abb == "MD") |
           (name == "prince george’s county public schools" & state.abb == "MD") |
           (name == "the board of education of montgomery county" & state.abb == "MD") |
           (name == "detroit public schools" & state.abb == "MI") |
           (name == "charlotte-mecklenburg board of education" & state.abb == "NC") |
           (name == "cumberland county board of education" & state.abb == "NC") |
           (name == "guilford county board of education" & state.abb == "NC") |
           (name == "wake county board of education" & state.abb == "NC") |
           (name == "winston-salem/forsyth county board of education" & state.abb == "NC") |
           (name == "douglas county school district no. 0001" & state.abb == "NE") |
           (name == "albuquerque municipal school district no. 12" & state.abb == "NM") |
           (name == "clark county school district" & state.abb == "NV") |
           (name == "washoe county school district" & state.abb == "NV") |
           (name == "new york city geographic district # 10" & state.abb == "NY") |
           (name == "new york city geographic district # 2" & state.abb == "NY") |
           (name == "new york city geographic district # 20" & state.abb == "NY") |
           (name == "new york city geographic district # 24" & state.abb == "NY") |
           (name == "new york city geographic district # 31" & state.abb == "NY") |
           (name == "school district of philadelphia" & state.abb == "PA") |
           (name == "charleston county school district" & state.abb == "SC") |
           (name == "the school district of greenville county" & state.abb == "SC") |
           (name == "board of education of shelby county" & state.abb == "TN") |
           (name == "knox county schools" & state.abb == "TN") |
           (name == "metropolitan nashville public schools" & state.abb == "TN") |
           (name == "rutherford county schools" & state.abb == "TN") |
           (name == "aldine independent school district" & state.abb == "TX") |
           (name == "arlington independent school district" & state.abb == "TX") |
           (name == "austin independent school district" & state.abb == "TX") |
           (name == "conroe independent school district" & state.abb == "TX") |
           (name == "cypress-fairbanks independent school district" & state.abb == "TX") |
           (name == "dallas independent school district" & state.abb == "TX") |
           (name == "el paso independent school district" & state.abb == "TX") |
           (name == "fort bend independent school district" & state.abb == "TX") |
           (name == "fort worth independent school district" & state.abb == "TX") |
           (name == "frisco independent school district" & state.abb == "TX") |
           (name == "garland independent school district" & state.abb == "TX") |
           (name == "houston independent school district" & state.abb == "TX") |
           (name == "humble independent school district" & state.abb == "TX") |
           (name == "katy independent school district" & state.abb == "TX") |
           (name == "klein independent school district" & state.abb == "TX") |
           (name == "lewisville independent school district" & state.abb == "TX") |
           (name == "north east independent school district" & state.abb == "TX") |
           (name == "northside independent school district" & state.abb == "TX") |
           (name == "pasadena independent school district" & state.abb == "TX") |
           (name == "plano independent school district" & state.abb == "TX") |
           (name == "round rock independent school district" & state.abb == "TX") |
           (name == "alpine school district" & state.abb == "UT") |
           (name == "davis school district" & state.abb == "UT") |
           (name == "granite school district" & state.abb == "UT") |
           (name == "jordan school district" & state.abb == "UT") |
           (name == "chesterfield county school board" & state.abb == "VA") |
           (name == "fairfax county public schools" & state.abb == "VA") |
           (name == "henrico county public schools" & state.abb == "VA") |
           (name == "loudoun county public schools" & state.abb == "VA") |
           (name == "prince william county public schools" & state.abb == "VA") |
           (name == "school board of the city of virginia beach" & state.abb == "VA") |
           (name == "seattle school district no. 1" & state.abb == "WA") |
           (name == "milwaukee public schools" & state.abb == "WI")
)


# Find NA data
na_schools <- schools %>%
  pivot_longer(cols = -c(year, name, `state.abb`), names_to = "variable") %>%
  filter(is.na(value)) %>%
  select(year, name, `state.abb`, variable) %>%
  arrange(year, name, variable)

# Count unique states per year
unique_schools_per_year <- schools %>%
  mutate(name = paste(name, `state.abb`)) %>%
  group_by(year) %>%
  summarise(unique_states = n_distinct(name))

# Create separate data frames for each year
schools_2020 <- schools %>%
  filter(year == 2020) %>%
  mutate(school = paste(name, `state.abb`, sep = "_")) %>%
  mutate(year_2020 = school) %>%
  select(year_2020, school)

schools_2021 <- schools %>%
  filter(year == 2021) %>%
  mutate(school = paste(name, `state.abb`, sep = "_")) %>%
  mutate(year_2021 = school) %>%
  select(year_2021, school)

schools_2022 <- schools %>%
  filter(year == 2022) %>%
  mutate(school = paste(name, `state.abb`, sep = "_")) %>%
  mutate(year_2022 = school) %>%
  select(year_2022, school)

unique_schools <- schools_2020  %>%
  left_join(schools_2021, by = "school") %>%
  left_join(schools_2022, by = "school") |>
  select(year_2022, year_2021, year_2020)





                                 