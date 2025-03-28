library(tidyverse)
library(dplyr)
library(janitor)
source("src/data_processing/cleaning_merging.R")

####Aggregated - large tool####
# acfrs_data <- readRDS("data/acfrs_data.RDS")
# nrow(acfrs_data %>% filter(year == 2023))
# nrow(acfrs_data)
columns_to_check <- c("state.abb",              "state.name" ,           "id"    ,               
                            "year"        ,          "name"  ,               
                      "category"      ,        "total_liabilities"   ,  "current_liabilities"  ,
                      "net_pension_liability", "net_pension_assets" ,   "net_opeb_liability"  , 
                       "net_opeb_assets"   ,    "total_assets"   ,       "current_assets"    ,   
                       "expenses"       ,       "revenues"  ,           
                      "unrestricted"  ,        "bonds_outstanding"   ,  "loans_outstanding" ,   
                      "notes_outstanding"   ,  "population"              
                       )


nrow(state_all)
nrow(county_all)
nrow(municipality_all)
nrow(school_districts_all)
us_population <- 333287557 # https://www.census.gov/newsroom/press-releases/2022/2022-population-estimates.html

large_tool_data <- rbind(
  #state
  state_all %>% select(all_of(columns_to_check)) %>% 
    mutate(category = "state"),
  
county_all %>% select(all_of(columns_to_check)) %>% 
  mutate(category = "county"),

municipality_all%>% select(all_of(columns_to_check)) %>% 
  mutate(category = "municipality"),

school_districts_all%>% 
  rename(population = enrollment_22) %>% 
  select(all_of(columns_to_check)) %>% 
  mutate(category = "school district")
) %>% 
  filter(year == 2023) %>% 
  mutate(across(7: ncol(.), as.numeric)) %>% 

  #TODO: quick and dirty fix population for IL and NV
  
    mutate(population = ifelse(state.abb == "IL", 12812545, population)) %>% 
    mutate(population = ifelse(state.abb == "NV", 3104624, population)) 

# total acfrs & each category
nrow(large_tool_data)

large_tool_data %>% 
  count(category)

#TODO: 
large_tool_data %>% 
  select(state.abb, id, name) %>% 
  filter(duplicated(id) | duplicated(id, fromLast = TRUE)) %>%
  View()

large_tool_data %>% filter(id == "32292")

# other entities DO NOT belong to above 4 categories
other_entities <- acfrs_data %>% filter(year == 2023) %>% 
  filter(!id %in% state_all$id) %>% 
  filter(!id %in% county_all$id) %>% 
  filter(!id %in% municipality_all$id) %>%  
  filter(!id %in% school_districts_all$id)

####Function####

calculate_summary_data <- function (data, population){
  data %>% 
  summarise(tot_total_assets = sum(total_assets, na.rm = TRUE)/1e12,
            tot_total_assets_perCap = sum(total_assets, na.rm = TRUE)/population,
            
            tot_current_assets = sum(current_assets, na.rm = TRUE)/1e12,
            tot_current_assets_perCap = sum(current_assets, na.rm = TRUE)/population,
            
            tot_total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
            tot_total_liabilities_perCap = sum(total_liabilities, na.rm = TRUE)/population,
            
            # net position 
            net_position = tot_total_assets - tot_total_liabilities,
            net_position_perCap = net_position*1e12/population,
            
            # net-net pension liability
            NPL = sum(net_pension_liability, na.rm = TRUE)/1e12,
            NPS = sum(net_pension_assets, na.rm = TRUE)/1e12,
            tot_net_pension_liability = NPL - NPS,
            tot_net_pension_liability_perCap = tot_net_pension_liability*1e12/population,
            
            #net-net OPEB liability - BILLION 
            OPEB_L = sum(net_opeb_liability, na.rm = TRUE)/1e9,
            OPEB_A = sum(net_opeb_assets, na.rm = TRUE)/1e9,
            tot_net_OPEB_liability = OPEB_L - OPEB_A,
            tot_net_OPEB_liability_perCap = tot_net_OPEB_liability*1e12/population,
            
            #loan, notes, bonds
            bonds_outstanding = sum(bonds_outstanding, na.rm = TRUE)/1e12,
            notes_outstanding = sum(notes_outstanding, na.rm = TRUE)/1e12,
            loans_outstanding = sum(loans_outstanding, na.rm = TRUE)/1e12,
            bonds_notes_loans = sum(bonds_outstanding + notes_outstanding + loans_outstanding),
            bonds_notes_loans_perCap = bonds_notes_loans*1e12/ population,
            
            #current liabilities
            tot_current_liabilities = sum(current_liabilities, na.rm = TRUE)/1e12,
            tot_current_liabilities_perCap = sum(current_liabilities, na.rm = TRUE)/population) 
}

# Aggregated statistics - in TRILLION unit

#####balance sheet####
balance_sheet <- large_tool_data %>% 
  
  summarise(tot_total_assets = sum(total_assets, na.rm = TRUE)/1e12,
            tot_total_assets_perCap = sum(total_assets, na.rm = TRUE)/us_population,
              
            tot_current_assets = sum(current_assets, na.rm = TRUE)/1e12,
            tot_current_assets_perCap = sum(current_assets, na.rm = TRUE)/us_population,
            
            tot_total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
            tot_total_liabilities_perCap = sum(total_liabilities, na.rm = TRUE)/us_population,
            
            # net position 
            net_position = tot_total_assets - tot_total_liabilities,
            net_position_perCap = net_position*1e12/us_population,
            
            # net-net pension liability
            NPL = sum(net_pension_liability, na.rm = TRUE)/1e12,
            NPS = sum(net_pension_assets, na.rm = TRUE)/1e12,
            tot_net_pension_liability = NPL - NPS,
            tot_net_pension_liability_perCap = tot_net_pension_liability*1e12/us_population,
              
            #net-net OPEB liability - BILLION 
            OPEB_L = sum(net_opeb_liability, na.rm = TRUE)/1e9,
            OPEB_A = sum(net_opeb_assets, na.rm = TRUE)/1e9,
            tot_net_OPEB_liability = OPEB_L - OPEB_A,
            tot_net_OPEB_liability_perCap = tot_net_OPEB_liability*1e12/us_population,
            
            #loan, notes, bonds
            bonds_outstanding = sum(bonds_outstanding, na.rm = TRUE)/1e12,
            notes_outstanding = sum(notes_outstanding, na.rm = TRUE)/1e12,
            loans_outstanding = sum(loans_outstanding, na.rm = TRUE)/1e12,
            bonds_notes_loans = sum(bonds_outstanding + notes_outstanding + loans_outstanding),
            bonds_notes_loans_perCap = bonds_notes_loans*1e12/ us_population,
            
            #current liabilities
            tot_current_liabilities = sum(current_liabilities, na.rm = TRUE)/1e12,
            tot_current_liabilities_perCap = sum(current_liabilities, na.rm = TRUE)/us_population) 


#####income statement####

income_statement <- large_tool_data %>% 
  summarise(tot_revenues = sum(revenues, na.rm = TRUE),
            tot_revenues_perCap = tot_revenues/us_population,
            
         tot_expense = sum(expenses, na.rm = TRUE),
         tot_expense_perCap = tot_expense/us_population,
         
         tot_current_liabilities = sum(current_liabilities, na.rm = TRUE),
         tot_current_liabilities_perCap = tot_current_liabilities/us_population
         ) %>% 
  mutate(free_cash_flow = (tot_revenues - (tot_expense + tot_current_liabilities)))

#####financial_analysis ####
# debt ratio
round(balance_sheet$tot_total_liabilities/ balance_sheet$tot_total_assets*100,2)

# current ratio
round(balance_sheet$tot_current_assets/balance_sheet$tot_current_liabilities, 2)

####By entity type####

by_entity_type <- large_tool_data %>% 
  group_by(category) %>% 
  summarise(total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
            total_assets = sum(total_assets, na.rm = TRUE)/1e12,
            current_liabilities = sum(current_liabilities, na.rm = TRUE)/1e12,
            current_assets = sum(current_assets, na.rm = TRUE)/1e12,
            .groups = "drop"
  ) %>%
  mutate(
    pct_total_liabilities = total_liabilities / sum(total_liabilities),
    pct_total_assets = total_assets / sum(total_assets),
    pct_current_liabilities = current_liabilities / sum(current_liabilities),
    pct_current_assets = current_assets / sum(current_assets)
  ) %>%
  # Add a "Total" row
  bind_rows(
    summarise(
      .,
      category = "Total",
      total_liabilities = sum(total_liabilities),
      total_assets = sum(total_assets),
      current_liabilities = sum(current_liabilities),
      current_assets = sum(current_assets)
    )
  ) 

####Top 10 States Agg####
top10_state_aggregates <- large_tool_data %>% 
  # have not released ACFRs 2023 or DOES NOT count
  #filter(!state.abb %in% c("DC", "NV", "IL")) %>%
  
  group_by(state.abb) %>% 
  summarise(total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
            total_assets = sum(total_assets, na.rm = TRUE)/1e12,
           
             net_position = total_assets - total_liabilities,
            population = sum(population[category == "state"])
            ) %>% 
  
 arrange(desc(total_liabilities)) %>% 

  #per Cap
  mutate( 
    total_liabilities_perCap = total_liabilities/population*1e12,
    total_assets_perCap = total_assets/population*1e12)
  
  
####Top 10 States####
top10_states <- large_tool_data %>% 
  filter(year == 2023 & category == "state") %>% 
  #filter(!state.abb %in% c("DC", "NV", "IL")) %>%
  group_by(state.abb) %>% 
  summarise(total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e9,
            total_assets = sum(total_assets, na.rm = TRUE)/1e9,
            current_liabilities = sum(current_liabilities, na.rm = TRUE)/1e9,
            current_assets = sum(current_assets, na.rm = TRUE)/1e9,
            
            population = sum(population[category == "state"]),
            
            #per Cap 
            total_liabilities_perCap = total_liabilities/population*1e9,
            total_assets_perCap = total_assets/population*1e9) 
 

top10_counties <- large_tool_data %>% 
  filter(year == 2023 & category == "county") %>% 
  select(name, state.abb, total_liabilities, current_liabilities,
         total_assets, current_assets, population)
 
top10_municipalities <- large_tool_data %>% 
  filter(year == 2023 & category == "municipality") %>% 
  select(name, state.abb, total_liabilities, current_liabilities,
         total_assets, current_assets, population)

top10_sd <- large_tool_data %>% 
  filter(year == 2023 & category == "school district") %>% 
  select(name, state.abb, total_liabilities, current_liabilities,
         total_assets, current_assets, population)


####MAP####

state_all %>% filter(year == 2023) %>% View()


large_tool_data %>% 
  filter(year == 2023 & category == "county") %>% 
  select(name, state.abb, total_liabilities, current_liabilities,
         total_assets, current_assets, population) %>% 
  summarise(total_liabilities = sum(total_liabilities, na.rm = TRUE))

county_2023 <- large_tool_data %>% 
  filter(year == 2023 & category == "county")

county_population <- large_tool_data %>% 
  filter(year == 2023 & category == "county") %>% 
  summarise(population = sum(population, na.rm = TRUE))

calculate_summary_data(large_tool_data, us_population) %>% View()
  
calculate_summary_data(county_2023, county_population)
