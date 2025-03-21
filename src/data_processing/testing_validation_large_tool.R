library(tidyverse)
library(dplyr)
library(janitor)
source("src/data_processing/cleaning_merging.R")

####Aggregated - large tool####
acfrs_data <- readRDS("data/acfrs_data.RDS")
nrow(acfrs_data %>% filter(year == 2023))
nrow(acfrs_data)

columns_to_check <- c("state.abb",              "state.name" ,           "id"    ,               
                            "year"        ,          "name"  ,               
                      "category"      ,        "total_liabilities"   ,  "current_liabilities"  ,
                      "net_pension_liability", "net_pension_assets" ,   "net_opeb_liability"  , 
                       "net_opeb_assets"   ,    "total_assets"   ,       "current_assets"    ,   
                       "expenses"       ,       "revenues"  ,           
                      "unrestricted"  ,        "bonds_outstanding"   ,  "loans_outstanding" ,   
                      "notes_outstanding"   ,  "population"              
                       )

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
  mutate(across(7: ncol(.), as.numeric))

# total acfrs
nrow(large_tool_data)

# number of acfrs in each category
nrow(state_all %>% filter(year == 2023))
nrow(county_all %>% filter(year == 2023))
nrow(municipality_all %>% filter(year == 2023))
nrow(school_districts_all%>% filter(year == 2023))

# other entities DO NOT belong to above 4 categories
other_entities <- acfrs_data %>% filter(year == 2023) %>% 
  filter(!id %in% state_all$id) %>% 
  filter(!id %in% county_all$id) %>% 
  filter(!id %in% municipality_all$id) %>%  
  filter(!id %in% school_districts_all$id)

# Aggregated statistics - in TRILLION unit

balance_sheet <- large_tool_data %>% 
  
  summarise(tot_total_assets = sum(total_assets, na.rm = TRUE)/1e12,
            tot_current_assets = sum(current_assets, na.rm = TRUE)/1e12,
            tot_total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
            net_position = tot_total_assets - tot_total_liabilities,
            
            # net-net pension liability
            NPL = sum(net_pension_liability, na.rm = TRUE)/1e12,
            NPS = sum(net_pension_assets, na.rm = TRUE)/1e12,
            tot_net_pension_liability = NPL - NPS,
            
            #net-net OPEB liability - BILLION 
            OPEB_L = sum(net_opeb_liability, na.rm = TRUE)/1e9,
            OPEB_A = sum(net_pension_assets, na.rm = TRUE)/1e9,
            tot_net_OPEB_liability = OPEB_L - OPEB_A,
            
            #current liabilities
            
            tot_current_liabilities = sum(current_liabilities, na.rm = TRUE)/1e12)

income_statement <- large_tool_data %>% 
  summarise(tot_revenues = sum(revenues, na.rm = TRUE)/1e12,
         tot_expense = sum(expenses, na.rm = TRUE)/1e12)


#financial_analysis 
round(balance_sheet$tot_total_liabilities/ balance_sheet$tot_total_assets*100,2)

# current ratio
round(balance_sheet$tot_current_assets/balance_sheet$tot_current_liabilities, 2)

#free cash flow -BILLION
round(
  (income_statement$tot_revenues - (income_statement$tot_expense +
                                     balance_sheet$tot_current_liabilities)
  ),2                                   
)*1000

####By entity type####

large_tool_data %>% 
  group_by(category) %>% 
  summarise(total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
            total_assets = sum(total_assets, na.rm = TRUE)/1e12,
            net_position = total_assets - total_liabilities)

####Top 10 States Agg####
top10_state_aggregates <- large_tool_data %>% 
  # have not released ACFRs 2023 or DOES NOT count
  filter(!state.abb %in% c("DC", "NV", "IL")) %>%
  
  group_by(state.abb) %>% 
  summarise(total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
            total_assets = sum(total_assets, na.rm = TRUE)/1e12,
           
             net_position = total_assets - total_liabilities,
            population = sum(population[category == "state"]),
            
            #per Cap 
            #total_liabilities_perCap = total_liabilities/population*1e12,
            net_position_perCap = net_position/population*1e12
            #total_assets_perCap = total_assets/population*1e12
            ) %>% 
  
 arrange(desc(net_position)) %>% 
  slice(1:10)
  
####Top 10 States####
top10_states <- large_tool_data %>% 
  filter(year == 2023 & category == "state") %>% 
  filter(!state.abb %in% c("DC", "NV", "IL")) %>%
  group_by(state.abb) %>% 
  summarise(total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
            total_assets = sum(total_assets, na.rm = TRUE)/1e12,
            
            #calculate net position
            net_position = total_assets - total_liabilities,
            population = sum(population[category == "state"]),
            
            #per Cap 
            net_position_perCap = net_position/population*1e12) %>% 
   arrange(desc(net_position)) %>% 
  slice(1:10)


top10_net_postion <- function(data, category) {
  data %>% 
  filter(year == 2023 & category == !!category) %>% 
    filter(!state.abb %in% c("DC", "NV", "IL")) %>%
    group_by(state.abb) %>% 
    summarise(total_liabilities = sum(total_liabilities, na.rm = TRUE)/1e12,
              total_assets = sum(total_assets, na.rm = TRUE)/1e12,
              
              #calculate net position
              net_position = total_assets - total_liabilities,
              population = sum(population[category == !!category], na.rm = TRUE),
              
              #per Cap 
              net_position_perCap = net_position/population*1e12) %>% 
    arrange(desc(net_position)) %>% 
    slice(1:10)
}


top10_state_net_postion <- top10_net_postion(large_tool_data, "state")
top10_county_net_position <- top10_net_postion(large_tool_data, "county")
top10_municipality_net_position <- top10_net_postion(large_tool_data, "municipality")
top10_school_district_net_position <- top10_net_postion(large_tool_data, "school district")
