library(tidyverse)
library(dplyr)
library(janitor)
source("src/data_processing/functions.R")
source("src/data_processing/cleaning_merging.R")

####Tests 1####
#   (bonds_outstanding, leases,
#    loans_outstanding, notes_outstanding,
#    net_pension_liability, net_opeb_liability)  CAN'T BE GREATER THAN total_liabilities

#NOTE: we no longer collect leases
# can't include current_liabilities because they may include some debts already. 

test1 <- function(df,year){
result <- df %>% 
  filter(year == !!year) %>% 
  select(state.abb, name, id, year,
         bonds_outstanding, 
         loans_outstanding, notes_outstanding,
         net_pension_liability, net_opeb_liability,
         total_liabilities) %>% 
 mutate(debt_components = rowSums(across(c(bonds_outstanding,  
                                           loans_outstanding, notes_outstanding,
                                           net_pension_liability, net_opeb_liability)), 
                                  na.rm = TRUE)) %>% 
    filter(debt_components > total_liabilities)
  
  return(result)
}

state <- do.call(rbind, lapply(2020:2023, function(year) test1(state_all, year)))
View(state)

test1_general_purpose <- do.call(rbind, lapply(2023, function(year) test1(acfrs_general_purpose, year))) %>% 
  arrange(desc(total_liabilities))%>% filter(state.abb != "NJ")
View(test1_general_purpose)

test1_general_purpose_allyears <- do.call(rbind, lapply(2020:2023, function(year) test1(acfrs_general_purpose, year))) %>% 
  arrange(desc(total_liabilities))%>% 
  filter(state.abb != "NJ")

#View(test1_general_purpose_allyears)
#test1_general_purpose_allyears %>% writexl::write_xlsx("tmp/test1_general_purpose_allyears.xlsx")

test1_school_districts_23 <- do.call(rbind, lapply(2023, function(year) test1(school_districts_all, year))) %>% 
  arrange(desc(total_liabilities)) 
View(test1_school_districts_23)

test1_school_districts_allyears <- do.call(rbind, lapply(2020:2023, function(year) test1(school_districts_all, year))) %>% 
  arrange(desc(total_liabilities)) 


test1 <- rbind((test1_general_purpose %>% add_column(category = "general purpose")),
               (test1_school_districts_23 %>% add_column(category = "school districts")))

test1 %>% View()
test1 %>% write.csv("tmp/test1.csv")


#View(test1_school_districts_allyears)
#test1_school_districts_allyears %>% writexl::write_xlsx("tmp/test1_school_districts_allyears.xlsx")

####Test 2####
# Test 2: total_liabilities CAN'T BE GREATER THAN total_assets
# total liabilities can sometimes be greater than total assets, but those cases are generally anomalies, 
# so it's worth paying attention to as it could indicate we got something wrong.
# 

test2 <- function(df,year){
  result <- df %>% 
    filter(year == !!year) %>% 
    select(state.abb, name, year,
           total_liabilities,
           total_assets
           ) %>% 
    filter(total_liabilities > total_assets)
  
  return(result)
}

test2_general_purpose <- do.call(rbind, lapply(2023, function(year) test2(acfrs_general_purpose, year))) %>% 
  arrange(desc(total_liabilities))
View(test2_general_purpose)

test2_school_districts_all <- do.call(rbind, lapply(2023, function(year) test2(school_districts_all, year))) %>% 
  arrange(desc(total_liabilities))
View(test2_school_districts_all)


test2 <- rbind((test2_general_purpose %>% add_column(category = "general purpose")),
               (test2_school_districts_all %>% add_column(category = "school districts")))

test2 %>% write.csv("tmp/test2.csv")

####Test 3####

# Are revenues +/- 20% of expenditures? Generally, a government will spend up to their revenues but they can't 
# spend significantly more than revenues. 
# Again, it's possible revenues could be +/- >20% of expenditures, but it's very unlikely.

test3 <- function(df,year){
  result <- df %>% 
    filter(year == !!year) %>% 
    select(state.abb, name, year,
           expenses, revenues
    ) %>% 
    mutate(expenses_revenues = expenses/revenues*100) %>% 
    filter(expenses_revenues >= 120)
  
  return(result)
}

test3_general_purpose <- do.call(rbind, lapply(2023, function(year) test3(acfrs_general_purpose, year))) 
View(test3_general_purpose)

test3_school_districts_all <- do.call(rbind, lapply(2023, function(year) test3(school_districts_all, year))) 
View(test3_school_districts_all)

test3 <- rbind((test3_general_purpose %>% add_column(category = "general purpose")),
      (test3_school_districts_all %>% add_column(category = "school districts")))

test3 %>% write.csv("tmp/test3.csv")
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

test_changes_all_columns <- function(df, columns) {
  df %>%
    select(id, state.abb, name, year, all_of(columns)) %>% 
    arrange(id, year) %>%  # Ensure data is sorted by id and year
    group_by(id) %>%       # Group by id
    mutate(across(
      all_of(columns),
      ~ ifelse(is.na(.) | . == 0, 1, .), # Replace NA or 0 with 1
      .names = "adjusted_{.col}"        # Create adjusted columns
    )) %>%
    mutate(across(
      starts_with("adjusted_"), 
      ~ abs((. - lag(.)) / lag(.) * 100),  # Calculate percentage change
      .names = "change_{.col}"            # Create new column names for changes
    )) %>%
    filter(if_any(
      starts_with("change_"), 
      ~ . > 50                            # Keep rows where at least one change > 50%
    )) %>%
    ungroup() %>% 
    select(id, state.abb, name, year, all_of(columns), starts_with("change_"))
}

test_changes_all_columns(state_all, "total_liabilities") %>% 
  View()

test4 <- test_changes_all_columns(acfrs_general_purpose, "total_liabilities") 

test4 %>% write.csv("tmp/test4.csv")

test4%>% 
  View()
  
