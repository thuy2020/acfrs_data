library(dplyr)
library(stringr)
library(janitor)
library(scales)
library(knitr)
source("src/data_processing/census.R")
source("src/data_processing/cleaning_merging.R")

fields_to_export <- c("state.abb", "state.name", "id", "year", "name", 
                      "category",
                      "net_pension_liability",
                      "total_liabilities", 
                      "net_pension_assets",
                      "net_opeb_liability", "net_opeb_assets", 
                      "total_assets", 
                      "population")
###Function###
get_state_census_acfr_data <- function(state_abb) {
  state_acfr <- state_gov %>%
    filter(state.abb == state_abb, year == 2022) %>%
    select(all_of(fields_to_export)) %>%
    mutate(category = "State")
  
  state_pop <- state_acfr %>%
    summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)
  
  counties_census <- census_all %>%
    filter(state.abb == state_abb, sumlev == 050, funcstat %in% c("A", "C")) %>%
    select(state.abb, state.name, name_census, population)
  
  counties_census_pop <- counties_census %>%
    summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)
  
  counties_acfr <- county_gov %>%
    filter(state.abb == state_abb, year == 2022) %>%
    select(all_of(fields_to_export)) %>%
    arrange(desc(net_pension_liability)) %>%
    mutate(category = "Counties")
  
  counties_acfr_pop <- counties_acfr %>%
    summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)
  
  municipalities_census <- census_incorporated %>%
    filter(state.abb == state_abb) %>%
    select(state.abb, name_census, population) %>%
    mutate(name_census = str_remove(name_census, "city$"),
           name_census = str_squish(name_census))
  
  municipalities_census_pop <- municipalities_census %>%
    summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)
  
  municipalities_acfr <- municipality_all %>%
    filter(state.abb == state_abb, year == 2022) %>%
    select(all_of(fields_to_export)) %>%
    arrange(desc(net_pension_liability)) %>%
    mutate(category = "Municipalities")
  
  municipalities_acfr_pop <- municipalities_acfr %>%
    summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)
  
  sd_nces <- nces %>%
    filter(state.abb == state_abb) %>%
    select(name_nces, enrollment_22, ncesID)
  
  sd_nces_pop <- sd_nces %>%
    summarise(pop = sum(enrollment_22, na.rm = TRUE)) %>% pull(pop)
  
  sd_acfr <- school_districts_all %>%
    filter(state.abb == state_abb, year == 2022) %>%
    select(any_of(fields_to_export), enrollment_22, ncesID) %>%
    arrange(desc(net_pension_liability)) %>% distinct() %>%
    mutate(category = "School Districts") %>% 
    rename(population = enrollment_22)
  
  sd_acfr_pop <- sd_acfr %>%
    summarise(pop = sum(population, na.rm = TRUE)) %>% pull(pop)
  
  return(list(
    state_acfr = state_acfr, state_pop = state_pop,
    counties_acfr = counties_acfr, counties_census = counties_census,
    counties_census_pop = counties_census_pop, counties_acfr_pop = counties_acfr_pop,
    municipalities_acfr = municipalities_acfr, municipalities_census = municipalities_census,
    municipalities_census_pop = municipalities_census_pop, municipalities_acfr_pop = municipalities_acfr_pop,
    sd_acfr = sd_acfr, sd_nces = sd_nces,
    sd_nces_pop = sd_nces_pop, sd_acfr_pop = sd_acfr_pop
  ))
}

#function to aggregate 

calculate_aggregate_data <- function(data) {
  total_collected <- data.frame(
    public_employers = c("State", "Counties", "Municipalities", "School Districts"),
    total_population = c(data$state_pop, 
                         data$counties_census_pop, 
                         data$municipalities_census_pop, 
                         data$sd_nces_pop),
    collected_population = c(data$state_pop, 
                             data$counties_acfr_pop, 
                             data$municipalities_acfr_pop, 
                             data$sd_acfr_pop),
    total_count = c(1, nrow(data$counties_census), 
                    nrow(data$municipalities_census), 
                    nrow(data$sd_acfr)),
    collected_count = c(1, nrow(data$counties_acfr), 
                        nrow(data$municipalities_acfr), 
                        nrow(data$sd_acfr))) %>% 

    mutate(`% of Population Captured` = paste0(round(collected_population / total_population * 100), "%"),
           `% of Number of Entity Captured` = paste0(round(collected_count / total_count * 100), "%")) %>%
    rename(`Public Employers` = public_employers)
  
  aggregate_by_government_type <- rbind(
    data$state_acfr, data$counties_acfr, 
    data$municipalities_acfr,
    data$sd_acfr %>% select(-c(ncesID))
  ) %>%
    select(-year) %>%
    group_by(category) %>%
    summarise(
      `Net Pension Liability` = sum(net_pension_liability, na.rm = TRUE),
      `Net OPEB Liability` = sum(net_opeb_liability, na.rm = TRUE),
      `Total Liabilities` = sum(total_liabilities, na.rm = TRUE),
      `Net Pension + OPEB Liability` = (`Net Pension Liability` + `Net OPEB Liability`)
    ) %>%
    bind_rows(summarise(., category = "Total", across(where(is.numeric), sum))) %>%
    mutate(`Net Pension + OPEB Liability Share of Total` = 
             paste0(round(`Net Pension + OPEB Liability` / `Total Liabilities` * 100), "%")) %>%
    rename(`Public Employers` = category) %>%
    left_join(total_collected %>% select(`Public Employers`, `% of Population Captured`))
  
  list(
    "aggregate by government type" = aggregate_by_government_type,
    "State" = data$state_acfr,
    "Counties" = data$counties_acfr,
    "Municipalities" = data$municipalities_acfr,
    "School Districts" = data$sd_acfr,
    "Total vs collected" = total_collected
  )
}


# Call the function to get the data
data <- get_state_census_acfr_data("OK")
# Calculate the aggregate data
data_list <- calculate_aggregate_data(data)


# Export the data to an Excel file
rio::export(data_list, "output/ACFRs data 2022_testing.xlsx")


####Result#### 
total_collected <- data.frame(
  public_employers = c("State", "Counties", "Municipalities", "School Districts"),
  total_population = c(state_pop, 
                       counties_census_pop, 
                       municipalities_census_pop, 
                       sd_nces_pop),
  collected_population = c(state_pop, 
                           counties_acfr_pop, 
                           municipalities_acfr_pop,
                           sd_acfr_pop),
  total_count = c(1, 
                  nrow(counties_census),
                  nrow(municipalities_census),
                  nrow(sd_nces)),
  collected_count = c(1, 
                      nrow(counties_acfr),
                      nrow(municipalities_acfrs),
                      nrow(sd_acfr))) %>% 
  
  mutate(`% of Population Captured` = paste0(round(collected_population / total_population * 100), "%"),
         `% of Number of Entity Captured` = paste0(round(collected_count / total_count * 100), "%")) %>% 
  rename(`Public Employers` = public_employers) 


# bind all types of governments
aggregate_by_government_type <- rbind(state_acfr, counties_acfr, 
                                      municipalities_acfr, 
                                      sd_acfr %>% select(-ncesID)) %>% 
                      select(-year) %>% 
                      group_by(category) %>% 
  
  #calculate summary
  summarise(`Net Pension Liability` = sum(net_pension_liability, na.rm = TRUE),
            `Net OPEB Liability` = sum(net_opeb_liability, na.rm = TRUE),
            `Total Liabilities` = sum(total_liabilities, na.rm = TRUE),
            
            `Net Pension + OPEB Liability` = (`Net Pension Liability` + `Net OPEB Liability`)) %>% 
  bind_rows(summarise(., category = "Total", across(where(is.numeric), sum))) %>% 
  
  mutate(`Net Pension + OPEB Liability Share of Total` = round(`Net Pension + OPEB Liability` /`Total Liabilities`*100), 
         `Net Pension + OPEB Liability Share of Total` = paste0(`Net Pension + OPEB Liability Share of Total`, "%")) %>% 
  
  rename(`Public Employers` = category) %>% 
  # join to get population captured
  left_join(total_collected %>% 
              select(`Public Employers`, `% of Population Captured`))

data_list <- list(
  "aggregate by government type" = aggregate_by_government_type,
  "State" = state_acfr,
  "Counties" = counties_acfr,
  "Municipalities" = municipalities_acfr,
  "School Districts" = sd_acfr,
  "Total vs collected" = total_collected)
rio::export(data_list, "output/ACFRs data 2022.xlsx")



####Checking####
#comparing census and acfrs collected
anti_join(counties_census,
          counties_acfr,
          by = c("state.abb", "name_census" = "name")) 

#checking counties having 2021 but not 2022
anti_join(county_gov %>% 
            filter(state.abb == "OK") %>% filter(year == 2021) %>% 
            select(state.abb, name),
          
          counties_acfr %>% select(state.abb, name)) %>% View()

# missing municipalities
anti_join(
  municipalities_census, 
  municipalities_acfrs,
  by = c("name_census" = "name", "state.abb")) %>% View()

#check which one acfr does not have
anti_join(
  sd_nces, 
  sd_acfr,
  by = c("ncesID")) %>% View()
