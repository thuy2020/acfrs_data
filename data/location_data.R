library(sf)
library(tidyverse)
library(httr)
library(jsonlite)

# Read in the data
data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/main/output/top100_sd_3years.csv")

# State Abbreviations and Names

states_data <- data.frame(state.abb, state.name)

# Create a list of unique cities with state names
city_list <- data %>%
  select(city, state.abb) %>%
  unique() %>%
  mutate(city = str_to_title(city)) %>%
  # Convert state.abb to state name
  mutate(name_state = paste0(city, ", ", state.abb)) %>%
  pull(name_state)

# Output the city_list
print(city_list)


# Function to geocode using OSM
geocode_osm <- function(query) {
  url <- "https://nominatim.openstreetmap.org/search"
  response <- GET(url, query = list(q = query, format = "json", limit = 1))
  
  if (status_code(response) == 200) {
    result <- content(response, as = "text", encoding = "UTF-8")
    json_result <- fromJSON(result)
    
    if (length(json_result) > 0) {
      return(data.frame(
        query = query,
        latitude = as.numeric(json_result$lat[1]),
        longitude = as.numeric(json_result$lon[1]),
        error = NA_character_,
        stringsAsFactors = FALSE
      ))
    } else {
      return(data.frame(
        query = query,
        latitude = NA_real_,
        longitude = NA_real_,
        error = NA_character_,
        stringsAsFactors = FALSE
      ))
    }
  } else {
    return(data.frame(
      query = query,
      latitude = NA_real_,
      longitude = NA_real_,
      error = paste("Error:", status_code(response)),
      stringsAsFactors = FALSE
    ))
  }
}

# Get coordinates for each city
coordinates_list <- lapply(city_list, function(city) {
  result <- geocode_osm(city)
  Sys.sleep(1) # Adjusted to 1 second for safer rate limiting
  return(result)
})

# Combine all data frames into one
coordinates <- do.call(rbind, coordinates_list)

coordinates_complete <- coordinates %>%
  mutate(
    name = sapply(strsplit(query, ", "), `[`, 1),
    state.name = sapply(strsplit(query, ", "), `[`, 2)
  ) %>%
  select(name, state.name, latitude, longitude) %>%
  as.data.frame()

# [Optional] Save the coordinates to an RDS file
# saveRDS(coordinates_complete, "coordinates_complete.RDS")

coordinates_complete_name <- coordinates_complete |>
  mutate(name_state = paste0(name, ", ", state.name))

saveRDS(coordinates_complete_name, "schoolCoordinates.RDS")