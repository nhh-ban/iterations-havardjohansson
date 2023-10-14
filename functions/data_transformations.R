library(purrr)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)

# Problem 2

transform_metadata_to_df <- function() {
  stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>% 
    mutate(latestData = force_tz(latestData, tzone = "UTC")) %>% 
    mutate(location = map(location, unlist)) |>  
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
}

# Problem 4a

to_iso8601 <- function(datetime, offset_in_days) {
  # Convert the input datetime string to POSIXct
  datetime <- as.POSIXct(datetime, tz = "UTC")
  
  # Add the offset in seconds
  datetime <- datetime + offset_in_days * 24 * 60 * 60
  
  # Format the datetime in ISO8601 with "Z" for UTC
  iso8601_datetime <- format(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  return(iso8601_datetime)
}

# Problem 5

library(jsonlite)

transform_volumes <- function(api_data) {
  # If your API response is in JSON format, parse it to a list or data frame.
  #api_data <- fromJSON(api_data)  
  
  # Extract relevant data from the API response and transform it into a data frame.
  # For example, assuming the response contains a list of volume records:
  volume_data <- lapply(api_data$volumeData, function(entry) {
    data.frame(
      Date = entry$from,
      Volume = entry$total$volumeNumbers$volume
    )
  })
  
  # Combine the list of data frames into one data frame.
  volume_df <- do.call(rbind, volume_data)

  return(volume_df)
}


