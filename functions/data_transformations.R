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

# Retrieving data

transform_metadata_to_df <- function() {
  stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%  
    mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>% 
    mutate(location = map(location, unlist)) |>  
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
}
stations_metadata_df <- 
  transform_metadata_to_df()