

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/mexico/datamares/public/raw"
outputdir <- "data/landings/mexico/datamares/public/processed"

# Read data
data <- readRDS(file=file.path(outputdir, "2006_2014_mexico_landings_datamares.Rds"))

# Google Maps API key
source("data/landings/pacfin/ggmap_api_key.R")
register_google(key=ggmap_api_key)

# Geocode offices
################################################################################

# Perform geocoding
build_yn <- F
if(build_yn==T){

  # Build office key
  office_key <- data %>% 
    # Identify offices
    select(state, office) %>% 
    unique() %>% 
    filter(!is.na(office)) %>% 
    # Build full name
    mutate(office_full=paste(office, state, "Mexico", sep=", "))
  
  # Geocode one location (test)
  # geocode(location="Bahia De Los Angeles, Baja California, Mexico",
  #         output=c('latlona'))
  
  # Geocode all locations
  office_key_xy <- mutate_geocode(office_key, office_full, output="latlona")
  
  # Convert to SF
  office_key_xy_sf <- office_key_xy %>%
    filter(!is.na(lon)) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Plot to check
  mapview(office_key_xy_sf)
  
  # Export geocode
  saveRDS(office_key_xy, file=file.path(inputdir, "CONAPESCA_offices_raw.rds"))
  
  
}


# Format geocoding
format_yn <- T
if(format_yn==T){
  
  # Read geocding results
  data_orig <- readRDS(file=file.path(inputdir, "CONAPESCA_offices_raw.rds"))
  
  # States
  states <- sort(unique(data_orig$state))
  states_more <- c("")
  states_all <- c(states, states_more)
  states_comma <- paste0(", ", states_all)
  states_comma_lower <- tolower(states_comma)
  states_comma_lower_merge <- paste(states_comma_lower, collapse="|")
  
  # Format geocoding results
  data <- data_orig %>% 
    # Rename columns
    rename(long_dd=lon, lat_dd=lat,
           office_orig=office, 
           office_full_orig=office_full) %>% 
    # Build formatted office name
    mutate(office=address %>% gsub(", mexico", "", .) %>% gsub(states_comma_lower_merge, "", .))
  
  
  
  
  
  
  
}






