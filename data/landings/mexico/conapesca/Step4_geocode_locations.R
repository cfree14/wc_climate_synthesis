

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(ggmap)
library(mapview)
library(tidyverse)

# Directories
inputdir <- "data/landings/mexico/conapesca/raw"
outputdir <- "data/landings/mexico/conapesca/processed"


# Format dataset
################################################################################

# If formatting the already built dataset
format_yn <- T
if(format_yn==T){
  
  # Read data
  offices_orig <- readRDS(file=file.path(inputdir, "CONAPESCA_offices.rds"))
  
  # GOC offices
  goc_offices <- c("Mexicali", "San Filipe", "Bahía de los Ángeles", "Santa Rosalía", "Loreto", "La Paz")
  
  # Format data
  offices <- offices_orig %>% 
    # Add coast
    mutate(coast=ifelse(office %in% goc_offices, "Gulf of California", "Pacific Ocean")) %>% 
    # Rename
    rename(lat_dd=lat, long_dd=lon) %>% 
    # Arrange
    select(coast, state, office, long_dd, lat_dd) %>% 
    arrange(coast, desc(lat_dd))
  
  # Export
  saveRDS(offices, file=file.path(outputdir, "CONAPESCA_baja_office_key.Rds"))
  
  
}


# Build dataset
################################################################################

# If building the dataset
build_yn <- F
if(build_yn==T){

  # Read data
  data_orig <- readRDS(file.path(outputdir, "2000_2015_baja_landings_translated.Rds"))
  
  # Offices
  office_key <- data_orig %>% 
    select(state, office) %>% 
    unique() %>% 
    mutate(office_full=paste(office, "Baja California, Mexico", sep=", "))
  
  # Google Maps API key
  source("data/landings/pacfin/ggmap_api_key.R")
  register_google(key=ggmap_api_key)
  
  # THIS IS ANNOYING
  # IT DOESN'T WORK ON ALL LOCATIONS SO I RAN TWICE AND MANUALLY MERGED
  
  # Geocode locations
  office_key_xy <- mutate_geocode(office_key, office_full)
  saveRDS(office_key_xy, file=file.path(inputdir, "CONAPESCA_offices.rds"))
  
  # Convert to SF
  office_key_xy_sf <- office_key_xy %>%
    filter(!is.na(lon)) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Plot to check
  mapview(office_key_xy_sf)

}

