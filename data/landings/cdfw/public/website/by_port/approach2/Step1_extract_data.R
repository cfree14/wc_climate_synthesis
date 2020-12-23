

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
basedir <- "data/landings/cdfw/public/raw"


# Function to merge and clean
################################################################################

year <- 2008
merge_data <- function(year){
  
  # Directory
  yrdir <- file.path(basedir, year)
  files_do <- list.files(yrdir, pattern=".xlsx")
  
  # Loop through files
  file_do <- files_do[1]
  data <- purrr::map_df(files_do, function(x) {
  # for(i in 1:length(files_do)){
    
    # Read data
    file_do <- x
    data1 <- readxl::read_excel(file.path(yrdir, file_do))
    
    # Column names
    ncols <- ncol(data1)
    colnames_add <- c("species1", "species2", "landings", "value", 
                      make.unique(rep("trash", ncols-4)))
    
    # Format data
    data2 <- data1 %>% 
      # Rename columns
      setNames(colnames_add) %>% 
      # Format species column
      mutate(species1=ifelse(is.na(species1), species2, species1)) %>% 
      # Move value/landings to the right if necessary
      mutate(flag=ifelse(landings%in%c("Area Totals:"), "yes", "no"),
             species1=ifelse(flag=="yes", landings, species1),
             landings=ifelse(flag=="yes", value, landings),
             value=ifelse(flag=="yes", trash, value)) %>% 
      # Remove useless columns
      select(species1, landings, value) %>% 
      # Remove useless rows
      rename(species=species1) %>% 
      filter(species!="Species" & !is.na(species) & 
               !grepl("System", species) & !grepl("Tables", species)) %>% 
      # Format species
      mutate(species=gsub("\\.", "", species)) %>% 
      # Add port
      mutate(port=ifelse(toupper(species)==species, species, NA)) %>% 
      select(port, species, landings, value) %>% 
      fill(port, .direction = "down") %>% 
      mutate(port=stringr::str_to_title(port)) %>% 
      # Remove blanks rows
      filter(!is.na(landings)) %>%
      # Convert to numeric
      filter(landings!="Total Records:") %>% 
      mutate(landings1=as.numeric(landings),
             value1=gsub("\\$|,", "", value) %>% as.numeric(.)) %>% 
      # Add and arrange rows
      mutate(year=year, 
             filename=file_do %>% gsub(".xlsx", ".pdf", .)) %>% 
      select(year, filename, port, species, landings, value)
    
    
  })
  
}


# Run function
################################################################################

data00 <- merge_data(year=2010) # 2008 and 2009 do wierdly




