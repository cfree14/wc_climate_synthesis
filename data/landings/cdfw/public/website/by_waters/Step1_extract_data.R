

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)

# Directories
rawdir <- "data/landings/cdfw/public/website/raw"
outdir <- "data/landings/cdfw/public/website/by_waters/raw"

# Build data
################################################################################

# Years to merge
years <- 2019:2000

# Merge data
data_orig <- purrr::map_df(years, function(x){
  
  # File to read
  indir <- file.path(rawdir, x, "other")
  infile <- file.path(indir, "Table7.pdf")
  
  # Read tables
  tables_list <- tabulizer::extract_tables(infile, output = "data.frame", method="stream")
  
  # Merge file data
  fdata <- purrr::map_df(1:length(tables_list), function(y){
  
    # Column names
    col_names <- c("Species", "California", "North", "South", "Unknown", "Total")
    
    # Extract table
    tdata <- tables_list[[y]] %>% 
      # Set names
      setNames(col_names) %>% 
      # Convert to character to ease merge
      mutate_all(as.character) %>% 
      # Add columns
      mutate(year=x) %>% 
      # Arrange
      select(year, everything())
    
  })
  
  # Return
  fdata
  
})

# Format merged data
data_full <- data_orig %>% 
  # Rename
  rename(comm_name_orig=Species) %>% 
  # Remove empty rows
  filter(California!="" & Total!="Total Landings") %>% 
  # Format common names
  mutate(comm_name_orig=gsub("\\.", "", comm_name_orig))

# Export data
write.csv(data_full, file.path(outdir, "2000_2019_landings_by_waters_messy.csv"), row.names=F)
