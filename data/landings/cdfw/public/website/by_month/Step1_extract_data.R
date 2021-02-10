

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
outdir <- "data/landings/cdfw/public/website/by_month/raw"


# Build data
################################################################################

# Years to merge
years <- 2000:2019

# Merge data
data_orig <- purrr::map_df(years, function(x){
  
  # File to read
  indir <- file.path(rawdir, x, "other")
  infile <- file.path(indir, "Table8.pdf")
  
  # Read tables
  tables_list <- tabulizer::extract_tables(infile, output = "data.frame", method="stream")
  
  # Merge file data
  fdata <- purrr::map_df(1:length(tables_list), function(y){
  
    # Column names
    nextra <- ncol(tables_list[[y]]) - 14
    col_names <- c("Species", month.name, "Landings")
    if(nextra>0){
      extra_cols <- paste0("extra", 1:nextra)
      col_names <- c(col_names, extra_cols)
    }
    
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

# Format data
data <- data_orig %>% 
  # Rename
  rename(comm_name_orig=Species) %>% 
  # Format common names
  mutate(rowid=1:n(), 
         comm_name_orig=gsub("\\.", "", comm_name_orig)) %>% 
  # Arrange
  select(rowid, everything())

# Export
write.csv(data, file.path(outdir, "2000_2019_landings_by_month_messy_v2.csv"), row.names=F)

  

