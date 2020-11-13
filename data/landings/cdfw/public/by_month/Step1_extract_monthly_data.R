

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)

# Monthly landings statewide
# Table 8 - Statewide

# Monthly landings by area
# Table 9 - Eureka
# Table 10 - San Francisco
# Table 11 - Monterey
# Table 12 - Santa Barbara
# Table 13 - Los Angeles
# Table 14BB - Bodega Bay
# Table 14DS - Sacramento Delta
# Table 14FB - Fort Bragg
# Table 14IW - Inland Waters
# Table 14MB - Morro Bay
# Table 14SD - San Diego


# Function to merge data
################################################################################

# Year
year <- 2019

# Function to extract and merge tables quickly
merge_data <- function(year){
  
  # Directory
  basedir <- "data/landings/cdfw/public/raw"
  datadir <- file.path(basedir, year, "by_month")
  # outdir <- "data/landings/cdfw/public/by_port/processed"
  
  # Identify files
  files_merge <- list.files(datadir, pattern=".pdf")
  
  # Loop through port files
  for(i in 1:length(files_merge)){
    
    # File to do
    file_do <- files_merge[i]
    print(paste(i, file_do))
    
    # Read tables
    tables_list <- tabulizer::extract_tables(file.path(datadir, file_do), output = "data.frame", method="stream")
    
    # Merge file data
    fdata <- purrr::map_df(1:length(tables_list), function(x) {
      
      # Column names
      ncol_add <- ncol(tables_list[[x]]) - 14
      cols <- c("species", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Total",
                make.unique(rep("Extra", ncol_add)))
      
      # Extract table
      tdata <- tables_list[[x]] %>% 
        # Set names
        setNames(cols) %>% 
        # Convert to character to ease merge
        mutate_all(as.character) %>% 
        # Add columns
        mutate(year=year, filename=file_do) %>% 
        # Arrange
        select(year, filename, everything())
      
    })
      
    # Merge
    if(i==1){ydata <- fdata}else{ydata <- bind_rows(ydata, fdata)}
      
  }
  
  # Export table to intermediate directory
  outfile <- paste(year, "_merged_messy.csv", sep="")
  write.csv(ydata, file.path(datadir, outfile), row.names=F, na="")
  
}


# Merge data
merge_data(year=2019)



# Function to merge data
################################################################################


# Year
year <- 2019

# Function to extract and merge tables quickly
merge_data <- function(year){
  
  # Directory
  basedir <- "data/landings/cdfw/public/raw"
  datadir <- file.path(basedir, year, "by_month")
  
  # Read data
  data_orig <- readxl::read_excel(file.path(datadir, paste0(year, "_merged_messy.xlsx")))
  
  # Format data
  data <- data_orig %>% 
    # Remove extra column
    select(year:Total) %>% 
    # Remove useless rows
    filter(!grepl("Species", species)) %>% 
    # Format species
    mutate(species=gsub("[[:digit:]]+", "", species) %>% stringr::str_trim()) %>% 
    # Gather
    gather(key="month", value="landings_lb", 4:ncol(.)) %>% 
    filter(!is.na(landings_lb)) %>% 
    # Convert landings to numeric
    mutate(landings_lb1=landings_lb %>% gsub(",", "", .) %>% as.numeric())
    
  
}














