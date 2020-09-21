

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
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/data/california/landings/cdfw_reports/raw"


# Setup
################################################################################


# Function to read and format port data
# file <- "Table16PUB_2019_ADA.pdf"
# file <- "Table19PUB_2019_ADA.pdf" # Santa Barbara
# file <- "Table21IWPUB_2019_ADA.pdf" #Inland Waters
format_ports_data <- function(file, dir){
  
  # Extract header (to get year and area)
  pdf_header <- pdftools::pdf_text(file.path(dir, file)) %>% 
    read_lines(n_max=3)
  
  # Extract area
  area <- pdf_header[2] %>% gsub('.*Port ', '', .) %>% gsub(' Area.*', '', .) %>% stringr::str_to_title()
  
  # Extract year
  year <- pdf_header[2] %>% 
    trimws() %>% 
    substr(., nchar(.)-3, nchar(.))
  
  # Read data
  data1 <- tabulizer::extract_tables(file.path(dir, file), output = "data.frame")
  
  # Try again if it doesn't work
  if(length(data1)==0){
    data1 <- tabulizer::extract_tables(file.path(dir, file), output = "data.frame", method="lattice")
  }
  
  # Merge tables
  x <- 1
  data2 <- purrr::map_df(1:length(data1), function(x) {
    
    # Number of columns
    ncols <- ncol(data1[[x]])
    
    # If three columns
    if(ncols==3){
      sdata1 <- data1[[x]]
    # If an extra columns gets added accidentally
    }
    
    # If four columns
    if(ncols==4)
      sdata1 <- data1[[x]] %>% 
        setNames(c("species", "extra", "catch_lb", "value_usd")) %>% 
        mutate(species=ifelse(species=="" | is.na(species), extra, species)) %>% 
        select(-extra)
    
    # If more than four
    if(ncols>4){
      sdata0 <- data1[[x]] 
      cols_with_data <- which(apply(sdata0, 2, FUN=function(x) sum(!is.na(x)))>0)
      sdata1 <- sdata0[,cols_with_data ] %>% 
        setNames(c("species", "catch_lb", "value_usd")) %>% 
        filter(species!='')
    }

    # Extract data
    sdata2 <- sdata1 %>% 
      # Reduce to important columns
      setNames(c("species", "catch_lb", "value_usd")) %>% 
      # Add columns
      mutate(rawfile=file, 
             year=year,
             area=area) %>% 
      # Format data
      mutate(catch_lb=gsub(",", "", catch_lb) %>% as.numeric(),
             value_usd=gsub(",", "", value_usd) %>% gsub("\\$", "", .) %>% as.numeric()) %>% 
      # Arrange columns
      select(rawfile, year, area, species, catch_lb, value_usd)
    
  })
  
  # Extract grand total
  grand_total <- data2 %>% 
    filter(species %in% c("Grand Total:", "Grand totals:"))
  
  # Format data
  data3 <- data2 %>% 
    # Remove useless rows
    filter(!species %in% c("Species", "Grand Total:", "Grand totals:") & !is.na(catch_lb))
  
  # Extract ports
  ports <- data3 %>% 
    mutate(rowid=1:n()) %>% 
    filter(grepl("Totals", species)) %>% 
    rename(port=species) %>% 
    mutate(port=gsub(" Port Totals:| Totals:", "", port)) %>% 
    select(port, rowid)
  
  # If ports
  if(nrow(ports)>0){
    
    # Add ports
    data4 <- data3 %>% 
      # Add port
      mutate(rowid=1:n()) %>% 
      mutate(port=cut(rowid, breaks=c(0,ports$rowid), labels = ports$port)) %>% 
      # Arrange
      select(year, area, port, species, catch_lb, value_usd) %>% 
      # Remove totals
      filter(!grepl("Totals", species))
    
  }else{
    
    # Add ports
    data4 <- data3 %>% 
      # Add port
      mutate(port=area) %>% 
      # Arrange
      select(year, area, port, species, catch_lb, value_usd) %>% 
      # Remove totals
      filter(!grepl("Totals", species))
    
  }
  
  # Return
  return(data4)
  
}

# Function to merge data
merge_ports_data <- function(year){
  
  # Diretory
  dir <- file.path(datadir, year)
  
  # Loop through files and merge
  files_do <- list.files(dir, pattern="PUB")
  data1 <- purrr::map_df(files_do, function(x) {format_ports_data(file=x, dir=dir)})
  
}


data19 <- merge_ports_data(year=2019)
data18 <- merge_ports_data(year=2018)















