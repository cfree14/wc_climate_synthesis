

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)


# Setup
################################################################################

# Year
year <- 2019

# Function to extract and merge tables quickly
format_ports_data <- function(year){
  
  # Directory
  basedir <- "data/landings/odfw/pdfs"
  datadir <- file.path(basedir, year)

  # Identify files
  files_do <- list.files(datadir) 
  
  # Loop through files
  x <- files_do[1]
  ydata <- purrr::map_df(files_do, function(x) {
  
    # Extract data  
    data_list <- tabulizer::extract_tables(file.path(datadir, x), output = "data.frame", method="stream")
    
    # Format data
    for(i in 1:length(data_list)){
    
      # Extract data
      fdata1 <- data_list[[i]]
      
      # Column names
      if(ncol(fdata1)==16){
        cols <- c("blank", "species", "type", colnames(fdata1)[4:ncol(fdata1)])
      }else{
        cols <- c("species", "type", colnames(fdata1)[3:ncol(fdata1)])
      }
      
      # Format data 
      fdata2 <- fdata1 %>% 
        setNames(cols) %>% 
        mutate(year=year, 
               filename=x, 
               type=recode(type, 
                           "#"="landings",
                           "$"="value")) %>% 
        select(year, filename, species, type, everything())
      
      # Merge data
      if(i==1){data_out <- fdata2}else{data_out <- bind_rows(data_out, fdata2)}
      
    }
    
    # Return
    data_out
    
  })
  

}
  
  
  