

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
  basedir <- "data/landings/cdfw/public/raw"
  datadir <- file.path(basedir, year)
  outdir <- "data/landings/cdfw/public/intermediate/by_port/1merged"
  
  # Identify files
  file_w_port_data <- list.files(datadir, pattern="PUB|pub")
  
  # Loop through port files
  # for(i in 1:length(file_w_port_data)){
  # f <- file_w_port_data[1]
  for(i in 1:length(file_w_port_data)){
    
    # File to do
    file_do <- file_w_port_data[i]
    
    # Determine areas
    file_cap <- toupper(file_do) %>% gsub("_2019_ADA", "", .)
    area <- "unmatched"
    if(grepl(16, file_cap)){area <- "Eureka"}
    if(grepl(17, file_cap)){area <- "San Francisco"}
    if(grepl(18, file_cap)){area <- "Monterey"}
    if(grepl(19, file_cap)){area <- "Santa Barbara"}
    if(grepl(20, file_cap)){area <- "Los Angeles"}
    if(grepl("21BB", file_cap)){area <- "Bodega Bay"}
    if(grepl("21MB", file_cap)){area <- "Morro Bay"}
    if(grepl("21DS", file_cap)){area <- "Sacremento Delta"}
    if(grepl("21FB", file_cap)){area <- "Fort Bragg"}
    if(grepl("21IW", file_cap)){area <- "Inland Waters"}
    if(grepl("21SD", file_cap)){area <- "San Diego"}
    
    # Read tables
    tables_list <- tabulizer::extract_tables(file.path(datadir, file_do), output = "data.frame", method="stream")
    
    # If no tables, print warning
    if(length(tables_list)==0){
      print(paste("Problem. Do this file manually:", file_do ))
    }else{
    
      # Merge file data
      fdata <- purrr::map_df(1:length(tables_list), function(x) {
        
        # Extract table
        tdata <- tables_list[[x]] %>% 
          mutate_all(as.character) %>% 
          mutate(year=year, 
                 filename=file_do, 
                 area=area,
                 port="") %>% 
          select(year, filename, area, port, everything())
        
        # Format column names
        cols <- c("year", "filename", "area", "port", paste0("col", 1:(ncol(tdata)-4)))
        tdata1 <- tdata %>% 
          setNames(cols) %>% 
          # Mark port slots
          mutate(port=ifelse(grepl("TOTAL", toupper(col1)), "xx", port),
                 port=ifelse(toupper(col1)==col1, col1, port)) %>% 
          # Remove periods in species column
          mutate(col1=gsub("\\.", "", col1) %>% stringr::str_trim(),
                 col2=gsub("\\.", "", col2) %>% stringr::str_trim())
        
      })
      
      # Merge
      if(i==1){pdata <- fdata}else{pdata <- bind_rows(pdata, fdata)}
      
    }
    
  }
  
  # Export table to intermediate directory
  outfile <- paste(year, "_terrible.csv", sep="")
  write.csv(pdata, file.path(outdir, outfile), row.names=F, na="")
  
}


#format_ports_data(year=2008)

for(i in 2000:2019){
  format_ports_data(year=i)
}
