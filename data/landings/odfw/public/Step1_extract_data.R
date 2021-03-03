

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)


# 2015-2019
################################################################################

# Year
year <- 2019

# Function to extract and merge tables quickly
format_ports_data <- function(year){
  
  # Directory
  basedir <- "data/landings/odfw/public/raw"
  datadir <- file.path(basedir, year, "by_port")

  # Identify files
  files_do <- list.files(datadir) 
  
  # Loop through files
  i <- 1
  for(i in 1:length(files_do)){
  
    # Extract data  
    file_do <- files_do[i]
    data_list <- tabulizer::extract_tables(file.path(datadir, file_do), output = "data.frame", method="stream")
    print(paste(i, file_do))
    
    # Format data
    for(j in 1:length(data_list)){
    
      # Extract data
      fdata1 <- data_list[[j]]
      print(j)
      
      # Months
      months <- c("January", "Feburary", "March", "April", "May", "June", "July", "August", "September", 'October', "November", "December", "Total")
      
      # If 16 columns
      if(ncol(fdata1)==16){
        cols <- c("blank", "species", "type", months)
        fdata2 <- fdata1 %>% 
          setNames(cols) %>% 
          select(-blank)
      }
      
      # If 15 columns
      if(ncol(fdata1)==15){
        cols <- c("species", "type", months)
        fdata2 <- fdata1 %>% 
          setNames(cols) 
      }
      
      # Format data 
      fdata3 <- fdata2 %>% 
        mutate(year=year, 
               filename=file_do, 
               type=recode(type, 
                           "#"="landings",
                           "$"="value")) %>% 
        select(year, filename, species, type, everything()) %>% 
        mutate_all(as.character)
      
      # Merge data
      if(j==1){data_file <- fdata3}else{data_file  <- bind_rows(data_file, fdata3)}
      
    }
    
    # Return
    if(i==1){data_yr <- data_file}else{data_yr <- bind_rows(data_yr, data_file)}
  
  }
  
  # Export data
  write.csv(data_yr, file.path(datadir, paste0(year, "_data_messy.csv")), row.names=F)
  
  # Return
  return(data_yr)
  

}
  
# data19 <- format_ports_data(year=2015)


# 2013-2014
################################################################################

year <- 2014
format_data_1file <- function(year){
  
  
  # Read file data
  if(year==2014){
    datadir <- "data/landings/odfw/public/raw/2014"
    file_do <- "Commercial food fish landings by port 2014.pdf"
    data_list <- tabulizer::extract_tables(file.path(datadir, file_do), output = "data.frame", method="stream")
  }
  if(year==2013){
    datadir <- "data/landings/odfw/public/raw/2013"
    file_do <- "1_YR_LBS_$$_BY_PORT.pdf"
    data_list <- tabulizer::extract_tables(file.path(datadir, file_do), output = "data.frame", method="stream")
  }
  
  # Collapse file data
  for(j in 1:length(data_list)){
    
    # Extract data
    fdata1 <- data_list[[j]]
    print(j)
    
    # Months
    months <- c("January", "Feburary", "March", "April", "May", "June", "July", "August", "September", 'October', "November", "December", "Total")
    
    # If 16 columns
    if(ncol(fdata1)==16){
      cols <- c("blank", "species", "type", months)
      fdata2 <- fdata1 %>% 
        setNames(cols) %>% 
        select(-blank)
    }
    
    # If 15 columns
    if(ncol(fdata1)==15){
      cols <- c("species", "type", months)
      fdata2 <- fdata1 %>% 
        setNames(cols) 
    }
    
    # Format data 
    fdata3 <- fdata2 %>% 
      mutate(year=year, 
             filename=file_do, 
             type=recode(type, 
                         "#"="landings",
                         "$"="value")) %>% 
      select(year, filename, species, type, everything()) %>% 
      mutate_all(as.character)
    
    
    # Merge data
    if(j==1){data_yr <- fdata3}else{data_yr  <- bind_rows(data_yr, fdata3)}
    
  }
  
  # Formatting
  data_yr1 <- data_yr %>% 
    mutate(port=ifelse(species==toupper(species), species, NA),
           port=ifelse(port=="", NA, port),
           species=ifelse(species==toupper(species) & species!="", "Total", species)) %>% 
    fill(port) %>% 
    select(year, filename, port, species, everything())
  
  # Export data
  write.csv(data_yr1, file.path(datadir, paste0(year, "_data_messy.csv")), row.names=F)
  return(data_yr1)
  
}

# format_data_1file(year=2013)


################################################################################
# 2004-2012
################################################################################

# Merge either landings/value files
merge_pre2013_files <- function(dir, files, type){
  
  # Loop through files
  files_do <- files 
  for(i in 1:length(files_do)){
    
    # Extract data  
    file_do <- files_do[i]
    print(paste(i, file_do))
    data_list <- try(tabulizer::extract_tables(file.path(dir, file_do), output = "data.frame", method="stream"))
    
    # Did it successfully find a table?
    if(inherits(data_list, "try-error")){
      success_yn <- F
    }else{
      success_yn <- length(data_list)>0
    }
    
    # If unsuccessful...
    ####################################
    if(success_yn==F){
      
      # Build data
      data_file <- tibble(year=year,
                          filename=file_do,
                          species="CHECK",
                          type=type) %>% 
        mutate_all(as.character) 
      
    }
    
    # If successful...
    ####################################
    if(success_yn==T){
      
      # Reduce to data frames
      data_list_use <- data_list[sapply(data_list, function(x) is.data.frame(x))]
      
      # Format data
      for(j in 1:length(data_list_use)){
        
        # Extract data
        fdata1 <- data_list_use[[j]]
        print(j)
        
        # How many columns in tables? There should be 14.
        ncols <- ncol(fdata1)
        
        # If 14 columns
        ####################################
        if(ncols==14){
        
          # Months
          months <- c("January", "Feburary", "March", "April", "May", "June", "July", "August", "September", 'October', "November", "December", "Total")
          
          # Format data
          fdata2 <- fdata1 %>% 
            # Set columns names
            setNames(c("species", months)) %>% 
            mutate(year=year,
                   filename=file_do,
                   type=type, 
                   flag="") %>% 
            select(year, filename, species, type, everything(), flag) %>% 
            mutate_all(as.character) %>% 
            # Format species
            mutate(species=gsub("\\.", "", species))
          
          # Make template row 1
          row1_species <- colnames(fdata1)[1]
          row1 <- fdata2 %>% 
            slice(1) %>% 
            mutate(species=row1_species) 
          row1[,5:ncol(row1)] <- "CHECK"
          
          # Merge row 1 and data
          fdata3 <- bind_rows(row1, fdata2)
          
        }
        
        # If 13 columns
        ####################################
        if(ncols!=14){
          
          # Number of missing ncol
          ncol_missing <- 14 - ncols
          
          # Months
          months <- c("January", "Feburary", "March", "April", "May", "June", "July", "August", "September", 'October', "November", "December", "Total")
          
          # Format data
          fdata2 <- fdata1 %>% 
            # Add a column
            cbind(data.frame(matrix(data="", ncol=ncol_missing, nrow=nrow(fdata1)))) %>% 
            # Set columns name
            setNames(c("species", months)) %>% 
            # Add new columns  
            mutate(year=year,
                   filename=file_do,
                   type=type, 
                   flag="CHECK") %>% 
            select(year, filename, species, type, everything(), flag) %>% 
            mutate_all(as.character) %>% 
            # Format species
            mutate(species=gsub("\\.", "", species))
          
          # Make template row 1
          row1_species <- colnames(fdata1)[1]
          row1 <- fdata2 %>%
            slice(1) %>%
            mutate(species=row1_species)
          row1[,5:ncol(row1)] <- "CHECK"

          # Merge row 1 and data
          fdata3 <- bind_rows(row1, fdata2)
          
        }
        
        # Merge table data
        if(j==1){data_file <- fdata3}else{data_file  <- bind_rows(data_file, fdata3)}
        
      } # list loop
      
    } # successful 
    
    # Merge file data
    if(i==1){data_yr <- data_file}else{data_yr <- bind_rows(data_yr, data_file)}
    
  } # file loop
  
  # Return file
  return(data_yr)
  
}




# Extract data
format_data_pre2013 <- function(year){
  
  # Directory
  basedir <- file.path("data/landings/odfw/public/raw", year, "by_port")
  ldatadir <- file.path(basedir, "landings")
  vdatadir <- file.path(basedir, "value")
  
  # Files to do
  lbs_files <- list.files(ldatadir, pattern=".pdf")
  usd_files <- list.files(vdatadir, pattern=".pdf")
  testthat::expect_equal(length(lbs_files), length(usd_files))
  
  # Extract and merge data
  ldata <- merge_pre2013_files(dir=ldatadir, files=lbs_files, type="landings")
  vdata <- merge_pre2013_files(dir=vdatadir, files=usd_files, type="value") # dir=vdatadir; files=usd_files; type="value"
  
  # Export data
  write.csv(ldata, file.path(ldatadir, paste0(year, "_landings_data_messy.csv")), row.names=F)
  write.csv(vdata, file.path(vdatadir, paste0(year, "_value_data_messy.csv")), row.names=F)
  

}

data12 <- format_data_pre2013(year=2012)
data11 <- format_data_pre2013(year=2011)
data10 <- format_data_pre2013(year=2010)


for(i in 2009:2004){
  dataX <- format_data_pre2013(year=i)
}

