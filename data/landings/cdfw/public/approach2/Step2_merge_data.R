

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
basedir <- "data/landings/cdfw/public/raw"
outputdir <- "data/landings/cdfw/public/approach2/intermediate/by_port"


# Function to merge and clean
################################################################################

year <- 2018
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
    
    # 2000-2014 procedure
    if(year %in% 2000:2014){
    
      # Column names
      ncols <- ncol(data1)
      colnames_add <- c("species1", "species2", "landings", "value", 
                        make.unique(rep("trash", ncols-4)))
      
      # Format data
      data2 <- data1 %>% 
        # Rename columns
        setNames(colnames_add) %>% 
        # Format species column
        mutate(species1=ifelse(species1=="Rockfish, ", paste(speices1, species2), species1)) %>% 
        mutate(species1=ifelse(is.na(species1), species2, species1)) %>% 
        # Move value/landings to the right if necessary
        mutate(flag=ifelse(landings%in%c("Area Totals:", "Area Totals"), "yes", "no"),
               species1=ifelse(flag=="yes", landings, species1),
               landings=ifelse(flag=="yes", value, landings),
               value=ifelse(flag=="yes", trash, value),
               trash=ifelse(flag=="yes", NA, trash)) %>% 
        # Fix rockfish (2011) and flounder (2012)
        mutate(flag2=ifelse(species1%in%c("Rockfish,", "Flounder,"), "yes", "no"),
               species1=ifelse(flag2=="yes", paste(species1, landings), species1),
               landings=ifelse(flag2=="yes", value, landings),
               value=ifelse(flag2=="yes", trash, value),
               trash=ifelse(flag2=="yes", NA, trash)) %>% 
        # Fix more rockfish (2012)
        mutate(flag3=ifelse(species1 %in% paste("Rockfish,", c(477, 592, 130, 125, 164)), "yes", "no"),
               value=ifelse(flag3=="yes", landings, value),
               landings=ifelse(flag3=="yes", gsub("Rockfish, ", "", species1), landings), 
               species1=ifelse(flag3=="yes", paste("Rockfish, ", species2), species1)) %>% 
        # Move port totals (2014)
        mutate(flag4=ifelse(grepl("Total", landings), "yes", "no"),
               species1=ifelse(flag4=="yes", landings, species1),
               landings=ifelse(flag4=="yes", value, landings), 
               value=ifelse(flag4=="yes", trash, value),
               trash=ifelse(flag4=="yes", NA, trash)) %>% 
        # Remove some useless rows/columns
        select(-species2) %>% 
        rename(species=species1) %>% 
        filter(species!="Species" & !is.na(species) & 
                 !grepl("System", species) & !grepl("Tables", species)) %>% 
        # Format species
        mutate(species=gsub("\\.", "", species)) %>% 
        # Add port
        mutate(port=ifelse(toupper(species)==species, species, NA)) %>% 
        select(port, species, landings, value, everything()) %>% 
        fill(port, .direction = "down") %>% 
        mutate(port=stringr::str_to_title(port)) %>% 
        # Move landings/value to the right one
        mutate(flag5=ifelse(is.na(landings), "yes", "no"),
               landings=ifelse(flag5=="yes", value, landings),
               value=ifelse(flag5=="yes", trash, value),
               trash=ifelse(flag5=="yes", NA, trash)) %>% 
        # Remove useless columns
        select(port, species, landings, value) %>% 
        # Remove blanks rows
        filter(!is.na(landings)) %>%
        # Convert to numeric
        filter(landings!="Total Records:") %>% 
        mutate(landings=as.numeric(landings),
               value=gsub("\\$|,", "", value) %>% as.numeric(.)) %>% 
        # Add and arrange rows
        mutate(year=year, 
               filename=file_do %>% gsub(".xlsx", ".pdf", .)) %>% 
        select(year, filename, port, species, landings, value)
      
    }
    
    # 2015-2018
    if(year %in% 2015:2018){

      # Column names
      ncols <- ncol(data1)
      colnames_add <- c("species", "landings", "value",
                        make.unique(rep("trash", ncols-3)))
      
      # Add extra column if needed
      if(ncols==4){
        data1$new <- NA
        colnames_add <- c(colnames_add, "trash.1")
      }
      if(ncols==3){
        data1$new <- NA
        data1$new1 <- NA
        colnames_add <- c(colnames_add, "trash", "trash.1")
      }

      # Format data
      data2 <- data1 %>%
        # Rename columns
        setNames(colnames_add) %>%
        # Remove useless rows
        mutate(species=ifelse(is.na(species), "", species)) %>% 
        filter(!grepl("Species|Poundage|Wildlife|Records", species)) %>%
        filter(!grepl("California|Species|Poundage", landings)) %>% 
        # Format species
        mutate(species=gsub("\\.", "", species)) %>%
        # Move when Total in landings column
        mutate(flag3=ifelse(grepl("Total", landings), "yes", "no"),
               species=ifelse(flag3=="yes", landings, species),
               landings=ifelse(flag3=="yes", value, landings),
               value=ifelse(flag3=="yes", trash, value), 
               trash=ifelse(flag3=="yes", NA, trash)) %>% 
        # Move when Total in value column
        mutate(flag4=ifelse(grepl("Total", value), "yes", "no"),
               species=ifelse(flag4=="yes", value, species),
               landings=ifelse(flag4=="yes", trash, landings),
               value=ifelse(flag4=="yes", trash.1, value), 
               trash=ifelse(flag4=="yes", NA, trash), 
               trash.1=ifelse(flag4=="yes", NA, trash.1)) %>% 
      # Move values displaced by two positions
      mutate(flag1=ifelse(is.na(landings) & !is.na(trash.1), "yes", "no"),
             landings=ifelse(flag1=="yes", trash, landings),
             value=ifelse(flag1=="yes", trash.1, value),
             trash=ifelse(flag1=="yes", NA, trash),
             trash.1=ifelse(flag1=="yes", NA, trash.1)) %>% 
        # Move values displaced by one positions
        mutate(flag2=ifelse(is.na(landings) & !is.na(value) & !is.na(trash), "yes", "no"),
               landings=ifelse(flag2=="yes", value, landings),
               value=ifelse(flag2=="yes", trash, value),
               trash=ifelse(flag2=="yes", NA, trash)) 
        
        
        # Remove blanks rows
        filter(!is.na(landings)) %>%
          # Add port
          mutate(port=ifelse(toupper(species)==species, species, NA)) %>%
          select(port, species, everything()) %>%
          fill(port, .direction = "down") %>%
          mutate(port=stringr::str_to_title(port)) %>%  
          
        # Convert to numeric
        filter(landings!="Total Records:") %>%
        mutate(landings=as.numeric(landings),
               value=gsub("\\$|,", "", value) %>% as.numeric(.)) %>%
        # Add and arrange rows
        mutate(year=year,
               filename=file_do %>% gsub(".xlsx", ".pdf", .)) %>%
        select(year, filename, port, species, landings, value) %>% 
        # Remove no landings
        filter(!is.na(landings))


    }
    
    # 2019
    if(year %in% 2019){
      
      # Column names
      ncols <- ncol(data1)
      colnames_add <- c("species", "landings", "value",
                        make.unique(rep("trash", ncols-3)))
      
      # Add extra column if needed
      if(ncols==4){
        data1$new <- NA
        colnames_add <- c(colnames_add, "trash.1")
      }
      if(ncols==3){
        data1$new <- NA
        data1$new1 <- NA
        colnames_add <- c(colnames_add, "trash", "trash.1")
      }
      
      # Format data
      data2 <- data1 %>%
        # Rename columns
        setNames(colnames_add) %>%
        # Remove useless rows
        mutate(species=ifelse(is.na(species), "", species)) %>% 
        filter(species!="Species" & !grepl("Poundage", species) &
                 !grepl("Wildlife", species)) %>%
        # Format species
        mutate(species=gsub("\\.", "", species)) %>%
        # Move values displaced by two positions
        mutate(flag1=ifelse(is.na(landings) & !is.na(trash.1), "yes", "no"),
               landings=ifelse(flag1=="yes", trash, landings),
               value=ifelse(flag1=="yes", trash.1, value),
               trash=ifelse(flag1=="yes", NA, trash),
               trash.1=ifelse(flag1=="yes", NA, trash.1)) %>% 
        # Move values displaced by one positions
        mutate(flag2=ifelse(is.na(landings) & !is.na(value) & !is.na(trash), "yes", "no"),
               landings=ifelse(flag2=="yes", value, landings),
               value=ifelse(flag2=="yes", trash, value),
               trash=ifelse(flag2=="yes", NA, trash)) %>% 
        # Move misplaced value
        mutate(flag3=ifelse(!is.na(landings) & is.na(value), "yes", "no"),
               value=ifelse(flag3=="yes", trash, value), 
               trash=ifelse(flag3=="yes", NA, trash)) %>% 
        # Move when Total in landings column
        mutate(flag4=ifelse(grepl("Total", landings), "yes", "no"),
               species=ifelse(flag4=="yes", landings, species),
               landings=ifelse(flag4=="yes", value, landings),
               value=ifelse(flag4=="yes", trash, value), 
               trash=ifelse(flag4=="yes", NA, trash)) %>% 
        # Remove blank species
        filter(species!="") %>% 
        # Add port
        mutate(port=ifelse(is.na(landings), species, NA)) %>%
        select(port, species, everything()) %>%
        fill(port, .direction = "down") %>%
        mutate(port=stringr::str_to_title(port)) %>%
        # Remove blanks rows
        filter(!is.na(landings)) %>%
        # Convert to numeric
        filter(landings!="Total Records:") %>%
        mutate(landings=as.numeric(landings),
               value=gsub("\\$|,", "", value) %>% as.numeric(.)) %>%
        # Add and arrange rows
        mutate(year=year,
               filename=file_do %>% gsub(".xlsx", ".pdf", .)) %>%
        select(year, filename, port, species, landings, value) %>% 
        # Remove no landings
        filter(!is.na(landings))
      
      
    }
    
    # Return
    data2
    
  })
  
  # Export data
  write.csv(data, file.path(outputdir, paste0(year, "_data.csv")), row.names=F)
  return(data)
  
}


# Run function
################################################################################

# Check
out <- merge_data(year=2019)

# 2000-2012
years <- 2000:2019
purrr::map(years, function(x){merge_data(year=x)})




