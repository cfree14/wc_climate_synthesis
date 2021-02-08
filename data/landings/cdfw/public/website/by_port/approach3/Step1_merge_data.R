

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
basedir <- "data/landings/cdfw/public/website/raw"
outputdir <- "data/landings/cdfw/public/website/by_port/approach3/intermediate"


# Merge data
################################################################################

# Merge all data from a single year
merge_year_data <- function(year){
  
  # Files to merge
  yeardir <- file.path(basedir, year, "by_port")
  files2merge <- list.files(yeardir, pattern="clean|copy")
  
  # Merge data
  data_orig <- purrr::map_df(files2merge, function(x) {
    
    # Read data and format
    fdata <- readxl::read_excel(file.path(yeardir, x)) %>% 
      # Convert to character (to prevent merge fail)
      mutate_all(.funs=as.character) %>% 
      mutate(year=year,
             year=as.numeric(year),
             filename=x %>% gsub("_clean| copy", "", .)) %>% 
      # Arrange
      select(year, filename, port, species, landings, value, everything()) 
      
  })
  
  # Format data
  data_all <- data_orig %>% 
    # Remove empty
    filter(!is.na(port)) %>% 
    # Format
    mutate(port=stringr::str_to_title(port) %>% stringr::str_trim(),
           species=stringr::str_trim(species),
           landings=landings %>% stringr::str_trim() %>% gsub(",", "", .) %>% as.numeric(),
           value=value %>% stringr::str_trim() %>% gsub("\\$|,", "", .) %>% as.numeric()) %>% 
    # Rename columns
    rename(landings_lb=landings,
           value_usd=value)
    
  # Inspect data
  freeR::complete(data_all)

  # QA/QC Check 1: Confirm that every port has a total 
  ######################################################################
  
  # QA/QC Check 1: Confirm that every port has a total 
  check1 <- data_all %>% 
    group_by(port) %>% 
    summarise(n=n(),
              tot_row_yn=ifelse(sum(grepl("TOTAL", toupper(species)))>0, "yes", "no")) %>% 
    ungroup() %>% 
    filter(tot_row_yn=="no")
  if(nrow(check1)>0){print("QA/QC Check 1: Not every port has a port total.")}

  # QA/QC Check 2: Confirm that totals add up to port totals
  ######################################################################
  
  # Separate data
  data <- data_all %>% 
    filter(species!="" & !grepl("TOTAL", toupper(species)))
  
  # Extract REPORTED totals
  tot_area_rep <- data_all %>% 
    filter(grepl("AREA|GRAND", toupper(species)))
  tot_port_rep <- data_all %>% 
    filter(!grepl("AREA|GRAND", toupper(species)) & grepl("TOTAL", toupper(species)))
  
  # Calculate OBSERVED totals
  tot_area_obs <- data %>% 
    group_by(filename) %>% 
    summarize(landings_lb_obs=sum(landings_lb, na.rm=T), 
              value_usd_obs=sum(value_usd, na.rm=T)) %>% 
    ungroup()
  tot_port_obs <- data %>% 
    group_by(filename, port) %>% 
    summarize(landings_lb_obs=sum(landings_lb, na.rm=T), 
              value_usd_obs=sum(value_usd, na.rm=T)) %>% 
    ungroup()
  
  # Merge PORT observed and reported and compare
  tot_port_check <- tot_port_rep %>% 
    left_join(tot_port_obs) %>% 
    # Calculate difference
    mutate(landings_lb_diff=landings_lb-landings_lb_obs,
           value_usd_diff=value_usd-value_usd_obs) %>% 
    # Reduce to ports with difference
    filter(landings_lb_diff!=0 | value_usd_diff!=0)
  
  # Merge AREA observed and reported and compare
  tot_area_check <- tot_area_rep %>% 
    left_join(tot_area_obs) %>% 
    mutate(landings_lb_diff=landings_lb-landings_lb_obs,
           value_usd_diff=value_usd-value_usd_obs)
  
  # Export
  ######################################################################
  
  # Export data
  saveRDS(data_all, file=file.path(outputdir, paste0(year, "_data.Rds")))
  
  # Return data
  return(data_all)
  
}

#  Do one
data19 <- merge_year_data(year=2013)

# Do all
for(i in 2010:2019){
  data <- merge_year_data(year=i)
}

