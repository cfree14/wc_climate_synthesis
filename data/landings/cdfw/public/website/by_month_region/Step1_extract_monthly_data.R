

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


# Build file key
################################################################################

# Build file key
file_key <- purrr::map_df(2000:2019, function(x){
  
  # Get files
  basedir <- "data/landings/cdfw/public/raw"
  datadir <- file.path(basedir, x, "by_month")
  yfiles <- list.files(datadir, pattern=".pdf")
  
  # Build key
  y_key <- tibble(year=x,
                  filename=yfiles)
  
})

# Add region
file_key <- file_key %>% 
  mutate(port_complex=ifelse(grepl("BB", toupper(filename)), "Bodega Bay", ""),
         port_complex=ifelse(grepl("DS", toupper(filename)), "Sacramento Delta", port_complex),
         port_complex=ifelse(grepl("FB", toupper(filename)), "Fort Bragg", port_complex),
         port_complex=ifelse(grepl("IW", toupper(filename)), "Inland Waters", port_complex),
         port_complex=ifelse(grepl("MB", toupper(filename)), "Morro Bay", port_complex),
         port_complex=ifelse(grepl("SD", toupper(filename)), "San Diego", port_complex),
         port_complex=ifelse(grepl("TABLE9|TABLE 9", toupper(filename)), "Eureka", port_complex),
         port_complex=ifelse(grepl("TABLE10|TABLE 10", toupper(filename)), "San Francisco", port_complex),
         port_complex=ifelse(grepl("TABLE11|TABLE 11", toupper(filename)), "Monterey", port_complex),
         port_complex=ifelse(grepl("TABLE12|TABLE 12", toupper(filename)), "Santa Barbara", port_complex),
         port_complex=ifelse(grepl("TABLE13|TABLE 13", toupper(filename)), "Los Angeles", port_complex)) %>% 
  # The missing ones appear to be San Diego
  mutate(port_complex=ifelse(port_complex=="", "San Diego", port_complex))

# Check port complexes
file_check <- file_key %>% 
  group_by(year, port_complex) %>% 
  summarize(n=n()) %>% 
  spread(key="port_complex", value=n)


# Function to merge data
################################################################################

# Function to extract and merge tables quickly: year <- 2000
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
    
    # If nothing
    if(length(tables_list)==0){
      
      # Empty table
      fdata <- tibble(year=year,
                      filename=file_do)
      print(paste0(file_do, " was empty"))
      
    # If something  
    }else{
      
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
      
    }
    
    # Merge
    if(i==1){ydata <- fdata}else{ydata <- bind_rows(ydata, fdata)}
      
  }
  
  # Format data after merge
  ydata1 <- ydata %>% 
    # Remove rows that just have months
    filter(!grepl("Species", species)) %>% 
    # Remove empty rows
    filter(!species=="") %>% 
    # Format species
    mutate(species=gsub("\\.", "", species)) %>% 
    # Add port_complex
    left_join(file_key, by=c("year", "filename")) %>% 
    # Mark rows to check
    # If there is a number in the species name, manually fix
    mutate(check=ifelse(grepl("[[:digit:]]", species), "check", "")) %>% 
    # Add empty columns
    mutate(area=NA,
           category=NA) %>% 
    # Arrange
    select(year, filename, port_complex, area, category, species, check, everything())
  
  # Add area and category
  ydata1$area[1] <- "California Waters"
  ydata1$category[1] <- "Finfish"
  ydata2 <- ydata1 %>% 
    # Add area/category
    mutate(area=ifelse(grepl("aters", tolower(species)) & !grepl("total", tolower(species)), species, area),
           category=ifelse((grepl(" – ", species) | Total=="") & !grepl("aters", tolower(species)), species, NA)) %>% 
    # Fill area/category
    fill(area, .direction="down") %>% 
    fill(category, .direction="down") %>% 
    # Edit area/category
    mutate(category=recode(category, 
                           "chinoderms"="Echinoderms",
                           "ishes"="Fishes",
                           "ollusks"="Mollusks",
                           "orms"="Worms",
                           "rustaceans"="Crustaceans"),
           area=recode(area, 
                       "aters North Of State"="Waters North Of State",
                       "aters South Of State"="Waters South Of State",    
                       "her Waters"="Other Waters",     
                       "lifornia Waters"="California Waters",     
                       "ther Waters"="Other Waters"))
  
  # If Extra columns
  if("Extra" %in% colnames(ydata2)){
    ydata2 <-  ydata2 %>% 
      mutate(check=ifelse(!is.na(Extra) | Extra=="", "check", check)) 
  }
  
  # Export table to intermediate directory
  outfile <- paste(year, "_merged_messy.csv", sep="")
  write.csv(ydata2, file.path(datadir, outfile), row.names=F, na="")
  
  # Return data
  return(ydata2)
  
}

# Perform merge
################################################################################

# Merge data
df19 <- merge_data(year=2019)
df18 <- merge_data(year=2018)
df17 <- merge_data(year=2017)
df16 <- merge_data(year=2016)
df15 <- merge_data(year=2015)

df14 <- merge_data(year=2014)
df13 <- merge_data(year=2013)
df12 <- merge_data(year=2012)
df11 <- merge_data(year=2011)
df10 <- merge_data(year=2010)

df09 <- merge_data(year=2009)
df08 <- merge_data(year=2008)
df07 <- merge_data(year=2007)
df06 <- merge_data(year=2006)
df05 <- merge_data(year=2005)

df04 <- merge_data(year=2004)
df03 <- merge_data(year=2003)
df02 <- merge_data(year=2002)
df01 <- merge_data(year=2001)
df00 <- merge_data(year=2000)















