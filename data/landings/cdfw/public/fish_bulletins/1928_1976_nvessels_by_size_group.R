##Cleans and wrangles commercial fishing vessels data from California historic Fish Bulletins

##################################################################################

## Clear workspace
rm(list = ls())

################################################################################
# Setup
## Packages
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/fish_bulletins/processed"


###################################################################################
## Read and merge tables

## Set 1: Table number varies. Data reported by port
fbs_1 <- c(44, 49, 57, 58, 59, 59, 63, 63, 67, 67, 74, 80, 80, 86, 89, 95, 102, 102, 105, 105)

table_name <- c("Table30", "Table141", "Table6", "Table12", "Table18", "Table19", "Table25", "Table26", "Table25", "Table26", "Table31", "Table7", "Table8", "Table7", "Table13", "Table9", "Table9", "Table10", "Table12", "Table13")

year <- c("1934", "1935", "1939-40", "1940- 41", "1941-42", "1942-43", "1943-44", "1944-45", "1945-46", "1946-47", "1947-48", "1948-49", "1949-50", "1950-51", "1951-52", "1952-53", "1953-54", "1954-55", "1955-56", "1956-57")

##Data frame with path to read each table and corresponding year
fb_table <- tibble(fbs_1, table_name, year) %>% 
  mutate(source = paste("FB", fbs_1), 
         path = paste0("fb", fbs_1, "/", "raw", "/", table_name, ".xlsx"))


##Merging
data_orig_older <- purrr::map_df(fb_table$path, function(x){

  # Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", fb)
  fdata <- readxl::read_excel(file.path(indir))
  
  fdata_1 <- fdata %>% 
    # pivot_longer
    pivot_longer(2:ncol(.),
                 names_to = "length_group_ft",
                 values_to = "nvessels") %>%
    # Rename
    setNames(c("region", "length_group_ft", "nvessels")) %>% 
    # Add and arrange source
    mutate(region_type = "port complex",
           path = x) %>% 
    # Convert to character
    mutate_all(as.character) %>% 
    left_join(fb_table, by = "path") %>% 
    select(source, year, length_group_ft, nvessels, region_type, region)

})


##Set 2: All tables are Table 5. Data reported statewise
fbs_2 <- c(108, 111, 117, 121, 125, 132, 135, 138, 144, 149, 153, 154, 159, 161, 163, 166, 168, 170)
x <- 108

data_orig_tb5 <- purrr::map_df(fbs_2, function(x){
  
  # Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", paste0("fb", x), "raw")
  infile <- list.files(indir, pattern="Table5")
  fdata <- readxl::read_excel(file.path(indir, infile))
  
  # Format
  ncols <- ncol(fdata)
  if(ncols==3){
    fdata1 <- fdata %>% 
      # pivot_longer
      pivot_longer(2:ncol(.),
                   names_to = "length_group_ft",
                   values_to = "nvessels") %>%
      # Rename
      setNames(c("length_group_ft", "year", "nvessels")) %>% 
      # Add and arrange source
      mutate(source=paste("FB", x),
             region_type = "state",
             region = "statewide") %>% 
      select(source, everything()) %>% 
      # Convert to character
      mutate_all(as.character)
  }
  
  if(ncols>3){
    fdata1 <- fdata %>% 
      # pivot_longer
      pivot_longer(2:ncol(.),
                   names_to = "length_group_ft",
                   values_to = "nvessels") %>%
      # Rename
      setNames(c("year", "length_group_ft", "nvessels")) %>% 
      # Add and arrange source
      mutate(source=paste("FB", x),
             region_type = "state",
             region = "statewide") %>% 
      select(source, everything()) %>% 
      # Convert to character
      mutate_all(as.character)
  }
  
  # Return
  fdata1
  
})




# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Remove totals and total checks
  filter(!grepl("total", tolower(length_group_ft))) %>% 
  # Format nvessels
  mutate(nvessels=as.numeric(nvessels)) %>% 
  # Format length group
  mutate(length_group_ft=gsub("\\.|\\_|\\,", "", length_group_ft),
         length_group_ft=gsub("- | -", "-", length_group_ft),
         length_group_ft=stringr::str_trim(length_group_ft), 
         length_group_ft=recode(length_group_ft, 
                                #"00-70",
                                "100 feet and over"="100+",
                                "40 to 64 feet"="40-64",
                                "181 and over"="181+",
                                "25 to 39 feet"="25-39",
                                "40 to 64 feet "="40-64",
                                "65 to 84 feet"="65-84",
                                "85 to 99 feet"="85-99",
                                "Up to 24 feet"="0-24",
                                "Up to 39 feet"="0-39"))

# Inspect
table(data$length_group_ft)

# Plot coverage
################################################################################


