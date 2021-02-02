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
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", x)
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
             region = "statewide",
             length_group_ft = str_remove(length_group_ft, "//.")) %>% 
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
      # Convert to character
      mutate_all(as.character) %>% 
      # Add and arrange source
      mutate(source=paste("FB", x),
             region_type = "state",
             region = "statewide") %>% 
      select(source, everything())
     
  }
  
  # Return
  fdata1
  
})

## FB129. Table6

nvessels_fb129 <- readxl::read_excel("data/landings/cdfw/public/fish_bulletins/raw/fb129/raw/Table6.xlsx") %>% 
  pivot_longer(2:ncol(.),
               names_to = "length_group_ft",
               values_to = "nvessels") %>%
  # Rename
  setNames(c("year", "length_group_ft", "nvessels")) %>% 
  # Add and arrange source
  mutate(source= "FB 129",
         region_type = "state",
         region = "statewide") %>% 
  select(source, everything()) %>% 
  # Convert to character
  mutate_all(as.character)

data_orig_tb5 <- rbind(data_orig_tb5, nvessels_fb129)

#################################################################################
## Totals df
# Set 1: Totals by region
totals_rgn <- data_orig_older %>% 
  filter(length_group_ft == "Total number of boats in each region")


# Set 2 + fb120
totals_yr <- data_orig_tb5 %>% 
  filter(length_group_ft == "Total") %>% 
  select(source, year, nvessels_toal = nvessels)

################################################################################

## Format data
# Set 1
data_older <- data_orig_older %>% 
  filter(!grepl("total", tolower(length_group_ft)),
         !grepl("total", tolower(region)),
         !grepl("number", tolower(length_group_ft)),
         !grepl("number", tolower(region))) %>% 
  # Format nvessels
  mutate(nvessels=as.numeric(nvessels)) %>% 
  # Format length group
  mutate(length_group_ft=gsub("\\.|\\_|\\,", "", length_group_ft),
         length_group_ft=gsub("- | -", "-", length_group_ft),
         length_group_ft=stringr::str_trim(length_group_ft),
         length_group_ft=recode(length_group_ft, 
                                "Up to 24 feet"="0-24",
                                "Up to 24'" = "0-24",
                                "25 to 39 feet"="25-39",
                                "25 feet to 39 feet" = "25-39",
                                "25' to 39'" = "25-39",
                                "40 to 64 feet"="40-64",
                                "40 feet to 64 feet" = "40-64",
                                "40' to 64'" = "40-64",
                                "65 to 84 feet"="65-84",
                                "65 feet to 84 feet" = "65-84",
                                "65' to 84'" = "65-84",
                                "65 to 84" = "65-84",
                                "85 to 99 feet"="85-99",
                                "85 to 99" = "85-99",
                                "85 feet and over" = "85+",
                                "85' and over" = "85+",
                                "100 feet and over"="100+",
                                "Up to 21 feet" = "0-24", ##fixing numbers that were incorrect in the xlsx file
                                "10 to 61 feet" = "40-64"))


# Set2
data_tb5 <- data_orig_tb5 %>% 
  # Remove totals and total checks
  filter(!grepl("total", tolower(length_group_ft))) %>% 
  # Format nvessels
  mutate(nvessels=as.numeric(nvessels)) %>% 
  # Format length group
  mutate(length_group_ft=gsub("\\.|\\_|\\,", "", length_group_ft),
         length_group_ft=gsub("- | -", "-", length_group_ft),
         length_group_ft=stringr::str_trim(length_group_ft),
         length_group_ft=recode(length_group_ft, 
                                "Up to 24 fcet"="0-24",
                                "Up to 24 feet"="0-24",
                                "25 to 39 feet"="25-39",
                                "40 to 64 feet"="40-64",
                                "65 to 84 feet"="65-84",
                                "85 to 99 feet"="85-99",
                                "100 feet and over"="100+",
                                "181 and over"="181+",
                                "40 to 61 feet"="40-64", ##fixing numbers that were incorrect in the xlsx file
                                "25 to 29 feet" = "25-39",
                                "15-11" = "11-15",
                                "150-160" = "156-160",
                                "20-25" = "21-25"))
                               
                                
                              
                                
                                
                              
# Inspect
table(data$length_group_ft)

# Plot coverage
################################################################################


