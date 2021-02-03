##Cleans and wrangles commercial fishers data from California historic Fish Bulletins

################################################################################
# Clear workspace
rm(list = ls())

################################################################################
# Setup

# Packages
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/fish_bulletins/processed"

################################################################################
# Read and merge tables of older issues

##Historic data only by year (1916-1936)
nfishers_16_36 <- readxl::read_excel("data/landings/cdfw/public/fish_bulletins/raw/fb49/raw/Table144b.xlsx") %>% 
  rename(season = "License Year", nfishers = "No. of Fishermen") %>% 
  mutate(source = "FB 49",
         table_or_pg = 144,
         region_type = "state",
         region = "Statewide")


## Set 1: Data by nationality and port complex. Years: 1935-1955
fbs_1 <- c(49, 57, 58, 59, 59, 63, 63, 67, 67, 74)

table_name <- c("Table144", "Table3", "Table11", "Table16", "Table17", "Table23", "Table24", "Table23", "Table24", "Table36")

year <- c("1935-36", "1939-40", "1940- 41", "1941-42", "1942-43", "1943-44", "1944-45", "1945-46", "1946-47", "1947-48")


##Data frame with path to read each table and corresponding year
fb_table_key <- tibble(fbs_1, table_name, year) %>% 
  mutate(source = paste("FB", fbs_1), 
         path = paste0("fb", fbs_1, "/", "raw", "/", table_name, ".xlsx"))

# Mergeing

data_orig_set1 <- purrr::map_df(fb_table_key$path, function(x){
  
# Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", x)
  fdata <- readxl::read_excel(file.path(indir))
 
# Format data
  fdata1 <- fdata %>% 
    # Harmonize names
    setNames(c("region", "nationality", "nfishers", "total_nfisher")) %>% 
    # Add source and years
    mutate(region_type = "area of residence",
           path = x) %>% 
    # Convert to character
    mutate_all(as.character) %>% 
    left_join(fb_table_key, by = "path") %>% 
    select(-path, -fbs_1)
  
  # Return
  fdata1

})


## Set 2: Data by port and year
fbs_2 <- c(80, 86, 89, 95, 102, 105, 108, 111, 117, 121, 125, 129, 132, 135, 138, 144, 149, 153, 154, 159, 161, 163, 166, 168, 170)

table_name_set2 <- c("Table4", "Table5", "Table11", "Table7", "Table7", "Table10", "Table4", "Table4", "Table4", "Table4", "Table4", "Table5", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4")

fb_table_key_set2 <- tibble(fbs_2, table_name_set2) %>% 
  mutate(source = paste("FB", fbs_2), 
         path = paste0("fb", fbs_2, "/", "raw", "/", table_name_set2, ".xlsx"))


data_orig_set2 <- purrr::map_df(fb_table_key_set2$path, function(x){
  
  # Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", x)
  fdata <- readxl::read_excel(file.path(indir))
  
  # Format
  ncols <- ncol(fdata)
  
  # Format
  ncols <- ncol(fdata)
 
    fdata1 <- fdata %>% 
      # pivot_longer
      pivot_longer(2:ncol(.),
                   names_to = "season",
                   values_to = "nfishers") %>% 
      # Rename
      setNames(c("region", "season", "nfishers")) %>% 
      # Add and arrange source
      mutate(region_type = "area of residence",
             path = x) %>%
      # Convert to character
      mutate_all(as.character) %>%
      left_join(fb_table_key_set2, by = "path") %>%
      select(-path, -fbs_2)
    
    # Return
    fdata1
  
})


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Remove totals and total checks
  filter(!grepl("total", tolower(residence))) %>% 
  # Format number of fishers
  mutate(nfishers=as.numeric(nfishers)) %>% 
  # Format residence
  mutate(residence=gsub("\\.|\\_|\\_|\\-", "", residence) %>% stringr::str_trim(.),
         residence=recode(residence,
                          # Fix other
                          "San Francisco—"="San Francisco",
                          # Fix Mexico
                          "Mexico"="Mexican nationals licensed in CA",
                          "Mexican nationals"="Mexican nationals licensed in CA",
                          "Mexican nationals licensed in California"="Mexican nationals licensed in CA",
                          # Fix AK/WA/OR
                          "AK, WA, OR"="AK/WA/OR fishermen licensed in CA",
                          "Alaska Washington and Oregon fishermen licensed in California"="AK/WA/OR fishermen licensed in CA",
                          "Alaska, Washington and Oregon fishermen licensed in California"="AK/WA/OR fishermen licensed in CA",
                          "Alaska, Washington, and Oregon"="AK/WA/OR fishermen licensed in CA",
                          "Alaska, Washington, and Oregon fishermen licensed in California"="AK/WA/OR fishermen licensed in CA"))



# Inspect
str(data)
freeR::complete(data)
table(data$residence)


# Plot data
################################################################################


# Export data
################################################################################
