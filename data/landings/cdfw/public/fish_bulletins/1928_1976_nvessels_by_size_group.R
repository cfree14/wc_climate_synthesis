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

################################################################################
# Merge data

# Which FBs
## Set 1: Table number varies. Data reported by port
fbs_1 <- c(44, 49, 57, 58, 59, 63, 67, 74, 80, 86, 89, 95, 102, 105)

##Set 2: All tables are Table 5. Data reported statewise
fbs_2 <- c(108, 111, 117, 121, 125, 132, 135, 138, 144, 149, 153, 154, 159, 161, 163, 166, 168, 170)

## Table name = Table5
fb_129 <- 129

## Read and bind tables
## Set 2, Table 4
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
      mutate(source=paste("FB", x)) %>% 
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
      mutate(source=paste("FB", x)) %>% 
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


