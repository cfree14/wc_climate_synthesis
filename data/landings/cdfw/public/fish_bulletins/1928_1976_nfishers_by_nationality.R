
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(here)

# Directories

outdir <- "data/landings/cdfw/public/fish_bulletins/processed"


# Merge data
################################################################################

# Which FBs?
fbs <- c(44, 49, 57, 58, 59, 63, 67, 74, 80, 86, 89, 95, 102, 105)
         
fbs_2 <- c(108, 111, 117, 121, 125, 129, 132, 135, 138, 144, 149, 153, 154, 159, 161, 163, 166, 168, 170)



# Merge data
x <- 129
data_orig <- purrr::map_df(fbs_2, function(fbs_2){
  
# Read data
indir <- file.path("data/landings/cdfw/public/fish_bulletins", paste0("fb", fbs_2), "raw")
  fdata <- readxl::read_excel(file.path(indir, "Table5.xlsx" | "Table6.xlsx"))
  
# Format data
  fdata1 <- fdata %>% 

# Gather
    gather(key="year", value="nfishers", 2:ncol(.)) %>% 
# Rename
    setNames(c("residence", "year", "nfishers")) %>% 
# Add and arrange source
    mutate(source=paste("FB", fbs_2)) %>% 
    select(source, everything()) %>% 
# Convert to character
    mutate_all(as.character)
  
  # Return
  fdata1

})


##test
x <- 129

# Read data
indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", paste0("fb", x), "raw")
fdata <- readxl::read_excel(file.path(indir, "Table6.xlsx"))

# Format data
fdata1 <- fdata %>% 
  
  # Gather
  gather(key="year", value="nfishers", 2:ncol(.)) %>% 
  # Rename
  setNames(c("residence", "year", "nfishers")) %>% 
  # Add and arrange source
  mutate(source=paste("FB", fbs_2)) %>% 
  select(source, everything()) %>% 
  # Convert to character
  mutate_all(as.character)




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
