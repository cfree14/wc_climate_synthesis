
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/fish_bulletins/processed"


# Merge data
################################################################################

# Which FBs?
fbs <- c(170, 168, 166, 163, 161, 159, 154, 153, 149)

# Merge data
x <- 170
data_orig <- purrr::map_df(fbs, function(x){
  
  # Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins", paste0("fb", x), "raw")
  fdata <- readxl::read_excel(file.path(indir, "Table4.xlsx"))
  
  # Format data
  fdata1 <- fdata %>% 
    # Gather
    gather(key="year", value="nfishers", 2:ncol(.)) %>% 
    # Rename
    setNames(c("residence", "year", "nfishers")) %>% 
    # Add and arrange source
    mutate(source=paste("FB", x)) %>% 
    select(source, everything()) %>% 
    # Convert to character
    mutate_all(as.character)
  
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
