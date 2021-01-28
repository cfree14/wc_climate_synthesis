
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# Read FB data
data_fb_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1941_1976_landings_by_port_species.Rds")

# Read website data
data_web_orig <- readRDS("data/landings/cdfw/public/website/by_port/processed/CDFW_2000_2019_landings_by_port.Rds")


# Build data
################################################################################

# Column names
colnames(data_fb_orig)
colnames(data_web_orig)

# Goal dataset
# source, table, port_complex, port_orig, port, type, year, comm_name_orig, landings_lb, landings_kg, value_usd

# Format web data
data_web <- data_web_orig %>% 
  # Rename
  rename(port_complex=area) %>% 
  # Add source/table
  mutate(source=paste("CDFW", year+1)) %>% 
  mutate(table=recode(port_complex, 
                      "Eureka"='Table 16',
                      "Fort Bragg"="Table21FB",
                      "San Francisco"="Table 17",
                      "Monterey"="Table 18",
                      "Morro Bay"="Table 21MB",
                      "Santa Barbara"="Table 19",
                      "Los Angeles"="Table 20",
                      "San Diego"="Table 21SD",
                      "Sacramento Delta"="Table 21DS",
                      "Inland Waters"="Table 21IW")) %>% 
  # Add type
  mutate(type="Landings") %>% 
  # Add port name
  mutate(port_orig=port) %>% 
  # Arrange
  select(source, table, port_complex, port_orig, port, type, year, 
         comm_name_orig, value_usd, landings_lb, landings_kg, everything()) %>% 
  select(-filename)

# Format Fish Bulletin data
data_fb <- data_fb_orig %>%
  # Rename
  rename(comm_name_orig=species) %>% 
  # Add pounds in kg
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  # Arrange
  select(source, table, port_complex, port_orig, port, type, year, 
         comm_name_orig, value_usd, landings_lb, landings_kg, everything())
  

# Merge data
data <- bind_rows(data_fb, data_web) %>% 
  # Arrange
  arrange(year, port_complex, port, comm_name_orig)

# Inspect data
str(data)
freeR::complete(data) # a few missing values in web, many missing landings in FB
range(data$year)
table(data$port_complex)
table(data$port)
table(data$comm_name_orig)


# Plot data
################################################################################



# Export data
################################################################################





