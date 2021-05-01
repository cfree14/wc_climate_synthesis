

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/pacfin/raw/GMT"
outputdir <- "data/landings/pacfin/processed"

# Read data
data_orig <- read.csv(file.path(datadir, "GMT005-1980---2020.csv"), as.is=T)

# Source: https://reports.psmfc.org/pacfin/f?p=501:402:5900103230722:INITIAL:::F_SELECTED_NODE:40&cs=3mClXWqoElXsdmdrVzVhWwua4AwKyWWhm8Sf5XnQFZsFBspTJE77vyMGMbxwDBo1hrLDWjanROq311zaXZClgBw


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Column names
  janitor::clean_names("snake") %>% 
  rename(year=landing_year, 
         mgmt_group_code=management_group_code, 
         species_code=pacfin_species_code,
         comm_name=common_name) %>% 
  # Rename to make month extraction go smoothly
  rename(tot_weight_mtons=total_weight_mtons,
         tot_revenue=total_revenue,
         tot_cf=total_cf) %>% 
  # Gather
  gather(key="metric", value="value", 7:ncol(.)) %>% 
  # Extract month
  mutate(port_code=substr(metric, 1, 3)) %>% 
  # Format metric
  mutate(metric=substr(metric, 5, nchar(metric))) %>% 
  # Arrange and spread
  select(year, port_code, mgmt_group_code, complex1_code, complex2_code, species_code, comm_name, metric, value) %>% 
  spread(key="metric", value="value", fill=0) %>% 
  # Rename metrics
  rename(revenues_usd=revenue,
         landings_mt=weight_mtons,
         confidential=cf) %>% 
  # Arrange
  select(year:comm_name, confidential, landings_mt, revenues_usd, everything()) %>% 
  # Format columns
  mutate(port_code=toupper(port_code),
         landings_mt=as.numeric(landings_mt),
         revenues_usd=as.numeric(revenues_usd))

# Ports
ports <- data %>% 
  select(port_code) %>% 
  unique()

# Export data
saveRDS(data, file.path(outputdir, "PACFIN_1980_2020_groundfish_landings_by_port.Rds"))





  
