

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
basedir <- "data/landings/odfw/public/raw"
workdir <- "data/landings/odfw/public/intermediate"
outputdir <- "data/landings/odfw/public/processed"


# Read data
data1_orig <- read.csv(file=file.path(outputdir, "ODFW_2004_2012_landings_by_port_month_species.csv"), as.is=T)
data2_orig <- read.csv(file=file.path(outputdir, "ODFW_2013_2019_landings_by_port_month_species.csv"), as.is=T)


# Merge data
################################################################################

# Format data
data2 <- data2_orig %>% 
  rename(filename_lb=filename) %>% 
  mutate(filename_usd=filename_lb)

# Merge data
data <- bind_rows(data1_orig, data2) %>% 
  select(year, port, filename_lb, filename_usd, everything())
  
  
  
  
  

