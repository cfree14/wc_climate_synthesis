
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/disasters/raw"
outdir <- "data/disasters/processed"
plotdir <- "data/disasters/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Federal_Fishery_Disasters.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake")
  
