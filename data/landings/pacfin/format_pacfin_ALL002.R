

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/pacfin/raw/ALL/ALL002"
outputdir <- "data/landings/pacfin/processed"

# Read data
wa_orig <- read.csv(file.path(datadir, "ALL001-Washington-1980---2021.csv"), as.is=T)

# Read codes
port_key <- read.csv(file.path(outputdir, "pacfin_port_codes_clean.csv"), as.is=T)
spp_key <- read.csv(file.path(outputdir, "pacfin_species_codes_clean.csv"), as.is=T)

# Formatting steps overview
# Step 1. Format each state's data and merge into single table
# Step 2. Add meta-data associated with codes




