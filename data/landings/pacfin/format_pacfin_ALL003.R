

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/pacfin/raw/ALL003"
outputdir <- "data/landings/pacfin/processed"

# Read data
# Manually converted the XLS to an XLSX
ca1_orig <- readxl::read_excel(file.path(datadir, "ALL003-California-1980---2021-All-Views.xlsx"), sheet=2) # landed units
ca2_orig <- readxl::read_excel(file.path(datadir, "ALL003-California-1980---2021-All-Views.xlsx"), sheet=4) # all units

# Read codes
port_key <- read.csv(file.path(outputdir, "pacfin_port_codes_clean.csv"), as.is=T)
spp_key <- read.csv(file.path(outputdir, "pacfin_species_codes_clean.csv"), as.is=T)

# Formatting steps overview
# Step 1. Format each state's data and merge into single table
# Step 2. Add meta-data associated with codes




