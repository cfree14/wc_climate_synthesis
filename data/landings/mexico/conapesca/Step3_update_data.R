

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/mexico/conapesca/raw"
outputdir <- "data/landings/mexico/conapesca/processed"

# Read data
data_orig <- readRDS(file.path(outputdir, "2000_2015_baja_landings_translated.Rds"))
