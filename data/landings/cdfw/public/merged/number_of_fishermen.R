
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

# Read FB 44-170 data (1934-1976)
data_fb1_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1916_1976_nfishers_by_residency.Rds")

# Read FB 181 data (1976-1999)
data_fb2_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table3_licenced_fishermen_vessels.csv")


# Build data
################################################################################

