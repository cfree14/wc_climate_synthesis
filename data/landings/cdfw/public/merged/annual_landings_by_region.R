
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


# FB 181

# Website data
data_web <- readRDS("data/landings/cdfw/public/website/by_region/processed/CDFW_2000_2019_landings_by_region_species.Rds")

# Format data
################################################################################



