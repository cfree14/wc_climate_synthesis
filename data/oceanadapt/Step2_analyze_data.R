

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/oceanadapt/raw"
outdir <- "data/oceanadapt/processed"
plotdir <- "data/oceanadapt/figures"


# Read data
data_orig <- readRDS(file=file.path(outdir, "OA_1977_2018_distribution_shifts.Rds"))


# Setup
################################################################################


spp_key <- data_orig %>% 
  select(species, comm_name) %>% 
  unique()

pop_key <- data_orig %>% 
  select(region, species, comm_name) %>% 
  unique()
