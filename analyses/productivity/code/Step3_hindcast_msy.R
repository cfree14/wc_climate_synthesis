
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(TMB)
library(devtools)
library(freeR)
library(tidyverse)
library(splink)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")

# Directories
sstdir <- "data/environmental/cobe/processed"
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"

# Read output
output <- readRDS(file.path(outputdir, "production_0.01p_sst_random.Rds"))
data <- output$data

# Read SST data
sst_orig <- read.csv(file=file.path(sstdir, "COBE_1981_2020_by_ram_stocks.csv"), as.is=T)


# Hindcast MSY
################################################################################

# Extract mean SST
sst_avgs <- data %>% 
  group_by(stockid) %>% 
  summarize(sst_c_avg=mean(sst_c)) %>% 
  ungroup()

# Prepare hindcast temperate
sst_df <- sst_orig %>% 
  rename(sst_c=sst_c_avg) %>% 
  # Reduce to stocks in data
  filter(stockid %in% data$stockid) %>% 
  # Add mean
  left_join(sst_avgs, by="stockid") %>% 
  # Scale temperatures
  mutate(sst_c_scaled=sst_c - sst_c_avg) %>% 
  # Arrange
  arrange(stockid, year)

# Hindcast MSY
msy_hindcast <- splink::calc_msy(output=output, cov_df=sst_df)

# Export
saveRDS(msy_hindcast, file=file.path(outputdir, "production_0.01p_sst_random_msy_hindcast.Rds"))

