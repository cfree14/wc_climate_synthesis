
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rerddap)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/environmental/indices/raw"
outdir <- "data/environmental/indices/processed"

# Source
# https://oceanview.pfeg.noaa.gov/dashboard/
# https://oceanview.pfeg.noaa.gov/erddap/tabledap/index.html?page=1&itemsPerPage=1000

# Multi-scale Ultra-high Resolution (MUR) SST Analysis Anomaly fv04.1, Global, 0.01°, 2002-present, Daily
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41anom1day.graph

# Build data
################################################################################

# Look up datasets (to future self: URL must end in /)
datasets_grids <- ed_datasets(which="griddap", url="https://coastwatch.pfeg.noaa.gov/erddap/")
datasets_tables <- ed_datasets(which="tabledap", url="https://coastwatch.pfeg.noaa.gov/erddap/")


# Get data
#################################################

# Get data
data_info <- info("jplMURSST41anom1day")
data_orig <- griddap(x=data_info, 
                     time = c("2015-09-01", "2015-09-02"),
                     longitude = c(-130, -114), latitude = c(32, 50), 
                     fields="sstAnom")


# Convert to data frame
data_df <- data_orig$data
data_df1 <- data_df %>% 
  # Rename
  rename(lat_dd=lat, long_dd=lon, sst_anom_c=sstAnom) %>% 
  # Add date
  mutate(date=gsub("T09:00:00Z", "", time)) %>% 
  # Arrange
  select(date, lat_dd, long_dd, sst_anom_c)

# Export
saveRDS(data_df1, file=file.path(outdir, "mursst_data.Rds"))






