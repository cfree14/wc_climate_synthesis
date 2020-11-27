

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ncdf4)
library(tidyverse)

# Directories
datadir <- "data/landings/swfsc/raw/netcdfs"

# Read data
data <- ncdf4::nc_open(file.path(datadir, "Abalonex2cx20Flat.nc"))

