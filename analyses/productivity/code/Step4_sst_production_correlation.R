
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
datadir <- "data/ramldb/processed"
codedir <- "analyses/productivity/"
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"

# Read data
data <- readRDS(file.path(datadir, "RAM_WC_production_data_prepped_final.Rds"))


# Calculate correlation coefficients
################################################################################

# Build stats
stockids <- sort(unique(data$stockid))
x <- stockids[1]
stats <- purrr::map_df(stockids, function(x){
  
  # Subset data
  sdata <- data %>% 
    filter(stockid==x)
  
  # Plot data
  # plot(sp_sd ~ sst_c_scaled, sdata)
  
  # Calculate correlation
  corr <- cor(sdata$sp_sd, sdata$sst_c_scaled)
  
  # Record data
  df <- tibble(stockid=x,
               corr=corr)
  
})

# Plot 
hist(stats$corr)

# Export data
saveRDS(stats, file=file.path(outputdir, "RAM_WC_production_sst_correlation.Rds"))




