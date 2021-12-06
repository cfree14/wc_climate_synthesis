

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
sstdir <- "data/environmental/cobe/processed"
datadir <- "data/ramldb/processed"
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"

# Read stock key
# stock_key_orig <- read.csv(file=file.path(datadir, "RAM_stock_key.csv"), as.is=T)

# Read SST data
sst_orig <- read.csv(file.path(sstdir, "COBE_1981_2020_by_ram_stocks.csv"))

# Read time series data
data_orig <- readRDS(file.path(datadir, "RAM_WC_production_data_prepped_final.Rds"))

# Read model output
sp_fixed_output <- readRDS(file=file.path(outputdir, "production_0.01p_sst_fixed.Rds"))
sp_random_output <- readRDS(file=file.path(outputdir, "production_0.01p_sst_random.Rds"))


# Setup
################################################################################

# Calculate average SST used in model fitting
stock_sst_key <- data_orig %>% 
  group_by(stockid) %>% 
  summarize(sst_c_avg=mean(sst_c),
            sst_c_min=min(sst_c),
            sst_c_max=max(sst_c)) %>% 
  ungroup()

# Build SST data
sst <- sst_orig %>% 
  # Reduce to stocks of interest
  filter(stockid %in% data_orig$stockid) %>% 
  # Rename
  rename(sst_c=sst_c_avg) %>% 
  # Arrange
  select(stockid, year, sst_c) %>% 
  arrange(stockid, year) %>% 
  # Add mean in data used for model fitting
  left_join(stock_sst_key, by="stockid") %>% 
  # Scale data
  mutate(sst_c_scaled=sst_c-sst_c_avg)

# Inspect
freeR::complete(sst)

# Prep for input into splink function
sst_df <- sst %>% 
  select(stockid, year, sst_c_scaled) %>% 
  rename(cov_col=sst_c_scaled)
  
# Hincast MSY
msy <- splink::calc_msy(output=sp_fixed_output, cov_df=sst_df)








