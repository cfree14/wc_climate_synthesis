

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
sstdir <- "data/environmental/cobe/processed"
datadir <- "data/ramldb/processed"
plotdir <- "data/ramldb/figures/trimming"

# Read RAM data
data_orig <- readRDS(file=file.path(datadir, "RAM_WC_production_data_trimmed.Rds")) 

# Read SST data
sst <- read.csv(file.path(sstdir, "COBE_1981_2020_by_ram_stocks.csv"))

# Merge data
################################################################################

# Build data
data <- data_orig %>% 
  # Add SST data
  left_join(sst, by=c("stockid", "year")) %>% 
  # Rename
  rename(sst_c=sst_c_avg) %>% 
  # Reduce to years with data
  filter(!is.na(b) & !is.na(sp) & !is.na(sst_c)) %>%
  # Scale biomass, production, temperature
  group_by(stockid) %>%
  mutate(b_scaled=b/max(b),
         sp_scaled=sp/max(b),
         sst_c_scaled=scale(sst_c, center=T, scale=F)) %>% 
  ungroup()

# Export data
saveRDS(data, file=file.path(datadir, "RAM_WC_production_data_prepped.Rds")) 




