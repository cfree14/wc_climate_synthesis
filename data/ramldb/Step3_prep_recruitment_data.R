

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
data_orig <- readRDS(file=file.path(datadir, "RAM_WC_recruitment_data_trimmed.Rds")) 

# Read SST data
sst <- read.csv(file.path(sstdir, "COBE_1981_2020_by_ram_stocks.csv"))


# Merge data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to years with data
  filter(!is.na(b) & !is.na(r)) %>%
  # Add SST
  left_join(sst %>% select(stockid, year, sst_c_avg)) %>% 
  rename(sst_c=sst_c_avg) %>% 
  # Reduce to years with data
  filter(!is.na(b) & !is.na(r) & !is.na(sst_c)) %>% 
  # Scale biomass, recruitment, temperature
  group_by(stockid) %>%
  mutate(b_scaled=b/max(b),
         r_scaled=r/max(r),
         sst_c_scaled=scale(sst_c, center=T, scale=F)) %>% 
  ungroup()

# Check scaling
check1 <- data %>% 
  group_by(stockid) %>% 
  summarize(sst_mu=mean(sst_c_scaled),
            b_max=max(b_scaled),
            r_mu=max(r_scaled))
  

# Export data
saveRDS(data, file=file.path(datadir, "RAM_WC_recruitment_data_prepped.Rds")) 




