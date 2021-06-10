

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

# Read recruitment age key
rec_age_key <- read.csv(file.path(datadir, "RAM_stock_key.csv"), as.is=T)


# Fill wholes in recruitment age key
################################################################################

# Build recruitment age key
rec_age_key1 <- rec_age_key %>% 
  rename(rec_age_yr_pop=rec_age_yr) %>% 
  # Species average
  group_by(species) %>% 
  mutate(rec_age_yr_spp=mean(rec_age_yr_pop, na.rm=T)) %>% 
  ungroup() %>% 
  # Family average
  group_by(family) %>% 
  mutate(rec_age_yr_fam=mean(rec_age_yr_pop, na.rm=T)) %>% 
  ungroup() %>% 
  # Determine type
  mutate(rec_age_source=ifelse(!is.na(rec_age_yr_pop), "stock",
                               ifelse(!is.na(rec_age_yr_spp), "species", "family")),
         rec_age_yr=ifelse(rec_age_source=="stock", rec_age_yr_pop,
                           ifelse(rec_age_source=="species", rec_age_yr_spp, rec_age_yr_fam))) %>% 
  # Fill in some additional gaps
  # Shrimps get 0 years
  # Red king crab get 10 yrs: https://www.npfmc.org/wp-content/PDFdocuments/resources/SAFE/CrabSAFE/2016CrabSAFE_final.pdf
  mutate(rec_age_yr=ifelse(family=="Pandalidae" & !is.na(rec_age_yr), 0, rec_age_yr)) %>% 
  # Round up
  mutate(rec_age_yr=ceiling(rec_age_yr)) %>% 
  # Remove those without reliable age at recruitment
  filter(!is.na(rec_age_yr))


# Lag SST time series
################################################################################

# Build lagged SST time series
sst_lagged <- map_df(1:nrow(rec_age_key1), function(i){
  
  # Stock to do
  stock <- rec_age_key1$stockid[i]
  stock_lag <- rec_age_key1$rec_age_yr[i]
  
  # Extract stock SST data
  sdata <- sst %>% 
    filter(stockid==stock) %>% 
    arrange(year) %>% 
    mutate(rec_lag_yr=stock_lag) %>% 
    mutate(sst_c_lagged=lag(sst_c_avg, stock_lag))
    
})




# Merge data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to years with data
  filter(!is.na(b) & !is.na(r)) %>% 
  # Add recruitment age
  left_join(rec_age_key1 %>% select(stockid, rec_age_source, rec_age_yr), by="stockid") %>% 
  # Add lagged SST
  left_join(sst_lagged %>% select(stockid, year, sst_c_avg, sst_c_lagged)) %>% 
  rename(sst_c=sst_c_avg) %>% 
  # Reduce to years with data
  filter(!is.na(b) & !is.na(r) & !is.na(sst_c_lagged)) %>% 
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




