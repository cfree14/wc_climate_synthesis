
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/data/california/landings/swfsc_erddap/"

# Read data
data1_orig <- read.csv(file.path(datadir, "erdCAMarCatLM_18ce_21b9_4a32.csv"), as.is=T, skip=1) # California Fish Market Catch Landings, Long List, 1928-2002, Monthly	
data2_orig <- read.csv(file.path(datadir, "erdCAMarCatLY_b66b_0d1d_cfd6.csv"), as.is=T, skip=1) # California Fish Market Catch Landings, Long List, 1928-2002, Yearly	
data3_orig <- read.csv(file.path(datadir, "erdCAMarCatSM_77fe_2568_160d.csv"), as.is=T, skip=1) # California Fish Market Catch Landings, Short List, 1928-2002, Monthly
data4_orig <- read.csv(file.path(datadir, "erdCAMarCatSY_b66b_0d1d_cfd6.csv"), as.is=T, skip=1) # California Fish Market Catch Landings, Short List, 1928-2002, Yearly

# Website
# https://www.pfeg.noaa.gov/products/las/CA_market_catch.html

# Format data
################################################################################

# Format
ll_yr <- data1_orig %>% 
  setNames(c("date_dummy", "year", "comm_name", "region", "landings_lb")) %>% 
  mutate(dataset="California Fish Market Catch Landings, Long List, 1928-2002, Monthly", 
         date_dummy=date_dummy %>% gsub("T00:00:00Z", "", .) %>% ymd(),
         month=month(date_dummy)) %>% 
  select(dataset, year, month, date_dummy, comm_name, region, landings_lb)

# Format
ll_mon <- data2_orig %>% 
  setNames(c("date_dummy", "year", "comm_name", "region", "landings_lb")) %>% 
  mutate(dataset="California Fish Market Catch Landings, Long List, 1928-2002, Yearly",
         date_dummy=date_dummy %>% gsub("T00:00:00Z", "", .) %>% ymd()) %>% 
  select(dataset, year, date_dummy, comm_name, region, landings_lb)

# Format
sl_mon <- data3_orig %>% 
  setNames(c("date_dummy", "year", "comm_name", "region", "landings_lb")) %>% 
  mutate(date_dummy=date_dummy %>% gsub("T00:00:00Z", "", .) %>% ymd(),
         dataset="California Fish Market Catch Landings, Short List, 1928-2002, Monthly",
         month=month(date_dummy)) %>% 
  select(dataset, year, month, date_dummy, comm_name, region, landings_lb)

# Format
sl_yr <- data4_orig %>% 
  setNames(c("date_dummy", "year", "comm_name", "region", "landings_lb")) %>% 
  mutate(dataset="California Fish Market Catch Landings, Short List, 1928-2002, Yearly",
         date_dummy=date_dummy %>% gsub("T00:00:00Z", "", .) %>% ymd())  %>% 
  select(dataset, year, date_dummy, comm_name, region, landings_lb)

# Export data
save(ll_yr, ll_mon, sl_mon, sl_yr, file=file.path(datadir, "1928_2002_CA_landings_data_swfsc_erddap.Rdata"))

