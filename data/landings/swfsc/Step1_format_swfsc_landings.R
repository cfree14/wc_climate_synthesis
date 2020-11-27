
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/landings/swfsc/raw"
outputdir <- "data/landings/swfsc/processed"

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
ll_mon <- data1_orig %>% 
  setNames(c("date_dummy", "year", "comm_name_orig", "port_complex", "landings_lb")) %>% 
  mutate(dataset="California Fish Market Catch Landings, Long List, 1928-2002, Monthly", 
         date_dummy=date_dummy %>% gsub("T00:00:00Z", "", .) %>% ymd(),
         month=month(date_dummy),
         landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  select(dataset, year, month, date_dummy, port_complex, comm_name_orig, landings_lb, landings_kg) %>% 
  filter(port_complex!="All" & landings_lb>0) %>% 
  arrange(year, month, port_complex)

# Format
ll_yr <- data2_orig %>% 
  setNames(c("date_dummy", "year", "comm_name_orig", "port_complex", "landings_lb")) %>% 
  mutate(dataset="California Fish Market Catch Landings, Long List, 1928-2002, Yearly",
         date_dummy=date_dummy %>% gsub("T00:00:00Z", "", .) %>% ymd(),
         landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  select(dataset, year, port_complex, comm_name_orig, landings_lb, landings_kg) %>% 
  filter(port_complex!="All" & landings_lb>0) %>% 
  arrange(year, port_complex)

# Format
sl_mon <- data3_orig %>% 
  setNames(c("date_dummy", "year", "comm_name_orig", "port_complex", "landings_lb")) %>% 
  mutate(date_dummy=date_dummy %>% gsub("T00:00:00Z", "", .) %>% ymd(),
         dataset="California Fish Market Catch Landings, Short List, 1928-2002, Monthly",
         month=month(date_dummy),
         landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  select(dataset, year, month, date_dummy, port_complex, comm_name_orig, landings_lb, landings_kg) %>% 
  filter(port_complex!="All" & landings_lb>0) %>% 
  arrange(year, month, port_complex)

# Format
sl_yr <- data4_orig %>% 
  setNames(c("date_dummy", "year", "comm_name_orig", "port_complex", "landings_lb")) %>% 
  mutate(dataset="California Fish Market Catch Landings, Short List, 1928-2002, Yearly",
         date_dummy=date_dummy %>% gsub("T00:00:00Z", "", .) %>% ymd(),
         landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg"))  %>% 
  select(dataset, year, port_complex, comm_name_orig, landings_lb, landings_kg) %>% 
  filter(port_complex!="All" & landings_lb>0) %>% 
  arrange(year, port_complex)

# Export data
save(ll_yr, ll_mon, sl_mon, sl_yr, file=file.path(outputdir, "1928_2002_CA_landings_data_swfsc_erddap.Rdata"))


# Plot data
################################################################################

# Port_complexs
sort(unique(ll_mon$port_complex))

# LL monthly
ll_mon_stats <- ll_mon %>% 
  filter(port_complex!="All") %>% 
  group_by(year, month, date_dummy, port_complex) %>% 
  summarize(landings_lb=sum(landings_lb))

g <- ggplot(ll_mon_stats, aes(x=date_dummy, y=landings_lb/1e6, fill=port_complex)) +
  geom_area() +
  labs(x="Month", y="Landings (millions of lbs)") +
  theme_bw()
g

# LL annuals
ll_yr_stats <- ll_yr %>% 
  filter(port_complex!="All") %>% 
  group_by(year, port_complex) %>% 
  summarize(landings_lb=sum(landings_lb))

g <- ggplot(ll_yr_stats, aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  geom_area() +
  labs(x="Year", y="Landings (millions of lbs)") +
  theme_bw()
g
