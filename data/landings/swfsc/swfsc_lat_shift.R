


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggmap)
library(rgeolocate)
library(tidyverse)

# Directories
datadir <- "data/landings/swfsc/raw"
outputdir <- "data/landings/swfsc/processed"

# Read data
load(file.path(outputdir, "1928_2002_CA_landings_data_swfsc_erddap.Rdata"))

# Read lat/long key
lats <- read.csv(file.path(datadir, "region_lat_longs.csv"), as.is=T)

# Build data
################################################################################

# Build data
data <- ll_yr %>% 
  # Add lat/long
  left_join(lats) %>% 
  # Summarize landings by year/region
  group_by(year, region, region_lat_dd) %>% 
  summarize(landings_lb=sum(landings_lb)) %>% 
  ungroup() %>% 
  # Calculate catch-weighted latitude
  group_by(year) %>%
  mutate(lat_dd_wt=weighted.mean(x=region_lat_dd, w=landings_lb)) %>% 
  ungroup()

# Plot 
g <- ggplot(data, aes(x=year, y=lat_dd_wt)) +
  geom_line() + 
  scale_x_continuous(breaks=seq(1930, 2020, 10), lim=c(NA, 2020)) +
  labs(x="Year", y="Latitudinal\ncenter of gravity") +
  theme_bw()
g
