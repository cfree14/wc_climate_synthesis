
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
data_orig <- read.csv(file.path(datadir, "CA_mkt_catch_update_9948_93e2_8482.csv"), as.is=T, skip=1)

# Website
# https://oceanview.pfeg.noaa.gov/erddap/tabledap/CA_mkt_catch_update.html


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  setNames(c("date", "comm_name_orig", "landings_lb", "port_complex")) %>% 
  # Format
  mutate(date_dummy=date %>% gsub("T00:00:00Z", "", .) %>% ymd(),
         year=year(date_dummy),
         month=month(date_dummy),
         landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  # Arrange
  select(port_complex, year, month, date_dummy, comm_name_orig, landings_lb, landings_kg)

# Inspect
str(data)
table(data$port_complex)
range(data$date_dummy)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "2003_2014_CA_landings_data_swfsc_erddap.Rds"))
