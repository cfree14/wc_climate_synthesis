

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/noaa/raw"
outputdir <- "data/landings/noaa/processed"
plotdir <- "data/landings/noaa/figures"

# Read data
data_orig <- read.csv(file.path(inputdir, "landings_by_top_us_ports.csv"), as.is=T, na.strings="")

# Source: https://foss.nmfs.noaa.gov/apexfoss/f?p=215:11:9450396640063::NO:::

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(landing_lb_mil=millions_of_pounds, 
         value_usd_mil=x_millions_of_dollars,
         port_long=port) %>% 
  # Add columns
  mutate(state=gsub(".*,\\s*", "", port_long),
         port=gsub(",.*$", "", port_long)) %>% 
  # Arrange
  select(state, port, port_long, year, everything()) %>% 
  arrange(state, port, year)


# Export data
write.csv(data, file=file.path(outputdir, "NOAA_1981_2019_usa_landings_by_state_port.csv"), row.names=F)





