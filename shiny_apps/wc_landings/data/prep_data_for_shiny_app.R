
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "shiny_apps/wc_landings/data"


# Spatial data
################################################################################

# # Get US states and Mexico
# usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
# mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
# canada <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")
# 
# # Export
# save(usa, mexico, file=file.path(outputdir, "country_shapefiles.Rdata"))


# NOAA data
################################################################################

# Read NOAA data
noaa_orig <- readRDS("data/landings/noaa/processed/NOAA_1950_2019_usa_landings_by_state_species.Rds")

# Format
noaa <- noaa_orig %>% 
  filter(state %in% c("California", "Oregon", "Washington"))

# Export
saveRDS(noaa, file=file.path(outputdir, "NOAA_1950_2019_wc_landings_by_state_species.Rds"))


# OceanAdapt data
################################################################################

# Read 
data_oa <- readRDS("data/oceanadapt/processed/OA_1977_2018_distribution_shifts.Rds")

# Export
saveRDS(data_oa, file=file.path(outputdir, "OA_1977_2018_distribution_shifts.Rds"))

# RAM Legacy data
################################################################################

# Read
load("data/ramldb/processed/RAM_wc_data.Rdata")
data_ram <- wc_data
stocks_ram <- wc_stocks

# Export
save(data_ram, stocks_ram, file=file.path(outputdir, "RAM_wc_data.Rdata"))
