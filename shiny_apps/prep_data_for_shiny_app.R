
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

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")

# Export
save(usa, mexico, file=file.path(outputdir, "country_shapefiles.Rdata"))


# NOAA data
################################################################################

# Read NOAA data
noaa_orig <- readRDS("data/landings/noaa/processed/1950_2019_usa_landings_by_state_species.Rds")

# Format
noaa <- noaa_orig %>% 
  filter(state %in% c("California", "Oregon", "Washington"))

# Export
saveRDS(noaa, file=file.path(outputdir, "NOAA_1950_2019_wc_landings_by_state_species.Rds"))
