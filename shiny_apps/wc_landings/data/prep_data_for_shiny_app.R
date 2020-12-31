
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
data_oa <- readRDS("data/oceanadapt/processed/OA_1977_2018_distribution_shifts.Rds") %>% 
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         species_label=ifelse(!is.na(comm_name), paste0(comm_name, " (", species, ")"), species)) %>% 
  select(region, species_orig, species, comm_name, species_label, everything())

# Export
saveRDS(data_oa, file=file.path(outputdir, "OA_1977_2018_distribution_shifts.Rds"))

# RAM Legacy data
################################################################################

# Read data
data_ram_status <- readRDS(file=file.path("data/ramldb/processed/RAM_WC_status_data.Rds"))
data_ram_sr <- readRDS(file=file.path("data/ramldb/processed/RAM_WC_recruitment_data.Rds"))
data_ram_sp <- readRDS(file=file.path("data/ramldb/processed/RAM_WC_production_data.Rds"))

# Export
saveRDS(data_ram_status, file.path(outputdir, "RAM_WC_status_data.Rds"))
saveRDS(data_ram_sr, file.path(outputdir, "RAM_WC_recruitment_data.Rds"))
saveRDS(data_ram_sp, file.path(outputdir, "RAM_WC_production_data.Rds"))







