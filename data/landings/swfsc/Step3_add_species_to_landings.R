

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
sppkeydir <- "data/landings/swfsc/species_key"

# Read data
load(file.path(outputdir, "1928_2002_CA_landings_data_swfsc_erddap.Rdata"))

# Read species key
spp_key <- readxl::read_excel(file.path(sppkeydir, "long_list_key_v2.xlsx"))


# Build data
################################################################################

# Annual long list
ll_yr_exp <- ll_yr %>%
  left_join(spp_key) %>% 
  select(dataset, port_complex, year, comm_name_orig, sci_name, level, presentation, everything())

# Inspect
freeR::complete(ll_yr_exp)


# Export data
################################################################################

# Export data
write.csv(ll_yr_exp, file=file.path(outputdir, "1928_2002_CA_landings_data_swfsc_longlist_annual.csv"), row.names = F)
