

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

# Read other key
spp_key_cdfw <- readxl::read_excel("data/landings/cdfw/public/species_key/CA_species_key_v3_group.xlsx") %>% 
  mutate(comm_name_orig=stringr::str_to_title(comm_name_orig))


# Build LL species key
################################################################################

# Step 1. Initial list
spp_key_ll <- ll_yr %>% 
  # Unique species
  select(comm_name_orig) %>% 
  unique() %>% 
  arrange(comm_name_orig) %>% 
  # Add CDFW key
  left_join(spp_key_cdfw %>% select(comm_name_orig, comm_name, sci_name, level, presentation))

# Export
write.csv(spp_key_ll, file=file.path(sppkeydir, "long_list_key_v1.csv"), row.names=F)

# Step 2. Check names
spp_key_ll2 <- readxl::read_excel(file.path(sppkeydir, "long_list_key_v2.xlsx"))
spp_key_ll2_species <- spp_key_ll2$sci_name[spp_key_ll2$level=="species"] %>% unique() %>% sort()
freeR::suggest_names(spp_key_ll2_species)








