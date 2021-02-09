

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "data/landings/cdfw/public/website/by_port/processed"
sppkeydir <- "data/landings/cdfw/public/website/by_port/species_key"
plotdir <- "data/landings/cdfw/public/website/by_port/figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "CDFW_2000_2019_landings_by_port.Rds"))

# Read species key
spp_key <- readxl::read_excel(file.path(sppkeydir, "CA_species_key_v3_group.xlsx"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Add species key
  left_join(spp_key %>% select(comm_name_orig, comm_name, sci_name, level, environment, taxa_group1, taxa_group2, presentation)) %>% 
  # Arrange
  select(year:comm_name_orig, comm_name, sci_name, level, taxa_group1, taxa_group2, environment, presentation, landings_lb:value_usd, everything())

# Inspect data
freeR::complete(data)


# Export data
################################################################################

# Export data
write.csv(data, file=file.path(outputdir, "CDFW_2000_2019_landings_by_port_expanded.csv"), row.names=F)

