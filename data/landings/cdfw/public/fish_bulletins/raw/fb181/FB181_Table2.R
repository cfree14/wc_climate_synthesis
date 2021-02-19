
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/cdfw/public/fish_bulletins/raw/fb181/raw"
outputdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb181/processed"

# Format data 
data_orig <- read.csv(file.path(datadir, "FB181_Table2_1916_1999_ca_landings_shipments_imperfect.csv"), as.is=T)


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(seafood_lb=quantity_lb) %>%
  # Format source/table
  mutate(table="Table 2",
         source="FB 181") %>% 
  # Format production
  mutate(seafood_lb=gsub(",", "", seafood_lb) %>% as.numeric() * 1000, # was in thousands of pounds
         seafood_kg=measurements::conv_unit(seafood_lb, "lbs", "kg"),
         seafood_mt=seafood_kg/1000) %>% 
  # Arrange
  select(source, table, year, seafood_lb, seafood_kg, seafood_mt, everything())

# Export
write.csv(data, file=file.path(outputdir, "FB181_Table2_1916_1999_ca_landings_shipments.csv"), row.names=F)













