
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# Read website data
data_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table2_1916_1999_ca_landings_shipments.csv", as.is=T)

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(seafood_lb=quantity_lb) %>%
  # Format source/table
  mutate(table="Table 2",
         source="FB 181") %>% 
  # Format production
  mutate(seafood_lb=gsub(",", "", seafood_lb) %>% as.numeric(),
         seafood_kg=measurements::conv_unit(seafood_lb, "lbs", "kg"),
         seafood_mt=seafood_kg/1000)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1916_1999_total_landings_shipments.Rds"))
write.csv(data, file=file.path(outdir, "CDFW_1916_1999_total_landings_shipments.csv"), row.names=F)




