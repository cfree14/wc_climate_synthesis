
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
data <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table2_1916_1999_ca_landings_shipments.csv", as.is=T)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1916_1999_total_landings_shipments.Rds"))
write.csv(data, file=file.path(outdir, "CDFW_1916_1999_total_landings_shipments.csv"), row.names=F)
