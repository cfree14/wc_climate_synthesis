
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"
tabledir <- "analyses/productivity/tables"

# Read stock key
stock_key_orig <- readRDS(file=file.path(outputdir, "RAM_WC_stock_key.Rds"))

# Simplify
################################################################################

# Format data
stock_key <- stock_key_orig %>% 
  # Simplify
  select(region, stockid, stocklong, yrs_prod, yrs_rec, nyr_prod, nyr_rec) %>%
  # Create years column
  mutate(years_prod=ifelse(!is.na(yrs_prod), paste0(yrs_prod, " (", nyr_prod, " yr)"), ""),
         years_rec=ifelse(!is.na(yrs_rec), paste0(yrs_rec, " (", nyr_rec, " yr)"), ""),) %>% 
  select(region, stockid, stocklong, years_prod, years_rec) %>% 
  # Arrange
  arrange(region, stockid)

# Export data
write.csv(stock_key, file=file.path(tabledir, "TableS1_stock_key.csv"), row.names=F)
