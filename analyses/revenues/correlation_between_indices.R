
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(corrgram)
library(tidyverse)

# Directories
envidir <- "data/environmental/indices/processed"
pacfindir <- "data/landings/pacfin/processed"
plotdir <- "analyses/revenues/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "indices_coastwide.Rds"))


# Setup
################################################################################

# Format data 
data <- data_orig %>% 
  # Simplify
  select(index_abbrev, date, value) %>% 
  # Spread
  spread(key="index_abbrev", value="value") %>% 
  # Eliminate NOI, which is monthly (not daily)
  select(-NOI)

# Plot correlogram
g <- corrgram(data, lower.panel = "panel.pts", upper.panel = "panel.shade")

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_indices_correlation.png"), 
       width=4.5, height=4.5, units="in", dpi=600)

