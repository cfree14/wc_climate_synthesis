

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/fb181/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/fb181/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/fb181/figures"

# Read data
data_orig <- read.csv(file.path(indir, "tabula-FB181_Table6_annual_sablefish_landings_adjusted.csv"), as.is=T)

# NOTE
# I did not check observed and reported totals

# Format data
################################################################################

# Format data
data_all <- data_orig %>% 
  janitor::clean_names("snake") %>% 
  # Gather
  gather(key="year", value="landings_lb", 2:ncol(.)) %>% 
  # Format
  mutate(year=gsub("x", "", year) %>% as.numeric(),
         landings_lb=gsub(",", "", landings_lb) %>% as.numeric())

# Remove totals
tots_rep <- data_all %>% 
  filter(port_complex %in% c("Total (mt)", "Total (pounds)"))

# Final format
data <- data_all %>% 
  filter(!port_complex %in% c("Total (mt)", "Total (pounds)")) %>% 
  mutate(source="FB 181 Table 6") %>% 
  select(source, everything())

# Export data
################################################################################

# Export data
write.csv(data, file=file.path(outdir, "FB181_Table6_1987_1999_sablefish_landings_by_port_complex_adjusted.csv"), row.names=F)



