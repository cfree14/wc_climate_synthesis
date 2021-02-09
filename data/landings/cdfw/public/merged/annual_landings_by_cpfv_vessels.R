
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

# Read FB XXX data (XXXX-XXXX)

# Read FB 181 data (1987-1999)
data2_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table7a_1987_1999_rec_landings_by_species.csv", as.is=T)

# Read website data (2000-2019)
data3_orig <- read.csv("data/landings/cdfw/public/website/cpfv/processed/CDFW_2000_2019_cpfv_landings_by_port_complex_species.csv", as.is=T)


# Build data
################################################################################

# Format website data
data3 <- data3_orig %>% 
  # Format common names
  rename(comm_name_orig=species) %>% 
  mutate(comm_name1=wcfish::reverse_names(comm_name_orig),
         comm_name1=recode(comm_name1, 
                           "Kelp (calico) bass"="Kelp bass",
                           "King (chinook) salmon"="King salmon"))


wcfish::check_names(data3$comm_name1, 2)
