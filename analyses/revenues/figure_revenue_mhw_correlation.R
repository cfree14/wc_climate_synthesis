
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
indices_orig <- readRDS(file.path(envidir, "1982_2020_daily_mhw_stats_regional.Rds"))
landings_orig <- readRDS(file=file.path(pacfindir, "PACFIN_ALL001_1980_2021_all_species_landings.Rds"))


# Function to make plots
###########################################################################################
  
# Annual sum area by region
indices <- indices_orig %>% 
  # Add state
  mutate(state=ifelse(region_id==1, "Washington",
                      ifelse(region_id==2, "Oregon", "California"))) %>% 
  # Mean
  group_by(state, year) %>% 
  summarize(mhw=sum(coverage_perc)) %>% 
  ungroup()
  
# Landings by state and management group
landings_mgmt <- landings_orig %>% 
  # Eliminate At-Sea
  filter(state!="At-Sea") %>% 
  # Total by year
  group_by(state, comm_name, year) %>% 
  summarise(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Add indices
  left_join(indices) %>% 
  # Eliminate NAs
  na.omit()
  
 