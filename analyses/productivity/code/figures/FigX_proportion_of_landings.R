

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(RColorBrewer)

# Directories
datadir <- "analyses/productivity/output"
tabledir <- "analyses/productivity/tables"
plotdir <- "analyses/productivity/figures"

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2018_fao_landings_data.Rds")

# Build FAO data
################################################################################

fao2000_wc <- fao_orig %>% 
  filter(area_type=="marine" & year==2000 & iso3_use %in% c("USA", "CAN") & grepl("Pacific, North", area) & units=="t") %>% 
  pull(quantity) %>% sum()
  

table(fao2000_wc$area)

# Build RAM data
################################################################################

# Read SP data
output_sp <- readRDS(file.path(datadir, "production_0.01p_sst_fixed.Rds"))
output_sr <- readRDS(file.path(datadir, "recruitment_ricker_sst_fixed.Rds"))

# Extract data
data_sp <- output_sp$data %>% 
  mutate(dataset="Production") %>% 
  select(dataset, stockid, year, c_type, c_units, c)

data_sr <- output_sr$data %>% 
  mutate(dataset="Recruitment") %>% 
  select(dataset, stockid, year, c_type, c_units, c)

# Which years have all data?
nstocks_sp <- n_distinct(data_sp$stockid)
table(data_sp$year[!is.na(data_sp$c)]) / nstocks_sp # 2000
nstocks_sr <- n_distinct(data_sr$stockid)
table(data_sr$year[!is.na(data_sr$c)]) / nstocks_sr


# Merge and compute statistics
stats <- bind_rows(data_sp, data_sr) %>% 
  group_by(dataset, year) %>% 
  summarise(catch_mt=sum(c, na.rm=T)) %>% 
  filter(year==2000) %>% 
  mutate(prop=catch_mt/fao2000_wc)

