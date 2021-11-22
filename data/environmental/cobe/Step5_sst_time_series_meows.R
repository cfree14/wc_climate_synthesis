

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)

# Directories
sstdir <- "data/environmental/cobe/processed"

# Read COBE SST annual averages
sst_brick <- raster::brick(file.path(sstdir, "COBE_1891_2021_annual_sst_mean.grd"))

# Get MEOWs
meows <- marineregions::meows


# Build annual averages
################################################################################

# MEOWs assess ids
ecoregions <- meows$ecoregion %>% as.character()

# Extract averages and SDs
sst_meow_avg <- extract(sst_brick,meows, method="simple", fun=mean, na.rm=T)
sst_meow_sd <- extract(sst_brick,meows, method="simple", fun=sd, na.rm=T)

# Format data
sst_meow_avg1 <- sst_meow_avg %>% 
  # Convert to data frame
  as.data.frame() %>% 
  # Add names
  mutate(ecoregion=ecoregions) %>% 
  # Arrange and gather
  dplyr::select(ecoregion, everything()) %>% 
  gather(key="year", value="sst_c_avg", 2:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X", "", .) %>% as.numeric())

# Format data
sst_meow_sd1 <- sst_meow_sd %>% 
  # Convert to data frame
  as.data.frame() %>% 
  # Add names
  mutate(ecoregion=ecoregions) %>% 
  # Arrange and gather
  dplyr::select(ecoregion, everything()) %>% 
  gather(key="year", value="sst_c_sd", 2:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X", "", .) %>% as.numeric())

# Merge data
data <- sst_meow_avg1 %>% 
  # Add SDs
  left_join(sst_meow_sd1, by=c("ecoregion", "year")) %>% 
  # Remove 2021 since incomplete
  filter(year!=2021) %>% 
  # Add MEOW meta-data
  left_join(meows %>% select(-geometry), by="ecoregion") %>% 
  # Arrange
  select(realm, province, ecoregion, year, sst_c_avg, sst_c_sd) %>% 
  arrange(realm, province, ecoregion, year)


# Export data
################################################################################

# Export
write.csv(data, file=file.path(sstdir, "COBE_1981_2020_by_meow.csv"), row.names=F)

