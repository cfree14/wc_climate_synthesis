

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)

# Directories
sstdir <- "data/environmental/cobe/processed"
fgdir <- "data/environmental/cobe/fishing_grounds"

# Read SST data
sst_ras <- raster::brick(file.path(sstdir, "COBE_1891_2021_annual_sst_mean.grd"))

# Read fishing groung polygons
fg_polys <- readRDS(file.path(fgdir, "west_coast_fishing_grounds.Rds"))


# Extract data
################################################################################

# Calculate mean SST in zones
sst_ts_mat <- raster::extract(x=sst_ras, y=fg_polys, method="simple", fun="mean", na.rm=T)

# Format data
sst_ts_df <- sst_ts_mat %>% 
  # Convert to df
  as.data.frame() %>% 
  # Add state
  mutate(state=fg_polys$state) %>% 
  select(state, everything()) %>% 
  # Gather
  gather(key="year", value="sst_c", 2:ncol(.)) %>% 
  # Convert year
  mutate(year=gsub("X", "", year) %>% as.numeric()) %>% 
  # Arrange
  arrange(state, year) %>% 
  # Filter
  filter(year!=2021)

# Plot data
g <- ggplot(sst_ts_df, aes(x=year, y=sst_c, color=state)) +
  geom_line() + theme_bw()
g

# Export data
################################################################################

# Export data
write.csv(sst_ts_df, file=file.path(sstdir, "COBE_1891_2020_sst_by_fishing_grounds.csv"), row.names=F)


  