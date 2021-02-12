
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(stars)
library(raster)
library(tidyverse)

# Directories
indir <- "data/environmental/cobe/raw"
outdir <- "data/environmental/cobe/processed"
fgdir <- "data/environmental/cobe/fishing_grounds"

# Read EEZs
eezs <- sf::st_read(dsn="/Users/cfree/Dropbox/Chris/UCSB/data/eezs/World_EEZ_v11_20191118_LR", layer="eez_v11_lowres") %>% 
  filter(SOVEREIGN1%in%c("United States", "Mexico")) %>% 
  sf::st_make_valid()

# Read GEBCO bathymetry data
gebco <- raster::raster("/Users/cfree/Dropbox/Chris/UCSB/data/bathymetry/GEBCO_2019/GEBCO_2019.nc")


# Build data
################################################################################

# Depth limit for fishing grounds (meters)
depth_limit_m <- measurements::conv_unit(600, from="ft", to="m") * -1

# Bounding box
bbox <- extent(-130, -90, 10, 50)

# Reclassification matrix (inside/outside fishing grounds)
reclass_mat <- matrix(data=c(-Inf, depth_limit_m, NA,
                             depth_limit_m, 0, 1,
                             0, Inf, NA), byrow=T, ncol=3)
# Subset data
gebco_wc <- gebco %>% 
  # Crop to West Coast
  crop(bbox) 

# Reclassify data
gebco_wc_rcl <- reclassify(gebco_wc, rcl=reclass_mat)
# plot(gebco_wc_rcl)

# Convert to polygon using the STARS package
# (aka not using raster::rasterToPolygons)
gebco_wc_rcl_poly <- gebco_wc_rcl %>% 
  stars::st_as_stars() %>% 
  sf::st_as_sf(as_points = FALSE, merge = TRUE) %>% 
  sf::st_make_valid()

# Plot check
plot(gebco_wc_rcl_poly)

# Clip by EEZ
fg_eez <- sf::st_intersection(gebco_wc_rcl_poly, eezs)

plot(fg_eez["SOVEREIGN1"])

# Break into parts
fg_eez_part <- fg_eez %>% 
  summarize() %>% 
  # Break into multipolyon
  sf::st_cast("POLYGON") %>%
  sf::st_cast("MULTIPOLYGON") %>%
  # Add unique identifier
  mutate(id=1:n()) %>%
  dplyr::select(id, everything()) 


# Export and perform erase in QGIS
sf::st_write(fg_eez_part, dsn=file.path(fgdir, "fishing_grounds_messy.shp"))

# Read cleanned
################################################################################

new <- sf::st_read(dsn=fgdir, layer="fishing_grounds_clean")

new1 <- new %>% 
  


