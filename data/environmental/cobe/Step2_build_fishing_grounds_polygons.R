
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
depth_ras <- raster::raster("/Users/cfree/Dropbox/Chris/UCSB/data/bathymetry/GEBCO_2019/GEBCO_2019.nc")


# Build data
################################################################################

# Step 0. Set depth threshhold
# Depth limit for fishing grounds (meters)
depth_limit_m <- measurements::conv_unit(600, from="ft", to="m") * -1

# Bounding box
bbox <- extent(-130, -90, 10, 50)

# Reclassification matrix (inside/outside fishing grounds)
reclass_mat <- matrix(data=c(-Inf, depth_limit_m, NA,
                             depth_limit_m, 0, 1,
                             0, Inf, NA), byrow=T, ncol=3)

# Step 1. Crop depth raster to West coast
depth_ras_wc <- depth_ras %>% 
  # Crop to West Coast
  crop(bbox) 

# Step 2. Reclassify raster by depth threshold
depth_ras_wc_rcl <- reclassify(depth_ras_wc, rcl=reclass_mat)
# plot(gebco_wc_rcl)

# Step 3. Convert to polygon using the STARS package
# (aka not using raster::rasterToPolygons)
depth_poly <- depth_ras_wc_rcl %>% 
  stars::st_as_stars() %>% 
  sf::st_as_sf(as_points = FALSE, merge = TRUE) %>% 
  sf::st_make_valid()
# plot(depth_poly)

# Step 4. Clip depth  polygon to EEZs of interest (USA/Mexico)
depth_poly_eez <- sf::st_intersection(depth_poly, eezs)
# plot(depth_poly_eez["SOVEREIGN1"])

# Step 5. Build state lines

# California-Oregon border
ca_or_border <- sf::st_linestring(x=matrix(data=c(-130,42,
                                          -110,42), ncol=2, byrow = T), dim="XY") %>% 
  sf::st_sfc(crs=sf::st_crs(depth_poly_eez))

# Oregon-Washington border
or_wa_border <- sf::st_linestring(x=matrix(data=c(-130,46.25,
                                                  -110,46.25), ncol=2, byrow = T), dim="XY") %>% 
  sf::st_sfc(crs=sf::st_crs(depth_poly_eez))

# Plot check
g <- ggplot() +
  geom_sf(data=depth_poly_eez, mapping=aes(fill=SOVEREIGN1)) +
  geom_sf(data=ca_or_border) +
  geom_sf(data=or_wa_border) +
  theme_bw()
g

# Step 5. Break into parts
depth_poly_eez_parts <- depth_poly_eez %>% 
  # Simplify
  dplyr::select(SOVEREIGN1) %>% 
  rename(country=SOVEREIGN1) %>% 
  # Dissolve
  group_by(country) %>% 
  summarize() %>% 
  # Split by line
  # sf::st_intersection(ca_or_border) %>%
  # Break into multipolyon
  sf::st_cast("POLYGON") %>%
  sf::st_cast("MULTIPOLYGON") %>%
  # Add unique identifier
  mutate(id=1:n()) %>%
  dplyr::select(id, country, everything()) 

# Export and perform manual erase in ArcGIS by BRIAN FREE!!!
sf::st_write(depth_poly_eez_parts, dsn=file.path(fgdir, "fishing_grounds_messy.shp"), append=F)


# Read cleaned
################################################################################

# Read data
fg_cut <- sf::st_read(dsn=fgdir, layer="west_coast_fisheries")

# Format data
fg <- fg_cut %>% 
  # Dissolve
  group_by(country, state) %>% 
  summarise()

# Plot data
g <- ggplot() +
  geom_sf(data=fg_cut, mapping=aes(fill=state))
g

# Export data
saveRDS(fg, file.path(fgdir, "west_coast_fishing_grounds.Rds"))

  


