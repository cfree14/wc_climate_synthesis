

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)

# Directories
indir <- "data/environmental/cobe/raw"
outdir <- "data/environmental/cobe/processed"
plotdir <- "data/environmental/cobe/figures"

# Read data
cobe <- raster::brick(file.path(outdir, "COBE_1891_2021_annual_sst_mean.grd"))

# Readfishing grounds
fg <- readRDS(file="data/environmental/cobe/fishing_grounds/west_coast_fishing_grounds.Rds")


# Build data
################################################################################

# Bounding box
bbox <- extent(-130, -90, 10, 50)

# SST data
cobe_use <- cobe[["X2020"]] %>% 
  # Crop
  crop(bbox) %>% 
  # Convert to dataframe
  raster::as.data.frame(xy=T) %>% 
  # Set names
  setNames(c("x", "y", "sst_c"))


# Plot data
################################################################################

# My themes
my_theme <-  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.text=element_text(size=6),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Land
us_states <- rnaturalearth::ne_states(country="United States of America", returnclass="sf")
all_countries <- rnaturalearth::ne_countries(returnclass="sf")

# Plot data
g <- ggplot() +
  # Plot SST
  geom_raster(data=cobe_use, aes(x=x, y=y, fill=sst_c)) +
  # Plot land
  geom_sf(data=all_countries, fill="grey90", col="white", lwd=0.2) +
  geom_sf(data=us_states, fill="grey90", col="white", lwd=0.2) +
  # Plot fishing grounds
  geom_sf(data=fg, color="grey30", fill=NA, lwd=0.2) +
  # Crop
  coord_sf(xlim=c(-126, -92), ylim=c(12, 48)) +
  # Labs
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="SST (°C)\nin 2020", colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "figure_cobe_sst_zone.png"), 
       width=4.5, height=5, units="in", dpi=600)
