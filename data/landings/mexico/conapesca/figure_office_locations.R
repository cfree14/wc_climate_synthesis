

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(ggmap)
library(mapview)
library(tidyverse)

# Directories
inputdir <- "data/landings/mexico/conapesca/raw"
outputdir <- "data/landings/mexico/conapesca/processed"
plotdir <- "data/landings/mexico/conapesca/figures"

# Read data
offices <- readRDS(file.path(outputdir, "CONAPESCA_baja_office_key.Rds"))


# Map
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_states(country="Mexico", returnclass = "sf")


# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  plot.title=element_text(size=10),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.title=element_text(size=8),
                  legend.text=element_text(size=6))

# Plot data
g <- ggplot() +
  # Plot states
  geom_sf(data=usa, fill="grey95", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey80", col="white", size=0.2) +
  # Plot organizations
  geom_point(data=offices, mapping=aes(x=long_dd, y=lat_dd, color=coast), size=2) +
  ggrepel::geom_text_repel(data=offices, mapping=aes(x=long_dd, y=lat_dd, color=coast, label=office), size=3, show.legend = F) +
  # Legend
  scale_color_manual(name="Coast", values=c("darkred", "navy")) +
  # Crop extent
  coord_sf(xlim = c(-118, -109), ylim = c(23, 33)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2,0.2))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_conapesca_offices.png"), 
       width=5.5, height=6.5, units="in", dpi=600)





