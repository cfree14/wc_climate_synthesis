
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(marineregions)

# Directories
plotdir <- "analyses/productivity/figures"

# World
states <- rnaturalearth::ne_states(country=c("Canada", "United States of America", "Mexico", "Russia"), scale="small", returnclass = "sf")
world <- rnaturalearth::ne_countries(returnclass = "sf")

# LMEs
lmes_use <- marineregions::lmes %>%
  filter(lme_name %in% c("Gulf of Alaska", "East Bering Sea", "Aleutian Islands", "California Current"))

meows <- marineregions::meows
meows_use <- meows %>% 
  filter(province=="Cold Temperate Northeast Pacific" | ecoregion=="Eastern Bering Sea")

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot map
g1 <- ggplot() +
  # LMEs
  geom_sf(data=meows_use, mapping=aes(fill=ecoregion), color=NA, alpha=0.3) +
  # Plot land
  geom_sf(data=world, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=states, fill="grey90", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim=c(-111, -180), ylim=c(32,65)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text=element_blank(),
        legend.position = "none")
g1
