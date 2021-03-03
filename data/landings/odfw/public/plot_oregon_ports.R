

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/odfw/public/processed"
plotdir <- "data/landings/odfw/public/figures"

# Read ports
ports <- readxl::read_excel(file.path(datadir, "oregon_ports.xlsx"))

# Order port complexed
sort(unique(ports$port_complex))
port_complexes <- c("Columbia River", "Tillamook", "Newport", "Coos Bay", "Brookings", "Inland")
ports <- ports %>% 
  mutate(port_complex=factor(port_complex, levels=port_complexes))

# Merge data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  legend.text = element_text(size=6),
                  legend.title = element_text(size=8),
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.1) +
  # Plot ports
  geom_point(ports, mapping=aes(x=long_dd, lat_dd, color=port_complex)) +
  ggrepel::geom_text_repel(ports, mapping=aes(x=long_dd, lat_dd, label=port, color=port_complex), show.legend = F, size=2) +
  # Labels
  labs(x="", y="") +
  # Crop plot
  coord_sf(xlim = c(-130, -116), ylim = c(41.5, 47)) +
  # Legend
  scale_color_discrete(name="Port complex\n(north to south)") +
  # Add Mendoncino-Sonoma county line
  # geom_hline(yintercept=38.77, size=0.5) + 
  theme_bw() + my_theme +
  theme(legend.position=c(0.1, 0.7))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "oregon_commercial_fishing_ports.png"), 
       width=6.5, height=4, units="in", dpi=600)


