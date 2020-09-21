

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/pacfin/raw/codes"
outputdir <- "data/landings/pacfin/processed"
plotdir <- "data/landings/pacfin/figures"

# Read data
port_key <- read.csv(file=file.path(outputdir, "pacfin_port_codes_clean.csv"), as.is=T)


# Format data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  plot.title=element_text(size=10),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.title=element_blank(),
                  legend.text=element_text(size=8))

# Plot data
g <- ggplot() +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2) +
  # Plot labels
  labs(title="PACFIN port locations") +
  # Plot organizations
  geom_point(data=port_key %>% filter(port_yn=="yes"), mapping=aes(x=long_dd, y=lat_dd, color=state1), size=1) +
  # Crop extent
  coord_sf(xlim = c(-170, -110), ylim = c(25, 70)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2,0.2))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_pacfin_port_map.png"), 
       width=5.5, height=6.5, units="in", dpi=600)










