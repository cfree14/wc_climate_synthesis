
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)
library(tigris)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# Read data
data_orig <- readxl::read_excel(file.path(outdir, "port_complex_lines.xlsx")) %>% 
  mutate(ypos=(lat_dd_lo+lat_dd_hi)/2, 
         zone_label=gsub(", ", ",\n", zone))


# Plot data
################################################################################

# Get borders
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
ca_counties <- tigris::counties(state="California", cb=T)

# Plot blocks
g <- ggplot() +
  # Plot land
  geom_sf(data=usa, fill="grey85", col="white", size=0.1) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.1) +
  # Plot and label counties
  geom_sf(data=ca_counties, fill=NA, col="white", size=0.1) +
  # geom_sf_text(ca_counties, mapping=aes(label=NAME), size=2.8, col="white") +
  # Plot zone lines
  geom_hline(yintercept=42, size=0.2) +
  geom_hline(data=data_orig, aes(yintercept=lat_dd_lo), size=0.2) +
  facet_wrap(~system, nrow=1) +
  # Label zones
  geom_text(data=data_orig, mapping=aes(x=-125.5, y=ypos, label=zone_label), 
            size=1, col="black", hjust=0) +
  # Crop plot
  coord_sf(xlim = c(-125.5, -116.6), ylim = c(32, 42)) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text=element_text(size=5),
        axis.title=element_blank(),
        strip.text=element_text(size=6),
        plot.title = element_blank(),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_port_complexes_over_time.png"), 
       width=6.5, height=2.6, units="in", dpi=600)




