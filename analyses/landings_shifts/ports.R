
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
sstdir <- "data/environmental/cobe/processed"
plotdir <- "analyses/landings_shifts/figures"

# Read data
ports_mex <- read.csv("data/landings/mexico/datamares/confidential/processed/office_key.csv", as.is=T) %>% 
  rename(port=office) %>% 
  mutate(state="Mexico") %>% 
  select(state, port, lat_dd, long_dd)
ports_or <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/landings/odfw/public/processed/oregon_ports.xlsx") %>% 
  mutate(state="Oregon") %>% 
  select(state, port, lat_dd, long_dd)
ports_ca <- wcfish::ports %>% 
  select(state, port, lat_dd, long_dd)
blocks_ca <- wcfish::blocks %>% 
  filter(block_state=="California")

# Merge ports
ports <- bind_rows(ports_mex, ports_or, ports_ca)

# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_blank(),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5))

# Countries
states <- rnaturalearth::ne_states(country=c("Mexico", "Canada", "United States of America"), returnclass = "sf")
world <- rnaturalearth::ne_countries(returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks_ca, color="grey60", fill=NA, size=0.1) +
  # Plot land
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2) + 
  geom_sf(data=states, fill="grey80", color="white", lwd=0.2) + 
  # Plot ports
  geom_point(data=ports, mapping=aes(x=long_dd, y=lat_dd, color=state)) +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="") +
  # Crop plot
  coord_sf(xlim = c(-128, -90), ylim = c(10, 50)) +
  theme_bw() + base_theme +
  theme(legend.position = c(0.2, 0.2))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_ports.png"), 
       width=6.5, height=7.5, units="in", dpi=600)
