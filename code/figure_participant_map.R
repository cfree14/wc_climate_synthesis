

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(rnaturalearth)


# Directories
datadir <- "admin"
plotdir <- "figures"

# Read data
people <- readxl::read_excel(file.path(datadir, "participants.xlsx"), sheet=1)
locations <-  readxl::read_excel(file.path(datadir, "participants.xlsx"), sheet=2)

# Setup
################################################################################

# Build data
stats <- people %>% 
  left_join(locations) %>% 
  janitor::clean_names("snake") %>% 
  group_by(organization, latitude, longitude) %>% 
  summarize(npeople=n()) %>% 
  ungroup() %>% 
  mutate(org_short=recode(organization, 
                          "Centro de Investigación Científica y de Educación Superior de Ensenada (CICESE)"="CICESE",
                          "DFO Pacific Biological Station"="DFO PBS",                                          
                          "Moss Landing Marine Laboratories"="Moss Landing",                                            
                          "Nature United"="Nature United",                                                             
                          "NOAA Alaska Fisheries Science Center"="NOAA AFSC",                                     
                          "NOAA Northwest Fisheries Science Center"="NOAA NWFSC",                                       
                          "NOAA Southwest Fisheries Science Center"="NOAA SWFSC",                                       
                          "Oregon State University"="OSU",                                                 
                          "Scripps Institution of Oceanography"="Scripps",                                   
                          "TNC"="TNC",                                                                         
                          "UC Santa Barbara"="UCSB",                                                             
                          "University of Alaska Southeast"="U. Alaska Southeast" ))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  plot.title=element_blank(),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.position="bottom", 
                  legend.title=element_text(size=8),
                  legend.text=element_text(size=8))

# Plot data
g <- ggplot() +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2) +
  # Plot organizations
  geom_point(data=stats, mapping=aes(x=longitude, y=latitude), size=2) +
  # Label organizations
  ggrepel::geom_text_repel(data=stats, mapping=aes(x=longitude, y=latitude, label=org_short), size=3.5) +
  # Crop extent
  coord_sf(xlim = c(-170, -110), ylim = c(25, 70)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_participant_map.png"), 
       width=5.5, height=6.5, units="in", dpi=600)



