
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(ggmap)
library(mapview)
library(tidyverse)
library(gridExtra)

# Directories
datadir <- "data/landings/cdfw/public/port_key"
plotdir <- "data/landings/cdfw/public/figures"

# Google Maps API key
source("data/landings/pacfin/ggmap_api_key.R")
register_google(key=ggmap_api_key)

# Read data
ports_orig <- readxl::read_excel(file.path(datadir, "port_key_v1.xlsx"))

# Read Dcrab port data
ports_dcrab <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/cdfw/landings_confidential/processed/CDFW_dungeness_landings_data_ports_xy.csv", as.is=T)
ports_pacfin <- read.csv("data/landings/pacfin/processed/pacfin_port_codes_clean.csv", as.is=T)
  
# Step 1. Build initial key
################################################################################

# If initial build
step1_yn <- F
if(step1_yn){

  # Build key
  port_key <- ports_orig %>% 
    # Add coordinates from Dcrab and PACFIN
    left_join(ports_dcrab %>% select(port, lat_dd, long_dd), by="port") %>% 
    left_join(ports_pacfin %>% select(port_name, lat_dd, long_dd), by=c("port"="port_name")) %>% 
    # Format data
    filter(port!="All Other Ports") %>% 
    rename(lat_dd1=lat_dd.x, long_dd1=long_dd.x,
           lat_dd2=lat_dd.y, long_dd2=long_dd.y) %>% 
    mutate(port_long=paste0(port, ", California")) %>% 
    select(port_complex, port, port_long, everything())
  
  # Geocode locations
  port_key_xy <- mutate_geocode(port_key, port_long)
  
  # Convert to SF
  port_key_xy_sf <- port_key_xy %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Plot to check
  mapview(port_key_xy_sf)
  
  # Final formatting
  port_key_xy_out <- port_key_xy %>% 
    rename(long_dd3=lon, lat_dd3=lat) %>% 
    select(port_complex:long_dd2, lat_dd3, long_dd3)
  
  # Export
  write.csv(port_key_xy_out, file=file.path(datadir, "port_key_v2_w_coords.csv"), row.names=F)

}


# Step 2. Build final key
################################################################################

# Read port
port_key2 <- read.csv(file.path(datadir, "port_key_v2_w_coords.csv"), as.is=T)

# Format
# Big Creek and Mill Creek are wrong
port_key3 <- port_key2 %>% 
  # Fix Big Creek, Monterey
  mutate(lat_dd_final=ifelse(port=="Big Creek", lat_dd1, lat_dd3),
         long_dd_final=ifelse(port=="Big Creek", long_dd1, long_dd3)) %>% 
  # Fix Mill Creek, Monterey
  mutate(lat_dd_final=ifelse(port=="Mill Creek", 35.982551, lat_dd_final),
         long_dd_final=ifelse(port=="Mill Creek", -121.491901, long_dd_final)) 

# Convert to SF
port_key3_sf <- port_key3 %>%
  st_as_sf(coords = c("long_dd_final", "lat_dd_final"), crs = 4326)

# Plot to check
mapview(port_key3_sf)

# Calculate average for complex
complex_vals <- port_key3 %>% 
  group_by(port_complex) %>% 
  summarize(lat_dd=mean(lat_dd_final),
            long_dd=mean(long_dd_final)) %>% 
  mutate(port=paste("Other", port_complex, "Ports")) %>% 
  select(port_complex, port, everything())

# Merge
port_key4 <- port_key3 %>% 
  select(port_complex, port, lat_dd_final, long_dd_final) %>% 
  rename(lat_dd=lat_dd_final, long_dd=long_dd_final) %>% 
  bind_rows(complex_vals) %>% 
  arrange(port_complex, port)

# Export
write.csv(port_key4, file=file.path(datadir, "port_key_v3.csv"), row.names=F)


# Step 2. Build final key
################################################################################

# Areas 
areas <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", "Sacramento Delta", "Monterey", "Morro Bay",
           "Santa Barbara", "Los Angeles", "San Diego")

# Remove other ports
port_key4_plot <- port_key4 %>% 
  filter(!grepl("Other", port)) %>% 
  mutate(port_complex=factor(port_complex, levels=areas))

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_text(size=12),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=16),
                  legend.position = c(0.15, 0.15),
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
  geom_point(port_key4_plot, mapping=aes(x=long_dd, lat_dd, color=port_complex)) +
  ggrepel::geom_text_repel(port_key4_plot, mapping=aes(x=long_dd, lat_dd, label=port, color=port_complex), show.legend = F) +
  # Labels
  labs(x="", y="") +
  # Crop plot
  coord_sf(xlim = c(-125.5, -116.6), ylim = c(32, 42)) +
  # Legend
  scale_color_discrete(name="Port complex\n(north to south)") +
  # Add Mendoncino-Sonoma county line
  # geom_hline(yintercept=38.77, size=0.5) + 
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "CA_commercial_fishing_ports.pdf"), 
       width=8.5, height=11, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "CA_commercial_fishing_ports.png"), 
       width=8.5, height=11, units="in", dpi=600)




