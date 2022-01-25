
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/environmental/indices/raw"
outdir <- "data/environmental/indices/processed"
plotdir <- "data/environmental/indices/figures"

# Read data
data_reg_orig <- readRDS(file.path(outdir, "1982_2020_daily_mhw_stats_regional.Rds"))
data_wc_orig <- readRDS(file.path(outdir, "1982_2020_daily_mhw_stats.Rds"))
events_orig <- readRDS(file.path(outdir, "1983_2021_mhw_events.Rds"))

# Read SST data
sst_orig <- readRDS(file.path(outdir, "mursst_data.Rds"))

# Meta-data
# https://oceanview.pfeg.noaa.gov/erddap/info/cciea_OC_MHW_regions/index.html

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")

# Format data
################################################################################

# Format regional data
data_reg <- data_reg_orig %>% 
  # Format region
  mutate(region=recode_factor(region,
                              "Southern California"="sCA",
                              "Central California"="cCA",
                              "Northern California"="nCA",
                              "Oregon"="OR",
                              "Washington"="WA"))

# Format event data
events <- events_orig %>% 
  # Shorten code
  mutate(mhw_name_short=gsub("NEP19|NEP20", "", mhw_name))
  
# Format SST data
sst <- sst_orig %>% 
  filter(date=="2015-09-01")
  
  
  

# Regional plot
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.2, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Events
g1 <- ggplot(events, aes(x=date, y=max_area_km2/1e6, size=duration_d, fill=mean_intensity, label=mhw_name_short)) +
  geom_point(pch=21, stroke=0.1) +
  geom_text(show.legend = F) +
  # Labels
  labs(x="", y="Event coverage\n(millions sqkm)") +
  # Date axis
  scale_x_date(breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="5 years"), date_labels = "%Y") +
  # Legends
  scale_size_continuous(name="Event\nduration (days)", breaks=c(1,10, 100, 500), range=c(0.5, 4)) +
  scale_fill_gradientn(name="Event\nintensity (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value = "grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g1

# Coverage
g2 <- ggplot(data_wc_orig, aes(x=date, y=area_km2/1e6, color=intensity_c)) +
  geom_line() +
  # Labels
  labs(x="", y="Coastwide coverage\n(millions sqkm)") +
  # Date axis
  scale_x_date(breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="5 years"), date_labels = "%Y") +
  # Legend
  scale_color_gradientn(name="Coastwide\nintensity (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g2

#  Coverage
g3 <- ggplot(data_reg, aes(y=region, x=date, fill=coverage_perc)) +
  geom_tile() +
  # Labels
  labs(x="", y=" \nRegion") +
  # Date axis
  scale_x_date(breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="5 years"), date_labels = "%Y") +
  # Legend
  scale_fill_gradientn(name="Regional\ncoverage (%)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g3

#  Intensity
g4 <- ggplot(data_reg, aes(y=region, x=date, fill=intensity_c)) +
  geom_tile() +
  # Labels
  labs(x="", y=" \nRegion") +
  # Date axis
  scale_x_date(breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="5 years"), date_labels = "%Y") +
  # Legend
  scale_fill_gradientn(name="Regional\nintensity (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value = "grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g4

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_mhw_indices.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




# Plot map
################################################################################

# Build map
g5 <- ggplot() +
  # Plot raster
  geom_tile(data=sst, mapping=aes(x=long_dd, y=lat_dd, fill=sst_anom_c)) +
  # Plot regional dividers
  geom_hline(yintercept = c(49, 46, 42, 38.6, 34.448, 32.5)) +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2) +
  # Crop extent
  coord_sf(xlim = c(-130, -114), ylim = c(32, 50)) +
  # Legend
  scale_fill_gradient2(name="SST anamoly (°C)", midpoint=0, mid="white", high="darkred", low="darkblue") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.82, 0.85),
        legend.key.size = unit(0.5, "cm"))
#g5

# Export
ggsave(g5, filename=file.path(plotdir, "figure_mhw_indices_map.png"), 
       width=3.5, height=5.25, units="in", dpi=600)


