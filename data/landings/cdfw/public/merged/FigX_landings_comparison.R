
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# Get data
noaa <- wcfish::noaa
pacfin <- wcfish::pacfin_all6

# Build data
################################################################################

# NOAA
noaa_ca <- noaa %>% 
  filter(state=="California") %>% 
  group_by(year) %>% 
  summarise(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  mutate(source="NOAA")

# NOAA
pacfin_ca <- pacfin %>% 
  filter(state=="California") %>% 
  group_by(year) %>% 
  summarise(landings_mt=sum(landings_mt, na.rm=T),
            revenues_usd=sum(revenues_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(landings_kg=landings_mt*1000, 
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>% 
  rename(value_usd=revenues_usd) %>% 
  mutate(source="PACFIN")

# Merge
data <- bind_rows(noaa_ca, pacfin_ca)

# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
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

# Plot 
g <- ggplot(data, aes(x=year, y=landings_lb/1e6, color=source)) +
  geom_line() +
  # Labels
  labs(x='Year', y="Landings (millions of lbs)") +
  # Theme
  theme_bw()
g
