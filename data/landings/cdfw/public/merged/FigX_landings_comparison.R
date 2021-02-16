
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
cdfw_waters <- wcfish::cdfw_waters
cdfw_ports <- wcfish::cdfw_ports

# Build data
################################################################################

# NOAA
noaa_ca <- noaa %>% 
  # California
  filter(state=="California") %>% 
  # Annual sums
  group_by(year) %>% 
  summarise(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  # Source
  mutate(source="NOAA")

# NOAA
pacfin_ca <- pacfin %>% 
  # California
  filter(state=="California") %>% 
  # Annual sums
  group_by(year) %>% 
  summarise(landings_mt=sum(landings_mt, na.rm=T),
            revenues_usd=sum(revenues_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Convert units
  mutate(landings_kg=landings_mt*1000, 
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>% 
  # Rename
  rename(value_usd=revenues_usd) %>% 
  # Source
  mutate(source="PACFIN")

# CDFW waters
cdfw1 <- cdfw_waters %>% 
  # Remove shipments
  filter(waters!="Shipments") %>% 
  # Annual sums
  group_by(year) %>% 
  summarise(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Add source
  mutate(source="CDFW (by waters)")

# CDFW ports
cdfw2 <- cdfw_ports %>% 
  # Remove shipments
  filter(type=="Landings") %>% 
  # Annual sums
  group_by(year) %>% 
  summarise(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Add source
  mutate(source="CDFW (by port)")

# Merge
data <- bind_rows(noaa_ca, pacfin_ca, cdfw1, cdfw2) %>% 
  # Remove zeros (not read)
  filter(landings_lb!=0) %>% 
  # Make complete (to handle NAs)
  complete(source, year)

# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=10),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=10),
                    strip.text=element_text(size=10),
                    plot.title=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

# Plot 
g <- ggplot(data, aes(x=year, y=landings_lb/1e6, color=source)) +
  geom_line(alpha=0.6, lwd=1) +
  # Limits
  scale_x_continuous(breaks=seq(1930,2020, 10)) +
  # Labels
  labs(x='Year', y="Landings (millions of lbs)",
       title="Annual California commercial fisheries landings by source") +
  # Legend
  scale_color_discrete(name="Source") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.87,0.75),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_landings_comparison.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

