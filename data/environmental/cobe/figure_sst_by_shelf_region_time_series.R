

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)

# Directories
indir <- "data/environmental/cobe/raw"
outdir <- "data/environmental/cobe/processed"
plotdir <- "data/environmental/cobe/figures"

# Read data
data_orig <- read.csv(file.path(outdir, "COBE_1891_2020_sst_by_fishing_grounds.csv"), as.is=T)

# Plot data
################################################################################

# My themes
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=10),
                   strip.text=element_text(size=10),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot data
g <- ggplot(data_orig, aes(x=year, y=sst_c, color=state)) +
  geom_line() +
  # Labels
  labs(x="", y="Sea surface temperate (°C)") +
  scale_x_continuous(breaks=seq(1890,2020,10)) +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "figure_cobe_sst_zone_time_series.png"), 
       width=4, height=4, units="in", dpi=600)


# Plot data
################################################################################

# Prepare California data
data_ca <- data_orig %>% 
  # Reduce
  filter(state=="California" & year>=1930) %>% 
  # Calculate anomaly
  mutate(sst_c_anom=sst_c-mean(sst_c[year %in% 1950:1980]))

# Plot data
g <- ggplot(data_ca, aes(x=year, y=sst_c_anom)) +
  # Add MHW rectangle
  geom_rect(ymin=-1, ymax=1.5, xmin=2013.5, xmax=2015.5, fill="grey80", color=NA) +
  annotate(geom="text", x=2013, y=1.3, label='The "blob"', hjust=1, color="grey30") +
  # Add SST line
  geom_line() +
  # Labels
  labs(x="", y="SST anomaly (°C)\n(relative to 1950-1980 average)") +
  scale_x_continuous(breaks=seq(1890,2020,10)) +
  # Horizontal line
  geom_hline(yintercept = 0, linetype="dashed", color="grey30") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_cobe_sst_zone_time_series_ca.png"), 
       width=6.5, height=3, units="in", dpi=600)


