
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(TMB)
library(devtools)
library(freeR)
library(tidyverse)
library(splink)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")

# Directories
sstdir <- "data/environmental/cobe/processed"
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"

# Read data
data_orig <- readRDS(file=file.path(outputdir, "production_0.01p_sst_random_msy_hindcast.Rds"))

# Build data
data <- data_orig$msy_global %>% 
  filter(year>=1930)

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text=element_text(size=6),
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

# Plot data
g <- ggplot(data, aes(x=year, y=msy_md/1e6)) +
  geom_ribbon(mapping = aes(x = year, ymin = msy_lo/1e6, 
                            ymax = msy_hi/1e6), fill = "grey80") +
  geom_line() +
  # Labels
  labs(x="Year", y="MSY (millions of tons)") +
  scale_x_continuous(breaks=seq(1930,2020,10)) +
  lims(y=c(0,NA)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_msy_hindcast.png"), 
       width=4.5, height=3.5, units="in", dpi=600)
