

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "analyses/productivity/output"
tabledir <- "analyses/productivity/tables"
plotdir <- "analyses/productivity/figures"

# Read fixed data
load(file.path(datadir, "pella_0.01p_fixed_sst.Rdata"))
results_fixed <- results

# Read random data
load(file.path(datadir, "pella_0.01p_random_sst.Rdata"))
results_random <- results[[1]]

# Build data
################################################################################

# Format data
data <- results_fixed %>% 
  # Simplify
  select(stockid, betaT) %>% 
  # Rename
  rename(betaT_fixed=betaT) %>% 
  # Add random
  left_join(results_random %>% select(stockid, betaT)) %>% 
  rename(betaT_random=betaT) %>% 
  # Remove problem stock
  filter(stockid!="STFLOUNSPCOAST")
  

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   legend.position = "bottom")

# Spline bars
g <- ggplot(data, aes(x=betaT_fixed, y=betaT_random)) +
  geom_point() +
  # Reference lines
  geom_vline(xintercept=0, linetype="dotted") +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_abline(slope=1) +
  # Labels
  labs(x="Fixed effects estimate", y="Random effects estimate") +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_theta_estimates_fixed_random_comparison.png"), 
       width=4, height=4, units="in", dpi=600)

