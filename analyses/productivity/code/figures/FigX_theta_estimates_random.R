

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

# Read data
load(file.path(datadir, "pella_0.01p_random_sst.Rdata"))

# Extract results
results_orig <- results
results <- results_orig[[1]]

# 
mu <- results_orig[[2]]$estimate[1]
mu_sd <- results_orig[[2]]$estimate[2]
mu_lo <- mu-1.96*mu_sd
mu_hi <- mu+1.96*mu_sd


# Build data
################################################################################

# Format data
data <- results %>% 
  # Reduce
  select(stockid, betaT, betaT_lo, betaT_hi, betaT_inf) %>% 
  # Order
  arrange(desc(betaT)) %>% 
  mutate(stockid=factor(stockid, levels=stockid)) %>% 
  mutate(betaT_inf=factor(betaT_inf, levels=c("negative", "none", "positive"))) %>% 
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
g <- ggplot() +
  # Global value
  geom_vline(xintercept = mu, color="black") +
  # Vertical
  geom_vline(xintercept = 0, color="grey30", linetype="dotted") +
  # Lines
  geom_errorbar(data=data, mapping=aes(y=stockid, xmin=betaT_lo, xmax=betaT_hi, color=betaT_inf), width=0, alpha=0.4) +
  geom_point(data=data, mapping=aes(y=stockid, x=betaT, color=betaT_inf)) +
  # Labels
  labs(x="Temperature effect", y="") +
  # Legend
  scale_color_manual(name="", values=c("red", "black", "blue"), drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_theta_estimates_random.png"), 
       width=6, height=8, units="in", dpi=600)

