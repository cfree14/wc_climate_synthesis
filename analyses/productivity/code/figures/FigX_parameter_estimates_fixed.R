

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(splink)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "analyses/productivity/output"
tabledir <- "analyses/productivity/tables"
plotdir <- "analyses/productivity/figures"

# Read data
output <- readRDS(file.path(datadir, "production_0.01p_sst_fixed.Rds"))

# Get results
results <- splink::get_results(output)

# Build data
################################################################################

# Format data
data <- results %>% 
  # Reduce
  select(stockid, r, k, betaT, sigmaP, contains("lo"), contains("hi")) %>% 
  # Gather
  gather(key="param", value="value", 2:ncol(.)) %>% 
  # Format
  mutate(type=ifelse(grepl("lo", param), "lo", 
                     ifelse(grepl("hi", param), "hi", "est")),
         param=gsub("_lo|_hi", "", param)) %>% 
  # Arrange
  select(stockid, param, type, value) %>% 
  # Spread
  spread(key="type", value="value") %>% 
  # Format parameters
  mutate(param=recode_factor(param,
                             "r"="Intrinsic growth rate, r",
                             "k"="Carrying capacity, K",
                             "sigmaP"="Process error, σ",
                             "betaT"="Temperature effect, θ")) %>% 
  # Arrange
  arrange(param, desc(est)) %>% 
  group_by(param) %>% 
  mutate(order=1:n())


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Histograms
g1 <- ggplot(data, aes(x=est)) +
  facet_wrap(~param, scales="free_x", nrow=1) +
  geom_histogram() +
  # Labels
  labs(x="Estimate", y="Frequency") +
  # Theme
  theme_bw() + my_theme
g1

# Spline bars
g2 <- ggplot(data, aes(y=order, x=est)) +
  facet_wrap(~param, scales="free_x", nrow=1) +
  # Lines
  geom_errorbar(data=data, mapping=aes(y=order, xmin=lo, xmax=hi), width=0, color="grey70") +
  # Points
  geom_point() +
  # Labels
  labs(x="Estimate", y="Stock") +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=2, heights=c(0.4, 0.6))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_parameter_estimates_fixed.png"), 
       width=6.5, height=5, units="in", dpi=600)

