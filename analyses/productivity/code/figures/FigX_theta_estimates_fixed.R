

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


# Build data
################################################################################

# Extract results
results <- splink::get_results(output)
splink::plot_thetas(results)

# Build data
data <- results %>% 
  # Reduce to thetas
  filter(param=="theta") %>% 
  # Add significance
  mutate(est_inf=ifelse(est_lo>0, "positive",
                        ifelse(est_hi<0, "negative", "none")),
         est_inf=factor(est_inf, levels=c("negative", "none", "positive"))) %>% 
  # Set order
  arrange(desc(est)) %>% 
  mutate(stockid=factor(stockid, levels=stockid)) 

# Example plots
################################################################################

stockids_ex <- 


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

# Y label colors
ylabel_colors <- c("red", "black", "blue")[data$est_inf]

# Spline bars
g1 <- ggplot() +
  # Vertical
  geom_vline(xintercept = 0, color="grey30") +
  # Lines
  geom_errorbar(data=data, mapping=aes(y=stockid, xmin=est_lo, xmax=est_hi, color=est_inf), width=0, alpha=0.4) +
  geom_point(data=data, mapping=aes(y=stockid, x=est, color=est_inf)) +
  xlim(c(-10,20)) +
  # Labels
  labs(x="Temperature effect", y="") +
  # Legend
  scale_color_manual(name="", values=c("red", "black", "blue")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(colour = ylabel_colors))
g1

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_theta_estimates_fixed.png"), 
       width=6, height=8, units="in", dpi=600)

