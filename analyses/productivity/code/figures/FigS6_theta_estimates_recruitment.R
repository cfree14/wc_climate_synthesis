

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(splink)
library(tidyverse)
library(RColorBrewer)

# Directories
outputdir <- "analyses/productivity/output"
tabledir <- "analyses/productivity/tables"
plotdir <- "analyses/productivity/figures"

# Read data
sp_fixed_output <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_fixed.Rds"))
sp_random_output <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_random.Rds"))
sp_corr_output <- readRDS(file=file.path(outputdir, "RAM_WC_recruitment_sst_correlation.Rds"))

# Build data
################################################################################

# Fixed
sp_fixed <- splink::get_results(sp_fixed_output) %>% 
  mutate(approach="Fixed effects")

# Random
sp_random <- splink::get_results(sp_random_output)[["stock"]] %>% 
  mutate(approach="Random effects")

# Correlation
sp_corr <- sp_corr_output %>% 
  rename(est=corr) %>%
  mutate(param="theta",
         approach="Correlation")

# Build data
data <- bind_rows(sp_fixed, sp_random, sp_corr) %>% 
  # Reduce to thetas
  filter(param=="theta") %>% 
  # Arrange columns
  select(approach, everything()) %>% 
  # Order
  arrange(approach, desc(est)) %>% 
  # Order approaches
  mutate(approach=factor(approach, levels=c("Fixed effects", "Random effects", "Correlation")))

# Stocks ordered
stocks_ordered <- data %>% 
  filter(approach=="Fixed effects") %>% 
  pull(stockid)

# Order data
data_ordered <- data %>% 
  mutate(stockid=factor(stockid, levels=stocks_ordered))



# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y=element_text(size=5.5),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot splines
g <- ggplot(data_ordered, aes(y=stockid, x=est)) +
  facet_wrap(~approach, scales="free_x") +
  # Lines
  geom_segment(data=data_ordered, mapping=aes(y=stockid, yend=stockid, x=est_lo, xend=est_hi), alpha=0.5, lwd=0.2) +
  # Points
  geom_point(size=0.4) +
  # Reference line
  geom_vline(xintercept=0) +
  # Labels
  labs(y="", x="Effect of SST warming\non recruitment") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS6_theta_estimates_recruitment.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



