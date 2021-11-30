

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
sr_fixed_output <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_fixed.Rds"))
sr_random_output <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_random.Rds"))


# Build data
################################################################################

# Fixed
sr_fixed <- splink::get_results(sr_fixed_output) %>% 
  mutate(approach="Fixed effects")

# Random
sr_random <- splink::get_results(sr_random_output)[["stock"]] %>% 
  mutate(approach="Random effects")

# Build data
data <- bind_rows(sr_fixed, sr_random) %>% 
  # Arrange columns
  select(approach, everything()) %>% 
  # Arrange rows
  group_by(approach, param) %>% 
  arrange(approach, param, desc(est)) %>% 
  mutate(order=1:n()) %>% 
  ungroup() %>% 
  # Arrange columns
  select(approach, param, order, stockid, est, everything()) %>% 
  # Cap high estimate values
  mutate(est_hi_cap=ifelse(param=="alpha", pmin(est_hi, 10), est_hi)) %>% 
  # Relabel parameter
  mutate(param=recode_factor(param, 
                             "alpha"="Maximum\nreprodutive rate, α",
                             "beta"="Rate of density-\ndependent regulation, β",
                             "sigmaR"="Residual process\nvariability, σ",
                             "theta"="Effect of warming\non productivity, θ"))

# Vertical line dataset
vlines <- tibble(param=c("theta"),
                 vline=c(0)) %>% 
  # Relabel parameter
  mutate(param=recode_factor(param, 
                             "B0"="Carrying capacity, K\n(prop of max biomass)",
                             "r"="Intrinsic growth rate, r",
                             "sigmaP"="Residual process\nvariability, σ",
                             "theta"="Effect of warming\non productivity, θ"))



# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
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
g1 <- ggplot(data, aes(y=order, x=est, color=approach)) +
  facet_grid(approach~param, scales="free") +
  # Lines
  geom_segment(data=data, mapping=aes(y=order, yend=order, x=est_lo, xend=est_hi_cap), alpha=0.5, lwd=0.2) +
  # Points
  geom_point(size=0.4) +
  # Vertical lines
  geom_vline(data=vlines, mapping=aes(xintercept=vline), linetype="dotted") +
  # Labels
  labs(y="Stock", x="Parameter estimate") +
  # Axis
  expand_limits(x=0) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
g1

# Plot histograms
g2 <- ggplot(data, aes(x=est, fill=approach)) +
  facet_wrap(~param, ncol=4, scale="free") +
  # Density
  geom_density(alpha=0.5) +
  # Vertical lines
  geom_vline(data=vlines, mapping=aes(xintercept=vline), linetype="dotted") +
  # Labels
  labs(x="Parameter estimate", y="Density") +
  # Axis
  expand_limits(x=0) +
  # Legend
  scale_fill_discrete(name="Modelling framework") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=2, heights=c(0.7, 0.3))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS4_parameter_estimates_recruitment.png"), 
       width=6.5, height=6, units="in", dpi=600)








