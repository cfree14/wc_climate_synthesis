

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

# Read stock key
stock_key_orig <- readRDS(file=file.path(outputdir, "RAM_WC_stock_key.Rds"))

# Read merged SST effects
data_orig <- readRDS(file=file.path(outputdir, "RAM_WC_sr_sp_sst_influence_results.Rds"))


# Build data
################################################################################

# Format SST effects
prod_thetas <- data_orig %>% 
  filter(prod_type=="Production" & model_type!="Correlation") %>% 
  select(stockid, model_type, est) %>% 
  mutate(model_type=recode(model_type, 
                           "Fixed"="theta_fixed", 
                           "Random"="theta_random")) %>% 
  spread(key=model_type, value=est)


# Add SST effects to stock key
stock_key <- stock_key_orig %>% 
  left_join(prod_thetas, by="stockid") %>%   
  filter(theta_fixed < 3 & theta_fixed > -3)


# Plot data
################################################################################


# Plot 
g1 <- ggplot(stock_key, aes(x=bbmsy_prod, y=theta_fixed)) +
  # LM fit
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=1) +
  # Points
  geom_point() +
  # Axis
  lims(x=c(0, NA)) +
  # Labels
  labs(x="B/BMSY", y="SST effect", tag="A") +
  # Theme
  theme_bw()
g1

# Plot 
g2 <- ggplot(stock_key, aes(x=ffmsy_prod, y=theta_fixed)) +
  # LM fit
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=1) +
  # Points
  geom_point() +
  # Axis
  lims(x=c(0, NA)) +
  # Labels
  labs(x="F/FMSY", y="SST effect", tag="B") +
  # Theme
  theme_bw()
g2

# Plot 
g3 <- ggplot(stock_key, aes(x=sst_c_trend_prod*10, y=theta_fixed)) +
  # LM fit
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  # Points
  geom_point() +
  # Axis
  lims(x=c(0, NA)) +
  # Labels
  labs(x="SST trend (°C/decade)", y="SST effect", tag="C") +
  # Theme
  theme_bw()
g3

# Plot 
g4 <- ggplot(stock_key, aes(x=linf_cm, y=theta_fixed)) +
  # LM fit
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  # Points
  geom_point() +
  # Axis
  lims(x=c(0, NA)) +
  # Labels
  labs(x="Asymptotic length, Linf (cm)", y="SST effect", tag="D") +
  # Theme
  theme_bw()
g4

# Plot 
g5 <- ggplot(stock_key, aes(x=k, y=theta_fixed)) +
  # LM fit
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  # Points
  geom_point() +
  # Axis
  lims(x=c(0, NA)) +
  # Labels
  labs(x="Growth rate, K", y="SST effect", tag="E") +
  # Theme
  theme_bw()
g5

# Plot 
g6 <- ggplot(stock_key, aes(x=tmax_yr, y=theta_fixed)) +
  # LM fit
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  # Points
  geom_point() +
  # Axis
  lims(x=c(0, NA)) +
  # Labels
  labs(x="Max age (yr)", y="SST effect", tag="F") +
  # Theme
  theme_bw()
g6

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, nrow=2)
g

# Export
# ggsave(g, filename=file.path(plotdir, "FigS6_theta_estimates_recruitment.png"), 
#        width=6.5, height=6.5, units="in", dpi=600)


lmfit <- lm(theta_random ~ k, stock_key)
summary(lmfit)

