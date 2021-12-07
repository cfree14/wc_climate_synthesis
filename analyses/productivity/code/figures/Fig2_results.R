
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
ramdir <- "data/ramldb/processed"
plotdir <- "analyses/productivity/figures"
outputdir <- "analyses/productivity/output"

# Read stock key
stock_key_orig <- read.csv(file=file.path(ramdir, "RAM_stock_key.csv"), as.is=T)

# Read merged SST effects
data_orig <- readRDS(file=file.path(outputdir, "RAM_WC_sr_sp_sst_influence_results.Rds"))

# Read model output
output_prod <- readRDS(file=file.path(outputdir, "production_0.01p_sst_fixed.Rds"))
output_rec <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_fixed.Rds"))


# Build raster data
################################################################################

# Format data
data <- data_orig %>% 
  # Standardize effect
  group_by(prod_type, model_type) %>% 
  mutate(est_scaled=est/sd(est)) %>% 
  ungroup()

# Stats
stats <- data %>% 
  group_by(stockid) %>% 
  summarize(est_avg=mean(est_scaled)) %>% 
  ungroup() %>% 
  arrange(est_avg)

# ORder data
data_ordered <- data %>% 
  mutate(stockid=factor(stockid, levels=stats$stockid),
         model_type=recode_factor(model_type, 
                                  "Fixed"="Fixed\neffects\nanalysis", 
                                  "Random"="Random\neffects\nanalysis",
                                  "Correlation"="Corr.\nanalysis"))



# Build production data
################################################################################

# Shape parameter
p <- 0.01

# Stocks of interest
stocks_use <- c("PCODWCVANI", "PERCHEBSAI", "PHAKEPCOAST")

# Extract data
data_prod <- output_prod$data %>% 
  # Reduce to stocks use
  filter(stockid %in% stocks_use) %>% 
  # Add label
  mutate(stockid_label=recode_factor(stockid, 
                                     "PERCHEBSAI"="PERCHEBSAI\n(warming winner)",
                                     "PHAKEPCOAST"="PHAKEPCOAST\n(warming neutral)",
                                     "PCODWCVANI"="PCODWCVANI\n(warming loser)"))

# Extract results
results_prod <- splink::get_results(output_prod) %>% 
  # Reduce to stocks use
  filter(stockid %in% stocks_use) %>% 
  # Add label
  mutate(stockid_label=recode_factor(stockid, 
                                     "PERCHEBSAI"="PERCHEBSAI\n(warming winner)",
                                     "PHAKEPCOAST"="PHAKEPCOAST\n(warming neutral)",
                                     "PCODWCVANI"="PCODWCVANI\n(warming loser)"))

# Params
spfits <- results_prod %>%
  select(stockid_label, param, est) %>%
  spread(key="param", value="est")

# Biomass values to evaluate
b <- seq(0, 1, 0.01)

# Covariate values to evaluate
cov_vals <- seq(-1, 1, 0.5)

# Create lines
sp_lines <- purrr::map_df(1:nrow(spfits), function(x){
  
  # Parameters
  stockid <- spfits$stockid_label[x]
  r <- spfits$r[x]
  k <- spfits$B0[x]
  theta <- spfits$theta[x]

  # Loop through covariate values
  sp_lines1 <- purrr::map_df(cov_vals, function(x){
    
    # Simulate data
    sp <- r/p * b * (1-(b/k)^p) * exp(theta*x)
    
    # Record production
    z <- data.frame(stockid_label=stockid,
                    cov_scaled=x,
                    b_scaled=b,
                    sp_scaled=sp)
    
  })
  
})



# Build recruitment data
################################################################################

# Stocks of interest
stocks_use <- c("PCODWCVANI", "PERCHEBSAI", "PHAKEPCOAST")

# Extract results
data_rec <- output_rec$data %>% 
  # Reduce to stocks use
  filter(stockid %in% stocks_use) %>% 
  # Add label
  mutate(stockid_label=recode_factor(stockid, 
                                     "PERCHEBSAI"="PERCHEBSAI\n(warming winner)",
                                     "PHAKEPCOAST"="PHAKEPCOAST\n(warming neutral)",
                                     "PCODWCVANI"="PCODWCVANI\n(warming loser)"))

# Extract results
results_rec <- splink::get_results(output_rec) %>% 
  # Reduce to stocks use
  filter(stockid %in% stocks_use) %>% 
  # Add label
  mutate(stockid_label=recode_factor(stockid, 
                                     "PERCHEBSAI"="PERCHEBSAI\n(warming winner)",
                                     "PHAKEPCOAST"="PHAKEPCOAST\n(warming neutral)",
                                     "PCODWCVANI"="PCODWCVANI\n(warming loser)"))

# Params
srfits <- results_rec %>%
  select(stockid_label, param, est) %>%
  spread(key="param", value="est")

# SR type
type <- output_rec$type

# Biomass values to evaluate
b <- seq(0, 1, 0.01)

# Covariate values to evaluate
cov_vals <- seq(-1, 1, 0.5)

# Create lines
sr_lines <- purrr::map_df(1:nrow(srfits), function(x){
  
  # Parameters
  stockid <- srfits$stockid_label[x]
  alpha <- srfits$alpha[x]
  beta <- srfits$beta[x]
  theta <- srfits$theta[x]
  
  # Loop through covariate values
  sr_lines1 <- purrr::map_df(cov_vals, function(x){
    
    # Simulate data
    if(type=="ricker"){
      recruits <- alpha * b * exp(-beta*b) * exp(theta*x)
    }
    if(type=="bev-holt"){
      recruits <- alpha * b / (beta + b) * exp(theta*x)
    }
    
    # Record production
    z <- data.frame(stockid_label=stockid,
                    cov_scaled=x,
                    b_scaled=b,
                    r_scaled=recruits)
    
  })
  
})

# Build key for stockid color
stockid_key <- tibble(stockid=levels(data_ordered$stockid)) %>% 
  # Mark example ids
  mutate(example_yn=ifelse(stockid %in% stocks_use, "yes", "no")) %>% 
  # Mark font face
  mutate(face=ifelse(example_yn=="yes", "bold", "plain"),
         color=ifelse(example_yn=="yes", "black", "grey40"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_blank(),
                   plot.tag=element_text(size=8),
                   plot.tag.position = c(0.075, 0.99),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   legend.key.size = unit(0.3, "cm"))


# Plot data
g1 <- ggplot(data_ordered, aes(y=stockid, x=model_type, fill=est_scaled)) +
  facet_wrap(~prod_type) +
  geom_tile(color="grey30", lwd=0.05) +
  # Labels
  labs(tag="A", x="Approach", y="") + 
  # Legend
  scale_fill_gradient2(name="Relative effect\nof SST warming") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_text(face=stockid_key$face, color=stockid_key$color))
g1

# Plot production data
g2 <- ggplot(data_prod, aes(x=b_sd, y=sp_sd, fill=sst_c_scaled)) +
  facet_wrap(~stockid_label, ncol=1, scales="free_y") +
  # Plot zero line
  geom_hline(yintercept=0, color="grey80") +
  # Plot points
  geom_point(pch=21, size=1.8) +
  # Plot SP curves
  geom_line(data=sp_lines, mapping=aes(x=b_scaled, y=sp_scaled, color=cov_scaled, group=cov_scaled), size=0.5, inherit.aes = F) +
  # Labels
  labs(tag="B",
       x="Biomass\n(scaled to max biomass)", 
       y="Surplus production\n(scaled to max biomass)") +
  # Legend
  scale_fill_gradient2(name="SST (°C)\nanomaly", midpoint=0, low="navy", high="darkred", mid="white") +
  scale_color_gradient2(name="SST (°C)\nanomaly", midpoint=0, low="navy", high="darkred", mid="grey60", guide="none") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot recruitment data
g3 <- ggplot(data_rec, aes(x=b_scaled, y=r_scaled, fill=sst_c_scaled)) +
  facet_wrap(~stockid_label, ncol=1, scales="free_y") +
  # Plot points
  geom_point(pch=21, size=2) +
  # Plot SR lines
  geom_line(data=sr_lines, mapping=aes(x=b_scaled, y=r_scaled, color=cov_scaled, group=cov_scaled), size=0.5, inherit.aes = F) +
  # Labels
  labs(tag="C",
       x="Biomass\n(scaled to max biomass)", 
       y="Recruitment\n(scaled to max recruitment)") +
  # Legend
  scale_fill_gradient2(name="SST (°C)\nanomaly", midpoint=0, low="navy", high="darkred", mid="white") +
  scale_color_gradient2(name="SST (°C)\nanomaly", midpoint=0, low="navy", high="darkred", mid="grey60", guide="none") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3, widths=c(0.5, 0.25, 0.25))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_results.png"), 
       width=6.5, height=6.5, units="in", dpi=600)








