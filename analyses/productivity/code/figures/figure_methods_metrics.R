
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
output_rec <- readRDS(file=file.path(outputdir, "recruitment_ricker.Rds"))
output_rec_fixed <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_fixed.Rds"))
output_rec_random <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_random.Rds"))
output_prod <- readRDS(file=file.path(outputdir, "production_0.01p.Rds"))
output_prod_fixed <- readRDS(file=file.path(outputdir, "production_0.01p_sst_fixed.Rds"))
output_prod_random <- readRDS(file=file.path(outputdir, "production_0.01p_sst_random.Rds"))


# Function
################################################################################

# Stockid
stockid <- "PCODWCVANI"

# Function to illustrate methods
plot_methods_metrics <- function(stockid){
  
  # Build data
  #######################################
  
  # Stock
  stockid_do <- stockid
  
  # Production
  data_prod <- output_prod$data %>% 
    filter(stockid==stockid_do)
  
  # Recruitment
  data_rec <- output_rec$data %>% 
    filter(stockid==stockid_do)
  
  # Build production curves
  sp_fixed_orig <- splink::build_sp_curves(output=output_prod_fixed, cov_vals=seq(-1,1, 0.5))
  sp_fixed <- sp_fixed_orig %>% 
    filter(stockid==stockid_do & cov_scaled==0)
  
  # Build recruitment curves
  sr_fixed_orig <- splink::build_sr_curves(output=output_rec_fixed, cov_vals=seq(-1,1, 0.5))
  sr_fixed <- sr_fixed_orig %>% 
    filter(stockid==stockid_do & cov_scaled==0)
  
  # Plot data
  #######################################
  
  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=5),
                     axis.title=element_text(size=6),
                     legend.text=element_text(size=5),
                     legend.title=element_text(size=6),
                     plot.title=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.3, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot production data
  g1 <- ggplot(data_prod, aes(x=b_sd, y=sp_sd, fill=sst_c_scaled)) +
    # Reference lines
    geom_hline(yintercept=0, linetype="dotted", color="grey30", lwd=0.3) +
    # Points
    geom_point(pch=21, size=2, stroke=0.2) +
    # Fits
    geom_line(data=sp_fixed, mapping=aes(x=b_scaled, y=sp_scaled), lwd=0.3, inherit.aes = F, show.legend = F) +
    # Labels
    labs(x="Biomass\n(scaled to max biomass)", 
         y="Surplus production\n(scaled to max biomass)",
         title="Overall (surplus) production") +
    # Legend
    scale_fill_gradient2(name="SST (°C)\nanomaly", low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme
  g1
  
  # Plot production data
  g2 <- ggplot(data_rec, aes(x=b_scaled, y=r_scaled, fill=sst_c_scaled)) +
    # Points
    geom_point(pch=21, size=2, stroke=0.2) +
    # Fits
    geom_line(data=sr_fixed, mapping=aes(x=b_scaled, y=r_scaled), lwd=0.3, inherit.aes = F, show.legend = F) +
    # Labels
    labs(x="Biomass\n(scaled to max biomass)", 
         y="Recruitment\n(scaled to max recruitment)",
         title="Recruitment") +
    # Legend
    scale_fill_gradient2(name="SST (°C)\nanomaly", low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme
  g2
  
  # Merge plots
  g <- gridExtra::grid.arrange(g1, g2, nrow=1)
  
  # Export plot
  figname <- paste0("figure_methods_metrics_", stockid_do, ".png")
  ggsave(g, filename=file.path(plotdir, figname), 
         width=6, height=2.5, units="in", dpi=600)
  print(g)
  
}


# Plot
plot_methods(stockid="PCODWCVANI")


# Plot other plot
############################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=10),
                   plot.title=element_text(size=12),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot recruitment vs. production
g3 <- ggplot() +
  # Reference lines
  geom_hline(yintercept=0, lwd=0.4) +
  geom_vline(xintercept=0, lwd=0.4) +
  # geom_abline(slope=1, linetype="dotted", lwd=0.4) +
  # Limits
  lims(x=c(-1,1), y=c(-1,1)) +
  # Labels
  labs(x="Impact of warming\non recruitment", 
       y="Impact of warming\non surplus production",
       title="Compensatory or depensatory effects?") +
  # Theme
  theme_bw() + my_theme
g3

ggsave(g3, filename=file.path(plotdir, "figure_prod_vs_rec_theta_conceptual.png"), 
       width=4.5, height=4.5, units="in", dpi=600)

