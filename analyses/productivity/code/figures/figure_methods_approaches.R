
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
output_fixed <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_fixed.Rds"))
output_random <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_random.Rds"))


# Function
################################################################################

# Stockid
stockid <- "PCODWCVANI"

# Function to illustrate methods
plot_methods <- function(stockid){
  
  # Build data
  #######################################
  
  # Build data
  stockid_do <- stockid
  sdata <- output_fixed$data  %>% 
    # Reduce to stocks of interest
    filter(stockid==stockid_do) %>% 
    # Mark MHW years
    mutate(sst_c_mhw=quantile(sst_c, probs=0.95),
           mhw_yn=ifelse(sst_c>=sst_c_mhw, "yes", "no"))
  
  # Build fixed effects curves
  sr_fixed_orig <- splink::build_sr_curves(output=output_fixed, cov_vals=seq(-1,1, 0.5))
  sr_fixed <- sr_fixed_orig %>% 
    filter(stockid==stockid_do)
  
  # Build random effects curves
  sr_random_orig <- splink::build_sr_curves(output=output_random, cov_vals=seq(-1,1, 0.5))
  sr_random <- sr_random_orig %>% 
    filter(stockid==stockid_do)
  
  # Extract MHW temp
  sst_c_mhw <- quantile(sdata$sst_c_scaled, probs=0.95)
  
  # Extract MHW years
  mhw_yrs <- sdata %>% 
    filter(mhw_yn=="yes") %>% 
    pull(year)
  
  # Plot data
  #######################################
  
  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=5),
                     axis.title=element_text(size=6),
                     legend.text=element_text(size=5),
                     legend.title=element_text(size=6),
                     plot.title=element_text(size=6),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.position = "bottom",
                     legend.key.size = unit(0.3, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot recruitment time series
  g1 <- ggplot(sdata, aes(x=year, y=r_scaled, fill=sst_c_scaled)) +
    # Points
    geom_vline(xintercept=mhw_yrs, inherit.aes=F, color="grey90", lwd=1) +
    geom_path(lwd=0.3, color="grey30") +
    geom_point(pch=21, size=1, stroke=0.2) +
    # Labels
    labs(x="Year\n ", 
         y="Recruitment\n(scaled to max)",
         title=" \nRecruitment and SST data") +
    # Legend
    scale_fill_gradient2(name="SST (°C)\nanomaly", low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme
  g1
  
  # Plot fixed effects model
  g2 <- ggplot(sdata, aes(x=b_scaled, y=r_scaled, fill=sst_c_scaled)) +
    # Points
    geom_point(pch=21, size=1, stroke=0.2) +
    # Fits
    geom_line(data=sr_fixed, mapping=aes(x=b_scaled, y=r_scaled, color=cov_scaled, group=cov_scaled), 
              lwd=0.3, inherit.aes = F, show.legend = F) +
    # Limit
    lims(x=c(0,1), y=c(0,1)) +
    # Labels
    labs(x="Biomass\n(scaled to max)", 
         y="Recruitment\n(scaled to max)",
         title="1) Population model\n(fixed effects framework)") +
    # Point legend
    scale_fill_gradient2(name="SST (°C)\nanomaly", low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Line legend
    scale_color_gradient2(name="SST (°C)\nanomaly", low="navy", high="darkred", mid="grey30", midpoint=0) +
    # Theme
    theme_bw() + my_theme
  g2
  
  # Plot random effects model
  g3 <- ggplot(sdata, aes(x=b_scaled, y=r_scaled, fill=sst_c_scaled)) +
    # Points
    geom_point(pch=21, size=1, stroke=0.2) +
    # Fits
    geom_line(data=sr_random, mapping=aes(x=b_scaled, y=r_scaled, color=cov_scaled, group=cov_scaled), 
              lwd=0.3, inherit.aes = F, show.legend = F) +
    # Limit
    lims(x=c(0,1), y=c(0,1)) +
    # Labels
    labs(x="Biomass\n(scaled to max)", 
         y="Recruitment\n(scaled to max)",
         title="2) Population model\n(random effects framework)") +
    # Point legend
    scale_fill_gradient2(name="SST (°C)\nanomaly", low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Line legend
    scale_color_gradient2(name="SST (°C)\nanomaly", low="navy", high="darkred", mid="grey30", midpoint=0) +
    # Theme
    theme_bw() + my_theme
  g3
  
  # Plot correlation
  g4 <- ggplot(sdata, aes(x=sst_c_scaled, y=r_scaled, fill=sst_c_scaled)) +
    # Smoother
    geom_smooth(method="lm", fill="grey80", color="black", lwd=0.3) +
    geom_point(pch=21, size=1, stroke=0.2) +
    # MHW line
    geom_vline(xintercept=sst_c_mhw, color="grey30", linetype="dotted", lwd=0.3) +
    annotate(geom="text", hjust=1.1, x=sst_c_mhw, y=1, label="MHW", color="grey30", size=1.5) +
    # Labels
    labs(x="SST anamoly (°C)\n ", 
         y="Recruitment\n(scaled to max)",
         title="3) Correlation and\n4) MHW ratio analysis") +
    # Legend
    scale_fill_gradient2(name="SST (°C)\nanomaly", low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme
  g4
  
  # Merge plots
  g <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow=1)
  
  # Export plot
  figname <- paste0("figure_methods_demo_", stockid_do, ".png")
  ggsave(g, filename=file.path(plotdir, figname), 
         width=6.5, height=2.25, units="in", dpi=600)
  print(g)
  
}


# Plot
plot_methods(stockid="PCODWCVANI")

