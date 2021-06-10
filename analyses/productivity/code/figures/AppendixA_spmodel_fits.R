
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"
tabledir <- "analyses/productivity/tables"

# Read data
load(file.path(outputdir, "pella_0.01p.Rdata"))
rm(sd, params, output, model, input.data, hess)


# Build data
################################################################################

# Shape parameter
p <- 0.01

# Build lines
for(i in 1:nrow(results)){
  
  # Parameters
  stockid <- results$stockid[i]
  print(stockid)
  r <- results$r[i]
  k <- results$k[i]

  # Calculate production 
  b <- seq(0,1,0.01)
  sp <- r/p * b * (1-(b/k)^p)
    
  # Record production
  z <- data.frame(stockid=stockid,
                  tb_sd=b,
                  sp_sd=sp)
  # Merge
  if(i==1){spfits <- z_all}else{spfits <- rbind(spfits, z_all)}
  
}


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=6),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Plot data
g <- ggplot(data, aes(x=b_sd, y=sp_sd, fill=sst_c_scaled)) +
  facet_wrap(~stockid, scales="free_y", ncol=5) +
  # Horizontal guide
  geom_hline(yintercept=0, linetype="solid", color="black", lwd=0.4) +
  geom_point(pch=21, size=1.8, color="grey50") +
  # Line
  geom_line(data=spfits, mapping=aes(x=tb_sd, y=sp_sd, color=sst_c, group=sst_c), size=0.4, inherit.aes = F, show.legend = F) +
  geom_line(data=spfits %>% filter(sst_c==0), mapping=aes(x=tb_sd, y=sp_sd), color="black", size=0.4, inherit.aes = F,  show.legend = F) +
  # Labels
  lims(x=c(0,1)) +
  labs(x="Biomass\n(scaled to max biomass)", y='Surplus production\n(scaled to max biomass)') +
  # Legend (points)
  scale_fill_gradient2(name="SST\n(°C, centered)",
                       mid="white", high="darkred", low="navy") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.hjust = 1)) +
  # Legend (lines)
  scale_color_gradient2(name="SST\n(°C, centered)",
                       mid="white", high="darkred", low="navy") +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g 

# Export
ggsave(g, filename=file.path(plotdir, "AppendixA_model_fits.png"), 
       width=8.5, height=20, units="in")


