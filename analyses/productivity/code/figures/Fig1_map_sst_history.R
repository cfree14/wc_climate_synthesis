
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(marineregions)

# Directories
plotdir <- "analyses/productivity/figures"

# Read SST data
sst <- read.csv("data/environmental/cobe/processed/COBE_1981_2020_by_meow.csv")

# World
states <- rnaturalearth::ne_states(country=c("Canada", "United States of America", "Mexico", "Russia"), returnclass = "sf")

# MEOWs
meows_orig <- marineregions::meows

# Build data
################################################################################

# MEOWs do
meows_do <- c("Eastern Bering Sea", 
              "Aleutian Islands",
              "Gulf of Alaska",
              "North American Pacific Fijordland",
              "Oregon, Washington, Vancouver Coast and Shelf",
              "Puget Trough/Georgia Basin",
              "Northern California",
              "Southern California Bight")

# MEOWs to use
meows <- meows_orig %>% 
  # Reduce to interest
  filter(ecoregion %in% meows_do) %>% 
  # Add label
  mutate(ecoregion_label=recode_factor(ecoregion,
                                       "Eastern Bering Sea"="Eastern\nBering Sea", 
                                       "Aleutian Islands"="Aleutian Islands",
                                       "Gulf of Alaska"="Gulf of Alaska",
                                       "North American Pacific Fijordland"= "North American\nPacific Fijordland",
                                       "Puget Trough/Georgia Basin"= "Puget Trough/\nGeorgia Basin",
                                       "Oregon, Washington, Vancouver Coast and Shelf"="Oregon, Washington,\nVancouver Coast and Shelf",
                                       "Northern California"="Northern\nCalifornia",
                                       "Southern California Bight"="Southern\nCalifornia Bight"))

# MHW years
mhw_perc <- 0.95

# Mark SST years
mhw_sst <- sst %>% 
  filter(!is.na(sst_c_avg)) %>% 
  group_by(ecoregion) %>% 
  summarize(sst_c_avg_mhw=quantile(sst_c_avg, probs=mhw_perc) %>% as.numeric()) %>% 
  ungroup()



# Plot data
################################################################################

# Plot temperature data
ecoregion <- meows_do[1]; color <- "red"
plot_sst <- function(ecoregion, color){
  
  # Theme
  sst_theme <-  theme(axis.text=element_text(size=5.2),
                      axis.title=element_text(size=7),
                      axis.title.x=element_blank(),
                      plot.title=element_blank(),
                      # Gridlines
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      axis.line = element_line(colour = "black"))  
  
  # Subset data
  ecoregion_do <- ecoregion
  sdata <- sst %>% 
    filter(ecoregion==ecoregion_do & year>=1900)
  ymax <- max(sdata$sst_c_avg+sdata$sst_c_sd)
  mhw_sst_use <- mhw_sst %>% 
    filter(ecoregion==ecoregion_do) %>% 
    pull(sst_c_avg_mhw) 
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=sst_c_avg)) +
    geom_ribbon(mapping=aes(ymin=sst_c_avg-sst_c_sd, ymax=sst_c_avg+sst_c_sd), fill=color, alpha=0.2) +
    geom_line(lwd=0.2) +
    # Add MHW line
    geom_hline(yintercept=mhw_sst_use, linetype="dotted", color="black") +
    # Add label
    annotate(geom="text", x=1900, y=ymax, label=ecoregion_do, hjust=0, size=2.4) +
    # Labels
    labs(x="", y="SST (°C)") +
    scale_x_continuous(breaks=seq(1900, 2020, 20)) +
    # Theme
    theme_bw() + sst_theme
  g
  
  ?# Return
  print(g)
  return(g)
  
}

# Theme
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
                   axis.line = element_line(colour = "black"))

# Plot map
g1 <- ggplot() +
  # LMEs
  geom_sf(data=meows, mapping=aes(fill=ecoregion_label), color=NA, alpha=0.3) +
  # Plot land
  geom_sf(data=states, fill="grey90", color="white", lwd=0.2) +
  # LME labels
  geom_sf_text(data=meows, mapping=aes(color=ecoregion_label, label=ecoregion_label), size=2.1) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim=c(-109, -180), ylim=c(24,70)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text=element_blank(),
        legend.position = "none")
g1

# Get colors
colors <- scales::hue_pal()(8)


# SST time series
g2 <- plot_sst(ecoregion="Eastern Bering Sea", color=colors[1])
g3 <- plot_sst(ecoregion="Aleutian Islands", color=colors[2])
g4 <- plot_sst(ecoregion="Gulf of Alaska", color=colors[3])
g5 <- plot_sst(ecoregion="North American Pacific Fijordland", color=colors[4])
g6 <- plot_sst(ecoregion="Oregon, Washington, Vancouver Coast and Shelf", color=colors[6])
g7 <- plot_sst(ecoregion="Northern California", color=colors[7])
g8 <- plot_sst(ecoregion="Southern California Bight", color=colors[8])


# Merge plots
layout_matrix <- matrix(data=c(1,1,1,2,
                               1,1,1,3, 
                               1,1,1,4,
                               8,7,6,5), ncol=4, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, layout_matrix=layout_matrix)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_map_sst_history.png"), 
       width=6.5, height=6, units="in", dpi=600)


