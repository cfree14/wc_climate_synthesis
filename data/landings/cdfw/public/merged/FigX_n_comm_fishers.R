
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_1916_1999_n_fishers_by_area_of_residence.Rds"))


# Build data
################################################################################

# Data for plotting
data_plot <- data_orig %>% 
  # Set statewide region to NA
  mutate(region=ifelse(region_type=="state", NA, region)) %>% 
  # Calculate proportions
  group_by(season, year) %>% 
  mutate(pfishers=nfishers/sum(nfishers)) %>% 
  # Temporary
  filter(!(year==1935 & region_type!="state"))


# Plot data
################################################################################

# Base theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   strip.text = element_text(size=6),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data_plot, aes(x=year, y=nfishers, fill=region)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Number of fishers") +
  scale_x_continuous(breaks=seq(1910,2000,10)) +
  # Legend
  scale_fill_discrete(name="Area of residence", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.1, 0.66),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot proportions
g2 <- ggplot(data_plot, aes(x=year, y=pfishers, fill=region)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion of fishers") +
  scale_x_continuous(breaks=seq(1910,2000,10)) +
  # Legend
  scale_fill_discrete(name="Area of residence", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.75, 0.25))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_n_fishers.png"), 
       width=6.5, height=4, units="in", dpi=600)

# # Plot data
# g <- ggplot(data_plot, aes(x=year, y=nvessels, fill=length_class_floor)) +
#   geom_bar(stat="identity", color='grey10', lwd=0.1) +
#   facet_grid(~length_class_system, scales="free_x", space="free_x") +
#   # Labels
#   labs(x="Year", y="Number of vessels") +
#   scale_x_continuous(breaks=seq(1930,2000,5)) +
#   # Legend
#   scale_fill_gradientn(name="Length class (ft)", 
#                        colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value="grey90") +
#   guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", 
#                                barheight = 4, barwidth = 0.8)) +
#   # Theme
#   theme_bw() + my_theme +
#   theme(legend.position = c(0.065,0.76))
# g
