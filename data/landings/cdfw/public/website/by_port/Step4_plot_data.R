
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(gridExtra)

# Directories
outputdir <- "data/landings/cdfw/public/processed"
plotdir <- "data/landings/cdfw/public/figures"

# Read data
data_orig <- read.csv(file.path(outputdir, "CDFW_2000_2019_landings_by_port_expanded.csv"), as.is=T)


# Plot data
################################################################################

# By port
#########################

# By port
data_port <- data_orig %>% 
  group_by(year, area) %>% 
  summarise(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  plot.title=element_text(size=10),
                  legend.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot landings
g1 <- ggplot(data_port, aes(x=year, y=landings_lb/1e6, fill=area)) +
  geom_area() +
  # Labels
  labs(x="Year", y="Landings (lbs, millions)") +
  scale_fill_discrete(name="Port complex") +
  scale_x_continuous(breaks=seq(2000,2020,5), lim=c(2000,2020)) +
  # Theme
  theme_bw() + my_theme + theme(legend.position = "none")
g1

# Plot landings
g2 <- ggplot(data_port, aes(x=year, y=value_usd/1e6, fill=area)) +
  geom_area() +
  # Labels
  labs(x="Year", y="Value (USD, millions)") +
  scale_fill_discrete(name="Port complex") +
  scale_x_continuous(breaks=seq(2000,2020,5), lim=c(2000,2020)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.4,0.6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "california_landings_by_port.png"), 
       width=8.5, height=3.5, units="in", dpi=600)


# By port
#########################

# By port
data_taxa1 <- data_orig %>% 
  group_by(year, taxa_group1) %>% 
  summarise(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  plot.title=element_text(size=10),
                  legend.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot landings
g1 <- ggplot(data_taxa1, aes(x=year, y=landings_lb/1e6, fill=taxa_group1)) +
  geom_area() +
  # Labels
  labs(x="Year", y="Landings (lbs, millions)") +
  scale_fill_discrete(name="Port complex") +
  scale_x_continuous(breaks=seq(2000,2020,5), lim=c(2000,2020)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot landings
g2 <- ggplot(data_taxa1, aes(x=year, y=value_usd/1e6, fill=taxa_group1)) +
  geom_area() +
  # Labels
  labs(x="Year", y="Value (USD, millions)") +
  scale_fill_discrete(name="Taxonomic group") +
  scale_x_continuous(breaks=seq(2000,2020,5), lim=c(2000,2020)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.4,0.6))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "california_landings_by_taxa.png"), 
       width=8.5, height=3.5, units="in", dpi=600)




