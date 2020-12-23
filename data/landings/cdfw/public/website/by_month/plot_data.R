

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)

# Directories
basedir <- "data/landings/cdfw/public/raw"
plotdir <- "data/landings/cdfw/public/by_month/figures"
outputdir <- "data/landings/cdfw/public/processed"

# Read data
data_orig <- readRDS(file=file.path(outputdir, "CDFW_2000_2019_monthly_landings_by_port_complex.Rds"))


# Plot data
################################################################################

# Calculate stats
stats <- data_orig %>% 
  group_by(year, port_complex, month) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(year, port_complex) %>% 
  mutate(landings_prop=landings_lb/sum(landings_lb)) %>% 
  ungroup()

# Theme
my_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot data
g1 <- ggplot(stats, aes(x=year, y=landings_lb/1e6, fill=month)) +
  facet_wrap(~port_complex, ncol=4, scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings (millions of lbs)") +
  scale_fill_discrete(name="Month") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank())
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "CA_landings_by_port_complex_month_barplot.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


# Plot data
################################################################################

# Plot data
g2 <- ggplot(stats, aes(x=month, y=landings_prop, color=year, group=year)) +
  facet_wrap(~port_complex, ncol=4, scales="free_y") +
  geom_line() +
  # Labels
  labs(x="", y="Proportion of annual landings") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Oranges")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # scale_fill_discrete(name="Month") +
  # Theme
  theme_bw() + my_theme
g2

# Export figure
ggsave(g2, filename=file.path(plotdir, "CA_landings_by_port_complex_month_proportion.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

# Statewide
################################################################################

# Calculate stats
stats <- data_orig %>% 
  group_by(year, month) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(landings_prop=landings_lb/sum(landings_lb)) %>% 
  ungroup()

# Plot data
g3 <- ggplot(stats, aes(x=month, y=landings_prop, color=year, group=year)) +
  geom_line() +
  # Labels
  labs(x="", y="Proportion of annual landings") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Oranges")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # scale_fill_discrete(name="Month") +
  # Theme
  theme_bw() + my_theme
g3

# Export figure
ggsave(g3, filename=file.path(plotdir, "CA_landings_by_month_statewide.png"), 
       width=4.5, height=3, units="in", dpi=600)

# Plot data
g3 <- ggplot(stats, aes(x=year, y=landings_lb/1e6, fill=month)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings (millions of lbs)") +
  # Legend
  scale_fill_discrete(name="Month") +
  # Theme
  theme_bw() + my_theme
g3

# Export figure
ggsave(g3, filename=file.path(plotdir, "CA_landings_by_month_statewide_barplot.png"), 
       width=4.5, height=3, units="in", dpi=600)







