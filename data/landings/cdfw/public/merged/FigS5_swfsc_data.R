
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
data_orig <- wcfish::swfsc

# Plot data
################################################################################

# Annual sum by port complex
stats1 <- data_orig %>% 
  group_by(year, port_complex) %>% 
  summarize(landings_lb=sum(landings_lb)) %>% 
  ungroup()

# Annual sum by month
stats2 <- data_orig %>% 
  group_by(year, month) %>% 
  summarize(landings_lb=sum(landings_lb)) %>% 
  ungroup()

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
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.size = unit(0.3, "cm"))

# Plot gear type
g1 <- ggplot(stats1, aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="Landings\n(millions of lbs)", tag="A") +  
  scale_fill_discrete(name="Port complex\n(north to south)") +
  scale_x_continuous(breaks=seq(1920,2010,10)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot by port complex
g2 <- ggplot(stats2, aes(x=year, y=landings_lb/1e6, fill=month)) +
  geom_bar(stat="identity", col="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="Landings\n(millions of lbs)", tag="B") +
  scale_fill_discrete(name="Month") +
  scale_x_continuous(breaks=seq(1920,2010,10)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1)

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_swfsc_data.png"), 
       width=6.5, height=4, units="in", dpi=600)



