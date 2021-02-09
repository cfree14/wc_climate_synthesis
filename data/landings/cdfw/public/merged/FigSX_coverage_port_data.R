
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
data_orig <- readRDS(file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.Rds"))


# Plot data
################################################################################

# Calculate coverage
coverage_ports <- data_orig %>% 
  group_by(source_type, port_complex, port, year) %>% 
  summarize(n=n())

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_blank(),
                   strip.text=element_text(size=6),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot coverage
g <- ggplot(coverage_ports, aes(x=year, y=port)) +
  facet_grid(port_complex~source_type, scales="free", space="free") +
  geom_tile() +
  labs(x="", y="") +
  scale_x_continuous(breaks=seq(1940,2020,5)) +
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_coverage_port_data.png"), 
       width=6.5, height=10, units="in", dpi=600)

