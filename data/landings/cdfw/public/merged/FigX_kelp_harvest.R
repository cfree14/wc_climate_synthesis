
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
data <- read.csv(file=file.path(outdir, "CDFW_1916_1976_annual_kelp_harvest_by_bed_type.csv"), as.is=T)

# Read NOAA data
data_noaa <- readRDS("data/landings/noaa/processed/NOAA_1950_2019_usa_landings_by_state_species.Rds") %>% 
  filter(state=="California" & sci_name=="Macrocystis spp.") %>% 
  # Summarize
  group_by(year) %>% 
  summarize(harvest_lb=sum(landings_lb),
            harvest_t=harvest_lb/2000)

# Plot data
################################################################################

# Reshape
data_plot <- data %>% 
  # Reshape
  select(-total_t) %>% 
  gather(key="bed_type", value="harvest_t", 4:5) %>% 
  # Add 0s for NAs for plotting
  mutate(harvest_t=ifelse(is.na(harvest_t), 0, harvest_t)) %>% 
  # Recode
  mutate(bed_type=recode(bed_type, 
                         "open_bed_t"="Open",
                         "leased_bed_t"="Leased"))

# Base theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=6),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.size=unit(0.5, units="cm"))

# Plot data
g <- ggplot(data_plot, aes(x=year, y=harvest_t/1000, fill=bed_type)) +
  geom_area(na.rm = T) +
  # Plot line
  geom_line(data=data_noaa, mapping=aes(x=year, y=harvest_t/1000), inherit.aes = F, 
            color="black", size=0.2, linetype="solid") +
  # Labels
  labs(x="Year", y="Kelp harvest\n(1000s of tons, wet weight)") +
  scale_fill_discrete(name="Bed type") +
  scale_x_continuous(breaks=seq(1920, 2010, 10)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.85,0.8))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_kelp_harvest.png"), 
       width=3.5, height=2.5, units="in", dpi=600)






