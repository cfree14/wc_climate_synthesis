
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
data <- wcfish::cdfw_kelp
data_tots <- wcfish::cdfw_kelp_tots


# Format for plotting
################################################################################

# Format by bed
data1 <- data %>% 
  # Reshape
  select(-total_t) %>% 
  gather(key="bed_type", value="harvest_t", 4:5) %>% 
  # Recode
  mutate(bed_type=recode(bed_type, 
                         "open_bed_t"="Open",
                         "leased_bed_t"="Leased")) %>% 
  select(source, year, bed_type, harvest_t)

# Format totals
data2 <- data_tots %>% 
  filter(year>1976) %>% 
  mutate(bed_type=NA) %>% 
  select(source, year, bed_type, harvest_t)

# Merge
data_plot <- bind_rows(data1, data2) %>% 
  filter(!is.na(harvest_t))


# Plot data
################################################################################

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
                   legend.key.size=unit(0.3, units="cm"))

# Plot data
g <- ggplot(data_plot, aes(x=year, y=harvest_t/1000, fill=bed_type)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Kelp harvest\n(1000s of short tons, wet weight)") +
  scale_fill_discrete(name="Bed type", na.value="grey80") +
  scale_x_continuous(breaks=seq(1910, 2020, 10)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.85,0.8))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_kelp_harvest.png"), 
       width=3.5, height=2.5, units="in", dpi=600)






