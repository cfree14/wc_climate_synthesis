
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
data1_orig <- readRDS(file=file.path(outdir, "CDFW_1934_1976_n_comm_vessels_by_length_class.Rds"))
data2_orig <- readRDS(file=file.path(outdir, "CDFW_1934_1956_n_comm_vessels_by_port_complex.Rds"))
data3_orig <- read.csv(file=file.path(outdir, "CDFW_1934_1999_n_comm_vessels.csv"), as.is=T)


# Build data
################################################################################

# Format main data
data1 <- data1_orig %>% 
group_by(source, season, year, region_type,
         length_class_system, length_class_group, length_class, length_class_floor) %>% 
  summarize(nvessels=sum(nvessels, na.rm=T)) %>% 
  ungroup()

# Format 1936-1938 data
data2 <- data3_orig %>% 
  filter(year %in% 1936:1938) %>% 
  mutate(region_type="state",
         length_class_system="1934-1947: 15-ft bins (85+ max)", 
         length_class_group=NA,
         length_class=NA,
         length_class_floor=NA)

# Format 1977-1999 data
data3 <- data3_orig %>% 
  filter(year>=1977) %>% 
  mutate(region_type="state",
         length_class_system="1977-1999: (no length class data)", 
         length_class_group=NA,
         length_class=NA,
         length_class_floor=NA)

# Format data for plotting
data_plot <- bind_rows(data1, data2, data3) %>% 
  mutate(length_class_system=gsub(": ", "\n", length_class_system),
         length_class_system=recode(length_class_system, 
                                    "1970-1976\n5-ft bins (181+ max)"="1970-1976\n5-ft bins\n(181+ max)")) %>% 
  arrange(season, year)

# Plot data
################################################################################

# Base theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
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
g <- ggplot(data_plot, aes(x=year, y=nvessels, fill=length_class_floor)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  facet_grid(~length_class_system, scales="free_x", space="free_x") +
  # Labels
  labs(x="Year", y="Number of vessels") +
  scale_x_continuous(breaks=seq(1930,2000,5)) +
  # Legend
  scale_fill_gradientn(name="Length class (ft)", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", 
                               barheight = 4, barwidth = 0.8)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.065,0.76))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_n_comm_vessels.png"), 
       width=6.5, height=3, units="in", dpi=600)



