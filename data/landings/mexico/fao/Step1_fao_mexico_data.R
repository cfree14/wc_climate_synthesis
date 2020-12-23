

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/mexico/fao"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2018_fao_landings_data.Rds")



# Visualize by region
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  plot.title=element_text(size=10),
                  strip.text = element_text(size=8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Mexico data
stats1 <- data_orig %>%
  # Reduce to Mexico and landings in tons
  filter(country_use=="Mexico" & units=="t") %>% 
  # Recode areas
  mutate(area1=ifelse(grepl("Atlantic", area), "Atlantic",
                      ifelse(grepl("Pacific", area), "Pacific", "Inland"))) %>% 
  group_by(area1, year) %>% 
  summarize(landings_mt=sum(quantity, na.rm=T)) %>% 
  ungroup()

# Plot data
g <- ggplot(stats1, aes(x=year, y=landings_mt/1e3, fill=area1)) +
  geom_area() +
  # Labels
  labs(x="", y="Landings (1000s mt)", title="Mexico fisheries landings by major region, 1950-2018") +
  scale_fill_discrete(name="Region") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(1950,2020)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(datadir, "FAO_1950_2018_mexico_landings_by_region.png"), 
       width=5.5, height=3.5, units="in", dpi=600)


# West Coast data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to Mexico and landings in tons
  filter(country_use=="Mexico" & units=="t" & area_type == "marine" & grepl("Pacific", area)) %>% 
  # Important columns
  select(area, major_group, isscaap, order, family, comm_name, species, year, quantity) %>% 
  # Rename landings
  rename(landings_mt=quantity)

# Export data
saveRDS(data, file=file.path(datadir, "FAO_1950_2018_mexico_pacific_landings_data.Rds"))

