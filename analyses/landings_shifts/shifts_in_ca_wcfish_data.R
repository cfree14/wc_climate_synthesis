
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
sstdir <- "data/environmental/cobe/processed"
plotdir <- "analyses/landings_shifts/figures"

# Get data
port_key <- wcfish::ports
data_orig <- wcfish::cdfw_ports

# SST average
ssts <- read.csv(file.path(sstdir, "COBE_1891_2020_sst_by_fishing_grounds.csv")) %>% 
  filter(state=="California") %>% 
  select(-state)

# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to landings
  filter(type=="Landings") %>% 
  # Remove Inland Waters and Sacramento Delta
  filter(!port_complex%in%c("Sacramento Delta", "Inland Waters", "Unknown")) %>% 
  # Summarize by port
  group_by(year, port_complex, port) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm = T),
            value_usd=sum(value_usd, na.rm = T)) %>% 
  ungroup() %>% 
  # Add latitude
  left_join(port_key %>% select(port, lat_dd), by="port") %>% 
  # Calculate latitude-weighted averages by year
  group_by(year) %>% 
  summarize(volume_lat_avg=weighted.mean(x=lat_dd, w=landings_lb),
            value_lat_avg=weighted.mean(x=lat_dd, w=value_usd)) %>% 
  ungroup() %>% 
  # Add temperature
  full_join(ssts, by="year") %>% 
  # Arrange
  arrange(year) %>% 
  select(year, sst_c, everything()) %>% 
  # Reduce 
  filter(year>=1941 & year < 2020)

# Format for plotting
data_plot <- data %>% 
  # Gather
  gather(key="metric", value="lat_dd", 3:ncol(.))  %>% 
  # Format metric
  mutate(metric=recode(metric, 
                       "value_lat_avg"="Value (dollars)",
                       "volume_lat_avg"="Volume (pounds)"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=10),
                   strip.text=element_text(size=10),
                   plot.title=element_text(size=12),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot lat/long trends
g1 <- ggplot(data_plot, aes(x=year, y=lat_dd, color=metric)) +
  geom_line() +
  # Labels
  labs(x="", y="Center of gravity (°N)") +
  scale_x_continuous(breaks=seq(1940,2020,10)) +
  scale_color_discrete(name="Landings") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8))
g1

# Plot SST trend
g2 <- ggplot(data_plot %>% select(year, sst_c) %>% unique(), aes(x=year, y=sst_c)) +
  geom_line() +
  # Labels
  labs(x="", y="SST (°C)") +
  scale_x_continuous(breaks=seq(1940,2020,10)) +
  # Theme
  theme_bw() + my_theme
g2

# Plot correlations
g3 <- ggplot(data_plot, aes(x=sst_c, y=lat_dd, color=metric)) +
  facet_wrap(~metric, ncol=1) +
  geom_point(show.legend = F) +
  # Labels
  labs(x="SST (°C)", y="Center of gravity (°N)") +
  theme_bw() + my_theme
g3

# Merge left panels
g12 <- gridExtra::grid.arrange(g1,g2,ncol=1)

# Merge all panels
g <- gridExtra::grid.arrange(g12, g3, ncol=2, widths=c(0.65, 0.45))
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_landings_shifts_ca_port.png"), 
       width=6.5, height=5, units="in", dpi=600)





