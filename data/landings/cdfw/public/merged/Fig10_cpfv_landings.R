
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
data_orig <- readRDS(file=file.path(outdir, "CDFW_1946_2019_annual_cpfv_landings_by_port_complex_species.Rds"))


# Build data
################################################################################

# Data for plotting
data_plot1 <- data_orig %>% 
  # Calculate proportions by species category
  group_by(year, category) %>% 
  summarize(nlandings=sum(landings_n)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(plandings=nlandings/sum(nlandings))

# Data for plotting
data_plot2 <- data_orig %>% 
  # Set statewide region to NA
  mutate(port_complex_group=as.character(port_complex_group)) %>% 
  mutate(port_complex_group=ifelse(region=="Statewide", NA, port_complex_group)) %>% 
  # Calculate proportions by region group
  group_by(year, port_complex_group, port_complex) %>% 
  summarize(nlandings=sum(landings_n)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(plandings=nlandings/sum(nlandings)) %>% 
  # Factor port complex groups
  mutate(port_complex_group=factor(port_complex_group, 
                                   levels=c("Eureka", "Bodega Bay", "San Francisco", "Monterey", 
                                            "Morro Bay", "Santa Barbara", "Los Angeles", "San Diego")))


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
g1 <- ggplot(data_plot1, aes(x=year, y=nlandings/1e6, fill=category)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Number of landed fish\n(millions)", tag="A") +
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Label World War 2
  annotate(geom="text", x=mean(1941:1946), y=0, vjust=-0.5, label="World\nWar II", color="grey40", size=2) +
  # Legend
  scale_fill_discrete(name="Species group", na.value="grey90") + # Taxonomic group
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.09, 0.71),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot proportions
g2 <- ggplot(data_plot1, aes(x=year, y=plandings, fill=category)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion of\nlanded fish") +
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Label World War 2
  annotate(geom="text", x=mean(1941:1946), y=0, vjust=-0.5, label="World\nWar II", color="grey40", size=2) +
  # Legend
  scale_fill_discrete(name="Area of residence", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# Plot data
g3 <- ggplot(data_plot2, aes(x=year, y=nlandings/1e6, fill=port_complex_group)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Number of landed fish\n(millions)", tag="B") +
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Label World War 2
  annotate(geom="text", x=mean(1941:1946), y=0, vjust=-0.5, label="World\nWar II", color="grey40", size=2) +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.065, 0.62),
        legend.key.size = unit(0.3, "cm"))
g3

# Plot proportions
g4 <- ggplot(data_plot2, aes(x=year, y=plandings, fill=port_complex_group)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion of\nlanded fish") +
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Label World War 2
  annotate(geom="text", x=mean(1941:1946), y=0, vjust=-0.5, label="World\nWar II", color="grey40", size=2) +
  # Legend
  scale_fill_discrete(name="Area of residence", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g4

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=1, heights=c(0.35, 0.15, 0.35, 0.15))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_cpfv_landings.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

