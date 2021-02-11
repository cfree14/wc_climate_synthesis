
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
data_orig <- readRDS(file=file.path(outdir, "CDFW_1946_2019_annual_cpfv_effort_by_port_complex.Rds"))


# Build data
################################################################################

# Data for plotting
data_plot <- data_orig %>% 
  # Calculate proportions by species category
  group_by(year, port_complex_group, port_complex) %>% 
  summarize(nlandings=sum(landings_n),
            nanglers=sum(anglers_n),
            nvessels=sum(cpfvs_n)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(plandings=nlandings/sum(nlandings),
         panglers=nanglers/sum(nanglers),
         pvessels=nvessels/sum(nvessels)) %>% 
  # Format port complex
  mutate(port_complex_group=ifelse(port_complex=="Statewide", NA, port_complex_group)) %>% 
  mutate(port_complex_group=factor(port_complex_group, 
                                 levels=c("Fort Bragg", "Bodega Bay", "San Francisco", "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles", "San Diego")))

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
g1 <- ggplot(data_plot, aes(x=year, y=nvessels, fill=port_complex_group)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Number\nof CPFVs") +
  scale_x_continuous(breaks=seq(1910,2020,10), limits=c(1930,2020)) +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.07, 0.66),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot proportions
g2 <- ggplot(data_plot, aes(x=year, y=pvessels, fill=port_complex_group)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion\nof CPFVs") +
  scale_x_continuous(breaks=seq(1910,2020,10), limits=c(1930,2020)) +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# Plot data
g3 <- ggplot(data_plot, aes(x=year, y=nanglers/1e3, fill=port_complex_group)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Thousands\nof CPFV anglers") +
  scale_x_continuous(breaks=seq(1910,2020,10), limits=c(1930,2020)) +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none",
        legend.key.size = unit(0.3, "cm"))
g3

# Plot proportions
g4 <- ggplot(data_plot, aes(x=year, y=panglers, fill=port_complex_group)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion\nof CPFV anglers") +
  scale_x_continuous(breaks=seq(1910,2020,10), limits=c(1930,2020)) +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)", na.value="grey90") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g4


# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=1, heights=c(0.35, 0.15, 0.35, 0.15))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_cpfv_effort.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

