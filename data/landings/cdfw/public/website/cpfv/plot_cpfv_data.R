

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
outputdir <- "data/landings/cdfw/public/cpfv/processed"
plotdir <- "data/landings/cdfw/public/cpfv/figures"

# Read data
data <- read.csv(file.path(outputdir, "CDFW_2000_2019_cpfv_landings_by_port_complex_species.csv"), as.is=T)
data_tots <- read.csv(file.path(outputdir, "CDFW_2000_2019_cpfvs_anglers_landings_by_port_complex.csv"), as.is=T)


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text=element_text(size=6),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Reshape data for plotting
data_tots_plot <- data_tots %>% 
  mutate(landings_n=landings_n/1e4,
         anglers_n=anglers_n/1e3) %>% 
  gather(key="metric", value="value", 4:ncol(.)) %>% 
  mutate(metric=recode_factor(metric, 
                              "cpfvs_n"="Number of\nCPFVs reporting",
                              "anglers_n"="Number of anglers\n(1000s of people)",
                              "landings_n"="Total landings\n(10,000s of fish)")) %>% 
  group_by(year, metric) %>% 
  mutate(prop=value/sum(value)) %>%  
  ungroup() %>% 
  gather(key="stat", value="value", 5:ncol(.)) %>% 
  mutate(stat=recode_factor(stat, "value"="Value", "prop"="Proportion")) 

# Plot data
g <- ggplot(data_tots_plot, aes(x=year, y=value, fill=port_complex)) +
  facet_grid(stat ~ metric, scales="free_y") +
  geom_area() +
  # Labels
  labs(x="", y="", title="CA Commercial passenger fishing vessel (CPFVs) participation") +
  scale_fill_discrete(name="Port complex") +
  scale_x_continuous(lim=c(2000,2020)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "CDFW_2000_2019_cpfv_angler_landings_totals.png"), 
       width=6.5, height=3.75, units="in", dpi=600)


# Plot data
################################################################################

# Calculate species totals
stats <- data %>% 
  group_by(year, species) %>% 
  summarize(landings_n=sum(landings_n)) %>% 
  ungroup() %>% 
  complete(year, species, fill=list(landings_n=0))

# Plot
g <- ggplot(stats, aes(x=year, y=landings_n/1e6, fill=species)) +
  geom_area() +
  # Labels
  labs(x="", y="Landings (millions of fish)") +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_blank(),
        strip.text=element_blank(),
        plot.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export
ggsave(g, filename=file.path(plotdir, "CPFV_landings_by_species.png"), 
       width=6.5, height=3, units="in", dpi=600)






