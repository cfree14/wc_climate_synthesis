
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

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

# Landings by port complex
data1 <- data_orig %>% 
  # Landings
  filter(type=="Landings") %>% 
  # Summarize by port complex-year
  group_by(port_complex, year) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            landings_kg=sum(landings_kg, na.rm=T),
            landings_mt=landings_kg/1000,
            value_usd=sum(value_usd, na.rm=T))

# Landings by port
data2 <- data_orig %>% 
  # Landings
  filter(type=="Landings") %>% 
  # Summarize by port-year
  group_by(port_complex, port, year) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            landings_kg=sum(landings_kg, na.rm=T),
            landings_mt=landings_kg/1000,
            value_usd=sum(value_usd, na.rm=T))

# Landings by species
data3 <- data_orig %>% 
  # Landings
  filter(type=="Landings") %>% 
  # Summarize by port-year
  group_by(port_complex, port, comm_name, year) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            landings_kg=sum(landings_kg, na.rm=T),
            landings_mt=landings_kg/1000,
            value_usd=sum(value_usd, na.rm=T))

# Volume plots
########################################

# Plot landings by port complex
vol1 <- ggplot(data1, aes(x=year, y=landings_mt/1e3, fill=port_complex)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings volume\n(1000s of mt)", title="Statewide landings volume by port complex") +
  # Legend
  scale_fill_discrete(name="Port complex") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.8))
vol1

# Plot landings by port
vol2 <- ggplot(data2 %>% filter(port_complex=="Eureka"), aes(x=year, y=landings_mt/1e3, fill=port)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings volume\n(1000s of mt)", title="Eureka region landings volume by port") +
  # Legend
  scale_fill_discrete(name="Port") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.8))
vol1

# Plot landings by species
vol3 <- ggplot(data3 %>% filter(port=="Crescent City"), aes(x=year, y=landings_mt/1e3, fill=comm_name)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings volume\n(1000s of mt)", title="Crescent City landings volume by taxonomic group") +
  # Legend
  scale_fill_discrete(name="Port complex") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
vol3


# Value plots
########################################

# Plot value
g2 <- ggplot(data1, aes(x=year, y=value_usd/1e6, fill=port_complex)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Landings value\n(millions of dollars)") +
  # Legend
  scale_fill_discrete(name="Port complex") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.8))
g2







