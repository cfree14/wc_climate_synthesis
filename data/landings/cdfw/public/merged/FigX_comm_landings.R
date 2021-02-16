
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
port_orig <- readRDS(file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.Rds"))
waters_orig <- readRDS(file=file.path(outdir, "CDFW_1936_2019_landings_by_waters_species.Rds"))
total_orig <- readRDS(file=file.path(outdir, "CDFW_1916_1999_total_landings_shipments.Rds"))


# Plot data
################################################################################

# Waters
waters_vec <- c("California", "North-of-State", "Continental", "South-of-State",
                "Central Pacific", "South Pacific", "Japan", "Africa", "Unknown", "Shipments")

# By waters
waters1 <- waters_orig %>%
  # Sum
  group_by(year, waters) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Factor source
  mutate(waters=factor(waters, levels=rev(waters_vec)))

# By waters (props)
waters2 <- waters_orig %>%
  # Remove shipments
  filter(waters!="Shipments") %>% 
  # Sum
  group_by(year, waters) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Props
  group_by(year) %>% 
  mutate(plandings=landings_lb/sum(landings_lb)) %>% 
  ungroup() %>% 
  # Factor source
  mutate(waters=factor(waters, levels=rev(waters_vec)))

# By port complex
port <- port_orig %>% 
  # Annual sum
  group_by(port_complex, year) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Proportions
  group_by(year) %>% 
  mutate(plandings=landings_lb/sum(landings_lb)) %>% 
  ungroup()
  

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

# By waters
###########################

# Plot data
g1 <- ggplot(waters1, aes(x=year, y=landings_lb/1e6, fill=waters)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Landings and shipments\n(millions of lbs)") +
  # Limits
  scale_y_continuous(limits=c(0,1500)) +
  scale_x_continuous(breaks=seq(1930,2020,10), limits=c(1930,2020)) +
  # Legend
  scale_fill_discrete(name="Source", na.value="grey80") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.8))
g1

# Plot data
g2 <- ggplot(waters2, aes(x=year, y=plandings, fill=waters)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Proportion\nof landings") +
  scale_x_continuous(breaks=seq(1930,2020,10), limits=c(1930,2020)) +
  # Legend
  scale_fill_discrete(name="Source", na.value="grey80", drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# By port complex
###########################

# Plot data
g3 <- ggplot(port, aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Landings\n(millions of lbs)") +
  # Limits
  scale_y_continuous(limits=c(0,1500)) +
  scale_x_continuous(breaks=seq(1930,2020,10), limits=c(1930,2020)) +
  # Legend
  scale_fill_discrete(name="Port complex", na.value="grey80") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.8))
g3

# Plot data
g4 <- ggplot(port, aes(x=year, y=plandings, fill=port_complex)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Proportion\nof landings") +
  scale_x_continuous(breaks=seq(1930,2020,10), limits=c(1930,2020)) +
  # Legend
  scale_fill_discrete(name="Port complex", na.value="grey80", drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g4

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, 
                             g3, g4, heights=c(0.35, 0.15, 0.35, 0.15))
g
