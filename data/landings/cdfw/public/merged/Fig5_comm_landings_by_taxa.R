
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
data_orig <- readRDS(file=file.path(outdir, "CDFW_1936_2019_landings_by_waters_species.Rds"))
totals_orig <- readRDS(file=file.path(outdir, "CDFW_1916_1999_total_landings_shipments.Rds"))

# Taxa key
taxa_key <- wcfish::taxa

# Build data
################################################################################

# Confrim all common names have taxa info
comm_in_data <- sort(unique(data_orig$comm_name))
comm_in_data[!comm_in_data%in%taxa_key$comm_name]

# Annual landings by taxa
data_use <- data_orig %>%
  # Add taxonomic group
  left_join(taxa_key %>% select(comm_name, type), by="comm_name") %>% 
  # Landings
  filter(waters!="Shipments") %>%
  # Format type
  mutate(type=recode(type,
                     "Amphibian"="Other",
                     "Reptile"="Other",
                     "Plant"="Other")) %>% 
  # Annual sum by group
  group_by(year, type) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup()
  
# Extract "totals" time series to supplement
totals_use <- totals_orig %>% 
  filter(!year %in% data_use$year) %>% 
  # Add taxa type
  mutate(type="Unknown") %>% 
  select(year, type, seafood_lb) %>% 
  # Rename
  rename(landings_lb=seafood_lb)
  
# Build final dataset
data <- bind_rows(data_use, totals_use) %>% 
  # Arrange
  arrange(year, type) %>% 
  select(year, type, landings_lb) %>%  
  # Props
  group_by(year) %>% 
  mutate(plandings=landings_lb/sum(landings_lb)) %>% 
  ungroup() %>% 
  # Factor groups
  mutate(type=ifelse(type=="Unknown", NA, type)) %>% 
  mutate(type=factor(type, levels=rev(c("Fish", "Mollusk", "Crustacean", "Other invertebrate", "Other"))))

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

# Plot data
g1 <- ggplot(data, aes(x=year, y=landings_lb/1e6, fill=type)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Landings\n(millions of lbs)") +
  # Limits
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Legend
  scale_fill_discrete(name="Taxonomic group", na.value="grey80") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.85,0.8),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Plot data
g2 <- ggplot(data, aes(x=year, y=plandings, fill=type)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion\nof landings") +
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Legend
  scale_fill_discrete(name="Source", na.value="grey80", drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.75, 0.25))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_comm_landings_by_taxa.png"), 
       width=6.5, height=4, units="in", dpi=600)


# For reviewer
################################################################################


# 
stats <- data_orig %>% 
  filter(comm_name %in% c("Market squid")) %>% 
  group_by(year, comm_name) %>% 
  summarize(landings_kg=sum(landings_kg)) %>% 
  mutate(landings_mt=landings_kg/1e3)

# Plot
g <- ggplot(stats, aes(x=year, y=landings_mt/1000)) +
  # Reference line
  geom_hline(yintercept=20, linetype="dotted", color="grey50") +
  geom_line() +
  # Labels
  labs(x="", y="Landings (1000s mt)") +
  # Axes
  scale_y_continuous(breaks=seq(0, 140, 20)) +
  scale_x_continuous(breaks=seq(1930, 2020, 5)) +
  # Theme
  theme_bw() + 
  theme(
    axis.text=element_text(size=6),
    axis.title=element_text(size=8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    # Gridlines
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"))
g

ggsave(g, filename="~/Desktop/market_squid.png", 
       width=4.5, height=2.5, units="in", dpi=600)

