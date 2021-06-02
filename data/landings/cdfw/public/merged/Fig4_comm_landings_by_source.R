
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
waters_orig <- readRDS(file=file.path(outdir, "CDFW_1936_2019_landings_by_waters_species.Rds"))
totals_orig <- readRDS(file=file.path(outdir, "CDFW_1916_1999_total_landings_shipments.Rds"))


# Taxonomoic resolutions
################################################################################

# Taxa key
taxa_key <- wcfish::taxa

# Species in data
spp <- waters_orig %>% 
  select(comm_name) %>% 
  unique() %>% 
  arrange() %>% 
  left_join(taxa_key)

n_distinct(spp$phylum)
n_distinct(spp$class)
n_distinct(spp$order)
n_distinct(spp$family)
n_distinct(spp$genus)

# Build data
################################################################################

# Extract totals to use
totals_use <- totals_orig %>% 
  filter(!year %in% waters_orig$year)

# Waters
others <- c("Continental", "Central Pacific", "South Pacific", "Japan", "Africa", "Other")
waters_vec <- c("California waters", "Waters south-of-state", "Waters north-of-state", "Other waters", "Shipments")

# Format "by waters" time series
waters_use <- waters_orig %>%
  # Add waters category
  mutate(waters_catg=ifelse(waters %in% others, "Others", waters)) %>% 
  # Sum by waters category
  group_by(year, waters_catg) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Rename
  rename(waters=waters_catg)
  
# Extract "totals" time series to supplement
totals_use <- totals_orig %>% 
  filter(!year %in% waters_use$year) %>% 
  # Add waters
  mutate(waters="Unknown") %>% 
  select(year, waters, seafood_lb) %>% 
  # Rename
  rename(landings_lb=seafood_lb)
  
# Build final "by waters" dataset
waters1 <- bind_rows(waters_use, totals_use) %>% 
  # Format waters
  mutate(waters=recode(waters, 
                       "Not specified"="Unknown",
                       "California"="California waters",
                       "North-of-State"="Waters north-of-state",
                       "South-of-State"="Waters south-of-state",
                       "Others"="Other waters")) %>% 
  mutate(waters_factor=factor(waters, levels=rev(waters_vec))) %>% 
  # Arrange
  arrange(year, waters) %>% 
  select(year, waters, waters_factor, landings_lb)

# By waters (props)
waters2 <- waters1 %>%
  # Remove shipments
  filter(waters!="Shipments") %>% 
  # Sum
  group_by(year, waters, waters_factor) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Props
  group_by(year) %>% 
  mutate(plandings=landings_lb/sum(landings_lb))
  

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
g1 <- ggplot(waters1, aes(x=year, y=landings_lb/1e6, fill=waters_factor)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  # Labels
  labs(x="Year", y="Landings and shipments\n(millions of lbs)") +
  # Limits
  scale_x_continuous(breaks=seq(1910,2020,10)) +
  # Legend
  scale_fill_discrete(name="Source", na.value="grey80") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.85,0.8),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Plot data
g2 <- ggplot(waters2, aes(x=year, y=plandings, fill=waters_factor)) +
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
ggsave(g, filename=file.path(plotdir, "FigX_comm_landings_by_source.png"), 
       width=6.5, height=4, units="in", dpi=600)


