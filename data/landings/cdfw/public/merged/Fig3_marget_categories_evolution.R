
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

# Taxa key
taxa_key <- wcfish::taxa

# Build data
################################################################################

# Number of market categories over time
stats <- data_orig %>%
  # California
  filter(waters!="Shipments") %>%
  # Annual catch by species
  group_by(year, comm_name, sci_name) %>%
  summarize(landings_kg=sum(landings_kg, na.rm=T)) %>%
  ungroup() %>% 
  # Mark whether species specific (no spp or comma)
  mutate(type=ifelse(grepl("spp|,", sci_name), "Not species-specific", "Species-specific"),
         type=factor(type, levels=c( "Species-specific", "Not species-specific"))) %>% 
  # Summarize by year and type
  group_by(year, type) %>% 
  summarize(nspp=n(),
            landings_kg=sum(landings_kg, na.rm=T)) %>% 
  ungroup() %>% 
  # Summarize by year
  group_by(year) %>% 
  mutate(plandings=landings_kg/sum(landings_kg))

# Evolution key
evo_key <- data_orig %>%
  # California
  filter(waters!="Shipments") %>%
  # Add taxa
  left_join(taxa_key, by="comm_name") %>% 
  # Mark whether species specific (no spp or comma)
  mutate(type2=ifelse(grepl("spp|,", sci_name), "Not/nspecies-specific", "Species-specific"),
         type2=factor(type2, levels=c( "Species-specific", "Not/nspecies-specific"))) %>% 
  # Annual catch by species
  group_by(year, type2, taxa_group, family, comm_name, sci_name, type) %>%
  summarize(landings_kg=sum(landings_kg, na.rm=T)) %>%
  ungroup() 

# Rockfish evolution key
rock_evo_key <- evo_key %>% 
  # Reduce to rockfish
  filter(taxa_group=="Rockfish") %>% 
  # Simplify common name for plotting
  mutate(comm_name=gsub(" rockfish", "", comm_name)) %>% 
  # Set plot order
  mutate(type3=ifelse(comm_name=="Rockfish", 1, 
                      ifelse(grepl("group", comm_name), 2, 3))) %>% 
  arrange(year, type3, comm_name)

# Pull out key
rock_key <- rock_evo_key %>% 
  select(type3, comm_name) %>% 
  unique() %>% 
  arrange(type3, comm_name)

# Set order
rock_evo_key1 <- rock_evo_key %>% 
  mutate(comm_name=factor(comm_name, levels=rock_key$comm_name))

# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    axis.title.x=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

# Adjusted base theme
base_theme2 <- theme(axis.text.y=element_text(size=4),
                     axis.text.x=element_text(size=6),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=6),
                     plot.title=element_blank(),
                     # Gridlines
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor.x = element_blank(),
                     panel.grid.minor.y = element_line(size=0.5),
                     panel.grid.major.y = element_line(size=0.5),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

# Number of market categories by year
g1 <- ggplot(stats, aes(x=year, y=nspp, fill=type)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="Number of\nmarket categories", tag="A") +
  # Limits
  scale_x_continuous(breaks=seq(1940,2020,10)) +
  # Legend
  scale_fill_discrete(name="Market category") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.12, 0.8),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Proportion of catch by year and category
g2 <- ggplot(stats, aes(x=year, y=plandings, fill=type)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="Proportion\nof landings", tag="B") +
  # Limits
  scale_x_continuous(breaks=seq(1940,2020,10)) +
  # Legend
  scale_fill_discrete(name="Market category") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Rockfish evolution
g3 <- ggplot(rock_evo_key1, aes(x=year, y=comm_name, fill=type2)) +
  # Facet
  # facet_grid(type2~., scales="free_y", space="free_y") +
  # Plot
  geom_tile() +
  # Labels
  labs(y="Rockfish market category", x="Year", tag="C") +
  # Limits
  scale_x_continuous(breaks=seq(1940,2020,10)) +
  scale_y_discrete(position="right") +
  # Theme
  theme_bw() + base_theme2 +
  theme(legend.position = "none",
        plot.tag.position="topright")
g3

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=1, heights=c(0.3, 0.15, 0.55))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_market_categories_evolution.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

