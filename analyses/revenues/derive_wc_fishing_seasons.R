
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
pacfindir <- "data/landings/pacfin/processed"
plotdir <- "analyses/revenues/figures"

# Read data
landings_orig <- readRDS(file=file.path(pacfindir, "PACFIN_ALL006_1980_2022_all_species_landings_by_month.Rds"))

# Build data
################################################################################

# Build data
data <- landings_orig %>% 
  # Sum by state-species-year-month (merging actual/nominal)
  group_by(state, mgmt_group_code, comm_name_parent, year, month_num) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate proportional catch by year
  filter(landings_mt>0) %>% 
  group_by(state,  mgmt_group_code, comm_name_parent, year) %>% 
  mutate(landings_prop=landings_mt/sum(landings_mt, na.rm=T)) %>% 
  # Calculate average since 2000
  filter(year>=2000) %>% 
  group_by(state,  mgmt_group_code, comm_name_parent, month_num) %>% 
  mutate(landings_prop_avg=mean(landings_prop)) %>% 
  # Eliminate at-sea
  filter(state!="At-Sea" & comm_name_parent!="Withheld for confidentiality**") %>% 
  # Eliminat some species
  filter(!grepl("Unsp.|Other|Misc.", comm_name_parent))


# Plot data
################################################################################

# Theme
my_theme <- my_theme <-  theme(axis.text=element_text(size=5),
                               axis.title=element_text(size=6),
                               legend.text=element_text(size=5),
                               legend.title=element_text(size=6),
                               strip.text=element_text(size=6),
                               plot.title=element_text(size=7),
                               # Facets
                               panel.spacing.y=unit(0, "lines"),
                               # Gridlines
                               panel.grid.major = element_blank(), 
                               panel.grid.minor = element_blank(),
                               panel.background = element_blank(), 
                               axis.line = element_line(colour = "black"),
                               # Legend
                               legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=month_num, y=comm_name_parent, fill=landings_prop_avg)) +
  facet_grid(mgmt_group_code~state, scales="free_y", space="free_y") +
  geom_tile(color="grey30") +
  # Labels
  labs(x="Month", y="") +
  # X-axis
  scale_x_continuous(breaks=1:12) +
  # Legend
  scale_fill_gradientn(name="Proportion of\nannual landings", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

