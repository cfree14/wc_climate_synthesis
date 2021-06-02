
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


# Number of market categories over time
################################################################################

#
stats <- data_orig %>% 
  # California
  filter(waters=="California") %>% 
  # Unique species by year
  select(year, sci_name) %>% 
  unique() %>% 
  # Number by species/group
  group_by(year) %>% 
  summarize(n_spp=sum(!grepl("spp", sci_name)),
            n_group=sum(grepl("spp", sci_name))) %>% 
  ungroup() %>% 
  # Gather
  gather(key="type", value="nspp", 2:3)

# Number of market categories by year
g <- ggplot(stats, aes(x=year, y=nspp, fill=type)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y="Number of market categories") +
  # Theme
  theme_bw()
g




