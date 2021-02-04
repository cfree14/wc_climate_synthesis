
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# Read vessel count data
vess1_orig <- readRDS(file=file.path(outdir, "CDFW_1934_1976_n_comm_vessels_by_length_class.Rds"))
vess2_orig <- readRDS(file=file.path(outdir, "CDFW_1934_1956_n_comm_vessels_by_port_complex.Rds"))
vess3_orig <- read.csv(file=file.path(outdir, "CDFW_1934_1999_n_comm_vessels.csv"), as.is=T)

# Landings by port
port1_orig <- readRDS(file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.Rds"))

# Build data
################################################################################

# Function to build year key
calc_coverage <- function(dataset, source, category, dataset_name){
  coverage <- dataset %>% 
    group_by(year) %>% 
    summarize(n=n()) %>% 
    ungroup() %>% 
    mutate(source=source,
           category=category,
           dataset=dataset_name) %>% 
    select(source, category, dataset, everything())
}

# Calculate coverages
vess1_coverage <- calc_coverage(vess1_orig, source="CDFW", category="Commercial\nparticipation", "Number of vessels\nby length class")
vess2_coverage <- calc_coverage(vess2_orig, source="CDFW", category="Commercial\nparticipation", "Number of vessels\nby port complex")
vess3_coverage <- calc_coverage(vess3_orig, source="CDFW", category="Commercial\nparticipation", "Number of vessels")

port1_coverage <- calc_coverage(port1_orig, source="CDFW", category="Commercial\nlandings", "Landings (value/volume)\nby port and species")

# Merge coverages
coverages <- bind_rows(vess1_coverage,
                       vess2_coverage,
                       vess3_coverage,
                       port1_coverage)

# Plot data
################################################################################

# Plot coverages
g <- ggplot(coverages, aes(x=year, y=dataset)) +
  facet_grid(category ~ source, scales="free_y", space="free_y") +
  geom_raster() +
  # Axis
  scale_x_continuous(limits=c(1916, 2020), breaks=seq(1910, 2020, 10)) +
  # Labels
  labs(x="Year", y="") + 
  # Theme
  theme_bw()
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigX_dataset_temporal_coverage.png"), 
       width=6.5, height=2.5, units="in", dpi=600)



