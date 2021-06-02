
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

# Comm landings datasets
ports_orig <- wcfish::cdfw_ports
waters_orig <- wcfish::cdfw_waters
kelp_orig <- wcfish::cdfw_kelp %>% 
  filter(!is.na(total_t))
kelp_tot_orig <- wcfish::cdfw_kelp_tots %>% 
  filter(!is.na(harvest_t))

# Comm participation datasets
vess1_orig <- wcfish::cdfw_n_comm_vessels_length
vess2_orig <- wcfish::cdfw_n_comm_vessels_port
vess3_orig <- wcfish::cdfw_n_comm_vessels
nfishers <- wcfish::cdfw_n_comm_fishers
nfishers_port <- nfishers %>% 
  filter(region_type=="area of residence")

# CPFV landings datasets
cpfv_tl <- wcfish::cdfw_cpfv
cpfv_tl_port <- cpfv_tl %>% 
  filter(region!="Statewide")

# CPFV effort datasets
cpfv_effort_orig <- wcfish::cdfw_cpfv_effort
cpfv_nanglers <- cpfv_effort_orig %>% 
  filter(!is.na(anglers_n))
cpfv_nvessels <- cpfv_effort_orig %>% 
  filter(!is.na(cpfvs_n))
cpfv_ntime <- cpfv_effort_orig %>% 
  filter(!is.na(days_n) | !is.na(hours_n))


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

# Commercial particpation
vess1_coverage <- calc_coverage(vess1_orig, source="CDFW", category="Commercial\nparticipation", "Number of vessels\nby length class")
vess2_coverage <- calc_coverage(vess2_orig, source="CDFW", category="Commercial\nparticipation", "Number of vessels\nby port complex")
vess3_coverage <- calc_coverage(vess3_orig, source="CDFW", category="Commercial\nparticipation", "Number of vessels")
fishers_coverage <- calc_coverage(nfishers, source="CDFW", category="Commercial\nparticipation", "Number of fishers")
fishers_port_coverage <- calc_coverage(nfishers_port, source="CDFW", category="Commercial\nparticipation", "Number of fishers\nby area of residence")

# Commercial landings
waters_coverage <- calc_coverage(waters_orig, source="CDFW", category="Commercial\nlandings", "Landings (value/volume)\nby source and species")
port_coverage <- calc_coverage(ports_orig, source="CDFW", category="Commercial\nlandings", "Landings (value/volume)\nby port and species")
kelp_coverage <- calc_coverage(kelp_orig, source="CDFW", category="Commercial\nlandings", "Kelp harvest (volume)\nby bed type")
kelp_tot_coverage <- calc_coverage(kelp_tot_orig, source="CDFW", category="Commercial\nlandings", "Kelp harvest (volume)")

# Recreation landings
cpfv_tl_coverage <- calc_coverage(cpfv_tl, source="CDFW", category="Recreational (CPFV)\nlandings/participation", "Landings (number of fish)")

# Recreational participation
cpfv_vessels_coverage <- calc_coverage(cpfv_nvessels, source="CDFW", category="Recreational (CPFV)\nlandings/participation", "Number of CPFV vessels")
cpfv_anglers_coverage <- calc_coverage(cpfv_nanglers, source="CDFW", category="Recreational (CPFV)\nlandings/participation", "Number of CPFV anglers")
cpfv_time_coverage <- calc_coverage(cpfv_ntime, source="CDFW", category="Recreational (CPFV)\nlandings/participation", "Number of days/hours\nof fishing by CPFV anglers")

# Merge coverages
coverages <- bind_rows(vess1_coverage,
                       vess2_coverage,
                       vess3_coverage,
                       fishers_coverage,
                       fishers_port_coverage,
                       waters_coverage,
                       port_coverage,
                       kelp_coverage,
                       kelp_tot_coverage,
                       cpfv_tl_coverage,
                       cpfv_vessels_coverage,
                       cpfv_anglers_coverage,
                       cpfv_time_coverage)

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_blank(),
                   strip.text=element_text(size=6),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot coverages
g <- ggplot(coverages, aes(x=year, y=dataset)) +
  facet_grid(category~., scales="free_y", space="free_y") +
  geom_tile(fill="grey70") +
  # Add dotted line
  geom_vline(xintercept=2000, linetype="dashed", color="grey30") +
  # Axis
  scale_x_continuous(limits=c(1916, 2020), breaks=seq(1910, 2020, 10)) +
  # Labels
  labs(x="", y="") + 
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigX_dataset_temporal_coverage.png"), 
       width=6.5, height=3.5, units="in", dpi=600)



