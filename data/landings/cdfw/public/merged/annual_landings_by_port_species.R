
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

# Read FB data
data_fb_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1941_1976_landings_by_port_species.Rds")

# Read website data
# data_web_orig <- readRDS("data/landings/cdfw/public/website/by_port/processed/CDFW_2000_2019_landings_by_port.Rds")
data_web_orig <- read.csv("data/landings/cdfw/public/website/by_port/processed/CDFW_2000_2019_landings_by_port_expanded.csv", as.is=T)

# Get port info
ports <- wcfish::ports

# Build data
################################################################################

# Column names
colnames(data_fb_orig)
colnames(data_web_orig)

# Goal dataset
# source, table, port_complex, port_orig, port, type, year, 
# comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg, value_usd

# Format web data
data_web <- data_web_orig %>% 
  # Remove unnecessary columns
  select(-c(sci_name, level, taxa_group1, taxa_group2, environment)) %>% 
  # Rename
  rename(port_complex=area) %>% 
  # Add source/table
  mutate(source=paste("CDFW", year+1)) %>% 
  mutate(table=recode(port_complex, 
                      "Eureka"='Table 16',
                      "Fort Bragg"="Table21FB",
                      "San Francisco"="Table 17",
                      "Monterey"="Table 18",
                      "Morro Bay"="Table 21MB",
                      "Santa Barbara"="Table 19",
                      "Los Angeles"="Table 20",
                      "San Diego"="Table 21SD",
                      "Sacramento Delta"="Table 21DS",
                      "Inland Waters"="Table 21IW")) %>% 
  # Add type
  mutate(type="Landings") %>% 
  # Add port name
  mutate(port_orig=port) %>% 
  # Fix presentation
  mutate(presentation=recode(presentation, "whole"="not specified")) %>% 
  # Fix common names
  rename(comm_name_temp=comm_name) %>% 
  mutate(comm_name_temp=recode(comm_name_temp, 
                               "Arrowtooth rockfish"="Arrowtooth flounder", 
                               'True smelts'='True smelt',
                               'Bocaccio/chili rockfish group'='Bocaccio/chilipepper rockfish group', 
                               'Unspecifed croaker'='Unspecified croaker')) %>% 
  # Harmonize common names and scientific names
  mutate(comm_name=wcfish::harmonize_names(comm_name_temp, "comm", "comm")) %>% 
  mutate(sci_name=wcfish::harmonize_names(x=comm_name, from="comm", to="sci")) %>%
  # Arrange
  select(source, table, port_complex, port, port_orig, type, year, 
         comm_name, comm_name_orig, sci_name, presentation, value_usd, landings_lb, landings_kg, everything()) %>% 
  # Remove unnecessary columns
  select(-c(filename, comm_name_temp))

# Inspect
wcfish::check_names(data_web$comm_name_temp)
freeR::complete(data_web) # some values are missing, everything else MUST be zero


# Format Fish Bulletin data
data_fb <- data_fb_orig %>%
  # Rename
  rename(comm_name_orig=species) %>% 
  # Add pounds in kg
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  # Add presentation
  mutate(presentation="not specified") %>% 
  # Harmonize common names and scientific names
  mutate(comm_name=wcfish::harmonize_names(comm_name_orig, "comm", "comm")) %>% 
  mutate(sci_name=wcfish::harmonize_names(x=comm_name, from="comm", to="sci")) %>%
  # Arrange
  select(source, table, port_complex, port, port_orig, type, year, 
         comm_name, comm_name_orig, sci_name, presentation, value_usd, landings_lb, landings_kg, everything())

# Inspect 
freeR::complete(data_fb) # many landings are missing, everything else MUST be zero

# Merge data
data <- bind_rows(data_fb, data_web) %>% 
  # Add source type
  mutate(source_type=ifelse(grepl("FB", source), "Fish Bulletins", "Website")) %>% 
  # Format some ports
  mutate(port=recode(port, 
                     "Santa Barbara Harbor"="Santa Barbara",
                     "Mission Beach"="Mission Bay",
                     "Gaviota"="Gaviota Beach",
                     "Avila/Port San Luis/Grover City"="Avila/Port San Luis",
                     "Avalon"="Avalon (Catalina Island)")) %>% 
  # Add harmonized port complex
  rename(port_complex_orig=port_complex) %>% 
  left_join(ports %>% select(port, port_complex1), by="port") %>% 
  rename(port_complex=port_complex1) %>% 
  # Arrange
  select(source_type, source, table, port_complex, port_complex_orig, port, port_orig, everything()) %>% 
  arrange(year, port_complex, port, comm_name_orig)

# Inspect data
str(data)
freeR::complete(data) # a few missing values in web, many missing landings in FB, everything else ZERO
range(data$year)
table(data$port_complex)
table(data$port)
table(data$type)
table(data$presentation)


# Inspect port coverage
################################################################################

# Calculate coverage
coverage_ports <- data %>% 
  group_by(source_type, port_complex, port, year) %>% 
  summarize(n=n())

# Plot coverage
g1 <- ggplot(coverage_ports, aes(x=year, y=port)) +
  facet_grid(port_complex~source_type, scales="free", space="free") +
  geom_tile() +
  labs(x="", y="") +
  theme_bw()
g1


# Inspect species coverage
################################################################################

# Confirm that species only occurs once per year-port
# Shark actually occurs twice in 1945 San Diego
# Copper rockfish appears as whitebelly rocksfish in 2001 ML / 2004 SB
spp_check <- data %>% 
  group_by(year, port, type, comm_name, presentation) %>% 
  summarize(n=n()) %>% 
  filter(n!=1)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.Rds"))
write.csv(data, file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.csv"), row.names=F)

