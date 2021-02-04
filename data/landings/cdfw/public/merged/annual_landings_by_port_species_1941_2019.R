
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
                               'True smelts'='True smelt')) %>% 
  # Harmonize common names and scientific names
  mutate(comm_name=wcfish::harmonize_names(comm_name_temp, "comm", "comm")) %>% 
  mutate(sci_name=wcfish::harmonize_names(x=comm_name, from="comm", to="sci")) %>%
  # Arrange
  select(source, table, port_complex, port_orig, port, type, year, 
         comm_name_orig, comm_name, sci_name, presentation, value_usd, landings_lb, landings_kg, everything()) %>% 
  # Remove unnecessary columns
  select(-c(filename, comm_name_temp))

# Inspect
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
  select(source, table, port_complex, port_orig, port, type, year, 
         comm_name_orig, comm_name, sci_name, presentation, value_usd, landings_lb, landings_kg, everything())

# Inspect 
freeR::complete(data_fb) # many landings are missing, everything else MUST be zero

# Merge data
data <- bind_rows(data_fb, data_web) %>% 
  # Arrange
  arrange(year, port_complex, port, comm_name_orig)

# Inspect data
str(data)
freeR::complete(data) # a few missing values in web, many missing landings in FB, everything else ZERO
range(data$year)
table(data$port_complex)
table(data$port)
table(data$type)
table(data$presentation)


# Plot data
################################################################################

ports <- data %>% 
  group_by(port) %>% 
  summarize(regions=paste(sort(unique(port_complex)), collapse=", ")) %>% 
  arrange(port)
write.csv(ports, file="~/Desktop/california_port_key.csv")

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.Rds"))
write.csv(data, file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.csv"), row.names=F)

