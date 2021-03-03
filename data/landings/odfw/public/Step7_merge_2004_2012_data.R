

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
basedir <- "data/landings/odfw/public/raw"
workdir <- "data/landings/odfw/public/intermediate"
outputdir <- "data/landings/odfw/public/processed"

# Read data
landings_orig <- readRDS(file=file.path(workdir, "2004_2012_landings_data_clean.Rds")) %>% mutate(metric="Landings")
values_orig <- readRDS(file=file.path(workdir, "2004_2012_value_data_clean.Rds")) %>% mutate(metric="Value")


# Inspect coverage
################################################################################

# Landings data coverage
coverage_l <- landings_orig %>% 
  select(year, port) %>% 
  unique() %>% 
  mutate(landings="yes")

# Value data coverage
coverage_v <- values_orig %>% 
  select(year, port) %>% 
  unique() %>%
  mutate(value="yes")

# Coverage key
coverage <- expand.grid(year=2004:2012, port=sort(unique(c(coverage_l$port, coverage_v$port)))) %>% 
  left_join(coverage_l) %>% 
  left_join(coverage_v)


# Build data
################################################################################

# Merge data
data <- bind_rows(landings_orig, values_orig) %>% 
  # Format a few species
  mutate(species=recode(species,
                        "Crab, Dungeness bay"="Crab, Dungeness (bay)",
                        "Halibut, california"="Halibut, California",
                        "Salmon, chinook"="Salmon, Chinook",
                        "Shad, american"="Shad, American",
                        "Smelt, other sp"="Smelt, other species",
                        "Grass, shrimp"="Shrimp, grass",
                        "Sole, c-o"="Sole, C-O")) %>% 
  # Spread
  select(-filename) %>% 
  spread(key="metric", value="value") %>% 
  # Rename
  rename(landings_lb=Landings, value_usd=Value)

# Inspect
str(data)
freeR::complete(data)
range(data$year)
table(data$port)
table(data$port)
table(data$category)
table(data$month)

# Species key
spp_key <- data %>% 
  select(category, species) %>% 
  unique() %>% 
  arrange(category, species)

# Inspect data missing values
check <- data %>% 
  filter(is.na(Value)) %>% 
  filter(!(port=="Astoria" & year %in% c(2005, 2007, 2008))) %>% 
  filter(!(grepl("Columbia", port) & year==2012))
  


# Build file key and add
################################################################################

# File key
file_key <- bind_rows(landings_orig, values_orig) %>% 
  select(year, port, metric, filename) %>% 
  unique() %>% 
  spread(key="metric", value="filename") %>% 
  rename(filename_lb=Landings, filename_usd=Value)

# Add to data
data_out <- data %>% 
  left_join(file_key) %>% 
  select(year, port, filename_lb, filename_usd, everything())


# Export
################################################################################

# Export
write.csv(data_out, file=file.path(outputdir, "ODFW_2004_2012_landings_by_port_month_species.csv"), row.names=F)


