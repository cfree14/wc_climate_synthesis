
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/environmental/indices/raw"
outdir <- "data/environmental/indices/processed"

# Source
# https://psl.noaa.gov/enso/mei/

# Read data
data_orig <- readRDS(file.path(outdir, "environmental_indices.Rds"))


# Build data
################################################################################

# Wide data
data <- data_orig %>% 
  # Reduce
  filter(!is.na(enso))

# Long data
data_long <- data %>% 
  # Reduce
  filter(!is.na(enso)) %>% 
  # Gather
  gather(key="index", value="value", 4:ncol(.)) %>% 
  # Format index
  mutate(index=toupper(index)) 


# Plot data
################################################################################

# Plot correlation
g <- ggplot(data, aes(x=enso, y=pdo)) +
  geom_point() +
  labs(x="ENSO", y="PDO") +
  theme_bw()
g

# Plot time series
g <- ggplot(data_long, aes(x=date, y=value, color=index)) +
  geom_line() +
  # Labels
  labs(x="", y="Index") +
  # Legend
  scale_color_discrete(name="Index") +
  # Theme
  theme_bw()
g

