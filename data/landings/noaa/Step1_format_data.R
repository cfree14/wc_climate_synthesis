

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/noaa/raw"
outputdir <- "data/landings/noaa/processed"
plotdir <- "data/landings/noaa/figures"

# Read data
data_orig <- read.csv(file.path(inputdir, "foss_landings.csv"), as.is=T, na.strings="")

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename column
  janitor::clean_names("snake") %>% 
  rename(comm_name=nmfs_name, 
         landings_lb=pounds,
         value_usd=dollars,
         sci_name=scientific_name) %>% 
  # Arrange columns
  select(region, source, state, comm_name, sci_name, year, landings_lb, value_usd, confidentiality) %>% 
  arrange(region, state, comm_name, year) %>% 
  # Format columns
  mutate(state=stringr::str_to_title(state),
         value_usd=value_usd %>% gsub(",", "", .) %>% as.numeric(),
         landings_lb=landings_lb %>% gsub(",", "", .) %>% as.numeric()) 

# Inspect data
str(data)
range(data$year)
table(data$state)
table(data$state)
table(data$collection) 
table(data$confidentiality)
table(data$summary_type)

# Build species key
spp_key <- data %>% 
  select(comm_name, sci_name) %>% 
  unique()

# Export data
saveRDS(data, file.path(outputdir, "1950_2019_usa_landings_by_state_species.Rds"))

# Export species key
write.csv(spp_key, file=file.path(outputdir, "usa_commercial_species_key.csv"), row.names=F)

