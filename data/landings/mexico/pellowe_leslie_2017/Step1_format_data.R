

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/mexico/pellowe_leslie_2017/raw"
outputdir <- "data/landings/mexico/pellowe_leslie_2017/processed"

# Read data
data_orig <- read.delim(file.path(inputdir, "pone.0182200.s003.csv"), fileEncoding="UTF-16LE", as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(comm_name=name_species, price_mxn_kg=price_pesos, landings_kg=weight_kilos) %>% 
  # Arrange columns
  select(year, month, office, comm_name, price_mxn_kg, landings_kg) %>% 
  # Format office
  mutate(office=stringr::str_to_title(office)) %>%
  mutate(office=recode(office, "Cd. Constitución"="Ciudad Constitución")) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name) %>% stringr::str_trim()) %>% 
  # Format landings/value
  mutate(landings_mt=landings_kg/1000,
         value_mxn=landings_kg*price_mxn_kg)

# Inspect
str(data)
freeR::complete(data)
range(data$year)
table(data$month)
sort(unique(data$office))
sort(unique(data$comm_name))

# Export
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "2001_2013_baja_landings_pellowe_leslie.Rds"))



# Plot check
################################################################################

# Annual stats
stats <- data %>% 
  group_by(year, office) %>% 
  summarize(landings_mt=sum(landings_mt))

# Plot
g <- ggplot(stats, aes(x=year, y=landings_mt/1e3, fill=office)) +
  geom_area() +
  labs(x="Year", y="Landings (1000s mt)") +
  theme_bw()
g





