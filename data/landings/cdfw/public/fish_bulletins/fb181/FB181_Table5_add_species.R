

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/fb181/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/fb181/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/fb181/figures"

# Read data
data_orig <- read.csv(file=file.path(outdir, "FB181_Table5_1987_1999_landings_by_port_complex_species.csv"), as.is=T)

# Read species key
spp_key_orig <- read.csv(file=file.path(outdir, "FB181_Table1_ca_species_list_final.csv"), as.is=T)


# Setup
################################################################################

# Check key
spp_key <- data_orig %>% 
  # Reduce to unique species
  select(comm_name_orig) %>% 
  unique() %>% 
  arrange(comm_name_orig) %>% 
  # Add existing key information
  left_join(spp_key_orig %>% select(comm_name_orig, sci_name, level)) %>% 
  # Fill in gaps (level)
  mutate(level=ifelse(is.na(level) & grepl("specified", comm_name_orig), "group", level),
         level=ifelse(comm_name_orig %in% c("Splittail", "Turbot, c-o"), "species", level),
         level=ifelse(comm_name_orig %in% c("Snail, top", "Snails, moon"), "group", level)) %>% 
  # Fill in gaps (sci name) 
  mutate(sci_name=ifelse(comm_name_orig=="Splittail", "Pogonichthys macrolepidotus", sci_name),
         sci_name=ifelse(comm_name_orig=="Turbot, c-o", "Pleuronichthys coenosus", sci_name),
         sci_name=ifelse(comm_name_orig=="Croaker, unspecified", "Sciaenidae", sci_name),
         sci_name=ifelse(comm_name_orig=="Invertebrate, unspecified", "Invertebrate", sci_name),
         sci_name=ifelse(comm_name_orig=="Snail, top", "Trochidae", sci_name),
         sci_name=ifelse(comm_name_orig=="Snails, moon", "Naticidae", sci_name),
         sci_name=ifelse(comm_name_orig=="Thornyheads (unspecified)", "Sebastolobus spp.", sci_name),
         sci_name=ifelse(comm_name_orig=="Unspecified species", "Pleuronichthys coenosus", sci_name),)

# Add species to data
data <- data_orig %>% 
  # Add scientific name and level
  left_join(spp_key) %>% 
  # Arrange
  select(source:comm_name_orig, sci_name, level, everything())

# Inspect
freeR::complete(data)


# Export
################################################################################

# Export
write.csv(data, file=file.path(outdir, "FB181_Table5_1987_1999_landings_by_port_complex_species_expanded.csv"), row.names=F)







