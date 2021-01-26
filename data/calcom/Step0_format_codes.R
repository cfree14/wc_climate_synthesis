
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
datadir <- "data/calcom/data/raw/codes"
outputdir <- "data/calcom/data/processed"


# Species key
################################################################################

# Species key
spp_key_orig <- readxl::read_excel(file.path(datadir, "species_key.xlsx"))

# Format
spp_key <- spp_key_orig %>% 
  # Trim all
  mutate_all(stringr::str_trim) %>% 
  # Format commone name
  mutate(common_name=stringr::str_to_sentence(common_name)) %>% 
  # Format scientific name
  mutate(scientific_name=stringr::str_to_sentence(scientific_name)) %>% 
  # Format group
  rename(market_group=market_group_code) %>% 
  mutate(market_group=stringr::str_to_sentence(market_group),
         market_group=recode(market_group,
                             "Invert"="Invertebrate",
                             "Other_gf"="Other groundfish")) %>% 
  # Arrange
  arrange(species_code)

# Inspect
str(spp_key)
table(spp_key$market_group)

# Export
write.csv(spp_key, file=file.path(outputdir, "species_key.csv"), row.names=F)
