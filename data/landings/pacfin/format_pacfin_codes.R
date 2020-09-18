

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/pacfin/raw/codes"
outputdir <- "data/landings/pacfin/processed"

# Read codes
list.files(inputdir)


# Species codes
################################################################################

# Read data
spp_code_orig <- read.csv(file.path(inputdir, "pacfin_species_code_list_alphabetical.csv"), as.is=T, na.strings="N/A")

# Format data
spp_code <- spp_code_orig %>% 
  # Columns names
  janitor::clean_names("snake") %>% 
  rename(spp_code=species_code, comm_name=species_common_name, sci_name=species_scientific_name, 
         mgmt_group_code=management_group_code, complex_code=complex) %>% 
  # Format species names
  mutate(comm_name=comm_name %>% gsub("__", "", .) %>% stringr::str_to_sentence(),
         sci_name=stringr::str_to_sentence(sci_name))

# Inspect
head(spp_code)  

# Confirm codes are unique
anyDuplicated(spp_code$spp_code) # YES
freeR:::which_duplicated(spp_code$spp_code) # FIX THIS

# Export
write.csv(spp_code, file=file.path(outputdir, "pacfin_species_codes_clean.csv"), row.names = F)

