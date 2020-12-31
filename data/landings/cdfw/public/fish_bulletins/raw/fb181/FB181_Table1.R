
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/cdfw/public/fish_bulletins/fb181/raw"
outputdir <- "data/landings/cdfw/public/fish_bulletins/fb181/processed"

# Read 
spp_key_orig <- readxl::read_excel(file.path(datadir, "FB181_Table1_ca_species_list.xlsx"))


# Build data
################################################################################

# Format data
spp_key <- spp_key_orig %>% 
  mutate(sci_name=sci_name_orig) %>% 
  mutate(sci_name=recode(sci_name, 
                         "Acanthocybium solanderi"="Acanthocybium solandri",
                         "Annipis trutta"="Arripis trutta",
                         "Callianassa californiensis"="Neotrypaea californiensis",
                         "Cancer magister"="Metacarcinus magister",
                         "Clupea pallasi"="Clupea pallasii pallasii",
                         "Doscidicus gigas"="Dosidicus gigas",
                         "Eopsetta exilis"="Lyopsetta exilis",
                         "Errex zachirus"="Glyptocephalus zachirus",
                         "Etrumeus teres"="Etrumeus sadina",
                         "Eusicyonia ingentus"="Sicyonia ingentis",
                         "Hemisquilla ensigera californiensis"="Hemisquilla ensigera",
                         "Hermosilla azurea"="Kyphosus azureus",
                         "Kelletia kelleti"="Kelletia kelletii",
                         "Loligo opalescens"="Doryteuthis opalescens",
                         "Penaeus californiensis"="Farfantepenaeus californiensis",
                         "Pleuronectes bilineatus"="Paraplagusia bilineata",
                         "Pleuronectes isolepis"="Isopsetta isolepis",
                         "Pleuronectes vetulus"="Parophrys vetulus",
                         "Protothaca staminea"="Leukoma staminea",
                         "Raja binoculata"="Beringraja binoculata",
                         "Raja inornata"="Beringraja inornata",
                         "Rhinobatos productus"="Pseudobatos productus",
                         "Sardinops sagax caeruleus"="Sardinops sagax",
                         "Strongylocentrotus franciscanus"="Mesocentrotus franciscanus",
                         "Symphurus atricauda"="Symphurus atricaudus",
                         "Torpedo californica"="Tetronarce californica",
                         "Tresus nuttalli"="Tresus nuttallii",
                         "Xenistius californiensis"="Haemulon californiensis")) %>% 
  # Arrange
  arrange(group, comm_name_orig)

# Species to check
species_check <- spp_key$sci_name[spp_key_orig$level=="species"] %>% unique() %>% sort()
freeR::check_names(species_check)

# Export
write.csv(spp_key, file=file.path(outputdir, "FB181_Table1_ca_species_list_final.csv"), row.names=F)













