

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

# The common/scientific names could be formatted way more rigourously.

# Source: https://www.fisheries.noaa.gov/national/sustainable-fisheries/commercial-fisheries-landings
# Asterisks indcate groups that are not species-specific

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename column
  janitor::clean_names("snake") %>% 
  rename(comm_name_orig=nmfs_name, 
         sci_name_orig=scientific_name,
         landings_lb=pounds,
         value_usd=dollars,
         fishery=collection) %>% 
  # Arrange columns
  select(region, source, state, fishery, tsn, comm_name_orig, sci_name_orig, year, landings_lb, value_usd, confidentiality, everything()) %>% 
  arrange(region, state, fishery, year, comm_name_orig) %>% 
  # Format state
  mutate(state=stringr::str_to_title(state)) %>% 
  # Format numeric values
  mutate(value_usd=value_usd %>% gsub(",", "", .) %>% as.numeric(),
         landings_lb=landings_lb %>% gsub(",", "", .) %>% as.numeric()) %>% 
  # Format species
  mutate(comm_name_orig=comm_name_orig %>% stringr::str_trim(),
         sci_name_orig=sci_name_orig %>% stringr::str_trim())
  
# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$state)
table(data$fishery) 
table(data$confidentiality)
table(data$summary_type)

# Build species key
################################################################################

# Build species key
spp_key <- data %>%
  # Unique species
  filter(!is.na(sci_name_orig)) %>% 
  select(comm_name_orig, sci_name_orig) %>% 
  unique() %>% 
  arrange(comm_name_orig) %>% 
  # New columns
  mutate(comm_name=comm_name_orig %>% gsub("\\*", "", .) %>% stringr::str_trim() %>% stringr::str_to_sentence(),
         comm_name=ifelse(comm_name=="", "Unknown", comm_name),
         sci_name=sci_name_orig %>% stringr::str_trim() %>% stringr::str_to_sentence(),
         sci_name=ifelse(comm_name=="Unknown", "Unknown spp.", sci_name),
         level=ifelse(grepl("\\*", comm_name_orig), "group", "species")) %>% 
  # Fix group level
  mutate(level=ifelse(grepl("spp", sci_name), "group", level),
         sci_name=gsub(" spp.", "", sci_name) %>% stringr::str_trim(),
         sci_name=ifelse(level=="group", paste(sci_name, "spp."), sci_name)) %>% 
  # Fix some scientific names
  mutate(sci_name=recode(sci_name,
                         "Antennarius ocellatus"="Fowlerichthys ocellatus",
                         "Anthias tenuis"="Choranthias tenuis",
                         "Arpisturus brunneus"="Apristurus brunneus",
                         "Bathyrajia interrupta"="Bathyraja interrupta",
                         "Cancer magister"="Metacarcinus magister",
                         "Carangoides ruber"="Caranx ruber",
                         "Carcharhinus perezii"="Carcharhinus perezi",
                         "Clupea pallasii"="Clupea pallasii pallasii",
                         "Dasyatis americana"="Hypanus americanus",
                         "Dasyatis sabina"="Hypanus sabinus",
                         "Elacatinus macrodon"="Tigrigobius macrodon",
                         "Emblemariopsis diaphanus"="Emblemariopsis diaphana",
                         "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                         "Epinephelus mystacinus"="Hyporthodus mystacinus",
                         "Epinephelus nigritus"="Hyporthodus nigritus",
                         "Epinephelus niveatus"="Hyporthodus niveatus",
                         "Etrumeus teres"="Etrumeus sadina",
                         "Gobioides broussonneti"="Gobioides broussonnetii",
                         "Hemanthias vivanus"="Baldwinella vivanus",
                         "Hemicranx amblyrhynchus"="Hemicaranx amblyrhynchus",
                         "Holocanthus bermudensis"="Holacanthus bermudensis",
                         "Hypselodoris edenticulata"="Felimare picta",
                         "Kyphosus sectator"="Kyphosus sectatrix",
                         "Lbatrossia pectoralis"="Albatrossia pectoralis",
                         "Lepisosteus spatula"="Atractosteus spatula",
                         "Lipogramma trilineatum"="Lipogramma trilineata",
                         "Loligo opalescens"="Doryteuthis opalescens",
                         "Loligo pealeii"="Doryteuthis pealeii",
                         "Muraena milaris"="Gymnothorax miliaris",
                         "Nassarius obsoletus"="Ilyanassa obsoleta",
                         "Octopus macropus"="Callistoctopus macropus",
                         "Paralichthys oblongus"="Hippoglossina oblonga",
                         "Protothaca staminea"="Leukoma staminea",
                         "Raja binoculata"="Beringraja binoculata",
                         "Raja inornata"="Beringraja inornata",
                         "Raja trachura"="Bathyraja trachura",
                         "Strombus costatus"="Macrostrombus costatus",
                         "Torpedo nobiliana"="Tetronarce nobiliana",
                         "Zenopsis conchifera"="Zenopsis conchifer"))

# Check species
# species <- spp_key$sci_name[spp_key$level=="species"] %>% unique() %>% sort()
# freeR::suggest_names(species)

# Final formatting
################################################################################

data_out <- data

# Format data
# data_out <- data %>% 
#   # Add species info
#   left_join(spp_key %>% select(comm_name_orig, comm_name, sci_name, level)) %>% 
#   # Arrange
#   select(region:sci_name_orig, comm_name, sci_name, level, everything())
# 
# # Inspect
# freeR::complete(data_out)


# Export data
################################################################################

# Export data
saveRDS(data_out, file.path(outputdir, "NOAA_1950_2019_usa_landings_by_state_species.Rds"))

# Export species key
write.csv(spp_key, file=file.path(outputdir, "NOAA_usa_commercial_species_key.csv"), row.names=F)





