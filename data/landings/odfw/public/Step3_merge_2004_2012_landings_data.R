

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
basedir <- "data/landings/odfw/public/raw"
workdir <- "data/landings/odfw/public/intermediate"
outputdir <- "data/landings/odfw/public/processed"


# Build file key
################################################################################

# Build file key
file_key <- purrr::map_df(2004:2012, function(x){
  datadir <- file.path(basedir, x, "by_port/landings") 
  df <- tibble(year=x,
               filename=list.files(datadir, pattern=".pdf"))
})

# Add port name
file_key <- file_key %>% 
  mutate(port=ifelse(grepl("22", filename), "Brookings", "other"),
         port=ifelse(grepl("21", filename), "Gold Beach", port),
         port=ifelse(grepl("20", filename), "Port Orford", port),
         port=ifelse(grepl("19", filename), "Bandon", port),
         port=ifelse(grepl("18", filename), "Charleston", port),
         port=ifelse(grepl("17", filename), "Winchester Bay", port),
         port=ifelse(grepl("16", filename), "Florence", port),
         port=ifelse(grepl("15", filename), "Waldport", port),
         port=ifelse(grepl("15A", toupper(filename)), "Yachats", port),
         port=ifelse(grepl("14", filename), "Newport", port),
         port=ifelse(grepl("13", filename), "Depoe Bay", port),
         port=ifelse(grepl("12", filename), "Siletz Bay", port),
         port=ifelse(grepl("11", filename), "Pacific City", port),
         port=ifelse(grepl("11A", toupper(filename)), "Salmon River", port),
         port=ifelse(grepl("10", filename), "Netarts Bay", port),
         port=ifelse(grepl("09|Table 9", filename) | filename=="9.pdf", "Garibaldi", port),
         port=ifelse(grepl("08|Table 8", filename) | filename=="8.pdf", "Nehalem Bay", port),
         port=ifelse(grepl("07|Table 7", filename) | filename=="7.pdf", "Astoria", port),
         port=ifelse(grepl("7A", toupper(filename)), "Gearhart-Seaside", port),
         port=ifelse(grepl("7B", toupper(filename)), "Cannon Beach", port),
         port=ifelse(grepl("06|Table 6", filename) | filename=="6.pdf", "Columbia River", port))


# Merge data
################################################################################

# Merge landings data
data_orig <- purrr::map_df(2004:2012, function(x){
  datadir <- file.path(basedir, x, "by_port/landings") 
  data <- readxl::read_excel(file.path(datadir, paste0(x, "_landings_data_clean.xlsx"))) %>% 
    mutate_all(as.character) %>% 
    mutate(year=x)
})

# Format data data
data_full <- data_orig %>% 
  # Remove useless columns
  select(-c(type, flag)) %>%
  select(year, everything()) %>% 
  # Add port
  left_join(file_key, by=c("year", "filename")) %>% 
  select(year, filename, port, everything()) %>% 
  # Format species names
  mutate(species=gsub("\\.|\\-|_|0", "", species),
         species=species %>% stringr::str_to_sentence() %>% stringr::str_trim(),
         species=gsub("Clams", "Clam", species), 
         species=gsub("pacific", "Pacific", species),
         species=recode(species,
                        "Anchovynorthern"="Anchovy, northern",
                        "Clambutter"="Clam, butter",
                        "Clam, nat littleneck"="Clam, native littleneck",
                        "Clingfish northern"="Clingfish, northern",
                        "Cod, pacific"="Cod, Pacific",
                        "Crab, dungeness  bay"="Crab, Dungeness (bay)",
                        "Crab, dungeness bay"="Crab, Dungeness (bay)",
                        "Crab, dungeness ocean"="Crab, Dungeness (ocean)",
                        "Crab, dungeness, bay"="Crab, Dungeness (bay)",
                        "Crab, dungeness, ocean"="Crab, Dungeness (ocean)",
                        "Crabdungenessbay"="Crab, Dungeness (bay)",
                        "Crabdungenessocean"="Crab, Dungeness (ocean)",
                        "Fish"="Fish:",
                        "Flounder, arrowtooth"="Flounder, arrowtooth",
                        "Flounder, starry"="Flounder, starry",
                        "Flounderarrowtooth"="Flounder, arrowtooth",
                        "Greenling sp"="Greenling, unspecified",
                        "Grenadier, pacific"="Grenadier, Pacific",
                        "Grenadier, unsp"="Grenadier, unspecified",
                        "Halibutpacific"="Halibut, pacific",
                        "Salmon chinook"="Salmon, Chinook",
                        "Salmonchinook"="Salmon, Chinook",
                        "Sharkblue"="Shark, blue",
                        "Sharkspinydogfish"="Shark, spiny dogfish",
                        "Shimp, mud"="Shrimp, mud",
                        "Sunfishocean"="Sunfish, ocean",
                        "Whiting, pac (hake)"="Whiting, Pacific (hake)",
                        "Clam, manila littlene"="Clam, manila littleneck",	
                        "Grass shrimp"="Grass, shrimp",	
                        "Greenling, sp"="Greenling, unspecified",	
                        "Grenadier"="Grenadier",	
                        "Gunnel sp"="Gunnel, unspecified",	
                        "Hagfish sp"="Hagfish, unspecified",	
                        "Prickleback spp"="Prickleback, unspecified",	
                        "Rockfish, black&yellow"="Rockfish, black and yellow",	
                        "Salmon, chinook"="Salmon, Chinook",	
                        "Sculpin, other sp"="Sculpin, other species",	
                        "Shark, other sp"="Shark, other species",	
                        "Squid, other sp"="Squid, other species",	
                        "Surfperch spp"="Surfperch, unspecified",
                        "Barracudinaduckbill"="Barracudina, duckbill",
                        "Grenadier45551"="Grenadier",
                        "Grenadier455"="Grenadier",
                        "HalibutPacific"="Halibut, Pacific",
                        "Lingcod177"="Lingcod",
                        "Sharkbrowncat"="Shark, brown cat",
                        "SharkPacificsleeper129"="Shark, Pacific sleeper",
                        "Sharkthresher"="Shark, thresher",
                        "Shrimpghost"="Shrimp, ghost",
                        "Smelt, other sp"="Smelt, other species",
                        "Thornyhead sp"="Thornyhead, other species",
                        "Tuneshoulder"="Tubeshoulder",
                        "Wrymouthgiant"="Wrymouth, giant",
                        "Eel pout"="Eelpout",
                        "Lingcod17"="Lingcod",
                        "SharkPacificsleeper"="Shark, Pacific sleeper",
                        "Solebutter"="Sole, butter",
                        "Wolf, eel"="Wolfeel")) %>% 
  # Remove empty species
  filter(species!="") %>% 
  # Add category
  mutate(category=ifelse(grepl(":", species), species, NA)) %>% 
  fill(category, .direction="down") %>% 
  # Use category to remove empty rows
  filter(!grepl(":", species)) %>% 
  select(year, filename, port, category, species, everything())

# Replace NAs with 0s
data_full[is.na(data_full)] <- "0"

# Inspect
freeR::complete(data_full)
str(data_full)
table(data_full$port)
table(data_full$category)
range(data_full$year)

# Sppecies key
spp_key <- tibble(species=sort(unique(data_full$species)))

# Export for manual edditing
write.csv(data_full, file=file.path(workdir, "2004_2012_landings_data_messy.csv"), row.names=F)

