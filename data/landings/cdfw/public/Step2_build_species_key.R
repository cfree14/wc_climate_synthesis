
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "data/landings/cdfw/public/processed"
sppkeydir <- "data/landings/cdfw/public/species_key"
plotdir <- "data/landings/cdfw/public/figures"

# Read data
data <- readRDS(file.path(outputdir, "CDFW_2000_2019_landings_by_port.Rds"))

# Read NOAA species key
spp_key_noaa <- read.csv("data/landings/noaa/processed/usa_commercial_species_key.csv", as.is=T) %>% 
  mutate(comm_name_upper=toupper(comm_name))


# Step 1. Identify species in data
################################################################################

# Unique species
key1 <- data %>% 
  select(comm_name_orig) %>%
  unique() %>% 
  arrange(comm_name_orig)


# Step 2. Build initial key
################################################################################

# Format common name
# x <- "Anchovy, northern"
format_name <- function(x){
  
  # Comma in name?
  comma_yn <- grepl(",", x)
  if(!comma_yn){
    out_name <- x
  }else{
    name_split <- strsplit(x, split=", ")
    part1 <- stringr::str_to_sentence(name_split[[1]][2]) %>% trimws()
    part2 <- tolower(name_split[[1]][1]) %>% trimws()
    out_name <- paste(part1, part2)
  }
  
  return(out_name)
  
}

# Do Step 2?
step2_yn <- F

if(step2_yn){

  # Build species key
  key2 <- key1 %>% 
    # Add formatted common name
    mutate(comm_name=purrr::map(comm_name_orig, function(x) format_name(x)) %>% unlist() %>% as.character()) %>% 
    # Add scientific name from NOAA
    mutate(comm_name_orig_upper=toupper(comm_name_orig)) %>% 
    left_join(spp_key_noaa %>% select(comm_name_upper, sci_name, level), by=c("comm_name_orig_upper"="comm_name_upper")) %>% 
    arrange(sci_name)
  
  # Export key
  write.csv(key2, file=file.path(sppkeydir, "CA_species_key_incomplete.csv"), row.names=F)

}

# Step 3. Build final key
################################################################################

# Build final key
key3_orig <- readxl::read_excel(file.path(sppkeydir, "CA_species_key_v1.xlsx"))

# Check scientific names: 3 remain unmatched
key3_orig_spp <- key3_orig %>% 
  filter(level=="species") %>% 
  pull(sci_name) %>% unique() %>% sort()
freeR::suggest_names(key3_orig_spp)

# Check scientific names for duplicates
sci_dups <- freeR::which_duplicated(key3_orig$sci_name)
key3_sci_dups <- key3_orig %>% 
  filter(sci_name %in% sci_dups) %>% 
  arrange(sci_name)

# Check coomon names for duplicates
comm_dups <- freeR::which_duplicated(key3_orig$comm_name)
key3_comm_dups <- key3_orig %>% 
  filter(comm_name %in% comm_dups) %>% 
  arrange(comm_name)

# Confirm that "Pacific ocean shrimp" and "Ocean (pink) shrimp" both Pandalus jordani
pac_shrimp_check <- data %>% 
  filter(comm_name_orig %in% c("Shrimp, ocean (pink)", "Shrimp, Pacific Ocean")) %>% 
  select(-c(landings_kg, value_usd)) %>% 
  spread(key=comm_name_orig, value=landings_lb)

# Build taxonomic key
spp_taxa <- freeR::taxa(key3_orig$sci_name)

# Add taxonomy
key3 <- key3_orig %>% 
  left_join(spp_taxa %>% select(-species), by=c("sci_name"="sciname"))

# Get habitat info
hab <- freeR::fishbase(dataset="species", species=key3$sci_name, level="species", cleaned=T, add_taxa = F)

# Export
write.csv(key3, file=file.path(sppkeydir, "CA_species_key_v2_taxa.csv"), row.names=F)






