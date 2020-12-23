

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/oceanadapt/raw"
outdir <- "data/oceanadapt/processed"
plotdir <- "data/oceanadapt/figures"


# Build data
################################################################################

# Files to merge
files2merge <- list.files(indir)

# Merge files
data_orig <- purrr:::map_df(files2merge, function(x){
  fdata <- read.csv(file.path(indir, x), as.is=T)
})

# Build data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(species_orig=species,
         lat_dd=latitude,
         lat_dd_se=latitude_std_err,
         long_dd=longitude,
         long_dd_se=longitude_std_err,
         depth_m=depth,
         depth_m_se=depth_std_err) %>% 
  # Add survey
  mutate(survey=ifelse(region=="West Coast Annual", "Annual survey", 
                       ifelse(region=="West Coast Triennial", "Triennial survey", "Alaska survey"))) %>% 
  # Format region
  mutate(region=ifelse(grepl("West Coast", region), "West Coast", region)) %>% 
  # Arrange
  select(region, species_orig, survey, year, everything()) %>% 
  arrange(region, species_orig, survey, year)

# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$region)
table(data$survey)

# Look for duplicated
check <- data %>% 
  group_by(region, species_orig, survey, year) %>% 
  summarize(n=n()) %>% 
  filter(n>1)

# Handle these duplicates
data1 <- data %>% 
  group_by(region, species_orig, survey, year) %>% 
  summarize_all(mean, na.rm=T)

# Means
data1_avgs <- data1 %>% 
  select(region, species_orig, survey, year, lat_dd, long_dd, depth_m) %>% 
  gather(key="metric", value="mean", 5:ncol(.))

# SDs
data1_sds <- data1 %>% 
  select(region, species_orig, survey, year, lat_dd_se, long_dd_se, depth_m_se) %>% 
  gather(key="metric", value="std_err", 5:ncol(.)) %>% 
  mutate(metric=recode(metric, 
                      "lat_dd_se"="lat_dd",
                      "long_dd_se"="long_dd",
                      "depth_m_se"="depth_m"))

# Merge
data2 <- data1_avgs %>% 
  left_join(data1_sds) %>% 
  mutate(metric=recode_factor(metric, 
                              "lat_dd"="Latitude (°N)",
                              "long_dd"="Longitude (°W)",
                              "depth_m"="Depth (m)"))

# Correct species
################################################################################

# Fix species names
data3 <- data2 %>% 
  mutate(species=recode(species_orig, 
                        "Allocentrotus fragilis"="Strongylocentrotus fragilis",
                        "Astropecten verilli"="Astropecten verrilli",
                        "Buccinum polare"="Buccinum polaris",
                        "Chrysaora fuscens"="Chrysaora fuscescens",
                        "Ciliatoclinocardium ciliatum"="Ciliatocardium ciliatum",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Colus halli"="Colus hallii",
                        "Coryphaenoides pectoralis"="Albatrossia pectoralis",
                        "Euspira lewisii"="Neverita lewisii",
                        "Loligo opalescens"="Doryteuthis opalescens",
                        "Lunatia pallida"="Euspira pallida",
                        "Parastichopus californicus"="Apostichopus californicus",
                        "Parastichopus leukothele"="Parastichopus leucothea",
                        "Plicifusus kroeyeri" ="Plicifusus kroyeri",
                        "Raja binoculata"="Beringraja binoculata",
                        "Raja inornata"="Beringraja inornata",
                        "Raja rhina"="Beringraja rhina",
                        "Raja stellulata"="Beringraja stellulata",
                        "Rhamphocottus richardsoni"="Rhamphocottus richardsonii",
                        "Torpedo californica"="Tetronarce californica")) %>% 
  mutate(species=ifelse(freeR::nwords(species)==1, paste(species, "spp."), species),
         species=gsub("sp\\.", "spp.", species)) %>% 
  select(species_orig, species, everything()) %>% 
  # Remove meaningless species
  filter(!species %in% c("Unsorted shab", "Red striated", "Purple striated anemone"))

# Check scientific names
species <- sort(unique(data3$species))
species2check <- species[!grepl("spp.", species)]
freeR::suggest_names(species2check)

# Crangon communis - appears correct
# Crossaster borealis - appears correct
# Grandicrepidula grandis - appears correct
# Neptunea borealis - appears correct
# Ophiura sarsii - appears correct
# Parastichopus leucothea - no guidance
# Plicifusus kroyeri - appears correct
# Stegophiura ponderosa - appears correct
# Tochuina gigantea - appears correct

# Export data
################################################################################

# Export data
saveRDS(data3, file=file.path(outdir, "OA_1977_2018_distribution_shifts.Rds"))
