
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/calcom/data/raw/landings"
outputdir <- "data/calcom/data/processed"

# Read keys
# The common and scientific names are not formatted perfectly but they are close
# (and slightly better than what CALCOM provides)
spp_key <- read.csv(file.path(outputdir, "species_key.csv"))
  

# Build data
################################################################################

# Files to merge
files2merge <- list.files(datadir)

# Merge data
data_orig <- purrr::map_df(files2merge, function(x){
  # Read data
  fdata <- read.csv(file.path(datadir, x))
})

# Format data
data <- data_orig %>% 
  # Format source
  mutate(source=stringr::str_to_title(source)) %>% 
  # Format port complex
  mutate(port_complex=recode(port_complex_code, 
                             "BDG"="Bodega Bay",
                             "BRG"="Fort Bragg",
                             "CRS"="Crescent City",
                             "ERK"="Eureka",
                             "MNT"="Monterey",
                             "MRO"="Morro Bay",
                             "OCA"="Other?",
                             "OLA"="Los Angeles", 
                             "OSB"="Santa Barbara",
                             "OSD"="San Diego",
                             "OSF"="San Francisco")) %>% 
  # Format gear 
  mutate(gear=recode(gear_code, 
                     "FPT"="Traps",
                     "HKL"="Hook and line",
                     "NET"="Net",
                     "OTH"="Other",
                     "TWL"="Trawl",
                     "UNK"="Unknown")) %>%
  # Add species
  left_join(spp_key) %>% 
  # Arrange
  select(market_group, species_code, common_name, scientific_name, 
         port_complex_code, port_complex, gear_code, gear, 
         year, source, live_yn, landings_lb, everything()) %>% 
  arrange(species_code, port_complex_code, port_complex, gear_code, gear, year)
  
# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$species_code)
table(data$port_complex_code)
table(data$port_complex)
table(data$gear_code)
table(data$gear)
table(data$source)
table(data$live_yn)

# Species included
sort(unique(data$common_name))

# Build data
################################################################################

# Export data
write.csv(data, file=file.path(outputdir, "CALCOM_1978_2019_groundfish_landings_by_port_complex_gear_species.csv"), row.names=F)





