

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb74/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb74/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb74/figures"


# Merge and format data
################################################################################

# Merge data
data_orig <- readxl::read_excel(file.path(indir, "Tables62-68.xlsx"))

# Format data
data_full <- data_orig %>%
  # Remove the total check row
  filter(species!="Total check") %>% 
  # Format type
  mutate(type="Landings") %>% 
  # Format values/pounds
  mutate(pounds=as.numeric(pounds),
         values= values %>% gsub("\\$", "", .) %>% as.numeric(.)) %>% 
  # Add port complex
  mutate(port_complex=recode(table,
                             "Table 62"="Eureka",
                             "Table 63"="Sacramento Delta",
                             "Table 64"="San Francisco",
                             "Table 65"="Monterey",
                             "Table 66"="Santa Barbara",
                             "Table 67"="Los Angeles",
                             "Table 68"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1947,
         source="FB 74") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=stringr::str_to_sentence(species),
         species=recode(species, 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'Blue fin tuna'='Bluefin tuna', 
                        'Broadbil swordfish'='Broadbill swordfish', 
                        'Rock fish'='Rockfish', 
                        'Roclcfish'='Rockfish', 
                        'Sablcfish'='Sablefish', 
                        'Sand dab'='Sanddab', 
                        'Spinv lobster'='Spiny lobster', 
                        'Spiny loleter'='Spiny lobster', 
                        'White sea bass'='White seabass', 
                        'Yellow tail'='Yellowtail'))

# Inspect data
str(data_full)
freeR::complete(data_full)
table(data_full$table)
table(data_full$port_complex)
table(data_full$port)
table(data_full$type)

# Check common names
names2check <- data_full$species[!grepl("total", tolower(data_full$species))]
wcfish::check_names(names2check)


# Finalize data
################################################################################

# Format data 
data <- data_full %>% 
  filter(!grepl("total", tolower(species)))

# Inspect
freeR::complete(data_full)

# Export data
################################################################################

# Export data
write.csv(data_full, file=file.path(outdir, "FB74_Tables62-68_1947_landings_by_port.csv"), row.names=F)



