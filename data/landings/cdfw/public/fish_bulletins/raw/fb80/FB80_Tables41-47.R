

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb80/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb80/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb80/figures"


# Merge and format data
################################################################################

# Merge data
data_orig <- readxl::read_excel(file.path(indir, "Tables41-47.xlsx"))

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
                             "Table 41"="Eureka",
                             "Table 42"="Sacramento Delta",
                             "Table 43"="San Francisco",
                             "Table 44"="Monterey",
                             "Table 45"="Santa Barbara",
                             "Table 46"="Los Angeles",
                             "Table 47"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1949,
         source="FB 80") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=stringr::str_to_sentence(species),
         species=recode(species, 
                        'Abalonc'='Abalone', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'Alhacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'Au other'='All other species', 
                        'Rluefin tuna'='Bluefin tuna', 
                        'Rock fish'='Rockfish', 
                        'Rockfvsh'='Rockfish', 
                        'Rroadbill swordfish'='Broadbill swordfish', 
                        'Sable fish'='Sablefish', 
                        'Sand dab'='Sanddab', 
                        'White sea bass'='White seabass'))

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
write.csv(data_full, file=file.path(outdir, "FB80_Tables41-47_1947_landings_by_port.csv"), row.names=F)



