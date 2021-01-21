

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb105/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb105/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb105/figures"


# Merge and format data
################################################################################

# Merge data
data_orig <- readxl::read_excel(file.path(indir, "Tables43-49.xlsx"))

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
                             "Table 43"="Eureka",
                             "Table 44"="Sacramento Delta",
                             "Table 45"="San Francisco",
                             "Table 46"="Monterey",
                             "Table 47"="Santa Barbara",
                             "Table 48"="Los Angeles",
                             "Table 49"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1956,
         source="FB 105") %>% 
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
                        'All othfr'='All other species', 
                        'Alnlonc'='Abalone', 
                        'Black sea lass'='Black sea bass', 
                        'Blucfin tuna'='Bluefin tuna', 
                        'California poinpno'='California pompano', 
                        'Giant pacific oyster'='Giant Pacific oyster', 
                        'Hite scalioss'='White seabass', 
                        'Hite seabass'='White seabass', 
                        'Iacific mackerel'='Pacific mackerel', 
                        'Iingcod'='Lingcod', 
                        'Irouper'='Grouper', 
                        'Ivific mackerel'='Pacific mackerel', 
                        'Jrouper'='Grouper', 
                        'Kockfish'='Rockfish', 
                        'Liant pacific oyster'='Giant Pacific oyster', 
                        'N hite croaker kingfish'='White croaker (kingfish)', 
                        'Petralc sole'='Petrale sole', 
                        'Petrale solo'='Petrale sole', 
                        'Petralo sole'='Petrale sole', 
                        'Rant pacific oyster'='Giant Pacific oyster', 
                        'Roekfish'='Rockfish', 
                        'Sablcfish'='Sablefish', 
                        'Saiuldab'='Sanddab', 
                        'White croaker kingfish'='White croaker (kingfish)', 
                        'Y ellowtail'='Yellowtail', 
                        'Ycllowfin tuna'='Yellowfin tuna', 
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
write.csv(data_full, file=file.path(outdir, "FB105_Tables43-49_1956_landings_by_port.csv"), row.names=F)



