

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb95/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb95/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb95/figures"


# Merge and format data
################################################################################

# Merge data
data_orig <- readxl::read_excel(file.path(indir, "Tables21-27.xlsx"))

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
                             "Table 21"="Eureka",
                             "Table 22"="Sacramento Delta",
                             "Table 23"="San Francisco",
                             "Table 24"="Monterey",
                             "Table 25"="Santa Barbara",
                             "Table 26"="Los Angeles",
                             "Table 27"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1952,
         source="FB 95") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=stringr::str_to_sentence(species),
         species=recode(species, 
                        'A ibacore'='Albacore tuna', 
                        'Aba lone'='Abalone', 
                        'Abalonc'='Abalone', 
                        'Ahnlone'='Abalone', 
                        'Alba core'='Albacore tuna', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'Albacoro'='Albacore tuna', 
                        'Alia lone'='Abalone', 
                        'All other'='All other species', 
                        'Alliacore'='Albacore tuna', 
                        'Allother'='All other species', 
                        'Alltaco re'='Albacore tuna', 
                        'Auiacore'='Albacore tuna', 
                        'Black sea baas'='Black sea bass', 
                        'Bluedn tuna'='Bluefin tuna', 
                        'Clam japanese'='Japanese clam',
                        'Gpinv lobster'='Spiny lobster', 
                        'Hliiefin tuna'='Bluefin tuna', 
                        'Hluefin tuna'='Bluefin tuna', 
                        'Iingcod'='Lingcod', 
                        'Kinafish'='Kingfish', 
                        'Kinefish'='Kingfish', 
                        'Kockfish'='Rockfish', 
                        'Porch'='Perch', 
                        'Rock fish'='Rockfish', 
                        'Rockfisli'='Rockfish', 
                        'Sablcfish'='Sablefish', 
                        'Sanddah'='Sanddab', 
                        'Spiny lolwter'='Spiny lobster', 
                        'Totols'='Totals', 
                        'White scabass'='White seabass', 
                        'White sea bass'='White seabass', 
                        'Ycllowfin tuna'='Yellowfin tuna', 
                        'Yellow fin tuna'='Yellowfin tuna', 
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
write.csv(data_full, file=file.path(outdir, "FB95_Tables21-27_1952_landings_by_port.csv"), row.names=F)



