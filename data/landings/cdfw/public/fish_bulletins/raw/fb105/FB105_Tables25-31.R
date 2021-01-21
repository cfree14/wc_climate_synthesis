

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
data_orig <- readxl::read_excel(file.path(indir, "Tables25-31.xlsx"))

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
                             "Table 25"="Eureka",
                             "Table 26"="Sacramento Delta",
                             "Table 27"="San Francisco",
                             "Table 28"="Monterey",
                             "Table 29"="Santa Barbara",
                             "Table 30"="Los Angeles",
                             "Table 31"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1955,
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
                        'Abalono'='Abalone', 
                        'Ahalone'='Abalone', 
                        'Albacore'='Albacore tuna', 
                        'Alhacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'Alliacore'='Albacore tuna', 
                        'Bieye tuna'='Bigeye tuna', 
                        'Blucfiu tuna'='Bluefin tuna', 
                        'Dlucfin tuna'='Bluefin tuna', 
                        'Giant pacific oyster'='Giant Pacific oyster', 
                        'Hex sole'='Rex sole', 
                        'Hluefin tuna'='Bluefin tuna', 
                        'Hock fish'='Rockfish', 
                        'Iingcnd'='Lingcod', 
                        'Iingrod'='Lingcod', 
                        'Itockfish'='Rockfish', 
                        'Knglish sole'='English sole', 
                        'Kockfish'='Rockfish', 
                        'Kook fish'='Rockfish', 
                        'Lingrod'='Lingcod', 
                        'Llalfmoon'='Halfmoon', 
                        'Pctrale sole'='Petrale sole', 
                        'Petralesole'='Petrale sole', 
                        'Sablcfah'='Sablefish', 
                        'Sanddah'='Sanddab', 
                        'Seulpin'='Sculpin', 
                        'White croaker kingfish'='White croaker (kingfish)', 
                        'White croaker kinkfish'='White croaker (kingfish)', 
                        'White scabass'='White seabass', 
                        'White sea bass'='White seabass', 
                        'Ycllowfin tuna'='Yellowfin tuna'))

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
write.csv(data_full, file=file.path(outdir, "FB105_Tables25-31_1955_landings_by_port.csv"), row.names=F)



