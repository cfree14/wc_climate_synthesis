

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb86/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb86/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb86/figures"


# Merge and format data
################################################################################

# Merge data
data_orig <- readxl::read_excel(file.path(indir, "Tables19-25.xlsx"))

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
                             "Table 19"="Eureka",
                             "Table 20"="Sacramento Delta",
                             "Table 21"="San Francisco",
                             "Table 22"="Monterey",
                             "Table 23"="Santa Barbara",
                             "Table 24"="Los Angeles",
                             "Table 25"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1950,
         source="FB 86") %>% 
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
                        'Ah other'='All other species', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'All othr'='All other species', 
                        'Blucfin tuna'='Bluefin tuna', 
                        'Flying fish'='Flyingfish', 
                        'Kockfish'='Rockfish', 
                        'Pisino clam'='Pismo clam', 
                        'Rock fish'='Rockfish', 
                        'Sable fish'='Sablefish', 
                        'Sahlefish'='Sablefish', 
                        'Sand dab'='Sanddab', 
                        'Totata'='Totals', 
                        'White sea bass'='White seabass', 
                        'Yeuowtail'='Yellowtail'))

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
write.csv(data_full, file=file.path(outdir, "FB86_Tables19-25_1950_landings_by_port.csv"), row.names=F)



