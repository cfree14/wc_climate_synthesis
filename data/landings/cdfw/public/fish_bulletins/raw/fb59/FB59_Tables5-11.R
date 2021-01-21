

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb59/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb59/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb59/figures"


# Merge and format data
################################################################################

# Merge data
data_orig <- readxl::read_excel(file.path(indir, "Tables5-11.xlsx"))

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
                             "Table 5"="Eureka",
                             "Table 6"="Sacramento Delta",
                             "Table 7"="San Francisco",
                             "Table 8"="Monterey",
                             "Table 9"="Santa Barbara",
                             "Table 10"="Los Angeles",
                             "Table 11"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1941,
         source="FB 59") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=stringr::str_to_sentence(species),
         species=recode(species, 
                        'Albacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'Allother'='All other species', 
                        'Black seabass'='Black sea bass', 
                        'Blucfin tuna'='Bluefin tuna', 
                        'Crah'='Crab', 
                        'Ovster'='Oyster', 
                        'Picific mackerel'='Pacific mackerel', 
                        'Pismo clams'='Pismo clam', 
                        'Rluefin tuna'='Bluefin tuna', 
                        'Roekfish'='Rockfish', 
                        'Ronito tuna'='Bonito tuna', 
                        'Solo'='Sole', 
                        'Spinv liohstcr'='Spiny lobster', 
                        'Ttluefin tuna'='Bluefin tuna', 
                        'Veliowfin tuna'='Yellowfin tuna', 
                        'Yellowfim tuna'='Yellowfin tuna'))

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
write.csv(data_full, file=file.path(outdir, "FB59_Tables5-11_1941_landings_by_port.csv"), row.names=F)



