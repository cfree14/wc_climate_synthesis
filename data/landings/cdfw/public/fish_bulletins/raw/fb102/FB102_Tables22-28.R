

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb102/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb102/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb102/figures"


# Merge and format data
################################################################################

# Merge data
data_orig <- readxl::read_excel(file.path(indir, "Tables22-28.xlsx"))

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
                             "Table 22"="Eureka",
                             "Table 23"="Sacramento Delta",
                             "Table 24"="San Francisco",
                             "Table 25"="Monterey",
                             "Table 26"="Santa Barbara",
                             "Table 27"="Los Angeles",
                             "Table 28"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1953,
         source="FB 102") %>% 
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
                        'Ail others'='All other species', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'Au other'='All other species', 
                        'Blucfin'='Bluefin tuna', 
                        'Bluefin'='Bluefin tuna', 
                        'Hockfish'='Rockfish', 
                        'Pbmo clam'='Pismo clam', 
                        'Sablcfish'='Sablefish', 
                        'Totab'='Totals', 
                        'White croaker kingfish'='White croaker (kingfish)', 
                        'White sealsass'='White seabass', 
                        'White sealxass'='White seabass', 
                        'Y piiow tail'='Yellowtail', 
                        'Ycllowfin'='Yellowfin tuna', 
                        'Ycllowtail'='Yellowtail', 
                        'Yellowfin'='Yellowfin tuna', 
                        'Yellowtiil'='Yellowtail'))

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
write.csv(data_full, file=file.path(outdir, "FB102_Tables25-31_1953_landings_by_port.csv"), row.names=F)



