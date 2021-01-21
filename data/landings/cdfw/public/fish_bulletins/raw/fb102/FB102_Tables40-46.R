

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
data_orig <- readxl::read_excel(file.path(indir, "Tables40-46.xlsx"))

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
                             "Table 40"="Eureka",
                             "Table 41"="Sacramento Delta",
                             "Table 42"="San Francisco",
                             "Table 43"="Monterey",
                             "Table 44"="Santa Barbara",
                             "Table 45"="Los Angeles",
                             "Table 46"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1954,
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
                        'Ail other'='All other species', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'AU other'='All other species', 
                        'Bigcye tuna'='Bigeye tuna', 
                        'BiReye tuna'='Bigeye tuna', 
                        'Black sea Isass'='Black sea bass', 
                        'Rock fish'='Rockfish', 
                        'Sable fish'='Sablefish', 
                        'Spiny loteter'='Spiny lobster', 
                        'Totab'='Totals', 
                        'White croaker Kingfish'='White croaker (kingfish)', 
                        'White sea bass'='White seabass', 
                        'Yellovrfin tuna'='Yellowfin tuna', 
                        'YeUowfin tuna'='Yellowfin tuna',
                        'Au other'='All other species', 
                        'Bireye tuna'='Bigeye tuna', 
                        'Black sea isass'='Black sea bass', 
                        'White croaker kingfish'='White croaker (kingfish)', 
                        'Yeuowfin tuna'='Yellowfin tuna'))

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
write.csv(data_full, file=file.path(outdir, "FB102_Tables40-46_1954_landings_by_port.csv"), row.names=F)



