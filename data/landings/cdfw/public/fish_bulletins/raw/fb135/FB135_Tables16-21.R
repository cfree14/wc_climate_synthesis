

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb135/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb135/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb135/figures"


# Merge and format data
################################################################################

# Files to merge
files2merge <- paste0("Table", 16:21, ".xlsx")

# Merge data
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  df1 <- readxl::read_excel(file.path(indir, x)) %>% 
    mutate_all(as.character) 
  
  # Set column names
  if(ncol(df1)==4){
    col_names <- c("port", "species", "values", "pounds")
  }
  if(ncol(df1)==5){
    col_names <- c("port", "type", "species", "values", "pounds")
  }
  
  # Format data
  df2 <- df1 %>%
    setNames(col_names) %>% 
    mutate(table=x) %>% 
    select(table, everything())
  
})

# Format data
data_full <- data_orig %>%
  # Remove the total check row
  filter(species!="Total check") %>% 
  # Format type
  mutate(type="Landings") %>% 
  # Format values/pounds
  mutate(pounds=as.numeric(pounds),
         values= values %>% gsub("\\$", "", .) %>% as.numeric(.)) %>% 
  # Format table
  mutate(table=gsub(".xlsx", "", table) %>% gsub("Table", "Table ", .)) %>% 
  # Add port complex
  mutate(port_complex=recode(table,
                             "Table 16"="Eureka",
                             "Table 17"="San Francisco",
                             "Table 18"="Monterey",
                             "Table 19"="Santa Barbara",
                             "Table 20"="Los Angeles",
                             "Table 21"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1965,
         source="FB 135") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=recode(species, 
                        'Abalonc'='Abalone', 
                        'Abalotic'='Abalone', 
                        'Alba core'='Albacore tuna', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'alifornia halibut'='California halibut', 
                        'All other siecies'='All other species', 
                        'All other sitccics'='All other species', 
                        'All other sjiecies'='All other species', 
                        'cllowfin tuna'='Yellowfin tuna', 
                        'fiiant Pacific oyster'='Giant Pacific oyster', 
                        'Grouiter'='Grouper', 
                        'Hex sole'='Rex sole', 
                        'hi to sea bass'='White seabass', 
                        'Hluefin tuna'='Bluefin tuna', 
                        'Hock fish'='Rockfish', 
                        'Inglish sole'='English sole', 
                        'Itock crab'='Rock crab', 
                        'Jack mackprel'='Jack mackerel', 
                        'Klyinjtfish'='Flyingfish', 
                        'Knglish sole'='English sole', 
                        'Kock crab'='Rock crab', 
                        'Kockfish'='Rockfish', 
                        'Koek crab'='Rock crab', 
                        'LiiiKCod'='Lingcod', 
                        'Market crav'='Market crab', 
                        'Miscellaneous animal food'='Miscellaneous (animal food)', 
                        'Pacific pompauo'='Butterfish (Pacific pompano)', 
                        'Pctrale sole'='Petrale sole', 
                        'Petralc sole'='Petrale sole', 
                        'Petralesole'='Petrale sole', 
                        'RiiKlish sole'='English sole', 
                        'Rnslish sole'='English sole', 
                        'Rock fish'='Rockfish', 
                        'Sablcfish'='Sablefish', 
                        'Sablifish'='Sablefish', 
                        'Sdmon'='Salmon', 
                        'Siuid'='Squid', 
                        'Sjlmon'='Salmon', 
                        'Soul pin'='Sculpin', 
                        'Spiny lobster'='California spiny lobster', 
                        'Spiny lolister'='California spiny lobster', 
                        'Spiny loljester'='California spiny lobster', 
                        'Spiny lolster'='California spiny lobster', 
                        'Spiny lolwter'='California spiny lobster', 
                        'Spiny lolxstor'='California spiny lobster', 
                        'Suid'='Squid', 
                        'V hitc soabass'='White seabass', 
                        'V kite seabass'='White seabass', 
                        'W kite sea bass'='White seabass', 
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
write.csv(data_full, file=file.path(outdir, "FB135_Tables16-21_1965_landings_by_port.csv"), row.names=F)



