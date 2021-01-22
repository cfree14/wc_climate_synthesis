


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb168/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb168/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb168/figures"


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
    col_names <- c("port", "species", "pounds", "values")
  }
  if(ncol(df1)==5){
    col_names <- c("port", "type", "species", "pounds", "values")
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
  mutate(type=ifelse(is.na(type), "Landings", type)) %>% 
  # Format values/pounds
  mutate(pounds=as.numeric(pounds),
         values=as.numeric(values)) %>% 
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
  mutate(year=1975,
         source="FB 168") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species) %>% stringr::str_trim(),
         species=recode(species,
                        "Albacore"="Albacore tuna",
                        "All ot her species"="All other species",
                        "Barracuda California"="California barracuda",
                        "Bay snnmp"="Bay shrimp",
                        "Bed abalone"="Red abalone",
                        "Blucfin tuna"="Bluefin tuna",
                        "California spins lobster"="California spiny lobster",
                        "Ciant sea bass"="Giant sea bass",
                        "Dungrnra crab"="Dungeness crab",
                        "English sol"="English sole", 
                        "Knglish sole"="English sole",
                        "FI yin gfish"="Flyingfish",
                        "Giant Pacific ovster"="Giant Pacific oyster",
                        "Hock fish"="Rockfish",
                        "Longiaw mudsucker"="Longjaw mudsucker",
                        "Prtrale sole"="Petrale sole",
                        "Re sole"="Rex sole",
                        "Rounder"="Flounder",
                        "Whi te sea bass"="White seabass",
                        "White sea bass"="White seabass",
                        "White seabaas"="White seabass",
                        "Tu r bot"="Turbot",
                        "Squid market"="Market squid",
                        "Sablefbh"="Sablefish",
                        "Rea abalone"="Red abalone",
                        "Pctrale vile"="Petrale sole",
                        "Pacific hemng"="Pacific herring",
                        "Miscellaneous sole a"="Miscellaneous sole"),
         species=stringr::str_trim(species))

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

# Inspect species
spp_key <- data_full %>% 
  select(species) %>% 
  unique() %>% 
  arrange(species)

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
write.csv(data_full, file=file.path(outdir, "FB168_Tables16-21_1975_landings_by_port.csv"), row.names=F)



