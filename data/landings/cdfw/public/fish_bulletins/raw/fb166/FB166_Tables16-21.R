


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb166/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb166/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb166/figures"


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
  mutate(year=1974,
         source="FB 166") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=recode(species, 
                        'A11 other species'='All other species', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'Alhacore'='Albacore tuna', 
                        'Allother species'='All other species', 
                        'Black alialonc'='Black abalone', 
                        'Blucfin tuna'='Bluefin tuna', 
                        'Dungcncss crab'='Dungeness crab', 
                        'Dungrncss crab'='Dungeness crab', 
                        'Hlucfui tuna'='Bluefin tuna', 
                        'Hock crab'='Rock crab', 
                        'Hock fish'='Rockfish', 
                        'Jingcod'='Lingcod', 
                        'Kex sole'='Rex sole', 
                        'Kockfish'='Rockfish', 
                        'Kook fish'='Rockfish', 
                        'Opal eye'='Opaleye', 
                        'Pacific butter fish'='Pacific butterfish', 
                        'Pacific Ocean shrimp'='Pacific ocean shrimp',
                        'Pctralc sole'='Petrale sole', 
                        'Pet rale sole'='Petrale sole', 
                        'Pink aba lone'='Pink abalone', 
                        'Pink abalonc'='Pink abalone', 
                        'Quccnfish'='Queenfish', 
                        'Red abaloue'='Red abalone', 
                        'Rock fish'='Rockfish', 
                        'Sabir fish'='Sablefish', 
                        'Sable fish'='Sablefish', 
                        'Sanddah'='Sanddab', 
                        'Threaded abalonc'='Threaded abalone', 
                        'White scabass'='White seabass', 
                        'Yeiiowtail'='Yellowtail', 
                        'Yellow tail'='Yellowtail', 
                        'Yellowfm tuna'='Yellowfin tuna'))

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
write.csv(data_full, file=file.path(outdir, "FB166_Tables16-21_1974_landings_by_port.csv"), row.names=F)



