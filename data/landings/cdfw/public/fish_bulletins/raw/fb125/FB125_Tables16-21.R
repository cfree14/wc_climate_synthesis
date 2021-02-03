

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb125/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb125/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb125/figures"


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
  mutate(year=1962,
         source="FB 125") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=recode(species, 
                        '1 lover sole'='Dover sole', 
                        '1ingeud'='Lingcod', 
                        'A1 bar ore'='Albacore tuna', 
                        'Ahalone'='Abalone', 
                        'Albacore'='Albacore tuna', 
                        'Albacure'='Albacore tuna', 
                        'Albarure'='Albacore tuna', 
                        'Alhacorc'='Albacore tuna', 
                        'Alhacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'Allother'='All other species', 
                        'Bluefiii tuna'='Bluefin tuna', 
                        'Dover solet'='Dover sole', 
                        'Flounder  1'='Flounder', 
                        'Giant m bass'='Giant sea bass', 
                        'Hlucfin tuna'='Bluefin tuna', 
                        'hnglish sole'='English sole', 
                        'Hnglish sole'='English sole', 
                        'iiant Pacific oyster'='Giant Pacific oyster', 
                        'Jark mackerel'='Jack mackerel', 
                        'Knglish sole'='English sole', 
                        'Miscellaneous animal food'='Miscellaneous (animal food)', 
                        'Mltiefin tuna'='Bluefin tuna', 
                        'Oiant Pacific oyster'='Giant Pacific oyster', 
                        'Pacific Imnito'='Pacific bonito', 
                        'Pctrale sole'='Petrale sole', 
                        'Pisnio clam'='Pismo clam', 
                        'Rex sole 1'='Rex sole', 
                        'rllowfin tuna'='Yellowfin tuna', 
                        'Rock fish'='Rockfish', 
                        'Rorkfah'='Rockfish', 
                        'Sablefiah'='Sablefish', 
                        'Salmon 1'='Salmon', 
                        'Skipjack'='Skipjack', 
                        'Skipjack 1'='Skipjack', 
                        'Spinv lobster'='Spiny lobster', 
                        'Spiny lolistcr'='Spiny lobster', 
                        'White scabass'='White seabass', 
                        'White sealwiss'='White seabass', 
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
write.csv(data_full, file=file.path(outdir, "FB125_Tables16-21_1962_landings_by_port.csv"), row.names=F)



