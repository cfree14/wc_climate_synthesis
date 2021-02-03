

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb129/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb129/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb129/figures"


# Merge and format data
################################################################################

# Files to merge
files2merge <- paste0("Table", 17:22, ".xlsx")

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
                             "Table 17"="Eureka",
                             "Table 18"="San Francisco",
                             "Table 19"="Monterey",
                             "Table 20"="Santa Barbara",
                             "Table 21"="Los Angeles",
                             "Table 22"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1963,
         source="FB 129") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=recode(species, 
                        'AH species'='All species', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'Albaeore'='Albacore tuna', 
                        'Alhacore'='Albacore tuna', 
                        'Bieeye tuna'='Bigeye tuna', 
                        'Blucfm tuna'='Bluefin tuna', 
                        'Blur fin tuna'='Bluefin tuna', 
                        'hite seabass'='White seabass', 
                        'Iingcod'='Lingcod', 
                        'Miscellaneous animal food'='Miscellaneous (animal food)', 
                        'Pctralc sole'='Petrale sole', 
                        'Rock fish'='Rockfish', 
                        'Stablefish'='Sablefish', 
                        'White sealsiss'='White seabass'))

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
write.csv(data_full, file=file.path(outdir, "FB129_Tables17-22_1963_landings_by_port.csv"), row.names=F)



