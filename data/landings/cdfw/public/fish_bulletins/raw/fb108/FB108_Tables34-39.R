

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb108/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb108/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb108/figures"


# Merge and format data
################################################################################

# Files to merge
files2merge <- paste0("Table", 34:39, ".xlsx")

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
                             "Table 34"="Eureka",
                             "Table 35"="San Francisco",
                             "Table 36"="Monterey",
                             "Table 37"="Santa Barbara",
                             "Table 38"="Los Angeles",
                             "Table 39"="San Diego")) %>% 
  mutate(port=port %>% gsub("[[:punct:]]", "", .) %>% stringr::str_trim(.) %>% stringr::str_to_title(.)) %>% 
  # Add columns
  mutate(year=1958,
         source="FB 108") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=stringr::str_trim(species),
         species=recode(species, 
                        'Abalonc'='Abalone', 
                        'Albacorc'='Albacore tuna', 
                        'Albacore'='Albacore tuna', 
                        'All other'='All other species', 
                        'Allother'='All other species', 
                        'Bigeyc tuna'='Bigeye tuna', 
                        'Caligorna halibut'='California halibut', 
                        'Ciant IVific oyster'='Giant Pacific oyster', 
                        'Hex sole'='Rex sole', 
                        'iant Pacific oyster'='Giant Pacific oyster', 
                        'Iingcod'='Lingcod', 
                        'Jiant Pacific oyster'='Giant Pacific oyster', 
                        'Kockfish'='Rockfish', 
                        'lingcod'='Lingcod', 
                        'lingeod'='Lingcod', 
                        'lingrod'='Lingcod', 
                        'OCean shrimp'='Ocean shrimp', 
                        'Packific mackerel'='Pacific mackerel', 
                        'Rock fish'='Rockfish', 
                        'Sable fish'='Sablefish', 
                        'Spinv lolnster'='Spiny lobster', 
                        'Totab'='Totals', 
                        'White scabiss'='White seabass', 
                        'White sea bass'='White seabass', 
                        'White seabadd'='White seabass', 
                        'Yellnwfin tuna'='Yellowfin tuna'))

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
write.csv(data_full, file=file.path(outdir, "FB108_Tables34-39_1958_landings_by_port.csv"), row.names=F)



