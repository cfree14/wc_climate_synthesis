


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb170/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb170/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb170/figures"


# Merge and format data
################################################################################

# Files to merge
files2merge <- paste0("Table", 16:21, ".xlsx")

# Merge data
data_orig <- purrr::map_df(files2merge, function(x){
  df <- readxl::read_excel(file.path(indir, x)) %>% 
    mutate_all(as.character) %>%
    mutate(table=x)
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
  mutate(port=stringr::str_to_title(port)) %>% 
  # Add columns
  mutate(year=1976,
         source="FB 170") %>% 
  # Arrange
  select(source, table, year, port_complex, port, type, species, pounds, values, everything()) %>% 
  # Rename columns
  rename(landings_lb=pounds, value_usd=values) %>% 
  # Format species
  mutate(species=gsub("[[:punct:]]", "", species),
         species=recode(species,
                       "Albacorc"="Albacore tuna",
                       'Albacore'="Albacore tuna",
                       "Bed abalone"="Red abalone",
                       "Bigeve tuna"="Bigeye tuna",
                       "Blucfin tuna"="Bluefin tuna",
                       "Creen abalone"="Green abalone",
                       "Crouper"="Grouper",
                       "Cunt sea bass"="Giant sea bass",
                       "Hyingfish"="Flyingfish",
                       "iAUoSJer species"="All other species",
                       "lack mackerel"="Jack mackerel",
                       "lark mackerel"="Jack mackerel",
                       "Pacific bomto"="Pacific bonito",
                       "Periale sole"="Petrale sole",
                       "PetTale sole"="Petrale sole",
                       "Pink abalonc"="Pink abalone",
                       "Rocknsh"="Rockfish",
                       "Roekfish"="Rockfish",
                       "Rockflsh"="Rockfish",
                       "Sablcfish"="Sablefish",
                       "Sableftsh"="Sablefish",
                       "Sandab"="Sanddab",
                       "Sworafish"="Swordfish",
                       "True melt"="True smelt",
                       "Wanoo"="Wahoo", 
                       "White seabaat"="White seabass",
                       "White seabxss"="White seabass",
                       "White scabass"="White seabass",
                       "YeUowtail"="Yellowtail",
                       "Port total"="Port totals"),
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


# QA/QC data
################################################################################

# Extract data
data <- data_full %>% 
  filter(!grepl("total", tolower(species)))

# Calculate observed totals
tot_obs <- data %>% 
  group_by(year, table, port_complex, port, type) %>% 
  summarize(landings_tot_obs=sum(landings_lb),
            value_tot_obs=sum(value_usd))

# Extract reported totals
tot_rep <- data_full %>% 
  filter(grepl("total", tolower(species)) & species!="Total check" & !grepl("area", tolower(species))) %>% 
  select(year, table, port_complex, port, type, species, landings_lb, value_usd) %>% 
  rename(landings_tot_rep=landings_lb, value_tot_rep=value_usd)

# Compare totals
tot_check <- tot_rep %>% 
  left_join(tot_obs) %>% 
  mutate(landings_tot_diff=landings_tot_rep-landings_tot_obs,
         value_tot_diff=value_tot_rep-value_tot_obs)


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
write.csv(data_full, file=file.path(outdir, "FB170_Tables16-21_1976_landings_by_port.csv"), row.names=F)



