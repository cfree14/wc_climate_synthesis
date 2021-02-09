##Cleans and wrangles Party vessels catch data from California historic Fish Bulletins

################################################################################
# Clear workspace
rm(list = ls())

################################################################################
# Setup

# Packages
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/fish_bulletins/processed"
indir <- "data/landings/cdfw/public/fish_bulletins/raw/"

################################################################################

##Information to import all necesary tables
fbs <- c(95, 102, 129, 161, 163, 166, 168, 170, 173)

table_name <- c("Table28", "Table47", "Table23", "Table22", "Table22", "Table22", "Table22", "Table22", "Table14")

fb_table_key <- tibble(fbs, table_name) %>% 
  mutate(source = paste0("FB", fbs), 
         path = paste0("fb", fbs, "/", "raw", "/", table_name, ".xlsx"))

# Importing and Mergeing

data_orig <- purrr::map_df(fb_table_key$path, function(x){ 
  
  # Read data
  fdata <- readxl::read_excel(paste0(indir, x))
  
  # Format data
  fdata1 <- fdata %>% 
    # Convert to character
    mutate_all(as.character) %>% 
    # pivot_longer
    pivot_longer(2:ncol(.),
                 names_to = "year",
                 values_to = "nfish_catch") %>%
    # Rename
    setNames(c("species", "year", "nfish_catch")) %>% 
    # Add source and seasons
    mutate(region_type = "state",
           region = "Satewide",
           path = x,
           year = as.numeric(year)) %>% 
    left_join(fb_table_key, by = "path") %>% 
    select(-path, -fbs)
  
  # Return
  fdata1
  
})

###########################################################################
## Check totals

totals <- data_orig %>% 
  filter(str_detect(species, "fish"),
         str_detect(species, "Total")) %>% 
  mutate(nfish_catch = as.numeric(nfish_catch)) %>% 
  rename(nfish_total = nfish_catch)

totals_sum <- data_orig %>% 
  filter(!str_detect(species, "Total"),
         !str_detect(species, "angler")) %>% 
  mutate(nfish_catch = as.numeric(nfish_catch)) %>% 
  group_by(year, source) %>% 
  summarise(total_sum = sum(nfish_catch, na.rm = T)) %>% 
  left_join(totals, by = c("year", "source")) %>% 
  mutate(dif = total_sum - nfish_total)
## Theara are 3 year that totals don't match: 
## 1977 difference = -360
## 1979 difference = -9
## 1980 difference = 10000

###############################################################################

## Note FB95 Table28, Rock bass included two species of kelp bass Paralabrax clathratus and sand bass P.nebulifer
catch_data <- data_orig %>% 
  filter(!str_detect(species, "angler"),
         !str_detect(species, "Total")) %>% 
  mutate(nfish_catch = as.numeric(nfish_catch)) %>% 
  # Format species
  mutate(species = gsub("\\.|\\_|\\_|\\-|\\,|\\’", "", species) %>% stringr::str_trim(.),
         species = str_to_sentence(species),
         species = recode(species,
                "Baas kdp and sand" = "Bass kelp and sand",
                "All other" = "All others",
                "Barracuda gdfotua" = "Barracuda california",
                "Bofuto pacific" = "Bonito pacific",
                "Booito acific" = "Bonito pacific",
                "Booito pacific" = "Bonito pacific",
                "Lins cod" = "Lingcod",
                "Mackarel jack" = "Mackerel jack",
                "Mackerel ndfie" = "Mackerel pacific",
                "Rock fish" = "Rockfish",
                "Seabaas white" = "Seabass white",      
                "Seabasa white" = "Seabass white",
                "Seabass white——" = "Seabass white",
                "Seaton while" = "Seabass white",
                "Sheep head california" = "Sheephead california",
                "Sheepbead california" = "Sheephead california",
                "Tun bluefin" = "Tuna bluefin",
                "Tuna blue fin" = "Tuna bluefin",
                "Tuna albaeorc" = "Tuna albacore",
                "Whitcfisb ocean" = "Whitefish ocean",
                "Whitefah ocean" = "Whitefish ocean",
                "Yefiowtail" = "Yellowtail",
                "Yeliowtnil" = "Yellowtail",
                "Yellowtail california" = "Yellowtail")) %>% 
  ## Gets rid of duplicates. When values are provided in two FBs then it keeps just one of them
  distinct(species, nfish_catch, region, region_type, .keep_all = T)




