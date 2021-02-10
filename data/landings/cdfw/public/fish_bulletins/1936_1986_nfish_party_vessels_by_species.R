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
## There are 3 year that totals don't match: 
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
         #counting all sequences on non-space characters
         word_count = str_count(species, "\\S+")) %>% 
  separate(species, c("w1", "w2"), remove = F) %>% 
  unite(species_2, w1, w2, sep = ", ") %>% 
  mutate(species = case_when(str_detect(species, "Rock") ~ species,
                             word_count == 2 ~ species_2,
                             T ~ species),
         comm_name_reg = wcfish::convert_names(species, to="regular"),
         comm_name_reg = recode(comm_name_reg,
                                "Baas kdp and sand" = "Kelp/sand bass",
                                'Bass kelp and sand' = "Kelp/sand bass",
                                "Cod lins" = "Lingcod",
                                "Gdfotua barracuda" = "California barracuda",
                                "Ndfie mackerel" = "Pacific mackerel",
                                "Other all" = "All other species",
                                "Others all" = "All other species",
                                "Sheep head California" = "California sheephead",
                                "Tuna blue fin" = "Bluefin tuna",
                                "While seaton" = "White seabass",
                                "Yefiowtail" = "Yellowtail",
                                "Yeliowtnil" = "Yellowtail",
                                'Albacore'='Albacore tuna', 
                                'Albaeorc tuna'='Albacore tuna',
                                'All Other species'='All other species', 
                                'Bluefin tun'='Bluefin tuna', 
                                'California Barracuda'='California barracuda',
                                'California sheepbead'='California sheephead', 
                                'Fish rock'='Rosefish rockfish group', 
                                'Jack mackarel'='Jack mackerel', 
                                'Ocean whitcfisb'='Ocean whitefish', 
                                'Ocean whitefah'='Ocean whitefish',
                                'Pacific bofuto'='Pacific bonito', 
                                'Pacific booito'='Pacific bonito',
                                'Rock fish'='Rockfish',
                                'White seabaas'='White seabass', 
                                'White seabasa'='White seabass'),
         comm_name= wcfish::harmonize_names(comm_name_reg, from="comm", to="comm"),
         comm_name = case_when(is.na(comm_name) ~ comm_name_reg,
                               comm_name_reg == "Rock bass" ~ "Kelp/sand bass",
                               T ~ comm_name)) %>% 
  select(source, table_name, year, comm_name, comm_name_reg, nfish_catch, region, region_type) %>% 
  ## Gets rid of duplicates. When values are provided in two FBs then it keeps just one of them
  distinct(comm_name, year, nfish_catch, region, region_type, .keep_all = T) %>% 
  ##fitering out duplicates that were not removed above
  filter(!(source == "FB102" & year == 1954 & comm_name == "All other species"))

## Inspect
freeR::complete(catch_data) # must all be 0
table(catch_data$source)
range(catch_data$year)
table(catch_data$year)
table(catch_data$region_type)
table(catch_data$region)
str(catch_data)

######################################################################################
## Double check total
double_check_totals <- catch_data %>% 
  group_by(year, source) %>% 
  summarise(total_sum = sum(nfish_catch, na.rm = T)) %>% 
  left_join(totals, by = c("year", "source")) %>% 
  mutate(dif = total_sum - nfish_total) 
## Same discrepancy than above. Note year 1954 sums to the corresponding total considering both FB. FB 129 has more detailed fishe species information.
table(double_check_totals$year)

#####################################################################################

pal_antique <- c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C")

area_cols <- 18
my_colors <- colorRampPalette(brewer.pal(8, "Set2"))(area_cols)
my_colors_2 <- colorRampPalette(brewer.pal(12, "Paired"))(area_cols)
my_colors_3 <- colorRampPalette(pal_antique)(area_cols)

party_vessels_ts <- ggplot(catch_data)+
  geom_bar(aes(x = year, y = nfish_catch/1000000, fill = comm_name), 
           stat = "identity")+
  theme_classic()+
  scale_x_continuous(breaks=seq(1936,1986,5)) +
  scale_fill_manual(values = rev(my_colors))+
  labs(fill = element_text("Fish Species"),
       title = "Party Vessels Catch",
       x= "Year",
       y = expression("Nº of Fish"~(~10^{6})))

