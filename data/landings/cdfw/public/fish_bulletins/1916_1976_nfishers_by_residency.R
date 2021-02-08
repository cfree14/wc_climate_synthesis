##Cleans and wrangles commercial fishers data from California historic Fish Bulletins

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
# Read and merge tables of older issues

##Historic data only by season (1916-1936)
nfishers_16_35 <- readxl::read_excel(paste0(indir,"fb49/raw/Table144b.xlsx")) %>% 
  rename(season = "License Year", nfishers = "No. of Fishermen") %>% 
  mutate(season = recode(season,
                         "1924—25" = "1924-25",
                         "1934—35" = "1934-35"),
         source = "FB 49",
         table_name = "Table144",
         region_type = "state",
         region = "Statewide",
         year = str_extract(season, "\\d{4}") %>% as.numeric(.))

# Inspect
str(nfishers_16_35)
freeR::complete(nfishers_16_35)
table(nfishers_16_35$season)


## Mising data years 1936, 1937, 1938
nfishers_36_38_raw <- readxl::read_excel(paste0(indir, "fb57/raw/Table4.xlsx")) %>% 
  rename(nat = "Country of birth")

totals_nfisher_36_38 <- nfishers_36_38_raw %>% 
  filter(nat == "Total") %>% 
  rename(nfishers_total = nfishers)

nfishers_36_38 <- nfishers_36_38_raw %>% 
  filter(!str_detect(nat, "Total")) %>% 
  group_by(season) %>% 
  summarise(nfishers = sum(nfishers)) %>% 
  ungroup()

total_check_36_38 <- nfishers_36_38 %>% 
  left_join(totals_nfisher_36_38, by = "season") %>% 
  mutate(dif = nfishers - nfishers_total) ## All 0! Check ok

nfishers_36_38 <- nfishers_36_38 %>% 
  mutate(source = "FB 57",
         table_name = "Table4",
         year = str_extract(season, "\\d{4}") %>% as.numeric(.),
         region = "Statewide",
         region_type = "state")


## Set 1: Data by nationality and area of residence. Years: 1935-1955
fbs_1 <- c(49, 57, 58, 59, 59, 63, 63, 67, 67, 74)

table_name <- c("Table144", "Table3", "Table11", "Table16", "Table17", "Table23", "Table24", "Table23", "Table24", "Table36")

season <- c("1935-36", "1939-40", "1940- 41", "1941-42", "1942-43", "1943-44", "1944-45", "1945-46", "1946-47", "1947-48")


##Data frame with path to read each table and corresponding season
fb_table_key <- tibble(fbs_1, table_name, season) %>% 
  mutate(source = paste("FB", fbs_1), 
         path = paste0("fb", fbs_1, "/", "raw", "/", table_name, ".xlsx"))

# Mergeing

data_orig_set1 <- purrr::map_df(fb_table_key$path, function(x){
  
# Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", x)
  fdata <- readxl::read_excel(file.path(indir))
 
# Format data
  fdata1 <- fdata %>% 
    # Harmonize names
    setNames(c("region", "nationality", "nfishers", "total_nfisher")) %>% 
    # Add source and seasons
    mutate(region_type = "area of residence",
           path = x) %>% 
    # Convert to character
    mutate_all(as.character) %>% 
    left_join(fb_table_key, by = "path") %>% 
    select(-path, -fbs_1)
  
  # Return
  fdata1

})


## Set 2: Data by port and season
fbs_2 <- c(80, 86, 89, 95, 102, 105, 108, 111, 117, 121, 125, 129, 132, 135, 138, 144, 149, 153, 154, 159, 161, 163, 166, 168, 170)

table_name_set2 <- c("Table4", "Table5", "Table11", "Table7", "Table7", "Table10", "Table4", "Table4", "Table4", "Table4", "Table4", "Table5", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4", "Table4")

fb_table_key_set2 <- tibble(fbs_2, table_name_set2) %>% 
  mutate(source = paste("FB", fbs_2), 
         path = paste0("fb", fbs_2, "/", "raw", "/", table_name_set2, ".xlsx"))


data_orig_set2 <- purrr::map_df(fb_table_key_set2$path, function(x){
  
  # Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", x)
  fdata <- readxl::read_excel(file.path(indir))
  
  # Format
  ncols <- ncol(fdata)
  
  # Format
  ncols <- ncol(fdata)
 
    fdata1 <- fdata %>% 
      # pivot_longer
      pivot_longer(2:ncol(.),
                   names_to = "season",
                   values_to = "nfishers") %>% 
      # Rename
      setNames(c("region", "season", "nfishers")) %>% 
      # Add and arrange source
      mutate(region_type = "area of residence",
             path = x) %>%
      # Convert to character
      mutate_all(as.character) %>%
      left_join(fb_table_key_set2, by = "path") %>%
      select(-path, -fbs_2)
    
    # Return
    fdata1
  
})

###############################################################################
## Check totals
## Set 1
totals_set1 <- data_orig_set1 %>% 
  filter(str_detect(region, "Total"),
         region != "Total check") %>% 
mutate(total_nfisher = case_when(is.na(total_nfisher) ~ nfishers,
                                 T ~ total_nfisher),
       total_nfisher = as.numeric(total_nfisher)) %>% 
  select(source, table_name, season, total_nfisher)

check_set1 <- data_orig_set1 %>%
  filter(!str_detect(region, "Total")) %>%
  mutate(nfishers = as.numeric(nfishers)) %>% 
  group_by(season, source) %>% 
  summarise(total_sum = sum(nfishers, na.rm = T)) %>% 
  left_join(totals_set1, by = c("season", "source")) %>% 
  mutate(dif_check = total_sum - total_nfisher) ##No differences! All good. FB 49 does not provide totals that is why is shows NA

## Set 2
totals_set2 <- data_orig_set2 %>% 
  filter(str_detect(region, "Total"),
         region != "Total check") %>% 
  mutate(nfishers = as.numeric(nfishers)) %>% 
  select(source, table_name_set2, season, nfishers)

check_set2 <- data_orig_set2 %>%
  filter(!str_detect(region, "Total")) %>%
  mutate(nfishers = as.numeric(nfishers)) %>% 
  group_by(season, source) %>% 
  summarise(total_sum = sum(nfishers, na.rm = T)) %>% 
  left_join(totals_set2, by = c("season", "source")) %>% 
  mutate(dif_check = total_sum - nfishers) ##No differences! All good.
  
################################################################################

# Format data
## Set 1
data_set1 <- data_orig_set1 %>% 
  # Remove totals and total checks
  filter(!str_detect(region, "Total")) %>%
  # Format number of fishers
  mutate(nfishers=as.numeric(nfishers)) %>% 
  # Format region of residence
  mutate(region = gsub("\\.|\\_|\\_|\\-|\\,", "", region),
         region = str_remove(region, "Region"),
         region = str_remove(region, "\\d+") %>% stringr::str_trim(.),
         region = recode(region,
                         "Oregon Washington and Alaska" = "AK/WA/OR",
                         "Alaska Washington and Oregon fishermen licensed in California"  = "AK/WA/OR",                 
                         "Alaska Washington Oregon and other out of state fishermen licensed in California"  = "AK/WA/OR/Other",
                         "Alaska Oregon Washington and other states licensed in California" = "AK/WA/OR/Other",
                         "Del Norte and Eureka" = "Del Norte/Eureka",
                         "Mexican fishermen licensed in California" = "Mexico",
                         "Mexican nationals licensed in California" = "Mexico"),
         ##Creating a column with just the one year
         year = str_extract(season, "\\d{4}") %>% as.numeric(.)) %>% 
  # Remove nationality, sum by residence region
  group_by(region, region_type, table_name, season, year, source) %>% 
  summarise(nfishers = sum(nfishers, na.rm = T)) %>% 
  ungroup() %>% 
  ## Adding missing values for year 1953 (see note at the end)
  add_row(region = "not specified", region_type = "area of residence", table_name = "Table144", season = "1935-36", year = 1935, source = "FB 49", nfishers = 631)
                         
# Inspect
str(data_set1)
freeR::complete(data_set1)
table(data_set1$region)
table(data_set1$season)

# Double check totals
double_check_set1 <- data_set1 %>% 
  group_by(season, source) %>% 
  summarise(total_sum = sum(nfishers, na.rm = T)) %>% 
  left_join(totals_set1, by = c("season", "source")) %>% 
  mutate(dif_check = total_sum - total_nfisher) ##No differences! All good.
  
## Set 2

data_set2 <- data_orig_set2 %>% 
  # Remove totals and total checks
  filter(!str_detect(region, "Total")) %>%
  # Format number of fishers
  mutate(nfishers=as.numeric(nfishers)) %>% 
  # Format region of residence
  mutate(region = gsub("\\.|\\_|\\_|\\-|\\,", "", region) %>% stringr::str_trim(.),
         region = recode(region,
                         "AK WA OR" = "AK/WA/OR",
                         "Alaska Washington and Oregon"  = "AK/WA/OR",                 
                         "Alaska Washington and Oregon fishermen licensed in California" = "AK/WA/OR",
                         "Alaska Washington and Oregon licensed in California" = "AK/WA/OR",
                         "Kureka"  = "Eureka",
                         "Mexican nationals" = "Mexico",
                         "Mexican nationals licensed in California" = "Mexico",
                         "Other registry" = "Other",
                         "Others" = "Other",
                         "San Francisco—" = "San Francisco"),
         ##removes all letters
         season = str_remove(season, "\\D+") %>% stringr::str_trim(.),
         ##extracts the fist 4 digits
         year = str_extract(season, "(\\d{4})") %>% as.numeric(.), 
         ##extracts the last 2 digits
         test =  str_sub(season, -2,-1)) %>% 
  unite(season, year, test, sep = "-", remove = FALSE) %>% 
  select(source, table_name = table_name_set2, season, year, nfishers, region, region_type) %>% 
  ## Gets rid of duplicates. When values are provided in two FBs then it keeps just one of them
  distinct(region, season, nfishers, region_type, .keep_all = T)

# Inspect
str(data_set2)
freeR::complete(data_set2)
table(data_set2$region)
table(data_set2$season)

# Double check totals
totals_set2 <- totals_set2 %>% 
  mutate(season = str_remove(season, "\\D+") %>% stringr::str_trim(.), ##removes all letters
         year = str_extract(season, "(\\d{4})") %>% as.numeric(.),  ##extracts the fist 4 digits
         test =  str_sub(season, -2,-1)) %>% 
  unite(season, year, test, sep = "-") %>% 
  distinct(season, nfishers, .keep_all = T)
  

double_check_set2 <- data_set2 %>% 
  group_by(season, source) %>% 
  summarise(total_sum = sum(nfishers, na.rm = T)) %>% 
  left_join(totals_set2, by = c("season", "source")) %>% 
  mutate(dif_check = total_sum - nfishers) ##No differences! All good.


#################################################################################
## Merge the all data sets

nfishers_complete <- bind_rows(nfishers_16_35, nfishers_36_38, data_set1, data_set2) %>% 
  ##remove the statewide value for year 1935 (we keep the values by area of residency)
  filter(!(year == 1935 & region == "Statewide")) %>% 
  ## Arrange
  select(source, table_name, season, year, nfishers, region, region_type) %>% 
  arrange(season, year, region)

## Inspect
freeR::complete(nfishers_complete) # must all be 0
table(nfishers_complete$source)
range(nfishers_complete$year)
table(nfishers_complete$season)
table(nfishers_complete$year)
table(nfishers_complete$region_type)
table(nfishers_complete$region)

################################################################################
# Plot data

pal_antique <- c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C")

area_cols <- 15
my_colors <- colorRampPalette(brewer.pal(8, "Set2"))(area_cols)
my_colors_2 <- colorRampPalette(brewer.pal(12, "Paired"))(area_cols)
my_colors_3 <- colorRampPalette(pal_antique)(area_cols)

nfishers_ts <- ggplot(nfishers_complete)+
  geom_bar(aes(x = year, y = nfishers, fill = region), 
           stat = "identity")+
  theme_classic()+
  scale_x_continuous(breaks=seq(1916,1980,5)) +
  scale_fill_manual(values = my_colors_3)+
  labs(fill = element_text("Region of residence"),
       title = "Commercial Fishers registered in CA",
       x= "Year",
       y = "Nº of Vessels")

# Export data
################################################################################

# saveRDS(nfishers_complete, file = file.path(outdir, "CDFW_1916_1976_nfishers_by_residency.Rds"))

#################################################################################
##Note
## Table144b in FB 49 reports 6007 registered commercial fisheres. This contradicts with the 5376 fishers resported in Table144a. The 6007 fishers match with what is reported latter in FB 74 Table33. Within the text in FB49 pg 144 says: "The records show that 209 commercial fishermen from Oregon, Washington and Alaska came south during the 1935–1936 license year and secured California commercial fishing licenses". We included the list the piece of the table that indicates the nationality of fishers comming from AK/WA/OR and added an "Other" row to make sure the total sum 209 as indicated in the text.Additionally, we added a "not speciefied" category under the region column to include the missing 631 fishers. This way match the numbers in both tables.


