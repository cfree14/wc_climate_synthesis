

# Turn off scientific notation
options(scipen=999)

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb173/raw"
outputdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb173/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb173/figures"

# Table key
table_key <- tibble(table=paste("Table", 4:13),
                    year=1977:1986)

# There is soemthing really wrong with the 1984 (Table 11) data - values/landings swapped?

# Merge data
################################################################################

# Table 4-13: 1977-1986
data_orig <- purrr::map_df(4:13, function(x){
  
  # Read data
  infile <- paste0("Table", x, ".xlsx")
  fdata_orig <- readxl::read_excel(file.path(inputdir, infile))
  
  # Format data
  data_full <- fdata_orig %>% 
    # Format species 
    mutate(species=species %>% gsub("\\.|-|—|‚Äî|_", "", .) %>% stringr::str_trim(.)) %>% 
    fill(species, .direction="down") %>% 
    # Format type/category
    mutate(type=gsub(":", "", type),
           category=gsub(":", "", category)) %>% 
    # Gather
    gather(key="port_complex", value="value", 5:ncol(.)) %>% 
    # Convert value
    filter(!is.na(value)) %>% 
    mutate(value=value %>% gsub('\\$|\\-|\\.|,|\\_|-|_|:|“|;|"', "", .) %>% stringr::str_trim() %>% as.numeric()) %>% 
    # Add column names
    mutate(source="FB 173",
           table=paste("Table", x)) %>% 
    select(source, table, everything())
  
  # Inspect data
  # str(data_full)
  # table(data_full$category)
  # table(data_full$metric)
  # table(data_full$type)
  # sort(unique(data_full$species))
  
})


# Format data
################################################################################

# Format data
data_full <- data_orig %>% 
  # Format category
  mutate(category=stringr::str_trim(category), 
         category=recode(category,
                         "Crustacean"="Crustaceans",
                         "Crustraceans"="Crustaceans",
                         "Echinodera"="Echinoderms",
                         "Echinoderm"="Echinoderms",
                         "Molluscs"="Mollusks",
                         "Mollusk"="Mollusks")) %>% 
  # Format species
  mutate(species=recode(species, 
                        "Shriap, miscellaneous"="Shrimp, miscellaneous",
                        "Shriap, Pacific ocean"="Shrimp, Pacific ocean",
                        "Shrimp, miscellanous"="Shrimp, miscellaneous",
                        "Shrimp, Pacific Ocean"="Shrimp, Pacific ocean",
                        "Miscellaneous crustacean"="Crustacean, miscellaneous", 
                        "Miscellaneous echniderm"="Echinoderm, miscellaneous",
                        "Miscellaneous echinoderm"="Echinoderm, miscellaneous",
                        "Butterf ish, Pacific"="Butterfish, Pacific", 
                        "Croaker white"="Croaker, white",
                        "Dolphinf ish"="Dolphinfish",
                        "Flounder, Miscellaneous"="Flounder, miscellaneous",
                        "Flounder, niscellaneous"="Flounder, miscellaneous",	
                        "Flounder/ starry"="Flounder, starry",
                        "Flyingfi«h"="Flyingfish",	
                        "Flylngfish"="Flyingfish",
                        "Grand total pounds"="Grand total",	
                        "Grand total value"="Grand total",
                        "Halfaoon"="Halfmoon",
                        "Miscellaneous fish"="Fish, miscellaneous",	
                        "Ray, Miscellaneous"="Ray, miscellaneous",
                        "Ray, niscellaneous"="Ray, miscellaneous",	
                        "Rockfish group, bocaccio/chili"="Rockfish group, boccaccio/chilipepper",	
                        "Rockfish group, boccaccio/chili"="Rockfish group, boccaccio/chilipepper",	
                        "Rockfish, niscellaneous"="Rockfish, miscellaneous",	
                        "Sablef ish"="Sablefish",	
                        "Saloon"="Salmon",	
                        "Sanddab Pacific"="Sanddab, Pacific",	
                        "Sole, mi seellaneous"="Sole, miscellaneous",	
                        "Total pounds"="Total",	
                        "Total value"="Total",
                        "Tuna, biqeye"="Tuna, bigeye",
                        "Tuna, Miscellaneous"="Tuna, miscellaneous",	
                        "Tuna, niscellaneous"="Tuna, miscellaneous",	
                        "Tuna, skipjack,black"="Tuna, skipjack, black",
                        "Yellowtai1"="Yellowtail",	
                        "Abalone, miscellaeou"="Abalone, miscellaneous",	
                        "Abalone, miscellaneous"="Abalone, miscellaneous",
                        "Abalone, miscellaeous"="Abalone, miscellaneous",
                        "Abalone, miscellanous"="Abalone, miscellaneous",
                        "Miscellaneous mollusc"="Mollusk, miscellaneous",	
                        "Miscellaneous mollusk"="Mollusk, miscellaneous",	
                        "Miscellaneous nollusk"="Mollusk, miscellaneous",	
                        "Miscellanesous mollusk"="Mollusk, miscellaneous",
                        "Squid, Market"="Squid, market"),
         species=stringr::str_trim(species)) %>% 
  # Add year
  left_join(table_key) %>% 
  # Arrange
  select(source, table, year, everything())
  
# Species key
spp_key <- data_full %>% 
  select(category, species) %>% 
  unique() %>% 
  arrange(category, species)

# Inspect data
str(data_full)
freeR::complete(data_full)
range(data_full$year)
table(data_full$source)
table(data_full$table)
table(data_full$type)
table(data_full$category)
table(data_full$metric)
table(data_full$port_complex)


# QA/QC row totals
################################################################################

# Calculate totals
tots_obs <- data_full %>% 
  filter(port_complex!="Total") %>% 
  group_by(table, year, type, species, metric) %>% 
  summarize(total_obs=sum(value, na.rm = T))

# Extract totals
tots_rep <- data_full %>% 
  filter(port_complex=="Total") %>% 
  select(table, year, type, species, metric, value) %>% 
  rename(total_rep=value)

# Compare totals
# Table 4 salmon and abalone miscellaneosu are CDFW errors
# Table 6 shark miscellaneous is CDFW error
# Table 7 1980 Landings total is CDFW error
# Table 9 1982 white abalone, shipment value total, and grant value total are CDFW errors
# Table 11 Grand total is CDFW error
# Table 13 errors are small and not worth investigating
tots_check <- tots_obs %>% 
  left_join(tots_rep) %>% 
  mutate(total_diff=total_obs-total_rep) %>% 
  filter(total_diff!=0 | is.na(total_diff))


# QA/QC port totals
################################################################################

# Calculate totals
tots_obs1 <- data_full %>% 
  filter(!grepl("total", tolower(species))) %>% 
  group_by(table, year, type, port_complex, metric) %>% 
  summarize(total_obs=sum(value, na.rm=T))

# Extract totals
tots_rep1 <- data_full %>% 
  filter(species=="Total") %>% 
  select(table, year, type, port_complex, metric, value) %>% 
  rename(total_rep=value)

# Compare totals
tots_check1 <- tots_obs1 %>% 
  left_join(tots_rep1) %>% 
  mutate(total_diff=total_obs-total_rep)

# Plot comparison
g <- ggplot(tots_check1, aes(x=year, y=port_complex, fill=total_diff)) +
  facet_grid(type~metric) +
  geom_raster() +
  scale_fill_gradient2("Difference", midpoint=0, low="red", high="blue", mid="grey80") +
  labs(x="", y="") +
  theme_bw()
g

# Export
################################################################################

# Extract data
data <- data_full %>% 
  # Remove totals
  filter(port_complex!="Total" & !grepl("total", tolower(species))) %>% 
  # Arrange columns
  select(source:year, type, port_complex, category, species, metric, value, everything()) %>% 
  arrange(year, type, port_complex, category, species) %>% 
  # Spread
  spread(key="metric", value="value") %>% 
  rename(landings_lb=Landings, value_usd=Value) %>% 
  # Add more weights
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  # Harmonize names
  rename(comm_name_orig=species) %>% 
  mutate(comm_name_reg=wcfish::convert_names(comm_name_orig, to="regular"),
         comm_name_reg=recode(comm_name_reg,
                              'Black tuna skipjack'='Black skipjack tuna', 
                              'Boccaccio/chilipepper rockfish group'='Bocaccio/chilipepper rockfish group', 
                              'Mackerel'='Unspecified mackerel', 
                              'Miscellaneous abalone'='Abalone', 
                              'Miscellaneous croaker'='Unspecified croaker', 
                              'Miscellaneous flounder'='Unspecified flounder', 
                              'Miscellaneous ray'='Unspecified ray', 
                              'Miscellaneous rockfish'='Rockfish', 
                              'Miscellaneous shark'='Unspecified shark', 
                              'Miscellaneous shrimp'='Unspecified shrimp', 
                              'Miscellaneous tuna'='Unspecified tuna', 
                              'Sea cucumber'='Unspecified sea cucumber', 
                              'Surfperch'='Unspecified surfperch', 
                              'Thornyhead'='Thornyheads')) %>% 
  mutate(comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm"),
         sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  select(-comm_name_reg) %>%
  select(source, table, year, port_complex,  
         category, type, comm_name_orig, comm_name, sci_name,
         landings_lb, landings_kg, value_usd, everything()) %>% 
  arrange(year, type, port_complex, category, comm_name_orig) %>% 
  # Eliminate 1984 which is super messed up
  filter(year!=1984)

# Check names
# wcfish::check_names(data$comm_name_reg)

# Inspect
freeR::complete(data)

# Export data
write.csv(data, file=file.path(outputdir, "CDFW_1977_1986_landings_by_port_complex.csv"), row.names=F)




