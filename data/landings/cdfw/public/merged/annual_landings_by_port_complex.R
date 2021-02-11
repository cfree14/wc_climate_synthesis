
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# Read FB 173
data_fb173_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb173/processed/CDFW_1977_1986_landings_by_port_complex.csv", as.is=T)

# Read FB 181 
data_fb181_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table5_1987_1999_landings_by_port_complex_species_expanded.csv", as.is=T)

# Read website data (2000-2019)
data_web_orig <- readRDS("data/landings/cdfw/public/website/by_region/processed/CDFW_2000_2019_landings_by_region_species.Rds")


###
# HEY !!!!! FB NEEDS CONFIMRED GOOD



# Format data
################################################################################

# Column names
colnames(data_fb173)
colnames(data_fb181)
colnames(data_web)

# Goal:
# source, table, year, port_complex, comm_name_orig, comm_name, sci_name, type, presentation, landings_lb, landings_kg, value_usd

# Format FB 173
data_fb173 <- data_fb173_orig %>% 
  # Rename
  rename(comm_name_orig=species) %>% 
  # Add presentation (roe/claws do not seem to be present here)
  mutate(presentation="not specified") %>% 
  # Regularize common name
  mutate(comm_name_reg=wcfish::convert_names(comm_name_orig, to="regular"),
         comm_name_reg=recode(comm_name_reg, 
                              'Black tuna skipjack'='Black skipjack tuna', 
                              'Boccaccio/chilipepper rockfish group'='Bocaccio/chilipepper rockfish group', 
                              'Giant pacific oyster'='Giant Pacific oyster', 
                              'Mackerel'='Unspecified mackerel', 
                              'Miscellaneous abalone'='Abalone', 
                              'Miscellaneous croaker'='Unspecifed croaker', 
                              'Miscellaneous crustacean'='Unspecified crustacean', 
                              'Miscellaneous echinoderm'='Unspecified echinoderm', 
                              'Miscellaneous fish'='Unspecified fish', 
                              'Miscellaneous flounder'='Unspecified flounder', 
                              'Miscellaneous ray'='Unspecified ray', 
                              'Miscellaneous rockfish'='Unspecified rockfish', 
                              'Miscellaneous shark'='Unspecified shark', 
                              'Miscellaneous shrimp'='Unspecified shrimp', 
                              'Miscellaneous tuna'='Unspecified tuna', 
                              'Sea cucumber'='Unspecified sea cucumber', 
                              'Surfperch'='Unspecified surfperch', 
                              'Thornyhead'='Thornyheads')) %>% 
  # Harmonize common/scientific names
  mutate(comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm"), 
         sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  select(-c(comm_name_reg, category)) %>% 
  select(source, table, year, port_complex, type, comm_name_orig, comm_name,  sci_name, presentation, landings_lb, landings_kg, value_usd, everything())

# Check names
# wcfish::check_names(data_fb173$comm_name_reg)

# Inspect
str(data_fb173)
freeR::complete(data_fb173) # SOMETHING WRONG!!!!!!!!!
table(data_fb173$source)
table(data_fb173$table)
table(data_fb173$year)
table(data_fb173$port_complex)


# Format FB 181
data_fb181 <- data_fb181_orig %>% 
  # Remove
  select(-c(sci_name, level, category)) %>% 
  # Add type
  mutate(type="Landings") %>%
  # Regularize common names
  mutate(comm_name_reg=wcfish::convert_names(comm_name_orig, to="regular")) %>% 
  mutate(comm_name_reg=recode(comm_name_reg, 
                              'Black tuna skipjack'='Black skipjack tuna', 
                              'C-o turbot'='C-O sole', 
                              'Claws crab'='Crab', 
                              'Curlfin turbot'='Curlfin sole', 
                              'Dolphin (fish)'='Dolphinfish', 
                              'Giant pacific oyster'='Giant Pacific oyster', 
                              'Group black/blue rockfish'='Black/blue rockfish group', 
                              'Group bocaccio/chilipepper rockfish'='Bocaccio/chilipepper rockfish group', 
                              'Group bolina rockfish'='Bolina rockfish group', 
                              'Group canary/vermilion rockfish'='Canary/vermilion rockfish group', 
                              'Group deepwater reds rockfish'='Deepwater reds rockfish group', 
                              'Group gopher rockfish'='Gopher rockfish', 
                              'Group nearshore rockfish'='Nearshore rockfish group', 
                              'Group red rockfish'='Red rockfish group', 
                              'Group rosefish rockfish'='Rosefish rockfish group', 
                              'Group small rockfish'='Small rockfish group', 
                              'Hagfish'='Unspecified hagfish', 
                              'Herring roe on kelp'='Pacific herring', 
                              'Red urchin'='Red sea urchin', 
                              'Rock unspecified crab'='Rock crab', 
                              'Roe (chinook and coho) salmon'='Chinook/coho salmon', 
                              'Roe herring'='Pacific herring', 
                              'Snapper -Mexico-'='Snapper', 
                              'Spotted cusk- eel'='Spotted cusk-eel', 
                              'Thornyheads (unspecified)'='Thornyheads', 
                              'Trawled fish for animal food'='Miscellaneous (animal food)', 
                              'True smelts'='True smelt', 
                              'Unspecified croaker'='Unspecifed croaker', 
                              'Unspecified jacks'='Unspecified jack', 
                              'Unspecified species'='Unspecified invertebrate', 
                              'White urchin'='White sea urchin', 
                              'Wolf (wolf-eel) eel'='Wolf eel')) %>% 
  # Harmonize common/scientific names
  mutate(comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm"), 
         sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  select(-comm_name_reg) %>% 
  select(source, table, year, port_complex, type, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg, value_usd, everything())

# Check names
wcfish::check_names(data_fb181$comm_name_reg)

# Inspect
str(data_fb181)
freeR::complete(data_fb181)
table(data_fb181$source)
table(data_fb181$table)
table(data_fb181$year)
table(data_fb181$port_complex)


# Format web data
data_web <- data_web_orig %>% 
  # Add type
  mutate(type="Landings") %>%
  # Arrange
  select(source, table, year, port_complex, type, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg, value_usd, everything())

# Merge data
data <- bind_rows(data_fb173, data_fb181, data_web) %>% 
  # Arrange
  arrange(year, port_complex, type, comm_name)

# Inspect
str(data)
freeR::complete(data) # SOMETHING WRONG!!!!!!!!!
table(data$source)
table(data$table)
table(data$year)
table(data$port_complex)

# Plot data
################################################################################

# Stats
stats <- data %>% 
  group_by(year, port_complex) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T))

# Plot pounds
ggplot(data %>% filter(type=="Landings"), aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of lbs)") +
  scale_fill_discrete(name="Port complex", na.value="grey80") +
  theme_bw()

# Plot pounds
ggplot(data %>% filter(type=="Landings"), aes(x=year, y=value_usd/1e6, fill=port_complex)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of dollars)") +
  scale_fill_discrete(name="Port complex", na.value="grey80") +
  theme_bw()


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1928_2019_landings_by_port_complex_species.Rds"))
write.csv(data, file=file.path(outdir, "CDFW_1928_2019_landings_by_port_complex_species.csv"), row.names=F)




