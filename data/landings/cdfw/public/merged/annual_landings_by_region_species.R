
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

# By port (1941-76, 2000-2019)
data1_orig <- readRDS(file.path(outdir, "CDFW_1941_2019_landings_by_port_species.Rds"))

# FB 173 (1977-1986)
data2_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb173/processed/CDFW_1977_1986_landings_by_port_complex.csv")

# FB 181 (1987-1999)
data3_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table5_1987_1999_landings_by_port_complex_species.csv")

# Website data (2000-2019)
data4_orig <- readRDS("data/landings/cdfw/public/website/by_region/processed/CDFW_2000_2019_landings_by_region_species.Rds")

# THINGS TO DO
# NA values in FB 173
# Website data not well inspected
# Replace port-level data with complex-level data

# THIS IS VERY FAR FROM COMPLETE -- REVIST RIGOUROUSLY


# Format data
################################################################################

# Inspect columns
colnames(data1_orig)
colnames(data2_orig)
colnames(data3_orig)
colnames(data4_orig)

# Goal
# source, table, year, port_complex, type, comm_name_orig, comm_name, sci_name,  presentation, landings_lb, landings_kg, value_usd

# Format port-level data
data1 <- data1_orig %>% 
  # Summarize
  group_by(source, table, year, port_complex, type, comm_name_orig, comm_name, sci_name, presentation) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T), 
            landings_kg=sum(landings_kg, na.rm=T), 
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Arrange
  arrange(year, port_complex, type, comm_name) %>% 
  # Reduce to years of need
  filter(year<1977)

# Inspect
freeR::complete(data1)

# Format FB 173 data
data2 <- data2_orig %>% 
  # Add presentation
  mutate(presentation="not specified") %>% 
  # Arrange
  select(-category) %>% 
  select(source, table, year, port_complex, type, comm_name_orig, comm_name, sci_name,  presentation, landings_lb, landings_kg, value_usd, everything()) %>% 
  arrange(year, port_complex, type, comm_name)

# Inspect
freeR::complete(data2)
  
# Format FB 181 data
data3 <- data3_orig %>% 
  # Add type
  mutate(type="Landings") %>% 
  # Format presentation
  mutate(presentation=recode(presentation, "whole"="not specified")) %>% 
  # Regularize names
  mutate(comm_name_reg=wcfish::convert_names(comm_name_orig, to="regular"),
         comm_name_reg=recode(comm_name_reg, 
                              'Black tuna skipjack'='Black skipjack tuna', 
                              'C-o turbot'='C-O sole', 
                              'Claws crab'='Crab', 
                              'Curlfin turbot'='Curlfin sole', 
                              'Dolphin (fish)'='Dolphinfish', 
                              'Group black/blue rockfish'='Black/blue rockfish group', 
                              'Group bocaccio/chilipepper rockfish'='Bocaccio/chilipepper rockfish group', 
                              'Group bolina rockfish'='Bolina rockfish group', 
                              'Group canary/vermilion rockfish'='Canary/vermilion rockfish group', 
                              'Group deepwater reds rockfish'='Deepwater reds rockfish group', 
                              'Group gopher rockfish'='Gopher rockfish group', 
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
                              'Unspecified jacks'='Unspecified jack', 
                              'Unspecified species'='Miscellaneous fish', 
                              'White urchin'='White sea urchin', 
                              'Wolf (wolf-eel) eel'='Wolf eel')) %>% 
  # Harmonize names
  mutate(comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm")) %>% 
  # Add scientific name
  mutate(sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  select(-c(comm_name_reg, category)) %>% 
  select(source, table, year, port_complex, type, comm_name_orig, comm_name, sci_name,  presentation, landings_lb, landings_kg, value_usd, everything()) %>% 
  arrange(year, port_complex, type, comm_name)

# Inspect data
freeR::complete(data3)

# Format website data
data4 <- data4_orig %>% 
  # Add type
  mutate(type="Landings") %>% 
  # Arrange
  select(source, table, year, port_complex, type, comm_name_orig, comm_name, sci_name,  presentation, landings_lb, landings_kg, value_usd) %>% 
  arrange(year, port_complex, type, comm_name)
  
# Inspect data
freeR::complete(data4)

# Final data
################################################################################

# Complexes
complexes2 <- c("Eureka", "San Francisco", "Sacramento Delta", 
               "Monterey", "Santa Barbara", 'Los Angeles', "San Diego", "Inland Waters", "Unknown")
complexes <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", "Sacramento Delta", 
               "Monterey", "Morro Bay", "Santa Barbara", 'Los Angeles', "San Diego", "Inland Waters", "Unknown")

# Merge data
data <- bind_rows(data1, data2, data3, data4) %>% 
  # Format port complex
  rename(port_complex_orig=port_complex) %>% 
  # Harmonize port complexes
  mutate(port_complex2=recode(port_complex_orig, 
                              "Fort Bragg"="Eureka",
                              "Bodega Bay"="San Francisco",
                              "Morro Bay"="Santa Barbara")) %>% 
  # Factor port complex
  mutate(port_complex2=factor(port_complex2, levels=complexes2)) %>% 
  # Arrange
  select(source, table, year, port_complex2, port_complex_orig,  type, 
         comm_name_orig, comm_name, sci_name,  presentation, 
         landings_lb, landings_kg, value_usd, everything()) %>% 
  arrange(year, port_complex2, port_complex_orig, type, comm_name)

# Inspect
str(data)
freeR::complete(data)
table(data$source)
table(data$table)
range(data$year)
table(data$port_complex_orig)
table(data$port_complex2)
table(data$type)
table(data$presentation)


# Plot data
################################################################################

# Annual sums
stats <- data %>% 
  filter(type=="Landings") %>% 
  group_by(year, port_complex2) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T))

# Plot data
g <- ggplot(stats, aes(x=year, y=landings_lb/1e6, fill=port_complex2)) +
  geom_bar(stat="identity") + 
  labs(x="Year", y="Landings (millions of lbs)") +
  theme_bw()
g


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1941_2019_landings_by_port_complex_species.Rds"))
write.csv(data, file=file.path(outdir, "CDFW_1941_2019_landings_by_port_complex_species.csv"), row.names=F)


