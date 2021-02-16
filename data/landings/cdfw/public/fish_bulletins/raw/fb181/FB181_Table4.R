

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb181/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb181/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb181/figures"

# Read data
data_orig <- read.csv(file.path(indir, "tabula-FB181_Table4_annual_landings_by_species_statewide.csv"), as.is=T)


# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  # Fix names
  rename(species="Fish.") %>% 
  # Add row id
  mutate(row_id=1:n()) %>% 
  select(row_id, everything()) %>% 
  # Add table id
  mutate(table_id=ifelse(grepl("Table|:|Total", species), species, NA)) %>% 
  select(row_id, table_id, everything()) %>% 
  fill(table_id, .direction="up") %>% 
  # Extract table and category
  mutate(source="FB 181",
         table=substr(table_id, 1, 8),
         table=recode(table, 
                      "Crustace"="Table 4c", 
                      "Echinode"="Table 4d", 
                      "Miscella"="Table 4g", 
                      "Mollusks"="Table 4e",  
                      "Plants:"="Table 4f", 
                      "Rockfish"="Table 4b"),
         table=ifelse(row_id<=399, "Table 4a", 
                      ifelse(row_id<=515, "Table 4b", 
                             ifelse(row_id<=574, "Table 4c", 
                                    ifelse(row_id<=591, "Table 4d", 
                                           ifelse(row_id<=675, "Table 4e", 
                                                  ifelse(row_id<=684, "Table 4f", "Table 4g")))))),
         category=recode(table, 
                         "Table 4a"="Fish",
                         "Table 4b"="Rockfish",
                         "Table 4c"="Crustaceans",
                         "Table 4d"="Echinoderms",
                         "Table 4e"="Mollusks",
                         "Table 4f"="Plants",
                         "Table 4g"="Miscellaneous species")) %>% 
  # Arrange
  select(-table_id) %>% 
  select(source, table, category, row_id, everything()) %>% 
  # Remove useless rows
  filter(!grepl("Table", species))

# Inspect
table(data1$table)

# Export for manual editing
# write.csv(data1, file=file.path(indir, "tabula-FB181_Table4_annual_landings_by_species_statewide_imperfect.csv"), row.names=F)

# Read manually editted (highlighted in yellow)
data2_orig <- readxl::read_excel(file.path(indir, "tabula-FB181_Table4_annual_landings_by_species_statewide_imperfect.xlsx"))

# Format data
data2 <- data2_orig %>% 
  # Remove useless rows
  mutate(species=ifelse(is.na(species), "", species)) %>% 
  filter(!grepl("ERASE|:", species)) %>% 
  # Add metric column
  mutate(metric=rep(c("Landings", "Value"), nrow(.)/2)) %>% 
  select(source:species, metric, everything()) %>% 
  # Format species column
  mutate(species=ifelse(species=="", NA, species)) %>% 
  fill(species, .direction="down") %>% 
  mutate(species=ifelse(grepl("Total", species), "Total", species)) %>% 
  # Gather years
  select(-row_id) %>% 
  gather(key="year", value="value", 6:ncol(.)) %>% 
  mutate(year=year %>% gsub("X", "", .) %>% as.numeric()) %>% 
  # Spread metrics
  spread(key="metric", value="value") %>% 
  rename(landings_lb=Landings, value_usd=Value) %>%
  # Simplify
  filter(!is.na(landings_lb) | !is.na(value_usd))
  

# QA/QC data
################################################################################

# Remove totals
data3 <- data2 %>% 
  filter(species!="Total")

# Reported totals
tots_rep <- data2 %>% 
  filter(species=="Total")

# Observed totals
tots_obs <- data3 %>% 
  group_by(source, table, category, year) %>% 
  summarize(landings_lb_obs=sum(landings_lb),
          value_usd_obs=sum(value_usd))

# Check totals
tots_check <- tots_rep %>% 
  left_join(tots_obs) %>% 
  mutate(landings_diff=(landings_lb_obs-landings_lb)/landings_lb*100,
         value_diff=(value_usd_obs-value_usd)/value_usd*100,
         value_diff1=abs(value_diff)>1)

range(tots_check$landings_diff)
range(tots_check$value_diff)


# Final formatting
################################################################################

# Format data
data4 <- data3 %>% 
  # Harmonize names
  rename(comm_name_orig=species) %>% 
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
                              'Sturgeons'='Sturgeon', 
                              'Thornyheads (unspecified)'='Thornyheads', 
                              'Trawled fish for animal food'='Miscellaneous (animal food)', 
                              'True smelts'='True smelt', 
                              'Unspecified croaker'='Unspecifed croaker', 
                              'Unspecified jacks'='Unspecified jack', 
                              'Unspecified species'='Unspecified fish', 
                              'White urchin'='White sea urchin', 
                              'Wolf (wolf-eel) eel'='Wolf eel'),
         comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm"),
         sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Add presentation
  mutate(presentation=ifelse(grepl("roe on kelp", comm_name_orig), "roe on kelp", 
                             ifelse(grepl("roe", comm_name_orig), "roe", 
                                    ifelse(grepl("claws", comm_name_orig), "claws", "not specified")))) %>% 
  # Convert units
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  # Arrange
  select(-comm_name_reg) %>% 
  select(source, table, category, comm_name_orig, comm_name, sci_name, year, presentation, landings_lb, landings_kg, value_usd, everything()) %>% 
  arrange(year, category, comm_name)

# Common names
# wcfish::check_names(data4$comm_name_reg)

# Inspect
str(data4)
freeR::complete(data4)
table(data4$source)
table(data4$table)
table(data4$category)


# Export data
################################################################################

# Export
write.csv(data4, file=file.path(outdir, "FB181_Table4_1987_1999_landings_by_species.csv"), row.names=F)








 