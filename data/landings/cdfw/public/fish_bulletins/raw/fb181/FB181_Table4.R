

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
indir <- "data/landings/cdfw/public/fish_bulletins/fb181/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/fb181/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/fb181/figures"

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
write.csv(data1, file=file.path(indir, "tabula-FB181_Table4_annual_landings_by_species_statewide_imperfect.csv"), row.names=F)

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


# Export data
################################################################################

# Export
write.csv(data3, file=file.path(outdir, "FB181_Table4_1987_1999_landings_by_species.csv"), row.names=F)








 