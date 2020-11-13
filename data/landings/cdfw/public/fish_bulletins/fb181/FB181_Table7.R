

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
data_orig <- read.csv(file.path(indir, "tabula-FB181_Table7_cpfv_landings_anglers.csv"), as.is=T)


# Format data
################################################################################

# Format landings
landings <- data_orig %>% 
  janitor::clean_names("snake") %>% 
  filter(species!="" & !grepl("Total", species)) %>% 
  gather(key="year", value="landings_n", 2:ncol(.)) %>% 
  mutate(year=year %>% gsub("x", "", .) %>% as.numeric(),
         landings_n=landings_n %>% gsub(",", "", .) %>% as.numeric(),
         source="FB 181 Table 7") %>% 
  select(source, species, year, landings_n) %>% 
  arrange(species, year)

# Inspect
table(landings$species)

# Anglers
anglers <- data_orig %>% 
  janitor::clean_names("snake") %>% 
  filter(species=="Total number of anglers") %>% 
  gather(key="year", value="anglers_n", 2:ncol(.)) %>% 
  mutate(year=year %>% gsub("x", "", .) %>% as.numeric(),
         anglers_n=anglers_n %>% gsub(",", "", .) %>% as.numeric(),
         source="FB 181 Table 7") %>% 
  select(source, year, anglers_n)

# Export data
################################################################################

# Export data
write.csv(landings, file=file.path(outdir, "FB181_Table7a_1987_1999_rec_landings_by_species.csv"), row.names=F)
write.csv(anglers, file=file.path(outdir, "FB181_Table7b_1987_1999_rec_anglers_by_year.csv"), row.names=F)



