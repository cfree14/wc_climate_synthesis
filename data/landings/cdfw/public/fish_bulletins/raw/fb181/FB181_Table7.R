

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/fish_bulletins/raw/fb181/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb181/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/raw/fb181/figures"

# Read data
data_orig <- read.csv(file.path(indir, "tabula-FB181_Table7_cpfv_landings_anglers.csv"), as.is=T)


# Format data
################################################################################

# Format landings
landings <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(comm_name_orig=species) %>% 
  # Remove totals
  filter(comm_name_orig!="" & !grepl("Total", comm_name_orig)) %>% 
  # Gather
  gather(key="year", value="landings_n", 2:ncol(.)) %>% 
  # Format
  mutate(year=year %>% gsub("x", "", .) %>% as.numeric(),
         landings_n=landings_n %>% gsub(",", "", .) %>% as.numeric(),
         source="FB 181", 
         table="Table 7") %>% 
  # Add common name
  mutate(comm_name=wcfish::reverse_names(comm_name_orig),
         comm_name=recode(comm_name, "Others, All"="All other species"),
         comm_name=harmonize_names(comm_name, from="comm", to="comm"),
         sci_name=harmonize_names(comm_name, from="comm", to="sci")) %>% 
  # Arrange
  select(source, table, comm_name_orig, comm_name, sci_name, year, landings_n) %>% 
  arrange(comm_name, year)

# Inspect
freeR::complete(landings)
table(landings$comm_name)
wcfish::check_names(landings$comm_name)
wcfish::harmonize_names(landings$comm_name, from="comm", to="comm")

# Anglers
anglers <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  # Remove totals
  filter(species=="Total number of anglers") %>% 
  # Gather
  gather(key="year", value="anglers_n", 2:ncol(.)) %>% 
  # Add columns
  mutate(year=year %>% gsub("x", "", .) %>% as.numeric(),
         anglers_n=anglers_n %>% gsub(",", "", .) %>% as.numeric(),
         source="FB 181",
         table="Table 7") %>% 
  select(source, table, year, anglers_n)

# Export data
################################################################################

# Export data
write.csv(landings, file=file.path(outdir, "FB181_Table7a_1987_1999_rec_landings_by_species.csv"), row.names=F)
write.csv(anglers, file=file.path(outdir, "FB181_Table7b_1987_1999_rec_anglers_by_year.csv"), row.names=F)



