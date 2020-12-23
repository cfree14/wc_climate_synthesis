

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
outputdir <- "data/landings/cdfw/public/website/cpfv/processed"
plotdir <- "data/landings/cdfw/public/website/cpfv/figures"

# To do: 
# You can definitely salvage 1999 from this
# You may be able to salvage other missing info from the repeat data
# You should check the duplicate data

# Build file key
################################################################################

# Build file key
file_key <- purrr::map_df(2000:2019, function(x){
  
  # Get files
  basedir <- "data/landings/cdfw/public/website/raw"
  datadir <- file.path(basedir, x, "cpfv")
  yfiles <- list.files(datadir, pattern=".pdf")
  
  # Build key
  y_key <- tibble(year=x,
                  filename=yfiles)
  
})

# Add region
file_key <- file_key %>% 
  mutate(type=ifelse(grepl("cover", tolower(filename)), "cover", ""),
         type=ifelse(grepl("port", tolower(filename)), "port", type),
         type=ifelse(grepl("state", tolower(filename)), "state", type),
         type=ifelse(grepl("cpfv2|cpfv 1|annual 1|page 2", tolower(filename)), "state", type),
         type=ifelse(grepl("cpfv1|cpfv 2|annual 2|page 1", tolower(filename)), "port", type)) %>% 
  # Manually change a few
  mutate(type=ifelse(filename=="landings08_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings08_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings07_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings07_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings06_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings06_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings05_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings05_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings04_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings04_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings03_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings03_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings02_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings02_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings01_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings01_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings00_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings00_cpfv2.pdf", "port", type))

# Check port complexes
file_check <- file_key %>% 
  group_by(year, type) %>% 
  summarize(n=n()) %>% 
  spread(key="type", value="n")


# Merge data
################################################################################

# Merge data
data_orig <- purrr::map_df(2000:2019, function(x) {
  
  # Directory
  basedir <- "data/landings/cdfw/public/website/raw"
  datadir <- file.path(basedir, x, "cpfv")

  # Identify files
  file_do <- file_key %>% 
    filter(year==x & type=="state") %>% pull(filename)
  
  # Read data
  fdata_orig <- read.csv(file.path(datadir, paste0(x, "_state_messy.csv")), as.is=T) %>% 
    janitor::clean_names("snake")
  
  # Format data
  fdata <- fdata_orig %>% 
    mutate(year=x,
           filename=file_do) %>% 
    select(year, filename, everything()) %>% 
    gather(key="region_year", value="landings_n", 4:ncol(.))
  
})


# Format data
################################################################################

# Format state data
data_full <- data_orig %>% 
  # Convert landings to numeric
  mutate(landings_n=landings_n %>% gsub(",", "", .) %>% as.numeric(.)) %>% 
  # Format species
  mutate(species=gsub("\\*|\\^", "", species) %>% stringr::str_trim(),
         species=recode(species,
                        "LANDINGS"="Total landings:",
                        "Total Landings:"="Total landings:",
                        "NUMBER OF ANGLERS"="Number of anglers:",
                        "Number of Anglers:"="Number of anglers:",
                        "Number of Fishers:"="Number of anglers:",
                        "REPORTING CPFVS"="Number of CPFVs reporting:",
                        "Reporting CPFV's"="Number of CPFVs reporting:",
                        "Reporting CPFVs:"="Number of CPFVs reporting:",
                        "Salmon, king (chinook)"="Salmon, king (Chinook)")) %>% 
  # Break apart region-year
  mutate(year1=gsub(".*_", "", region_year) %>% as.numeric(),
         region=gsub("_.*", "", region_year) %>% stringr::str_to_sentence()) %>% 
  # Arrange
  select(year, filename, region, year1, species, landings_n)

# Inspect data
freeR::complete(data_full)
str(data_full)
range(data_full$year)
table(data_full$region)
sort(unique(data_full$species))


# Extract data components
################################################################################

# Extract data
data <- data_full %>% 
  # Remove totals
  filter(!grepl(":", species)) %>% 
  # Remove duplicated
  filter(year==year1) %>% 
  select(-year1)

# Extract totals
data_tots <- data_full %>% 
  # Reduce to totals
  filter(grepl(":", species)) %>% 
  # Spread metrics
  spread(key="species", value="landings_n") %>% 
  # Rename columns
  rename(landings_n="Total landings:", anglers_n="Number of anglers:", cpfvs_n="Number of CPFVs reporting:") %>% 
  # Reduce to reporting year's data
  filter(year==year1) %>% 
  select(-year1)
  

# QA/QC
################################################################################

# Observed totals
tots_obs <- data %>% 
  group_by(year, region) %>% 
  summarize(landings_n_obs=sum(landings_n, na.rm=T))

# Reported totals
tots_rep <- data_tots %>% 
  select(year, region, landings_n) %>% 
  rename(landings_n_rep=landings_n)

# Check totals
tots_check <- tots_obs %>% 
  left_join(tots_rep) %>% 
  mutate(landings_n_diff=landings_n_obs-landings_n_rep)


# Export data
################################################################################

# Export data
write.csv(data, file=file.path(outputdir, "CDFW_2000_2019_cpfv_landings_statewide.csv"), row.names = F)




