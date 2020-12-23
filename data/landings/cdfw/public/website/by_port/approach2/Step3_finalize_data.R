

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/cdfw/public/approach2/intermediate/by_port"
outputdir <- "data/landings/cdfw/public/approach2/intermediate/"


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(inputdir, pattern=".csv")

# Merge data
data_orig <- purrr::map_df(files2merge, function(x) {
  fdata <- read.csv(file.path(inputdir, x), as.is=T)
})

# Inspect data
table(data_orig$year)
table(data_orig$port) 
table(data_orig$species)


# Basic formatting
################################################################################

# Format data
data1 <- data_orig %>% 
  # Rename columns
  rename(landings_lb=landings, value_usd=value) %>% 
  # Convert landings units
  mutate(landings_kg=measurements::conv_unit(landings_lb, from="lbs", to="kg"),
         landings_mt=landings_kg/1000) %>% 
  # Arrange columns
  select(year, filename, port, species, landings_lb, landings_kg, landings_mt, value_usd, everything())

# Extract totals
totals <- data1 %>% 
  # Totals
  filter(grepl("TOTAL", toupper(species)))

# Inspect data
table(data1$year)
table(data1$port)
table(data1$species)


# QA/QC number 1: make sure every year-area-port has a total row
################################################################################

# See which files don't have a total
check1 <- data1 %>% 
  ungroup() %>% 
  group_by(year, filename, port) %>% 
  summarise(n=n(),
            tot_row_yn=ifelse(sum(grepl("TOTAL", toupper(species)))>0, "yes", "no")) %>% 
  ungroup() %>% 
  filter(tot_row_yn=="no") %>% 
  arrange(desc(year), filename)

# 2008 and before is good

# Export
################################################################################

# Export
data_out <- data1 %>% 
  filter(year<=2008)
write.csv(data_out, file=file.path(outputdir, "CDFW_2000_2008_landings_by_port_approach2.csv"), row.names=F)
