
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(tabulizer)
library(pdftools)

# Directories
datadir <- "data/landings/cdfw/public/website_licenses/commercial/raw"
outputdir <- "data/landings/cdfw/public/website_licenses/commercial/processed"

# TO DO
# You could probably format the license names and types more nicely


# Extract data
################################################################################

# Files to merge
files2merge <- list.files(datadir, pattern=".pdf")

# Export data
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){

  # Read data
  infile <- file.path(datadir, x)
  tables_list <- tabulizer::extract_tables(infile, output = "data.frame", method="stream")
  
  # Extract table
  tdata <- tables_list[[1]]
  
  # Export 
  write.csv(tdata, file.path(datadir, gsub(".pdf", ".csv", x)), row.names=F)
  
})

# Merge data
################################################################################

# Files to merge
files2merge <- list.files(datadir, pattern=".xlsx")

# Merge data
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  tdata_orig <- readxl::read_excel(file.path(datadir, x), na=c("N/A", "NA", "Not Avail.", "See Below"))
  
  # Format data
  tdata <- tdata_orig %>% 
    # Rename
    rename(license=Licenses, license_type=Type) %>% 
    # Remove empty rows
    filter(!is.na(license_type)) %>% 
    # Gather
    gather(key="year", value="value", 3:ncol(.)) %>% 
    # Format year
    mutate(year=gsub("X", "", year) %>% as.numeric())

})

# Inspect data
str(data_orig)


# Format data
################################################################################

# You could probably format the license names and types more nicely

# Format data
data <- data_orig %>% 
  # Format license name
  mutate(license=stringr::str_to_title(license),
         license=gsub("Nr", "NR", license),
         license=gsub("Nt", "NT", license))

# Inspect licensees
licenses <- data %>% 
  group_by(license_type, license) %>% 
  summarize(n=n())


# Extract useful datasets
################################################################################

# Number commercial fishing licenses
nfishers <- data %>% 
  # Filter
  filter(grepl("Comm Fishing License", license)) %>% 
  # Spread
  select(-license_type) %>%  
  spread(key="license", value="value") %>% 
  # Rename
  setNames(c("year", "nfishers", "nfishers_nr", "nfishers_r")) %>% 
  # Derive
  mutate(nfishers=ifelse(is.na(nfishers), nfishers_nr+nfishers_r, nfishers))

# Number of commercial fishing vessels
nvessels <- data %>% 
  # Filter
  filter(grepl("Comm Boat Registration", license)) %>% 
  # Spread
  select(-license_type) %>%  
  spread(key="license", value="value") %>% 
  # Rename
  setNames(c("year", "nvessels", "nvessels_nr", "nvessels_r")) %>% 
  # Derive
  mutate(nvessels=ifelse(is.na(nvessels), nvessels_nr+nvessels_r, nvessels))


# Export data
################################################################################

# Export data
write.csv(data, file=file.path(outputdir, "CDFW_1970_2020_license_statistics.csv"), row.names=F)
write.csv(nfishers, file=file.path(outputdir, "CDFW_1970_2020_n_licensed_comm_fishers.csv"), row.names=F)
write.csv(nvessels, file=file.path(outputdir, "CDFW_1970_2020_n_licensed_comm_vessels.csv"), row.names=F)

