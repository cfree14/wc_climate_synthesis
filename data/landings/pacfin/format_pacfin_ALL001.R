

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/pacfin/raw/ALL/ALL001"
outputdir <- "data/landings/pacfin/processed"

# Read data
wa_orig <- read.csv(file.path(datadir, "ALL001-Washington-1980---2021.csv"), as.is=T)
or_orig <- read.csv(file.path(datadir, "ALL001-Oregon-1980---2021.csv"), as.is=T)
ca_orig <- read.csv(file.path(datadir, "ALL001-California-1980---2021.csv"), as.is=T)
sea_orig <- read.csv(file.path(datadir, "ALL001-At-Sea-1980---2021.csv"), as.is=T)

# Read codes
port_key <- read.csv(file.path(outputdir, "pacfin_port_codes_clean.csv"), as.is=T)
spp_key <- read.csv(file.path(outputdir, "pacfin_species_codes_clean.csv"), as.is=T)

# ALL001 - Species Report: Commercial Landed Catch: Metric-Tons (mt), Revenue, and Price-per-pound (Price/lbs)
# This report was generated using the state agency fish ticket data from the PacFIN comprehensive fish ticket table and at sea observer data. This report includes all U.S catch areas including the Puget Sound, and other inland areas where marine fish are caught. Canadian and Alaskan catches have been excluded. Shoreside reported catches have species and area composition samples applied. On board observers did not distinguish between retained and discarded catch prior to 1997, so at-sea data prior to 1997 is not included. Data that involve fewer than three vessels or dealers have been withheld to preserve confidentiality.


# Format data
################################################################################

# Format data
data <- bind_rows(ca_orig, or_orig, wa_orig, sea_orig) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(year=landing_year, state=agency_code, 
         mgmt_group_code=management_group_code, complex_code=complex,
         comm_name_orig=pacfin_species_common_name, species_code=pacfin_species_code,
         value_usd=exvessel_revenue, landings_mt=landed_weight_mtons,
         price_usd_lb=landed_weight_ppp) %>% 
  # Format state
  mutate(state=recode(state, "C"="California", "O"="Oregon", "W"="Washington", "AT-SEA"="At-Sea")) %>% 
  # Add common/scientific name
  left_join(spp_key %>% select(spp_code, comm_name, sci_name), by=c("species_code"="spp_code")) %>% 
  # Convert landings
  mutate(landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>% 
  # Arrange
  select(state, year, mgmt_group_code, complex_code, 
         species_code, comm_name_orig, comm_name, sci_name,
         value_usd, landings_mt, landings_kg, landings_lb, price_usd_lb, confidential_flag, everything())

# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$state)
table(data$mgmt_group_code)
table(data$complex_code)


# Export data
################################################################################

# Export
saveRDS(data, file.path(outputdir, "PACFIN_ALL001_1980_2021_all_species_landings.Rds"))






