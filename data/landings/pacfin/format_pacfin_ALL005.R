

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/pacfin/raw/ALL/ALL005"
outputdir <- "data/landings/pacfin/processed"

# Read data
ca_orig <- read.csv(file.path(datadir, "ALL005-C-1980---2020.csv"), as.is=T, na.strings=c("", "...."))
or_orig <- read.csv(file.path(datadir, "ALL005-O-1980---2020.csv"), as.is=T, na.strings=c("", "...."))
wa_orig <- read.csv(file.path(datadir, "ALL005-W-1980---2020.csv"), as.is=T, na.strings=c("", "...."))

# Read codes
port_key <- read.csv(file.path(outputdir, "pacfin_port_codes_clean.csv"), as.is=T)
spp_key <- read.csv(file.path(outputdir, "pacfin_species_codes_clean.csv"), as.is=T)

# Formatting steps overview
# Step 1. Format each state's data and merge into single table
# Step 2. Add meta-data associated with codes


# Step 1. Format each state's data and merge into single table
################################################################################

# Format data
ca <- ca_orig %>% 
  # Column names
  janitor::clean_names("snake") %>% 
  rename(year=landing_year,
         agency=agency_code,
         mgmt_group_code=management_group_code, 
         species_code=pacfin_species_code,
         comm_name=pacfin_species_common_name) %>% 
  # Rename to make month extraction go smoothly
  rename(tot_landed_weight_mtons=total_landed_weight_mtons,
         tot_landed_weight_ppp=total_landed_weight_ppp,  
         tot_exvessel_revenue=total_exvessel_revenue,
         tot_confidential_flag=total_confidential_flag) %>% 
  # Gather
  gather(key="metric", value="value", 7:ncol(.)) %>% 
  # Extract month
  mutate(port_code=substr(metric, 1, 3)) %>% 
  # Format metric
  mutate(metric=substr(metric, 5, nchar(metric))) %>% 
  # Arrange and spread
  select(year, agency, port_code, mgmt_group_code:species_code, metric, value) %>% 
  spread(key="metric", value="value", fill=0) %>% 
  # Rename metrics
  rename(revenues_usd=exvessel_revenue,
         landings_mt=landed_weight_mtons,
         price_usd_lb=landed_weight_ppp,
         confidential=confidential_flag) %>% 
  # Arrange
  select(year:species_code, confidential, landings_mt, price_usd_lb, revenues_usd, everything()) %>% 
  # Format columns
  mutate(agency=recode(agency, "C"="California", "O"="Oregon", "W"="Washington"),
         port_code=toupper(port_code),
         landings_mt=as.numeric(landings_mt),
         revenues_usd=as.numeric(revenues_usd),
         price_usd_lb=as.numeric(price_usd_lb))


# Format data
or <- or_orig %>% 
  # Column names
  janitor::clean_names("snake") %>% 
  rename(year=landing_year,
         agency=agency_code,
         mgmt_group_code=management_group_code, 
         species_code=pacfin_species_code,
         comm_name=pacfin_species_common_name) %>% 
  # Rename to make month extraction go smoothly
  rename(tot_landed_weight_mtons=total_landed_weight_mtons,
         tot_landed_weight_ppp=total_landed_weight_ppp,  
         tot_exvessel_revenue=total_exvessel_revenue,
         tot_confidential_flag=total_confidential_flag) %>% 
  # Gather
  gather(key="metric", value="value", 7:ncol(.)) %>% 
  # Extract month
  mutate(port_code=substr(metric, 1, 3)) %>% 
  # Format metric
  mutate(metric=substr(metric, 5, nchar(metric))) %>% 
  # Arrange and spread
  select(year, agency, port_code, mgmt_group_code:species_code, metric, value) %>% 
  spread(key="metric", value="value", fill=0) %>% 
  # Rename metrics
  rename(revenues_usd=exvessel_revenue,
         landings_mt=landed_weight_mtons,
         price_usd_lb=landed_weight_ppp,
         confidential=confidential_flag) %>% 
  # Arrange
  select(year:species_code, confidential, landings_mt, price_usd_lb, revenues_usd, everything()) %>% 
  # Format columns
  mutate(agency=recode(agency, "C"="California", "O"="Oregon", "W"="Washington"),
         port_code=toupper(port_code),
         landings_mt=as.numeric(landings_mt),
         revenues_usd=as.numeric(revenues_usd),
         price_usd_lb=as.numeric(price_usd_lb))

# Format data
wa <- wa_orig %>% 
  # Column names
  janitor::clean_names("snake") %>% 
  rename(year=landing_year,
         agency=agency_code,
         mgmt_group_code=management_group_code, 
         species_code=pacfin_species_code,
         comm_name=pacfin_species_common_name) %>% 
  # Rename to make month extraction go smoothly
  rename(tot_landed_weight_mtons=total_landed_weight_mtons,
         tot_landed_weight_ppp=total_landed_weight_ppp,  
         tot_exvessel_revenue=total_exvessel_revenue,
         tot_confidential_flag=total_confidential_flag) %>% 
  # Gather
  gather(key="metric", value="value", 7:ncol(.)) %>% 
  # Extract month
  mutate(port_code=substr(metric, 1, 3)) %>% 
  # Format metric
  mutate(metric=substr(metric, 5, nchar(metric))) %>% 
  # Arrange and spread
  select(year, agency, port_code, mgmt_group_code:species_code, metric, value) %>% 
  spread(key="metric", value="value", fill=0) %>% 
  # Rename metrics
  rename(revenues_usd=exvessel_revenue,
         landings_mt=landed_weight_mtons,
         price_usd_lb=landed_weight_ppp,
         confidential=confidential_flag) %>% 
  # Arrange
  select(year:species_code, confidential, landings_mt, price_usd_lb, revenues_usd, everything()) %>% 
  # Format columns
  mutate(agency=recode(agency, "C"="California", "O"="Oregon", "W"="Washington"),
         port_code=toupper(port_code),
         landings_mt=as.numeric(landings_mt),
         revenues_usd=as.numeric(revenues_usd),
         price_usd_lb=as.numeric(price_usd_lb))

# Merge data
data1 <- bind_rows(ca, or, wa) %>% 
  # Add "Confidential" species code
  mutate(species_code=ifelse(comm_name=="WITHHELD FOR CONFIDENTIALITY**", "Confidential", species_code))

# Inspect
freeR::complete(data1)

# Examine missing species
check <- data1 %>% 
  filter(is.na(species_code))


# Step 2. Add meta-data associated with codes
################################################################################

# Add meta-data
data2 <- data1 %>% 
  # Remove TOTALS for each state
  filter(port_code!="TOT") %>% 
  # Remove redundant species columns
  select(-c(comm_name, mgmt_group_code, complex)) %>% 
  # Add formatted species common name, scientific name
  left_join(spp_key %>% select(spp_code, comm_name, sci_name ), by=c("species_code"="spp_code")) %>% 
  mutate(comm_name=ifelse(species_code=="Confidential-", "Confidential", comm_name)) %>% 
  # Add port info
  left_join(port_key %>% select(port_code, port_name)) %>% 
  # Arrange
  select(year, agency, port_code, port_name,
         species_code, comm_name, sci_name, 
         confidential:revenues_usd) %>% 
  # Rename some columns
  rename(state=agency, spp_code=species_code)

# Inspect
freeR::complete(data2)


# Ports
ports <- data2 %>% 
  select(state, port_code, port_name) %>% 
  unique()


# Export
################################################################################

# Export data
saveRDS(data2, file.path(outputdir, "PACFIN_ALL005_1980_2020_all_species_landings_by_port.Rds"))


