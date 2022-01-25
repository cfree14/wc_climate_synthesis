

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/pacfin/raw/ALL/ALL006"
outputdir <- "data/landings/pacfin/processed"

# Read codes
port_key <- read.csv(file.path(outputdir, "pacfin_port_codes_clean.csv"), as.is=T)
spp_key <- read.csv(file.path(outputdir, "pacfin_species_codes_clean.csv"), as.is=T)

# Formatting steps overview
# Step 1. Format each state's data and merge into single table
# Step 2. Add meta-data associated with codes

# Format data
################################################################################

# Files to merges
files2merge <- list.files(datadir)


# Loop through files and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata_orig <- read.csv(file.path(datadir, x), as.is=T)
  
  # Format data
  fdata <- fdata_orig %>% 
    janitor::clean_names("snake")
  
})

# Format data
data <- data_orig %>% 
  # Rename
  rename(year=landing_year, state=agency_code, mgmt_group_code=management_group_code, 
         comm_name=pacfin_species_common_name, spp_code=pacfin_species_code) %>% 
  # Format state
  mutate(state=recode(state, "AT-SEA"="At-Sea", "C"="California", "O"="Oregon", "W"="Washington")) %>% 
  # Gather columns
  gather(key="metric_long", value="value", 7:ncol(.)) %>% 
  # Extract month
  mutate(month=substr(metric_long, 1, 3)) %>% 
  # Spread by metric
  filter(!grepl("total", metric_long)) %>% 
  mutate(metric_long=substr(metric_long, 5, nchar(metric_long))) %>% 
  spread(key="metric_long", value="value") %>% 
  # Rename metrics
  rename(value_usd=exvessel_revenue,
         landings_mt=landed_weight_mtons,
         price_usd_lb=landed_weight_ppp, 
         confidential=confidential_flag) %>% 
  # Convert to numeric
  mutate(landings_mt=as.numeric(landings_mt),
         price_usd_lb=as.numeric(price_usd_lb),
         value_usd=as.numeric(value_usd)) %>% 
  # Add landings weights
  mutate(landings_kg=landings_mt*1000,
         landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>% 
  # Derive price per pound (ppp)
  # Interesting, these don't perfectly match reported values
  mutate(price_usd_lb1=value_usd/landings_lb) %>% 
  select(-price_usd_lb1) %>% 
  # Reduce 
  filter(!is.na(landings_mt) | !is.na(price_usd_lb) | !is.na(value_usd)) %>% 
  # Add date
  mutate(month=stringr::str_to_title(month),
         date=lubridate::mdy(paste(month, "1,", year)),
         month_num=lubridate::month(date)) %>% 
  # Format common name
  mutate(spp_type=ifelse(grepl("NOM.", comm_name), "nominal", "actual"),
         comm_name_parent=gsub("NOM. ", "", comm_name),
         comm_name_parent=stringr::str_to_sentence(comm_name_parent),
         comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format management group code
  mutate(mgmt_group_code=recode(mgmt_group_code, 
                                "XXXX SUBTOTAL"="Unknown")) %>% 
  # Arrange
  select(state, mgmt_group_code, complex, comm_name_parent, comm_name, spp_code, spp_type, year, month, month_num, date, 
         landings_mt, landings_kg, landings_lb, price_usd_lb, value_usd, confidential, everything()) %>% 
  arrange(state, mgmt_group_code, complex, comm_name_parent, comm_name, spp_code, year, date)

# Inspect
str(data)
freeR::complete(data)
table(data$year)
table(data$month)
table(data$mgmt_group_code)

# Species
spp_key <- count(data, comm_name_parent, comm_name, spp_code) %>% arrange(desc(n))

# Export data
saveRDS(data, file.path(outputdir, "PACFIN_ALL006_1980_2022_all_species_landings_by_month.Rds"))

