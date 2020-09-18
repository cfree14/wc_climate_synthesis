

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/pacfin/raw"
outputdir <- "data/landings/pacfin/processed"

# Read data
data_orig <- read.csv(file.path(datadir, "CRAB002-W-O-C-1980---2020.csv"), as.is=T)

# Source: https://reports.psmfc.org/pacfin/f?p=501:402:5900103230722:INITIAL:::F_SELECTED_NODE:40&cs=3mClXWqoElXsdmdrVzVhWwua4AwKyWWhm8Sf5XnQFZsFBspTJE77vyMGMbxwDBo1hrLDWjanROq311zaXZClgBw


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Column names
  janitor::clean_names("snake") %>% 
  rename(year=crab_year, 
         agency=agency_code, 
         port_code=pacfin_group_port_code, 
         port_desc=port_description) %>% 
  # Rename to make month extraction go smoothly
  rename(tot_round_weight_mtons=total_round_weight_mtons,
         tot_round_weight_ppp=total_round_weight_ppp,  
         tot_exvessel_revenue=total_exvessel_revenue,
         tot_confidential_flag=total_confidential_flag) %>% 
  # Gather
  gather(key="metric", value="value", 5:ncol(.)) %>% 
  # Extract month
  mutate(month=substr(metric, 1, 3)) %>% 
  # Format metric
  mutate(metric=substr(metric, 5, nchar(metric))) %>% 
  # Arrange and spread
  select(year, agency, port_code, port_desc, month, metric, value) %>% 
  spread(key="metric", value="value", fill=0) %>% 
  # Rename metrics
  rename(revenues_usd=exvessel_revenue,
         landings_mt=round_weight_mtons,
         price_usd_lb=round_weight_ppp,
         confidential=confidential_flag) %>% 
  # Arrange
  select(year, agency, port_code, port_desc, month, confidential, landings_mt, price_usd_lb, revenues_usd, everything()) %>% 
  # Format columns
  mutate(agency=recode(agency, "C"="California", "O"="Oregon", "W"="Washington"),
         landings_mt=as.numeric(landings_mt),
         revenues_usd=as.numeric(revenues_usd),
         price_usd_lb=as.numeric(price_usd_lb))

# Summarize by state-year
stats <- data %>% 
  filter(month=="tot") %>% 
  group_by(agency, year) %>% 
  summarize(landings_mt=sum(landings_mt)) %>% 
  ungroup()

# Plot data
g <-ggplot(stats, aes(x=year, y=landings_mt/1e3, fill=agency)) +
  geom_area() +
  labs(x="Year", y= "Landings (1000s mt)") +
  theme_bw()
g

# Export data
saveRDS(data, file.path(outputdir, "PACFIN_1980_2020_dungeness_crab_landings_by_port_month.Rds"))





  
