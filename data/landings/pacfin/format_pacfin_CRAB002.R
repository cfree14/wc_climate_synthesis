

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/landings/pacfin/raw/CRAB"
outputdir <- "data/landings/pacfin/processed"

# Read data
data_orig <- read.csv(file.path(datadir, "CRAB002-W-O-C-1980---2021.csv"), as.is=T)

# Source: https://reports.psmfc.org/pacfin/f?p=501:402:5900103230722:INITIAL:::F_SELECTED_NODE:40&cs=3mClXWqoElXsdmdrVzVhWwua4AwKyWWhm8Sf5XnQFZsFBspTJE77vyMGMbxwDBo1hrLDWjanROq311zaXZClgBw


# Format data
################################################################################

# Seasons
# Washington: Dec 1 - Sep 15
# Oregon: Dec 1 - Aug 14
# N California: Dec 1 - July 15
# C California: Nov 15 - June 30

# Format data
data <- data_orig %>% 
  # Column names
  janitor::clean_names("snake") %>% 
  rename(year=crab_year, 
         state=agency_code, 
         port_code=pacfin_group_port_code, 
         port_complex=port_description) %>% 
  # Format ports
  mutate(port_complex=stringr::str_to_title(port_complex)) %>% 
  # Rename to make month extraction go smoothly
  rename(tot_round_weight_mtons=total_round_weight_mtons,
         tot_round_weight_ppp=total_round_weight_ppp,  
         tot_exvessel_revenue=total_exvessel_revenue,
         tot_confidential_flag=total_confidential_flag) %>% 
  # Gather
  gather(key="metric", value="value", 5:ncol(.)) %>% 
  # Extract month
  mutate(month=substr(metric, 1, 3),
         month=recode(month,
                      "jan"="January",
                      "feb"="February",
                      "mar"="March",
                      "apr"="April",
                      "may"="May",
                      "jun"="June",
                      "jul"="July",
                      "aug"="August",
                      "sep"="September",
                      "oct"="October",
                      "nov"="November",
                      "dec"="December",
                      "tot"="Total")) %>% 
  # Remove month totals
  filter(month!="Total") %>% 
  mutate(month=factor(month, levels=month.name)) %>% 
  # Fix year and add season
  mutate(year=year-1, 
         season=paste(year, year+1, sep="-")) %>% 
  # Add date dummy
  mutate(date=ifelse(month %in% c("November", "December"), 
                     paste0(as.character(month), " 1, ", year),
                     paste0(as.character(month), " 1, ", year+1)),
         date=mdy(date)) %>% 
  # Format metric
  mutate(metric=substr(metric, 5, nchar(metric))) %>% 
  # Arrange and spread
  select(year,  season, month, date, state, port_code, port_complex, metric, value) %>% 
  spread(key="metric", value="value", fill=0) %>% 
  # Rename metrics
  rename(revenues_usd=exvessel_revenue,
         landings_mt=round_weight_mtons,
         price_usd_lb=round_weight_ppp,
         confidential=confidential_flag) %>% 
  # Arrange
  select(season, year, month, date, 
         state, port_code, port_complex, 
         confidential, landings_mt, price_usd_lb, revenues_usd, everything()) %>% 
  # Format columns
  mutate(state=recode(state, "C"="California", "O"="Oregon", "W"="Washington"),
         landings_mt=as.numeric(landings_mt),
         revenues_usd=as.numeric(revenues_usd),
         price_usd_lb=as.numeric(price_usd_lb))
  # Convert negative metrics
  # mutate(revenues_usd=abs(revenues_usd),
  #        price_usd_lb=abs(price_usd_lb),
  #        landings_mt=abs(landings_mt))

# Ports
ports <- data %>% 
  select(state, port_code, port_complex) %>% 
  unique()

# Inspect data
str(data)
freeR::complete(data)


# Export data
################################################################################

# Landings

# Plot data
g <- ggplot(data, aes(x=date, y=port_complex, fill=landings_mt)) +
  facet_grid(state~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Date", y="") +
  # Legend
  scale_fill_gradientn(name="Landings (mt)", colors=RColorBrewer::brewer.pal(9, "Oranges"), 
                       na.value = "white") +
  # Theme
  theme_bw()
g

# Summarize by state-year
stats <- data %>% 
  group_by(state, season) %>% 
  summarize(landings_mt=sum(landings_mt)) %>% 
  ungroup()

# Plot data
g <-ggplot(stats, aes(x=season, y=landings_mt/1e3, fill=state)) +
  geom_bar(stat="identity") +
  labs(x="Year", y= "Landings (1000s mt)") +
  theme_bw() +
  theme() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Revenues

# Plot data
g <- ggplot(data, aes(x=date, y=port_complex, fill=revenues_usd)) +
  facet_grid(state~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Date", y="") +
  # Legend
  scale_fill_gradientn(name="Revenues (USD)", colors=RColorBrewer::brewer.pal(9, "Oranges"), 
                       na.value = "white") +
  # Theme
  theme_bw()
g

# Summarize by state-year
stats <- data %>% 
  group_by(state, season) %>% 
  summarize(revenues_usd=sum(revenues_usd)) %>% 
  ungroup()

# Plot data
g <-ggplot(stats, aes(x=season, y=revenues_usd/1e6, fill=state)) +
  geom_bar(stat="identity") +
  labs(x="Year", y= "Revenues (millions of mt)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g


# Export data
################################################################################

# Export data
saveRDS(data, file.path(outputdir, "PACFIN_CRAB002_1980_2021_dcrab_landings_by_port_month.Rds"))





  
