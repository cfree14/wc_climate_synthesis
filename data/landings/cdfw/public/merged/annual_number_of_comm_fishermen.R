
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# Read FB 49-170 data (1916-1976)
data_fb1_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1916_1976_nfishers_by_residency.Rds")

# Read FB 181 data (1976-1999)
data_fb2_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table3_licenced_fishermen_vessels.csv", as.is=T)

# Read web data (2000-2020)
data_web_orig <- read.csv("data/landings/cdfw/public/website_licenses/commercial/processed/CDFW_1970_2020_n_licensed_comm_fishers.csv", as.is=T)


# Build data
################################################################################

# Format FB 49-170 data (1916-1976)
data_fb1 <- data_fb1_orig %>% 
  # Rename
  rename(table=table_name) %>% 
  # Format table
  mutate(table=gsub("Table", "Table ", table)) %>% 
  # Format season
  mutate(season=gsub(" ", "", season)) %>% 
  # Arrange
  select(source, table, season, year, region_type, region, nfishers, everything())

# Inspect data
str(data_fb1)
freeR::complete(data_fb1)
table(data_fb1$source)
table(data_fb1$table)
table(data_fb1$season)
range(data_fb1$year)
table(data_fb1$region_type)
table(data_fb1$region)

# Plot data
g <- ggplot(data_fb1, aes(x=year, y=nfishers, fill=region)) +
  geom_bar(stat="identity") +
  theme_bw()
g

# Format FB 181 data (1976-1999)
data_fb2 <- data_fb2_orig %>% 
  # Remove 1976
  filter(year!="1976-77") %>% 
  # Reduce
  select(source, year, n_fishers) %>% 
  # Rename
  rename(season=year, nfishers=n_fishers) %>% 
  # Format source/table
  mutate(source="FB 181",
         table="Table 3") %>% 
  # Format year
  mutate(year=substr(season, 1, 4) %>% as.numeric()) %>% 
  # Format number of vessels
  mutate(nfishers=nfishers %>% gsub(",", "", .) %>% as.numeric()) %>% 
  # Add region info
  mutate(region_type="state",
         region="Statewide") %>% 
  # Arrange
  select(source, table, season, year, region_type, region, nfishers, everything()) %>% 
  arrange(season, year)

# Format web data
data_web <- data_web_orig %>% 
  # Reduce 
  filter(year>=2000) %>% 
  # Add source/table
  mutate(source="CDFW 2021",
         table="N/A",
         season=paste(year, year+1-2000, sep="-"),
         region_type="state",
         region="Statewide") %>% 
  # Arrange
  select(source, table, season, year, region_type, region, nfishers, everything()) %>% 
  arrange(season, year)


# Merge data
data <- bind_rows(data_fb1, data_fb2, data_web) %>% 
  # Add region group
  # Aggregate regions
  mutate(region_group=recode(region, 
                             "AK/WA/OR"="AK/WA/OR/Other",
                             "Other"="AK/WA/OR/Other",
                             "Del Norte"="Eureka",
                             "Del Norte/Eureka"="Eureka",
                             "Sacramento"="Sacramento Delta")) %>% 
  # Arrange
  arrange(season, year, region_group, region)

# Plot data
g <- ggplot(data, aes(x=year, y=nfishers, fill=region_group)) +
  geom_bar(stat="identity") +
  theme_bw()
g

# Export data
write.csv(data, file=file.path(outdir, "CDFW_1916_2020_n_fishers_by_area_of_residence.csv"), row.names = F)
saveRDS(data, file=file.path(outdir, "CDFW_1916_2020_n_fishers_by_area_of_residence.Rds"))
