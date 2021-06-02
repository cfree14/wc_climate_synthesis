
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

# Read CPFV landings
landings_orig <- readRDS(file=file.path(outdir, "CDFW_1946_2019_annual_cpfv_landings_by_port_complex_species.Rds"))

# Read FB old data (1936-1986)
data_old_a_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CPFV_1936_1986_anglers.Rds")
data_old_b_orig <- readxl::read_excel("data/landings/cdfw/public/fish_bulletins/processed/CPFV_1941_1976_party_vessel_w_commercial_licence.xlsx")

# Read FB 173 data (1977-1986) - total numbers of fish/anglers
data_fb173_orig <- readxl::read_excel("data/landings/cdfw/public/fish_bulletins/raw/fb173/raw/Table14.xlsx") # anglers, landings

# Read FB 181 data (1987-1999)
data_fb181a_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table7b_1987_1999_rec_anglers_by_year.csv", as.is=T) # 1987-1999 anglers
data_fb181b_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table3_licenced_fishermen_vessels.csv", as.is=T) # 1976-1999 boats

# Read website data (2000-2019)
data_web_orig <- read.csv("data/landings/cdfw/public/website/cpfv/processed/CDFW_2000_2019_cpfvs_anglers_landings_by_port_complex.csv", as.is=T) # 2000-2019 boats, anglers, landings


# Format datasets
################################################################################

# Goal:
# source, table, region, port_complex_group, port_complex, year, cpfvs_n, anglers_n, landings_n

# Format FB old data (1941-1976): vessels
data_old_b <- data_old_b_orig %>% 
  # Rename
  rename(table=table_name, cpfvs_n=nvessels_commercial, cpfvs_only_n=nvessels_party_only, vessels_n=nvessel_total) %>% 
  # Format source/table
  mutate(source=gsub("FB", "FB ", source),
         table=gsub("Table", "Table ", table))
  
# Format FB old data (1936-1986): anglers, landings, hours
data_old_a <- data_old_a_orig %>% 
  # Rename
  rename(table=table_name, days_n=n_days, anglers_n=n_anglers, hours_n=n_hours, landings_n=nfish_total) %>% 
  # Format source/table
  mutate(source=gsub("FB", "FB ", source),
         table=gsub("Table", "Table ", table)) %>% 
  # Add source year
  mutate(source_num=gsub("FB ", "", source) %>% as.numeric()) %>% 
  # Add region info
  mutate(region="Statewide",
         port_complex_group="Statewide", 
         port_complex="Statewide") %>% 
  # Select most recent data
  arrange(year, desc(source_num)) %>% 
  # Arrange
  select(-effort_hours_calc) %>% 
  select(source_num, source, table, region, port_complex_group, port_complex, year, anglers_n, landings_n, everything())

# Format FB 173 data: 1977-1986 anglers, landings
data_fb173 <- data_fb173_orig %>%
  # Remove total check
  filter(Species!="Total check") %>% 
  # Reduce to totals
  filter(grepl("Total", Species)) %>% 
  # Gather
  gather(key="year", value="total", 2:ncol(.)) %>% 
  spread(key="Species", value="total") %>% 
  mutate(year=as.numeric(year)) %>% 
  # Rename
  setNames(c("year", "anglers_n", "landings_n")) %>% 
  # Add info
  mutate(source="FB 173",
         table="Table 14",
         region="Statewide",
         port_complex_group="Statewide", 
         port_complex="Statewide") %>% 
  # Arrange
  select(source, table, region, port_complex_group, port_complex, year, anglers_n, landings_n, everything())
  
# Format FB 181 data: 1987-1999 anglers
data_fb181a <- data_fb181a_orig %>% 
  # Add info
  mutate(source="FB 181",
         table="Table 7b",
         region="Statewide",
         port_complex_group="Statewide", 
         port_complex="Statewide") %>% 
  # Arrange
  select(source, table, region, port_complex_group, port_complex, year, anglers_n, everything())

# Format FB 181 data: 1976-1999 boats
data_fb181b <- data_fb181b_orig %>% 
  # Rename
  rename(season=year, cpfvs_n=n_cpfvs) %>% 
  mutate(year=substr(season, 1, 4) %>% as.numeric()) %>% 
  # Format source
  mutate(table="Table 3",
         source="FB 181") %>% 
  # Reduce
  select(source, table, season, year, cpfvs_n)

# Format web data
data_web <- data_web_orig %>% 
  # Format source/table
  rename(table=filename) %>% 
  mutate(source=paste("CDFW", year+1)) %>% 
  # Format port
  mutate(port_complex=recode(port_complex,
         "Monterey-Moss Land-Santa Cruz"="Monterey-Moss Landing-Santa Cruz")) %>% 
  # Add port complex
  mutate(port_complex_group=recode(port_complex, 
         "Avila Beach-Morro Bay"="Morro Bay",
         "Fort Bragg-Eureka-Crescent City"="Eureka",
         "Monterey-Moss Landing-Santa Cruz"="Monterey",
         "Newport Beach"="Los Angeles",
         "Oceanside-Dana Harbor"="Los Angeles",
         "Princeton-Bodega Bay"="Bodega Bay",
         "San Diego-Mission Bay"="San Diego",
         "San Francisco-SF Bay-Delta"="San Francisco",
         "Seal Beach-Long Beach-San Pedro"="Los Angeles",
         "Redondo-Marina del Rey-Malibu"="Los Angeles",
         "Port Hueneme-Oxnard-Ventura-Santa Barbara"="Santa Barbara")) %>% 
  # Arrange
  select(source, region, table, port_complex_group, port_complex, year, anglers_n, cpfvs_n, landings_n, everything())

# Inspect
table(data_web$port_complex_group)


# Build FB datasets individually
################################################################################

# Build number of vessels
##################################

# 1941-1976
nvessels1941 <- data_old_b %>% 
  select(source, table, year, cpfvs_n) %>% 
  filter(year!=1976)
  
# 1976-1999 boats (use 1976 from this one)
nvessels1976 <- data_fb181b %>% 
  select(source, table, year, cpfvs_n)

# Merge
nvessels <- bind_rows(nvessels1941, nvessels1976) %>% 
  # Arrange
  arrange(year) %>% 
  # Format source
  mutate(source_cpfvs=paste(source, table)) %>% 
  select(-c(source, table)) %>% 
  select(source_cpfvs, everything())

# Check
freeR::complete(nvessels)
freeR::which_duplicated(nvessels$year)


# Build number of anglers
##################################

# 1936-1986
nfishers1936 <- data_old_a %>% 
  # Reduce to rows with anglers
  filter(!is.na(anglers_n)) %>% 
  # Select most recent value
  arrange(year, desc(source_num)) %>% 
  group_by(year) %>% 
  slice(1) %>% 
  # Simplify
  select(source, table, year, anglers_n)
  
# 1977-1986 (don't use)
nfishers1977 <- data_fb173 %>% 
  # Simplify
  select(source, table, year, anglers_n)

# 1987-1999
nfishers1987 <- data_fb181a %>% 
  # Simplify
  select(source, table, year, anglers_n)

# Merge
nfishers <- bind_rows(nfishers1936, nfishers1987) %>% 
  # Arrange
  arrange(year) %>% 
  # Format source
  mutate(source_anglers=paste(source, table)) %>% 
  select(-c(source, table)) %>% 
  select(source_anglers, everything())

# Check
freeR::complete(nfishers)
freeR::which_duplicated(nfishers$year)


# Build total landings
##################################

# 1936-1986
nlandings1936 <- data_old_a %>% 
  # Reduce to rows with anglers
  filter(!is.na(landings_n)) %>% 
  # Select most recent value
  arrange(year, desc(source_num)) %>% 
  group_by(year) %>% 
  slice(1) %>% 
  # Simplify
  select(source, table, year, landings_n)

# 1987-1999
nlandings1986 <- landings_orig %>% 
  # Annual sums
  group_by(source, table, year) %>% 
  summarize(landings_n=sum(landings_n, na.rm=T)) %>% 
  ungroup() %>% 
  # Filter to 1987-1999
  filter(year%in%1987:1999)

# Merge
nlandings <- bind_rows(nlandings1936, nlandings1986) %>% 
  # Arrange
  arrange(year) %>% 
  # Format source
  mutate(source_landings=paste(source, table)) %>% 
  select(-c(source, table)) %>% 
  select(source_landings, everything())

# Check
freeR::complete(nlandings)
freeR::which_duplicated(nlandings$year)

# Build days/hours
##################################

# Effort
nhours <- data_old_a %>% 
  # Reduce to rows with anglers
  filter(!is.na(days_n) | !is.na(hours_n)) %>% 
  # Select most recent value
  arrange(year, desc(source_num)) %>% 
  group_by(year) %>% 
  slice(1) %>% 
  # Simplify
  select(source, table, year, days_n, hours_n) %>% 
  # Arrange
  arrange(year) %>% 
  # Format source
  mutate(source_time=paste(source, table)) %>% 
  select(-c(source, table)) %>% 
  select(source_time, everything())


# Finalize FB data
################################################################################

# Buil FB data
data_fb <- tibble(year=1916:1999) %>% 
  # Add landings
  left_join(nlandings) %>% 
  # Add vessels
  left_join(nvessels) %>% 
  # Add anglers
  left_join(nfishers) %>% 
  # Add time
  left_join(nhours) %>% 
  # Add region info
  mutate(region="Statewide",
         port_complex_group="Statewide", 
         port_complex="Statewide") %>% 
  # Arrange
  arrange(year) %>% 
  filter(year>=1936) %>% 
  select(source_landings, source_cpfvs, source_anglers, source_time, 
         region, port_complex_group, port_complex, year, 
         landings_n, cpfvs_n, anglers_n, days_n, hours_n)

# Check
freeR::complete(data_fb)
freeR::which_duplicated(data_fb$year)


# Merge FB and website data
################################################################################

# Format web data before merge
data_web_new <- data_web %>% 
  # Source
  mutate(source_landings=paste(source, table),
         source_cpfvs=source_landings, 
         source_anglers=source_landings, 
         source_time=source_landings) %>% 
  select(-c(source, table))

# Merge data
data <- bind_rows(data_fb, data_web_new) %>% 
  # Arrange
  arrange(year, port_complex_group, port_complex)



# Plot data
################################################################################

# Plot data
g2 <- ggplot(data, aes(x=year, y=cpfvs_n, fill=port_complex_group)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Number of CPFVs") +
  theme_bw()
g2

# Plot data
g1 <- ggplot(data, aes(x=year, y=anglers_n/1e3, fill=port_complex_group)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Thousands of CPFV anglers") +
  theme_bw()
g1


# Export data
################################################################################

# Export data
write.csv(data, file=file.path(outdir, "CDFW_1936_2019_annual_cpfv_effort_by_port_complex.csv"), row.names = F)
saveRDS(data, file=file.path(outdir, "CDFW_1936_2019_annual_cpfv_effort_by_port_complex.Rds"))




