
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

# Read FB XXX data (XXXX-XXXX)

# Read FB 173 data (1977-1986) - total numbers of fish/anglers
data_fb173_orig <- readxl::read_excel("data/landings/cdfw/public/fish_bulletins/raw/fb173/raw/Table14.xlsx") # anglers, landings

# Read FB 181 data (1987-1999)
data_fb181a_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table7b_1987_1999_rec_anglers_by_year.csv", as.is=T) # 1987-1999 anglers
data_fb181b_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table3_licenced_fishermen_vessels.csv", as.is=T) # 1976-1999 boats

# Read website data (2000-2019)
data_web_orig <- read.csv("data/landings/cdfw/public/website/cpfv/processed/CDFW_2000_2019_cpfvs_anglers_landings_by_port_complex.csv", as.is=T) # 2000-2019 boats, anglers, landings


# Build data
################################################################################

# Goal:
# source, table, region, port_complex_group, port_complex, year, cpfvs_n, anglers_n, landings_n

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

# Build FB data
data_fb <- bind_rows(data_fb173, data_fb181a) %>% 
  # Add number of boats
  left_join(data_fb181b %>% select(-season), by="year") %>% 
  # Merge sources/tables
  mutate(source=paste(source.x, source.y, sep="/"),
         source=recode(source, "FB 181/FB 181"="FB 181"),
         table=paste(table.x, table.y, sep="/")) %>% 
  # Simplify
  select(source, table, region, port_complex_group, port_complex, year, cpfvs_n, anglers_n, landings_n)

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
         "Fort Bragg-Eureka-Crescent City"="Fort Bragg",
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


# Merge data
################################################################################

# Merge data
data <- bind_rows(data_fb, data_web) %>% 
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
write.csv(data, file=file.path(outdir, "CDFW_1946_2019_annual_cpfv_effort_by_port_complex.csv"), row.names = F)
saveRDS(data, file=file.path(outdir, "CDFW_1946_2019_annual_cpfv_effort_by_port_complex.Rds"))




