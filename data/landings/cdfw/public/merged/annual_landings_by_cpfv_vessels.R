
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

# Read FB 95-173 data (1936-1986)
data1_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CPFV_1936_1986_nfish_by_species.Rds")

# Read FB 181 data (1987-1999)
data2_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table7a_1987_1999_rec_landings_by_species.csv", as.is=T)

# Read website data (2000-2019)
data3_orig <- read.csv("data/landings/cdfw/public/website/cpfv/processed/CDFW_2000_2019_cpfv_landings_by_port_complex_species.csv", as.is=T)


# Format data
################################################################################

# Columns
colnames(data1_orig)
colnames(data2_orig)
colnames(data3_orig)

# Goal
# source, table, region, port_complex_group, port_complex, year, comm_name_orig, comm_name, sci_name, landings_n

# Format 96-173 data (1936-1986)
data1 <- data1_orig %>% 
  # Rename
  rename(table=table_name, comm_name_orig=comm_name_reg, landings_n=nfish_catch) %>% 
  # Format source/table
  mutate(source=gsub("FB", "FB ", source),
         table=gsub("Table", "Table ", source)) %>% 
  # Add region info
  select(-c(region_type, region)) %>% 
  mutate(region="Statewide",
         port_complex_group="Statewide",
         port_complex="Statewide") %>% 
  # Add scientific name
  mutate(sci_name=harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  select(source, table, region, port_complex_group, port_complex, year, comm_name_orig, comm_name, sci_name, landings_n)

# Inspect old
table(data1_orig$region)
table(data1_orig$region_type)

# Inspect new
str(data1)
freeR::complete(data1)
table(data1$source)
table(data1$table)

# Format FB 181 data (1987-1999)
data2 <- data2_orig %>% 
  # Add columns
  mutate(region="Statewide", 
         port_complex_group="Statewide",
         port_complex="Statewide") %>% 
  # Arrange
  select(source, table, region, port_complex_group, port_complex, year, comm_name_orig, comm_name, sci_name, landings_n)

# Inspect
freeR::complete(data2)

# Format website data (2000-2019)
data3 <- data3_orig %>% 
  # Rename
  rename(table=filename) %>% 
  # Add source
  mutate(source=paste("CDFW", year+1)) %>% 
  # Add port complex group
  mutate(port_complex_group=recode(port_complex, 
                                   "Fort Bragg-Eureka-Crescent City"="Eureka",
                                   "Princeton-Bodega Bay"="Bodega Bay",
                                   "San Francisco-SF Bay-Delta"="San Francisco",
                                   "Monterey-Moss Landing-Santa Cruz"="Monterey",
                                   "Avila Beach-Morro Bay"="Morro Bay",
                                   "Port Hueneme-Oxnard-Ventura-Santa Barbara"="Santa Barbara",
                                   "Newport Beach"="Los Angeles",
                                   "Oceanside-Dana Harbor"="Los Angeles",
                                   "Seal Beach-Long Beach-San Pedro"="Los Angeles",
                                   "Redondo-Marina del Rey-Malibu"="Los Angeles",
                                   "San Diego-Mission Bay"="San Diego")) %>% 
  # Arrange
  select(source, table, region, port_complex_group, port_complex, year, comm_name_orig, comm_name, sci_name, landings_n)

# Inspect
freeR::complete(data3)


# Merge data
################################################################################

# Read species groups
spp_key <- readxl::read_excel(file.path(outdir, "CPFV_species_groups.xlsx"))

# Merge data
data <- bind_rows(data1, data2, data3) %>% 
  # Add species groups
  left_join(spp_key, by="comm_name") %>% 
  # Factor port complex group
  mutate(port_complex_group=factor(port_complex_group, 
                                   levels=c("Eureka", "Bodega Bay", "San Francisco", "Monterey", 
                                            "Morro Bay", "Santa Barbara", "Los Angeles", "San Diego", "Statewide"))) %>% 
  # Arrange
  select(source:year, category, everything()) %>% 
  arrange(year, region, port_complex_group, port_complex, category, comm_name)

# Inspect data
str(data)
freeR::complete(data)


# Plot
################################################################################

# Plot
g <- ggplot(data, aes(x=year, y=landings_n/1e6, fill=port_complex_group)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of fish)") +
  theme_bw()
g

# Plot
g <- ggplot(data, aes(x=year, y=landings_n/1e6, fill=category)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of fish)") +
  theme_bw()
g

# Export
################################################################################

# Export
write.csv(data, file=file.path(outdir, "CDFW_1946_2019_annual_cpfv_landings_by_port_complex_species.csv"), row.names = F)
saveRDS(data, file=file.path(outdir, "CDFW_1946_2019_annual_cpfv_landings_by_port_complex_species.Rds"))

