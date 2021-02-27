
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

# Read 1941-1956 landings by port complex
data_fb1_orig <-readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1941_1956_landings_by_port_complex.Rds")

# Read 1941-1976 landings by port and species
data_fb2_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1941_1976_landings_by_port_species.Rds")

# Read 1977-1986 (FB 173) landings by port complex and species
data_fb3_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb173/processed/CDFW_1977_1986_landings_by_port_complex.csv")

# Read 1987-1999 (FB 181) landings by port complex and species
data_fb4_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table5_1987_1999_landings_by_port_complex_species.csv") 

# Read 2000-2019 landings by port and species
data_web1_orig <- read.csv("data/landings/cdfw/public/website/by_port/processed/CDFW_2000_2019_landings_by_port_expanded.csv", as.is=T)

# Read 2000-2019 landings by port complex and species
data_web2_orig <- readRDS("data/landings/cdfw/public/website/by_region/processed/CDFW_2000_2019_landings_by_region_species.Rds")

# Read port data
data_port_orig <- readRDS(file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.Rds"))


# THIS FILE IS ONLY MEANT TO BE TEMPORARY. IT IS VERY CRAPPY.


# Build data
################################################################################

# Format FB 1  (1941-1946)
data_fb1 <- data_fb1_orig %>% 
  select(year, port_complex, type, value_usd, landings_lb) %>% 
  filter(year<=1946)

# Format FB 2 (1946-1976)
data_fb2 <- data_fb2_orig %>% 
  group_by(year, port_complex, type) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T),
            landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  filter(year>=1947)

# Format FB 3 (1977-1986)
data_fb3 <- data_fb3_orig %>% 
  group_by(year, port_complex, type) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T),
            landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup()

# Format FB 4 (1987-1999)
data_fb4 <- data_fb4_orig %>% 
  mutate(type="Landings") %>% 
  group_by(year, port_complex, type) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T),
            landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup()

# Read data (1947-1976; 2000-2019)
data_port <- data_port_orig %>% 
  group_by(year, port_complex, type) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T),
            landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  filter(year>=1947)

# Merge data
data <- bind_rows(data_fb1, data_port, data_fb3, data_fb4) %>% 
  arrange(year, port_complex) %>% 
  # Recode to 7-region typology
  # Harmonize port complexes
  mutate(port_complex2=recode(port_complex, 
                              "Fort Bragg"="Eureka",
                              "Bodega Bay"="San Francisco",
                              "Morro Bay"="Santa Barbara")) %>% 
  # Arrange
  select(year, port_complex2, port_complex, landings_lb, value_usd, everything())

freeR::complete(data)

g <- ggplot(data_fb1, aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  geom_bar(stat="identity")
g

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1941_2019_landings_by_port_complex.Rds"))

