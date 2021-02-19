
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

# Read FB 44-170 data (1934-1976)
data_fb1_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1934_1976_nvessels_by_length.Rds")

# Read FB 57 data (1936-1938)
data_fb2_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1936_1938_nvessels_by_port.Rds")

# Read FB 181 data (1976-1999)
data_fb3_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table3_licenced_fishermen_vessels.csv", as.is=T)

# Read website data (2000-2020)
data_web <- read.csv("data/landings/cdfw/public/website_licenses/commercial/processed/CDFW_1970_2020_n_licensed_comm_vessels.csv", as.is=T)



# Build annual totals by group
################################################################################

# Format FB 44-170 data (1934-1976)
#############################################

# Geographic resolutions
# Port complex: 1934-1956
# Statewide: 1957-1976

# Length class systems
# 1934-1947: 15-ft bins (85+ max)
# 1948-1969: 15-ft bins (100+ max)
# 1970-1976: 5-ft bins

# Format 1934-1976
data_fb1 <- data_fb1_orig %>% 
  # Rename columns
  rename(season=year, length_class=length_class_orig, table=table_name) %>% 
  # Format table
  mutate(table=gsub("Table", "Table ", table)) %>% 
  # Format number of vessels
  # I inspected the raw data and confirmed that blank entries are true zeros
  mutate(nvessels=ifelse(is.na(nvessels), 0, nvessels)) %>% 
  # Format season
  mutate(season=gsub(" ", "", season),
         season=recode(season, 
                       "1934"="1934-35",
                       "1935"="1935-36"),
         year=substr(season, 1, 4) %>% as.numeric()) %>% 
  # Format region
  mutate(region=stringr::str_trim(region),
         region=recode(region, 
                       "statewide"="Statewide",
                       "Del Norte Eureka"="Del Norte/Eureka")) %>% 
  # Add length class system
  mutate(length_class_system=cut(year, breaks=c(-Inf, 1947, 1969, Inf),
         labels=c("1934-1947: 15-ft bins (85+ max)", 
                  "1948-1969: 15-ft bins (100+ max)",
                  "1970-1976: 5-ft bins (181+ max)"))) %>% 
  # Add length class floor
  mutate(length_class_floor=length_class %>% 
           gsub("(^[^-]+)-.*", "\\1", .) %>% gsub("\\+", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(source, table, season, year, region_type, region, 
         length_class_system, length_class_group, length_class, length_class_floor,
         nvessels, everything()) %>% 
  arrange(season, year, region, length_class_group, length_class)

# Inspect data
freeR::complete(data_fb1) # must all be 0
table(data_fb1$source)
table(data_fb1$table)
range(data_fb1$year)
table(data_fb1$season)
table(data_fb1$year)
table(data_fb1$region_type)
table(data_fb1$region)
table(data_fb1$length_class_system)
table(data_fb1$length_class_group)
table(data_fb1$length_class)

# Plot data to figure out length class systems
g <- ggplot(data_fb1, aes(x=year, y=length_class, fill=nvessels)) +
  geom_raster() +
  labs(x="Year", y="Length class (ft)") +
  scale_fill_gradientn(name="Number of vessels", 
                        colors=RColorBrewer::brewer.pal(9, "Oranges")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw()
g

# Format data for plotting
data_fb1_plot <- data_fb1 %>% 
  group_by(source, season, year, region_type,
           length_class_system, length_class_group, length_class, length_class_floor) %>% 
  summarize(nvessels=sum(nvessels, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(length_class_system=gsub(": ", ":\n", length_class_system))

# Plot data
g <- ggplot(data_fb1_plot, aes(x=year, y=nvessels, fill=length_class_floor)) +
  geom_bar(stat="identity", color='grey10', lwd=0.1) +
  facet_grid(~length_class_system, scales="free_x", space="free_x") +
  # Labels
  labs(x="Year", y="Number of vessels") +
  scale_x_continuous(breaks=seq(1930,1980,5)) +
  # Legend
  scale_fill_gradientn(name="Length class (ft)", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
g

# Export data
write.csv(data_fb1, file=file.path(outdir, "CDFW_1934_1976_n_comm_vessels_by_length_class.csv"), row.names = F)
saveRDS(data_fb1, file=file.path(outdir, "CDFW_1934_1976_n_comm_vessels_by_length_class.Rds"))


# Format FB 57 data (1936-1938)
#############################################

# Format 1934-1976
data_fb2 <- data_fb2_orig %>% 
  # Rename
  rename(table=table_name) %>% 
  mutate(table=gsub("Table", "Table ", table)) %>% 
  # Format season/year
  rename(season=year) %>% 
  mutate(year=substr(season, 1, 4) %>% as.numeric()) %>% 
  # Format region
  mutate(region=recode(region, "Del Norte Eureka"= "Del Norte/Eureka")) %>% 
  # Arrange
  select(source, table, season, year, region_type, region, nvessels, everything()) %>% 
  arrange(season, year, region)


# Format FB 181 data (1976-1999)
#############################################

# Format 1976-1999
data_fb3 <- data_fb3_orig %>% 
  # Reduce
  select(source, year, n_vessels) %>% 
  # Rename
  rename(season=year, nvessels=n_vessels) %>% 
  # Format source/table
  mutate(source="FB 181",
         table="Table 3") %>% 
  # Format year
  mutate(year=substr(season, 1, 4) %>% as.numeric()) %>% 
  # Format number of vessels
  mutate(nvessels=nvessels %>% gsub(",", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(source, table, season, year, nvessels, everything()) %>% 
  arrange(season, year)



# Build statewide totals
################################################################################

# Format 1934-1976
data_fb1_tots <- data_fb1 %>% 
  group_by(source, table, season, year) %>% 
  summarize(nvessels=sum(nvessels)) %>% 
  ungroup() %>% 
  arrange(year) %>% 
  filter(year!=1976)

# Format 1936-1938
data_fb2_tots <- data_fb2 %>% 
  group_by(source, table, season, year) %>% 
  summarize(nvessels=sum(nvessels)) %>% 
  ungroup() %>% 
  arrange(year)

# Format 1976-1999
data_fb3_tots <- data_fb3 %>% 
  # Select columns of interest
  select(source, table, season, year, nvessels)

# Format 2000-2020
data_web_tots <- data_web %>% 
  # Reduce
  filter(year>=2000) %>% 
  select(-c(nvessels_r, nvessels_nr)) %>% 
  # Add columns
  mutate(source="CDFW 2021",
         table="NA",
         season=paste(year, year+1, sep="-")) %>% 
  # Arrange
  select(source, table, season, year, nvessels)

# Merge
data_tots <- bind_rows(data_fb1_tots, data_fb2_tots, data_fb3_tots, data_web_tots) %>% 
  arrange(year)

# Plot
g <- ggplot(data_tots, aes(x=year, y=nvessels)) +
  geom_line() +
  theme_bw()
g

# Export data
write.csv(data_tots, file=file.path(outdir, "CDFW_1934_2020_n_comm_vessels.csv"), row.names = F)


# Build totals by port complex
################################################################################

# Build totals by port (1934-1976, but missing 1936-1938)
data_fb1_tots_port <- data_fb1 %>% 
  filter(region_type=="port complex") %>% 
  group_by(source, table, season, year, region) %>% 
  summarize(nvessels=sum(nvessels)) %>% 
  ungroup() %>% 
  arrange(season, year, region)

# Build totals by port (1936-1938)
data_fb2_tots_port <- data_fb2 %>% 
  select(source, table, season, year, region, nvessels)

# Merge
data_tots_port <- bind_rows(data_fb1_tots_port, data_fb2_tots_port) %>% 
  # Format port complex
  rename(port_complex=region) %>% 
  mutate(port_complex=factor(port_complex,
                             levels=c("OR, WA, AK", "Del Norte/Eureka", "Del Norte", "Eureka", "Sacramento", "San Francisco", 
                                      "Monterey", "Santa Barbara", "Los Angeles", "San Diego", "Mexico", "Other registry"))) %>% 
  # Arrange
  arrange(season, year, port_complex)
  
# Plot
g <-  ggplot(data_tots_port, aes(x=year, y=port_complex, fill=nvessels)) +
  geom_raster() +
  labs(x="", y="") +
  scale_x_continuous(breaks=1934:1956) +
  theme_bw()
g

# Plot
g <- ggplot(data_tots_port, aes(x=year, y=nvessels, fill=port_complex)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of vessels") +
  scale_x_continuous(breaks=seq(1930, 1960, 5)) +
  # Legend
  scale_fill_discrete(name="Port complex\n(from north to south)") +
  # Theme
  theme_bw()
g

# Export data
write.csv(data_tots_port, file=file.path(outdir, "CDFW_1934_1956_n_comm_vessels_by_port_complex.csv"), row.names = F)
saveRDS(data_tots_port, file=file.path(outdir, "CDFW_1934_1956_n_comm_vessels_by_port_complex.Rds"))

