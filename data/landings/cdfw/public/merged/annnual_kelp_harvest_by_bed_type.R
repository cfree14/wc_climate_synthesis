
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

# Read FB 170 (1916-1976) data
data_fb_orig <- readxl::read_excel("data/landings/cdfw/public/fish_bulletins/raw/fb170/raw/AppendixIII.xlsx")

# Read NOAA (1950-2003) data
data_noaa_orig <- readRDS("data/landings/noaa/processed/NOAA_1950_2019_usa_landings_by_state_species.Rds") %>% 
  filter(state=="California" & sci_name=="Macrocystis spp.") %>% 
  # Summarize
  group_by(year) %>% 
  summarize(harvest_lb=sum(landings_lb),
            harvest_t=harvest_lb/2000)

# Read CDFW (1999-2019) data
# DataThief from: https://wildlife.ca.gov/Conservation/Marine/Kelp/Commercial-Harvest
data_cdfw_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1999_2019_kelp_harvest_data_thief.csv", as.is=T)


# Format data
################################################################################

# Format data
data1 <- data_fb_orig %>% 
  # Remove check
  select(-total_check) %>% 
  # Add source info
  mutate(source="FB 170",
         table="Appendix III") %>% 
  select(source, table, year, everything())

# Add on
data_add <- tibble(source="FB 170",
                   table="Appendix III",
                   year=1921:1930)

# Add add on
data2 <- bind_rows(data1, data_add) %>% 
  arrange(year)


# Export data
write.csv(data2, file=file.path(outdir, "CDFW_1916_1976_annual_kelp_harvest_by_bed_type.csv"), row.names = F)


# Plot data
################################################################################

# Reshape
data_plot <- data2 %>% 
  # Reshape
  select(-total_t) %>% 
  gather(key="bed_type", value="harvest_t", 4:5) %>% 
  # Add 0s for NAs for plotting
  mutate(harvest_t=ifelse(is.na(harvest_t), 0, harvest_t)) %>% 
  # Recode
  mutate(bed_type=recode(bed_type, 
                         "open_bed_t"="Open",
                         "leased_bed_t"="Leased"))

# Base theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.size=unit(0.5, units="cm"))

# Plot data
g <- ggplot(data_plot, aes(x=year, y=harvest_t/1000, fill=bed_type)) +
  geom_area(na.rm = T) +
  # Labels
  labs(x="Year", y="Kelp harvest\n(1000s of tons, wet weight)") +
  scale_fill_discrete(name="Bed type") +
  scale_x_continuous(breaks=seq(1920,1970, 10)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.85,0.8))
g



# Build totals dataset
################################################################################

# 1916-1976 (FB 170)
data_fb_tots <- data2 %>% 
  mutate(type="Reported") %>% 
  select(source, year, type, total_t) %>% 
  rename(harvest_t=total_t)

# 1977-2003 (NOAA)
data_noaa_tots <- data_noaa_orig %>% 
  filter(year>1976) %>% 
  mutate(source="NOAA FOSS",
         type="Reported") %>% 
  select(source, year, type, harvest_t)

# 2004-2019 (CDFW website)
data_cdfw_tots <- data_cdfw_orig %>% 
  filter(year>2003) %>% 
  mutate(source="CDFW website",
         type="Estimated") %>% 
  select(source, year, type, harvest_t)

# Merge
data_tots <- bind_rows(data_fb_tots, data_noaa_tots, data_cdfw_tots) %>% 
  # Add units
  mutate(harvest_lb=harvest_t*2000,
         harvest_kg=measurements::conv_unit(harvest_lb, "lbs", "kg"),
         harvest_mt=harvest_kg/1000) %>% 
  # Arrange
  select(source, year, type, harvest_lb, harvest_t, harvest_kg, harvest_mt) %>% 
  arrange(year)

# Plot 
g <- ggplot(data_tots, aes(x=year, y=harvest_t/1000)) +
  geom_bar(stat="identity") +
  labs(x="year", y="Harvest (1000s wet tons)") +
  theme_bw()
g  

# Export data
write.csv(data_tots, file=file.path(outdir, "CDFW_1916_2019_annual_kelp_harvest.csv"), row.names = F)
  
  
