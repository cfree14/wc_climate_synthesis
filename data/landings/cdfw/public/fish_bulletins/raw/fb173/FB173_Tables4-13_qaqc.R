

# Turn off scientific notation
options(scipen=999)

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/cdfw/public/fish_bulletins/fb173/raw"
outputdir <- "data/landings/cdfw/public/fish_bulletins/fb173/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/fb173/figures"

# Read data
data_fb_orig <- read.csv(file.path(outputdir, "CDFW_1977_1986_landings_by_port_complex.csv"), as.is=T)

# Load SWFSC data
load("data/landings/swfsc/processed/1928_2002_CA_landings_data_swfsc_erddap.Rdata")
data_sw_orig <- ll_yr
rm(ll_mon, sl_mon, sl_yr, ll_yr)


# Compare 1984 data
################################################################################

# Prepare for merge
data_fb <- data_fb_orig %>% 
  filter(year==1984 & type=="Landings") %>% 
  select(year, port_complex, species, landings_lb) %>% 
  mutate(species=toupper(species))

# Prepar for merge
data_sw <- data_sw_orig %>%
  filter(year==1984) %>% 
  rename(species=comm_name_orig) %>% 
  select(year, port_complex, species, landings_lb) %>% 
  mutate(species=toupper(species))

# Merge data
data <- data_sw %>% 
  left_join(data_fb, by=c("year", "port_complex", "species")) %>% 
  rename(landings_lb_sw=landings_lb.x, landings_lb_fb=landings_lb.y) %>% 
  mutate(diff=landings_lb_sw-landings_lb_fb)

ggplot(data, aes(x=landings_lb_sw/1e6, y=landings_lb_fb/1e6, label=species)) +
  geom_point()



