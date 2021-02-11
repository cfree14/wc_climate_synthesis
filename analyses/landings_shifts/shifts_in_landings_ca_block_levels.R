
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
datadir <- "data/landings/cdfw/confidential/processed"
outputdir <- "data/landings/cdfw/confidential/processed"
plotdir <- "data/landings/cdfw/confidential/figures"

# Read 
data_orig <- readRDS("data/landings/cdfw/confidential/processed/CDFW_2000_2020_monthly_landings_by_port_block_species.Rds")

# Blocks
blocks_df <- wcfish::blocks %>% 
  sf::st_drop_geometry() %>% 
  mutate(block_id=as.character(block_id))


# All data
################################################################################

# Build data
data <- data_orig %>% 
  # Annual catch by block
  group_by(year, block_id) %>% 
  summarize(landings_lb=sum(landings_lb),
            value_usd=sum(value_usd)) %>% 
  ungroup() %>% 
  # Add block attributes
  left_join(blocks_df, by="block_id") %>% 
  # Remove unanalyzable data
  filter(!is.na(block_type) & block_type!="Offshore")


# Loop through years
cog_ts <- purrr::map_df(2000:2019, function(x){
  
  # Subste data
  sdata <- data_orig %>% 
    filter(year==x)
  
  # Calculate center of gravity 
  cog_out <- COGravity(x=sdata$block_long_dd, y=sdata$block_lat_dd, wt=sdata$landings_lb)
  
  # Organize output
  cog_df <- tibble(year=x,
                   long_dd=cog_out["COGx"],
                   long_dd_sd=cog_out["COGx.sd"],
                   lat_dd=cog_out["COGy"],
                   lat_dd_sd=cog_out["COGy.sd"])
  
})

# Line plot
#######################################

# Plot data
g <- ggplot(cog_ts, aes(x=year, y=lat_dd)) +
  # MHW rectangle
  geom_rect(mapping=aes(xmin=2013, xmax=2015, ymin=min(lat_dd), ymax=max(lat_dd)), fill="grey90", color=NA) +
  geom_line() +
  geom_point() +
  # Labels
  labs(x="Year", y="Latitude (°N)") +
  # geom_errorbar(mapping=aes(ymin=lat_dd-lat_dd_sd, ymax=lat_dd+lat_dd_sd)) +
  # Theme
  theme_bw()
g


# Map
#######################################

# Land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass="sf")
canada <- rnaturalearth::ne_countries(country="Canada", returnclass="sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass="sf")

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=usa, fill="grey90", col="grey30", lwd=0.5) +
  geom_sf(data=canada, fill="grey90", col="grey30", lwd=0.5) +
  geom_sf(data=mexico, fill="grey90", col="grey30", lwd=0.5) +
  # Plot points/lines
  geom_path(data=cog_ts, mapping=aes(x=long_dd, y=lat_dd)) +
  geom_point(data=cog_ts, mapping=aes(x=long_dd, y=lat_dd, fill=year), pch=21, size=4) +
  # Crop
  coord_sf(xlim=c(-124.5, -117), ylim=c(32, 42)) +
  # Labs
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g



# Species-level shifts
################################################################################

# Species-level
data_spp <- data_orig %>% 
  # Annual catch by block
  group_by(comm_name, sci_name, year, block_id) %>% 
  summarize(landings_lb=sum(landings_lb),
            value_usd=sum(value_usd)) %>% 
  ungroup() %>% 
  # Add block attributes
  left_join(blocks_df, by="block_id") %>% 
  # Remove unanalyzable data
  filter(!is.na(block_type) & block_type!="Offshore")

# Loop through years
cog_ts_spp <- purrr::map_df(2000:2019, function(x){
  
  # Subste data
  sdata <- data_orig %>% 
    filter(year==x)
  
  # Calculate center of gravity 
  cog_out <- COGravity(x=sdata$block_long_dd, y=sdata$block_lat_dd, wt=sdata$landings_lb)
  
  # Organize output
  cog_df <- tibble(year=x,
                   long_dd=cog_out["COGx"],
                   long_dd_sd=cog_out["COGx.sd"],
                   lat_dd=cog_out["COGy"],
                   lat_dd_sd=cog_out["COGy.sd"])
  
})









