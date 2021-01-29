
# Plot OceanAdapt distribution shift map
# dataset <- data_oa; species <- "Fuzzy crab (Acantholithodes hispidus)"; region <- "Aleutian Islands"
plot_oa_dist_shift_map <- function(dataset, species, region, base_theme, usa, mexico, canada){
  
  # Species to do
  species_do <- species
  region_do <- region
  
  # Build data
  sdata <- dataset %>% 
    # Filter
    filter(species_label==species_do & region == region_do) %>% 
    # Reduce
    select(-std_err) %>% 
    # Spread
    spread(key="metric", value="mean") %>% 
    # Rename
    rename(lat_dd="Latitude (°N)",
           long_dd="Longitude (°W)",
           depth_m="Depth (m)")
  
  # Plot data
  g <- ggplot() +
    # Plot land
    geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
    geom_sf(data=mexico, fill="grey85", col="white", size=0.1) +
    geom_sf(data=canada, fill="grey85", col="white", size=0.1) +
    # Plot center of biomass
    geom_path(data=sdata, mapping=aes(x=long_dd, y=lat_dd), color="grey40") +
    geom_point(data=sdata, mapping=aes(x=long_dd, y=lat_dd, fill=year), pch=21, size=2) +
    # Crop plot
    coord_sf(xlim = c(-180, -116), ylim = c(32, 70)) +
    # Labels
    labs(x="", y="") +
    # Legend
    scale_fill_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme 
    theme_bw() + base_theme +
    theme(legend.pos=c(0.1, 0.2),
          axis.text.y = element_text(angle = 90, hjust = 0.5))
  g
  
 
}
  