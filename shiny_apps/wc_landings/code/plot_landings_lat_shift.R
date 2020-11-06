
# Plot map of landings by port
# dataset <- data_ca; ports <- ports_ca; species_label <- "Northern anchovy (Engraulis mordax)"
plot_landings_lat_shift <- function(dataset, ports, species_label, base_theme){
  
  # Species to do
  species_label_do <- species_label
  
  # Calculate weighted-latitudes
  sdata <- dataset %>% 
    # Reduce to species of interest
    filter(species_label==species_label_do) %>% 
    # Calculate annual landings
    group_by(year, area, port) %>% 
    summarize(landings_lb=sum(landings_lb, na.rm=T),
              value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Gather
    gather(key="metric", value="value", 4:5) %>% 
    mutate(metric=recode(metric, "landings_lb"="Landings", "value_usd"="Value")) %>% 
    # Add coordinates
    left_join(ports %>% select(-port_complex), by="port") %>% 
    # Caluclate weighted latitudes
    group_by(year, metric) %>% 
    mutate(lat_dd_wt=weighted.mean(x=lat_dd, w=value)) %>% 
    ungroup()

  # Plot data
  g <- ggplot(sdata, aes(x=year, y=lat_dd_wt, color=metric, fill=metric)) +
    geom_point() +
    geom_smooth(method="gam", show.legend = F) +
    # Labels
    labs(x="", y="Weighted latitude (°N)", subtitle="Landing/value-weighted latitude of port-level landings", title=species_label_do) +
    scale_x_continuous(breaks=seq(2000,2020,5), lim=c(2000,2020)) +
    # Legends
    scale_color_discrete("Weighted by:") +
    scale_fill_discrete(guide="none") +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "bottom")
  g
  
}
  