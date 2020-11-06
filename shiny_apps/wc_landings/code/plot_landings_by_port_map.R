
# Plot map of landings by port
# dataset <- data_ca; ports <- ports_ca; species_label <- "Northern anchovy (Engraulis mordax)"
plot_landings_by_port_map <- function(dataset, ports, species_label, base_theme, usa, mexico){
  
  # Species to do
  species_label_do <- species_label
  
  # Calculate recent sums 
  sdata <- dataset %>% 
    # Reduce to species of interest and last 5 years
    filter(species_label==species_label_do & year %in% 2015:2019) %>% 
    # Calculate annual landings
    group_by(year, area, port) %>% 
    summarize(landings_lb=sum(landings_lb, na.rm=T),
              value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Average of last 5 years
    group_by(area, port) %>% 
    summarize(landings_lb=mean(landings_lb, na.rm=T),
              value_usd=mean(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Add coordinates
    left_join(ports %>% select(-port_complex), by="port")
  
  # Plot data
  g <- ggplot() +
    # Plot land
    geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
    geom_sf(data=mexico, fill="grey85", col="white", size=0.1) +
    # Plot ports
    geom_point(sdata , mapping=aes(x=long_dd, lat_dd, color=area, size=landings_lb/1e3)) +
    ggrepel::geom_text_repel(sdata, mapping=aes(x=long_dd, lat_dd, label=port, color=area, size=landings_lb/1e3), show.legend = F) +
    # Labels
    labs(x="", y="", subtitle="Average landings by port from 2015-2019", title=species_label_do) +
    # Crop plot
    coord_sf(xlim = c(-125.5, -116.6), ylim = c(32, 42)) +
    # Legend
    scale_color_discrete(name="Port complex\n(north to south)") +
    scale_size_continuous(name="Landings (1000s lbs)", range=c(4,8)) +
    # Add Mendoncino-Sonoma county line
    # geom_hline(yintercept=38.77, size=0.5) + 
    theme_bw() + base_theme +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
  g
  
}
  