
# Plot landings time series by area
# dataset <- data_ca; species_label <- "Northern anchovy (Engraulis mordax)"
plot_landings_ts_by_area_prop <- function(dataset, species_label, base_theme){
  
  # Species to do
  species_label_do <- species_label
  
  # Build data
  data_plot <- dataset %>% 
    # Filter to species of interest
    filter(species_label==species_label_do) %>% 
    # Calculate annual totals
    group_by(year, area) %>% 
    summarize(landings_lb=sum(landings_lb, na.rm=T)/1e6,
              value_usd=sum(value_usd, na.rm=T)/1e6) %>% 
    ungroup() %>% 
    # Reshape data
    gather(key="metric", value="value", 3:4) %>% 
    mutate(metric=recode_factor(metric, 
                                "landings_lb"="Landings",
                                "value_usd"="Value")) %>% 
    # Expand to include missing values
    complete(year, area, metric, fill=list(value=0)) %>% 
    # Calculate proportion
    group_by(year, metric) %>% 
    mutate(prop=value/sum(value)) %>% 
    ungroup()
  
  # Plot data
  g <- ggplot(data_plot, aes(x=year, y=prop, fill=area)) +
    facet_wrap(~metric, scales="free_y") +
    geom_area() +
    # Labels
    labs(x="", y="Proportion", subtitle="Proportion of landings coming from each port complex", title=species_label_do) +
    scale_x_continuous(breaks=seq(2000,2020,5), lim=c(2000,2020)) +
    # Legend
    scale_fill_discrete(name="Port complex\n(north to south)") +
    # Theme
    theme_bw() + base_theme
  g
  
}
  