
# Plot landings time series by area
# dataset <- data_ca; noaa <- data_noaa_orig; species_label <- "Northern anchovy (Engraulis mordax)"
plot_landings_ts_by_area <- function(dataset, noaa, species_label, base_theme){
  
  # Species to do
  species_label_do <- species_label
  
  # Extract species
  species_do <- gsub("[\\(\\)]", "", regmatches(species_label_do, gregexpr("\\(.*?\\)", species_label_do))[[1]])
  
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
                                "landings_lb"="Landings (millions of lbs)",
                                "value_usd"="Value (millions of dollars)")) %>% 
    # Expand to include missing values
    complete(year, area, metric, fill=list(value=0))
  
  # Prepare NOAA data
  noaa_plot <- noaa %>% 
    filter(state=="California" & sci_name==species_do) %>% 
    select(year, landings_lb, value_usd) %>% 
    mutate(landings_lb=landings_lb/1e6,
           value_usd=value_usd/1e6) %>% 
    arrange(year) %>% 
    gather(key="metric", value="value", 2:3) %>% 
    mutate(metric=recode_factor(metric, 
                                "landings_lb"="Landings (millions of lbs)",
                                "value_usd"="Value (millions of dollars)"))
  
  # Plot data
  g <- ggplot(data_plot, aes(x=year, y=value, fill=area)) +
    facet_wrap(~metric, scales="free_y") +
    geom_area() +
    # Plot NOAA line
    geom_line(noaa_plot, mapping=aes(x=year, y=value), inherit.aes = F, col="black") +
    # Labels
    labs(x="", y="", subtitle="Landings and value over time", title=species_label_do) +
    scale_x_continuous(breaks=seq(1920,2020,10), lim=c(NA,2020)) +
    # Legend
    scale_fill_discrete(name="Port complex\n(north to south)") +
    # Theme
    theme_bw() + base_theme 
  g
  
}
  