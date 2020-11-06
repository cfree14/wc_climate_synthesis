
# Plot landings time series by area
# dataset <- data_ca; species_label <- "Northern anchovy (Engraulis mordax)"
plot_rank_by_species <- function(dataset, species_label, base_theme){
  
  # Species to do
  species_label_do <- species_label
  
  # Calculate state-wide ranks
  ranks_ca <- dataset %>% 
    # Calculate annual sums
    group_by(year, comm_name, sci_name, species_label) %>% 
    summarize(landings_lbs=sum(landings_lb, na.rm=T),
              value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Calculate annual ranks
    arrange(year, desc(landings_lbs)) %>% 
    group_by(year) %>% 
    mutate(landings_rank=rank(-landings_lbs),
           value_rank=rank(-value_usd)) %>% 
    ungroup() %>% 
    # Subset to species of interest
    filter(species_label==species_label_do) %>% 
    # Reshape for plotting
    select(-c(landings_lbs, value_usd)) %>% 
    gather(key="metric", value="rank", 5:6) %>% 
    mutate(metric=recode(metric, 
                         "landings_rank"="Landings",
                         "value_rank"="Value"))
  
  # Calculate area-level ranks
  ranks_area <- dataset %>% 
    # Calculate annual sums
    group_by(year, area, comm_name, sci_name, species_label) %>% 
    summarize(landings_lbs=sum(landings_lb, na.rm=T),
              value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Calculate annual ranks
    arrange(year, area, desc(landings_lbs)) %>% 
    group_by(year, area) %>% 
    mutate(landings_rank=rank(-landings_lbs),
           value_rank=rank(-value_usd)) %>% 
    ungroup() %>% 
    # Subset to species of interest
    filter(species_label==species_label_do) %>% 
    # Reshape for plotting
    select(-c(landings_lbs, value_usd)) %>% 
    gather(key="metric", value="rank", 6:7) %>% 
    mutate(metric=recode(metric, 
                         "landings_rank"="Landings",
                         "value_rank"="Value"))
  
  # Plot ranks
  g <- ggplot(ranks_area, aes(x=year, y=rank, color=area)) +
    facet_wrap(~metric) +
    geom_line(alpha=0.5) +
    # State-wide
    geom_line(ranks_ca, mapping=aes(x=year, y=rank), inherit.aes=F, color="black", lwd=1.2) +
    # Limits
    lims(y=c(1,NA)) +
    # Labels
    labs(x="", y="Rank among\ncommercial species", subtitle="Importance rank state-wide and among port complexes", title=species_label_do) +
    scale_x_continuous(breaks=seq(2000,2020,5), lim=c(2000,2020)) +
    # Legend
    scale_color_discrete(name="Port complex\n(north to south)") +
    # Theme
    theme_bw() + base_theme 
  g

  
}
  