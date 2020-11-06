
# Plot landings time series by area
# dataset <- data_ca; species_label <- "Northern anchovy (Engraulis mordax)"
plot_rank_scatterplot <- function(dataset, species_label, base_theme){
  
  # Species to do
  species_label_do <- species_label
  
  # Calculate recent sums 
  sdata <- dataset %>% 
    # Calculate annual landings
    group_by(year, taxa_group1, comm_name, sci_name, species_label) %>% 
    summarize(landings_lb=sum(landings_lb, na.rm=T),
              value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Reduce to last 5 years
    filter(year %in% 2015:2019) %>% 
    # Average of last 5 years
    group_by(taxa_group1, comm_name, sci_name, species_label) %>% 
    summarize(landings_lb=mean(landings_lb, na.rm=T),
              value_usd=mean(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Remove 0 values
    filter(value_usd>0)
  
  # Number of species
  n_spp <- nrow(sdata)
  
  # Species values
  landings_spp <- sdata$landings_lb[sdata$species_label==species_label_do]
  value_spp <- sdata$value_usd[sdata$species_label==species_label_do]
  
  # plot data
  g <- ggplot(sdata, aes(x=landings_lb, y=value_usd, fill=taxa_group1)) +
    # Line segments
    geom_segment(mapping=aes(x=landings_spp, y=1, xend=landings_spp, yend=value_spp), inherit.aes = F, linetype="dashed", color="grey30") +
    geom_segment(mapping=aes(x=1, y=value_spp, xend=landings_spp, yend=value_spp), inherit.aes = F, linetype="dashed", color="grey30") +
    geom_point(size=4, pch=21, alpha=0.7) +
    # Text
    annotate(geom="text", label=paste(n_spp, "species"), hjust=0, x=1e0, y=1e8, size=5) +
    # Limits
    scale_x_continuous(trans="log10", breaks=10^c(0:10), lim=c(1, NA)) +
    scale_y_continuous(trans="log10", breaks=10^(0:10), lim=c(1, NA)) +
    # Labels
    labs(x="Landings (lbs)", y="Value (USD)", subtitle="Mean landings/value from 2015-2019", title=species_label_do) +
    # Legend
    scale_fill_discrete(name="Taxanomic group") +
    # Theme
    theme_bw() + base_theme
  g
  
}
  