
# Plot OceanAdapt distribution shift
# dataset <- data_oa; species <- "Acantholithodes hispidus"
plot_oa_dist_shift <- function(dataset, species, base_theme){
  
  # Species to do
  species_do <- species
  
  # Subset data
  sdata <- dataset %>% 
    filter(species==species_do)
  
  # Plot status
  g <- ggplot(sdata, aes(x=year, y=mean, color=survey, group=survey)) +
    facet_grid(metric~region, scales="free_y") +
    geom_ribbon(mapping=aes(ymin=mean-std_err, ymax=mean+std_err, fill=survey), alpha=0.2, color=NA, show.legend=F) +
    geom_line(lwd=1) +
    # Limits
    scale_x_continuous(lim=c(1977,2020), breaks=seq(1980, 2020, 10)) +
    # Labels
    labs(x="", y="") +
    scale_fill_discrete(name="", drop=F) +
    scale_color_discrete(name="", drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "bottom")
  g
  
 
}
  