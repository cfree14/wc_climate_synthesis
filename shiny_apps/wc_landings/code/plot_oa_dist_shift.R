
# Plot OceanAdapt distribution shift
# dataset <- data_oa; species <- "Acantholithodes hispidus"; region <- "West Coast"
plot_oa_dist_shift <- function(dataset, species, region, base_theme){
  
  # Species to do
  species_do <- species
  region_do <- region
  
  # Subset data
  sdata <- dataset %>% 
    filter(species_label==species_do & region == region_do)
  
  # Label key
  label_key <- sdata %>% 
    mutate(upper=mean+std_err,
           lower=mean-std_err) %>% 
    group_by(metric) %>% 
    summarize(ymin=min(lower, na.rm=T),
              ymax=max(upper, na.rm=T)) %>% 
    gather(key="type", value="yvalue", 2:3) %>% 
    mutate(label=paste(metric, type, sep="-"),
           label=recode(label, 
                        "Latitude (°N)-ymin"="Southward",  
                        "Longitude (°W)-ymin"="More offshore",   
                        "Depth (m)-ymin"="Shallower",       
                        "Latitude (°N)-ymax"="Northward",    
                        "Longitude (°W)-ymax"="More inshore",
                        "Depth (m)-ymax"="Deeper"))
  
  # Legend position
  if(region=="West Coast"){
    legend.pos <- "bottom"
  }else{
    legend.pos <- "none"
  }

  # Plot status
  g <- ggplot(sdata, aes(x=year, y=mean, color=survey, group=survey)) +
    facet_wrap(~metric, scales="free_y") +
    geom_ribbon(mapping=aes(ymin=mean-std_err, ymax=mean+std_err, fill=survey), 
                alpha=0.2, color=NA, show.legend=F) +
    geom_line(lwd=1) +
    # Limits
    scale_x_continuous(lim=c(1977,2020), breaks=seq(1980, 2020, 10)) +
    # Labels
    labs(x="", y="") +
    scale_fill_discrete(name="", drop=F) +
    scale_color_discrete(name="", drop=F) +
    # Add text labels
    geom_text(data=label_key, mapping=aes(x=1977, y=yvalue, label=label), inherit.aes=F, hjust=0, color="grey40", size=5) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = legend.pos)
  g
  
 
}
  