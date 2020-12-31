
# Plot RAM stock status
# dataset <- data_ram_sr; species <- "Arrowtooth flounder (Atheresthes stomias)"; stock <- "Pacific Coast"
plot_ram_recruitment <- function(dataset, species, stock, base_theme){
  
  # Species to do
  species_do <- species
  area_do <- stock
  
  # Subset data
  sdata <- dataset %>% 
    filter(species_label == species_do & area == area_do)
  
  # Plot biomass
  g1 <- ggplot(sdata, aes(x=year, y=b/1e3)) +
    geom_line() +
    # Limits
    lims(y=c(0,NA)) +
    scale_x_continuous(lim=c(NA, 2020), breaks=seq(1800,2020,10)) +
    # Labels
    labs(x="", y="Biomass (1000s)") +
    # Theme
    theme_bw() + base_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g1
  
  # Plot recruitment
  g2 <- ggplot(sdata, aes(x=year, y=r/1e6)) +
    geom_line() +
    # Limits
    lims(y=c(0,NA)) +
    scale_x_continuous(lim=c(NA, 2020), breaks=seq(1800,2020,10)) +
    # Labels
    labs(x="", y="Recruitment (millions)") +
    # Theme
    theme_bw() + base_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g2
  
  # Plot stock-recruitment relatioship
  g3 <- ggplot(sdata, aes(x=b/1e3, y=r/1e6, fill=year)) +
    geom_point(pch=21, size=2) +
    # Limits
    lims(x=c(0,NA), y=c(0,NA)) +
    # Labels
    labs(x="Biomass (1000s)", y="Recruitment (millions)") +
    # Legend
    scale_fill_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Oranges")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme
  g3
  
  # Merge plots
  g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3, widths=c(0.3, 0.3, 0.4))
  g
 
}
  