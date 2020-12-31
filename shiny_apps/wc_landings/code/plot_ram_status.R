
# Plot RAM stock status
# dataset <- data_ram_status; species <- "Alaska plaice (Pleuronectes quadrituberculatus)"; stock <- "Bering Sea and Aleutian Islands"
plot_ram_status <- function(dataset, species, stock, base_theme){
  
  # Species to do
  species_do <- species
  area_do <- stock
  
  # Subset data
  sdata <- dataset %>% 
    filter(species_label == species_do & area == area_do)
  
  # Plot status
  g1 <- ggplot(sdata, aes(x=year, y=bbmsy)) +
    geom_line() +
    # Lines
    geom_hline(yintercept = 1.0, linetype="dashed") +
    geom_hline(yintercept = 0.5, linetype="dotted") +
    # Limits
    lims(y=c(0,NA)) +
    scale_x_continuous(lim=c(NA, 2020), breaks=seq(1800,2020,10)) +
    # Labels
    labs(x="", y=expression("B/B"["MSY"])) +
    # Theme
    theme_bw() + base_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g1
  
  # Plot status
  g2 <- ggplot(sdata, aes(x=year, y=uumsy)) +
    geom_line() +
    # Lines
    geom_hline(yintercept = 1.0, linetype="dashed") +
    # Limits
    lims(y=c(0,NA)) +
    scale_x_continuous(lim=c(NA, 2020), breaks=seq(1800,2020,10)) +
    # Labels
    labs(x="", y=expression("U/U"["MSY"])) +
    # Theme
    theme_bw() + base_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g2
  
  # Plot status
  g3 <- ggplot(sdata, aes(x=bbmsy, y=uumsy)) +
    geom_path(color="grey40", alpha=0.3) +
    geom_point(mapping=aes(color=year)) +
    # Lines
    geom_hline(yintercept = 1.0, linetype="dashed") +
    geom_vline(xintercept = 1.0, linetype="dashed") +
    # Limits
    lims(x=c(0,NA), y=c(0,NA)) +
    # Labels
    labs(x=expression("B/B"["MSY"]), y=expression("U/U"["MSY"])) +
    # Legend
    scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Oranges")) +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme
  g3
  
  # Merge plots
  g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3, widths=c(0.3, 0.3, 0.4))
  g
 
}
  