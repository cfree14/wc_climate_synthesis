
# Plot RAM stock status
# dataset <- data_ram_sp; species <- "Arrowtooth flounder (Atheresthes stomias)"; stock <- "Pacific Coast"
plot_ram_production <- function(dataset, species, stock, base_theme){
  
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
  
  # Plot catch
  g2 <- ggplot(sdata, aes(x=year, y=c/1e3)) +
    geom_line() +
    # Limits
    lims(y=c(0,NA)) +
    scale_x_continuous(lim=c(NA, 2020), breaks=seq(1800,2020,10)) +
    # Labels
    labs(x="", y="Catch (1000s mt)") +
    # Theme
    theme_bw() + base_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g2
  
  # Plot surplus production
  g3 <- ggplot(sdata, aes(x=year, y=sp/1e3)) +
    geom_line() +
    # Lines
    geom_hline(yintercept = 0, linetype="dotted", color="grey30") +
    # Limits
    scale_x_continuous(lim=c(NA, 2020), breaks=seq(1800,2020,10)) +
    # Labels
    labs(x="", y="Surplus production (1000s mt)") +
    # Theme
    theme_bw() + base_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g3
  
  # Plot production relationship
  g4 <- ggplot(sdata, aes(x=b/1e3, y=sp/1e3, fill=year)) +
    geom_point(pch=21, size=2) +
    # Lines
    geom_hline(yintercept = 0, linetype="dotted", color="grey30") +
    # Limits
    lims(x=c(0,NA), y=c(0,NA)) +
    # Labels
    labs(x="Biomass (1000s)", y="Surplus production (1000s mt)") +
    # Legend
    scale_fill_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Oranges")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme
  g4
  
  # Merge plots
  g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=3)
  g
 
}
  