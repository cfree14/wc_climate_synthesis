
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(corrgram)
library(tidyverse)

# Directories
envidir <- "data/environmental/indices/processed"
pacfindir <- "data/landings/pacfin/processed"
plotdir <- "analyses/revenues/figures"

# Read data
indices_orig <- readRDS(file=file.path(envidir, "indices_coastwide.Rds"))
landings_orig <- readRDS(file=file.path(pacfindir, "PACFIN_ALL001_1980_2021_all_species_landings.Rds"))


# Function to make plots
################################################################################

# Plot
index <- "ENSO"
plot_correlation <- function(index){
  
  # Prep data
  ###############################
  
  # Build indices
  index_do <- index
  indices <- indices_orig %>% 
    # Index of interest
    filter(index_abbrev==index_do) %>% 
    # Average annual value
    group_by(year) %>% 
    summarize(index_avg=mean(value, na.rm=T))
  
  # Axis title
  index_title <- index_do
  index_title <- indices_orig %>% filter(index_abbrev==index_do) %>% pull(index) %>% unique()
  
  # Landings overall
  landings_tot <- landings_orig %>% 
    # Eliminate At-Sea
    filter(state!="At-Sea") %>% 
    # Total by year
    group_by(year) %>% 
    summarise(value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Add indices
    left_join(indices) %>% 
    # Eliminate NAs
    na.omit() %>% 
    # Add state
    mutate(state="Coastwide")
  
  # Landings by state
  landings_state <- landings_orig %>% 
    # Eliminate At-Sea
    filter(state!="At-Sea") %>% 
    # Total by year
    group_by(state, year) %>% 
    summarise(value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Add indices
    left_join(indices) %>% 
    # Eliminate NAs
    na.omit()
  
  # Merge total and state
  landings_state2 <- bind_rows(landings_tot, landings_state) %>% 
    mutate(state=factor(state, levels=c("Coastwide", "California", "Oregon", "Washington")))
  
  # Landings by state and management group
  landings_mgmt <- landings_orig %>% 
    # Eliminate At-Sea
    filter(state!="At-Sea") %>% 
    # Total by year
    group_by(state, mgmt_group_code, year) %>% 
    summarise(value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    # Add indices
    left_join(indices) %>% 
    # Eliminate NAs
    na.omit()
  
  # Plot data
  ###############################
  
  # Theme
  my_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     plot.title=element_text(size=9),
                     plot.tag=element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot coastwide
  g1 <- ggplot(landings_tot %>% filter(year>=2006), aes(x=index_avg, y=value_usd/1e6)) +
    # Regression
    geom_smooth(method="lm", fill="grey70", color="black", size=0.4) +
    # Points
    geom_point() +
    # Reference line
    geom_vline(xintercept=0, linetype="dotted") +
    # Labels
    labs(x=index_do, y="Revenues (USD millions)", tag="A", title=index_title) +
    # Theme
    theme_bw() + my_theme 
  g1
  
  # Plot statewide
  g2 <- ggplot(landings_state %>% filter(year>=2006), aes(x=index_avg, y=value_usd/1e6)) +
    facet_wrap(~state, nrow=1) +
    # Regression
    geom_smooth(method="lm", fill="grey70", color="black", size=0.4) +
    # Points
    geom_point() +
    # Reference line
    geom_vline(xintercept=0, linetype="dotted") +
    # Labels
    labs(x=index_do, y="Revenues (USD millions)", tag="B") +
    # Theme
    theme_bw() + my_theme 
  g2
  
  # Plot statewide
  g12 <- ggplot(landings_state2 %>% filter(year>=2006), aes(x=index_avg, y=value_usd/1e6)) +
    facet_wrap(~state, nrow=1) +
    # Regression
    geom_smooth(method="lm", fill="grey70", color="black", size=0.4) +
    # Points
    geom_point() +
    # Reference line
    geom_vline(xintercept=0, linetype="dotted") +
    # Labels
    labs(x=index_do, y="Revenues (USD millions)", tag="A", title=index_title) +
    # Theme
    theme_bw() + my_theme 
  g12
  
  # Plot by state+mgmt group
  g3 <- ggplot(landings_mgmt %>% filter(year>=2006), aes(x=index_avg, y=value_usd/1e6)) +
    facet_grid(mgmt_group_code~state, scales="free_y") +
    # Regression
    geom_smooth(method="lm", fill="grey70", color="black", size=0.4) +
    # Points
    geom_point() +
    # Reference line
    geom_vline(xintercept=0, linetype="dotted") +
    # Labels
    labs(x=index_do, y="Revenues (USD millions)", tag="B") +
    # Theme
    theme_bw() +my_theme 
  g3
  
  # Merge plots
  # layout_matrix <- matrix(data=c(1,2,2,2,
  #                                3,3,3,3), byrow=T, nrow=2)
  # g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, heights=c(0.25, 0.8))
  # g
  
  # Merge plots
  g <- gridExtra::grid.arrange(g12, g3, heights=c(0.25, 0.8))
  g
  
  # Export
  figname <- paste0("figure_corr_revenues_", tolower(index_do), ".png")
  ggsave(g, filename=file.path(plotdir, figname), 
         width=6.5, height=8, units="in", dpi=600)
  
}


# Plot
sort(unique(indices_orig$index_abbrev))
plot_correlation(index="ENSO")
plot_correlation(index="PDO")
plot_correlation(index="NOI")
plot_correlation(index="NPGO")
plot_correlation(index="NPH")
plot_correlation(index="ONI")




