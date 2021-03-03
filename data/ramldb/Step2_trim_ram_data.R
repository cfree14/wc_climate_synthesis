

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/ramldb/processed"
plotdir <- "data/ramldb/figures/trimming"

# Read data
data_orig <- readRDS(file=file.path(datadir, "RAM_WC_production_data.Rds")) 


# Sample size
n_distinct(data_orig$stockid)
n_distinct(data_orig$species)

# Setup trim key
################################################################################

# Export template trim key
if(F){
  trim_key <- data_orig %>% 
    select(stockid) %>% 
    unique() %>% 
    arrange(stockid)
  write.csv(trim_key, file.path(datadir, "RAM_years_to_trim_key.csv"), row.names=F)
}

# Read trim key
trim_key <- readxl::read_excel(file.path(datadir, "RAM_years_to_trim_key.xlsx")) %>% 
  # Fill missing value
  mutate(biomass_yr1=ifelse(is.na(biomass_yr1), -Inf, biomass_yr1), 
         biomass_yr2=ifelse(is.na(biomass_yr2), Inf, biomass_yr2),
         catch_yr1=ifelse(is.na(catch_yr1), -Inf, catch_yr1), 
         catch_yr2=ifelse(is.na(catch_yr2), Inf, catch_yr2),
         recruits_yr1=ifelse(is.na(recruits_yr1), -Inf, recruits_yr1), 
         recruits_yr2=ifelse(is.na(recruits_yr2), Inf, recruits_yr2))

# Mark years to use
data1 <- data_orig %>% 
  left_join(trim_key, by="stockid") %>% 
  mutate(b_use=ifelse(year<biomass_yr1 | year>biomass_yr2 , "no", "yes"),
         c_use=ifelse(year<catch_yr1 | year>catch_yr2, "no", "yes"),
         r_use=ifelse(year<recruits_yr1 | year>recruits_yr2, "no", "yes"),
         use=ifelse(b_use=="no" | c_use=="no" | r_use=="no", "no", "yes")) %>% 
  # Convert to factor
  mutate(b_use=factor(b_use, levels=c("no", "yes")),
         c_use=factor(c_use, levels=c("no", "yes")),
         r_use=factor(r_use, levels=c("no", "yes")),
         use=factor(use, levels=c("no", "yes")))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=7),
                    plot.title=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.position = "none")


# Function to plot data
stockid <- "ALPLAICBSAI"; export=T
plot_data <- function(stockid, export){
  
  # Subset data
  stock_do <- stockid
  sdata <- data1 %>% 
    filter(stockid==stock_do)
  
  # Plot biomass
  g1 <- ggplot(sdata, aes(x=year, y=b/1e3)) +
    geom_line() + 
    geom_point(mapping=aes(color=b_use), size=0.5) +
    labs(x="", y="Biomass (1000s)", title=stock_do) +
    scale_color_manual(values=c("red", "black"), drop=F) +
    theme_bw() + base_theme
  g1
  
  # Plot catch
  g2 <- ggplot(sdata, aes(x=year, y=c/1e3)) +
    geom_line() + 
    geom_point(mapping=aes(color=c_use), size=0.5) +
    labs(x="", y="Catch (1000s)", title=" ") +
    scale_color_manual(values=c("red", "black"), drop=F) +
    theme_bw() + base_theme
  g2
  
  # Plot recruitment
  g3 <- ggplot(sdata, aes(x=year, y=r/1e9)) +
    geom_line() + 
    geom_point(mapping=aes(color=r_use), size=0.5) +
    labs(x="", y="Recruitment (billions)", title=" ") +
    scale_color_manual(values=c("red", "black"), drop=F) +
    theme_bw() + base_theme
  g3
  
  # Plot production relationship
  g4 <- ggplot(sdata, aes(x=b/1e3, y=sp/1e3)) +
    geom_point(mapping=aes(color=use), size=0.5) + 
    labs(x="Biomass (1000s)", y="Production (1000s)",title=" ") +
    scale_color_manual(values=c("red", "black"), drop=F) +
    theme_bw() + base_theme
  g4
  
  # Plot stock-recruit relationship
  g5 <- ggplot(sdata, aes(x=b/1e3, y=r/1e9)) +
    geom_point(mapping=aes(color=use), size=0.5) + 
    labs(x="Biomass (1000s)", y="Recruitment (billions)",title=" ") +
    scale_color_manual(values=c("red", "black"), drop=F) +
    theme_bw() + base_theme
  g5
  
  # Merge
  g <- gridExtra::grid.arrange(g1,g2,g3,g4,g5, nrow=1)
  g
  
  # Export
  if(export==T){
    ggsave(g, filename=file.path(plotdir, paste0(stock_do, ".png")), 
           width=6.5, height=1.5, units="in", dpi=600)
  }
  
  # Return
  return(g)

}

plot_data(trim_key$stockid[126], export=T)

# Loop through
for(i in 1:nrow(trim_key)){
  plot_data(stockid = trim_key$stockid[i], export=T)
}

