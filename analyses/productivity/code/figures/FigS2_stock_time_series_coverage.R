
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
ramdir <- "data/ramldb/processed"
plotdir <- "analyses/productivity/figures"

# Read data
data_rec <- readRDS(file.path(ramdir, "RAM_WC_recruitment_data_prepped_final.Rds")) %>% 
  mutate(dataset="Recruitment")
data_prod <- readRDS(file.path(ramdir, "RAM_WC_production_data_prepped_final.Rds")) %>% 
  mutate(dataset="Surplus production")

# Build data
################################################################################

# Build data
data <- bind_rows(data_rec, data_prod) %>% 
  # Simplify
  select(dataset, stockid, stocklong, year, sst_c) %>% 
  # Order production/recruitment
  mutate(dataset=factor(dataset, levels=c("Surplus production", "Recruitment"))) %>% 
  # Standardize SST
  group_by(stockid) %>% 
  mutate(sst_c_sd=scale(sst_c, center=T)) %>% 
  ungroup()

# Sample size
nyr <- data %>% 
  group_by(dataset, stockid) %>% 
  summarize(nyr=n()) %>% 
  ungroup()

# Determine sort order
stocks <- data %>% 
  # Calculate 1st year
  group_by(dataset, stockid, stocklong) %>% 
  summarize(year=min(year)) %>% 
  ungroup() %>% 
  # Reshape
  mutate(dataset=recode(dataset, "Surplus production"="production", "Recruitment"="recruitment")) %>% 
  spread(key="dataset", value="year") %>% 
  # Sort 
  arrange(production)

# Order data
data_ordered <- data %>% 
  mutate(stockid=factor(stockid, levels=stocks$stockid),
         stocklong=factor(stocklong, levels=stocks$stocklong))
  

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data_ordered, aes(x=year, y=stockid)) +
  # Plot heatwave period
  geom_rect(xmin=2013, xmax=2016, ymin=0, ymax=nrow(stocks)+0.5, fill="grey90", alpha=0.05, inherit.aes = F) +
  facet_wrap(~dataset) +
  geom_raster() + 
  # Labels
  labs(x="", y="") +
  scale_x_continuous(breaks=seq(1920, 2020, 10)) +
  # Legend
  # scale_fill_gradient2(name="SST anamoly", high="darkred", low="navy", mid="white", midpoint=0) +
  # guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS2_stock_time_series_coverage.png"), 
       width=6.5, height=7, units="in", dpi=600)



