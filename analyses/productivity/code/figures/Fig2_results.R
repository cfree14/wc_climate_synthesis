
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
ramdir <- "data/ramldb/processed"
plotdir <- "analyses/productivity/figures"
outputdir <- "analyses/productivity/output"

# Read stock key
stock_key_orig <- read.csv(file=file.path(ramdir, "RAM_stock_key.csv"), as.is=T)

# Read data
data_orig <- readRDS(file=file.path(outputdir, "RAM_WC_sr_sp_sst_influence_results.Rds"))


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Standardize effect
  group_by(prod_type, model_type) %>% 
  mutate(est_scaled=est/sd(est)) %>% 
  ungroup()

# Stats
stats <- data %>% 
  group_by(stockid) %>% 
  summarize(est_avg=mean(est_scaled)) %>% 
  ungroup() %>% 
  arrange(est_avg)

# ORder data
data_ordered <- data %>% 
  mutate(stockid=factor(stockid, levels=stats$stockid),
         model_type=recode_factor(model_type, 
                                  "Fixed"="Fixed\neffects\nanalysis", 
                                  "Random"="Random\neffect\nanalysis",
                                  "Correlation"="Corr.\nanalysis"))



# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text.y=element_text(size=4.75),
                   axis.text.x=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")


# Plot data
g1 <- ggplot(data_ordered, aes(y=stockid, x=model_type, fill=est_scaled)) +
  facet_wrap(~prod_type) +
  geom_tile(color="grey30", lwd=0.05) +
  # Labels
  labs(x="Approach", y="") + 
  # Legend
  scale_fill_gradient2(name="Relative effect\nof warming") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g1

# Export plot
ggsave(g1, filename=file.path(plotdir, "Fig2_results.png"), 
       width=3.75, height=6.5, units="in", dpi=600)








