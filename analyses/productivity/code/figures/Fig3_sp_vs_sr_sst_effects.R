
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
stock_key <- readRDS(file=file.path(outputdir, "RAM_WC_stock_key.Rds"))

# Read merged SST effects
data_orig <- readRDS(file=file.path(outputdir, "RAM_WC_sr_sp_sst_influence_results.Rds"))

# Prep data
data <- data_orig %>% 
  # Fixed effects estimates
  filter(model_type=="Fixed") %>% 
  spread(key="prod_type", value="est") %>% 
  rename(theta_prod=Production, theta_rec=Recruitment) %>% 
  filter(!is.na(theta_prod) & !is.na(theta_rec))  %>% 
  # Add stock info
  left_join(stock_key, by="stockid") %>% 
  # Calculate mean SST and trend
  rowwise() %>% 
  mutate(sst_c_avg=mean(sst_c_avg_prod, sst_c_avg_rec, na.rm=T),
         sst_c_decade=mean(sst_c_trend_prod, sst_c_trend_rec, na.rm=T)*10) %>% 
  ungroup()


# Plot data
################################################################################

# Quadrant labels
xmin <- range(data$theta_rec)[1]
xmax <- pmax(range(data$theta_rec)[2], 1.5)
ymin <- range(data$theta_prod)[1]
ymax <- range(data$theta_prod)[2]
q_labels <- tibble(quadrant=c("topleft", "topright", "bottomright", "bottomleft"),
                   x=c(xmin, xmax, xmax, xmin),
                   y=c(ymax, ymax, ymin, ymin),
                   hjust=c(0,1,1,0)) %>% 
  mutate(quadrant_label=recode(quadrant,
                                "topleft"="Recruitment losses\noffset by growth/mortality gains", 
                                "topright"="Across the board gains\n(or overwhelming recruitment gains)", 
                                "bottomright"="Recruitment gains\noffset by growth/mortality losses",
                                "bottomleft"="Across the board losses\n(or overwhelming recruitment losses)"))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=theta_rec, y=theta_prod)) +
  # Reference lines
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope=1, linetype="dotted") +
  # Points
  geom_point() +
  # Quadrant labels
  geom_text(data=q_labels, mapping=aes(x=x, y=y, hjust=hjust, label=quadrant_label), inherit.aes=F, size=2.1, color="grey50") +
  # Labels
  labs(x="Effect of warming\non recruitment",
       y="Effect of warming\non surplus production") +
  # Scales
  scale_x_continuous(breaks=seq(-2,1.5,0.5), lim=c(NA,1.5)) +
  scale_y_continuous(breaks=seq(-5,3,0.5)) +
  # Legend
  # scale_size_continuous(name="Max age (yr)") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_sp_vs_sr_sst_effects.png"), 
       width=4.25, height=3.75, units="in", dpi=600)


