

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(RColorBrewer)

# Directories
datadir <- "analyses/productivity/output"
tabledir <- "analyses/productivity/tables"
plotdir <- "analyses/productivity/figures"

# Build data
################################################################################

# Read SP data
output_sp <- readRDS(file.path(datadir, "production_0.01p_sst_fixed.Rds"))
output_sr <- readRDS(file.path(datadir, "recruitment_ricker_sst_fixed.Rds"))

# Extract results
data_sp <- splink::get_results(output_sp) 
data_sr <- splink::get_results(output_sr)

# Format results
data_sp1 <- data_sp %>% 
  filter(param=="theta") %>% 
  select(-param)

data_sr1 <- data_sr %>% 
  filter(param=="theta") %>% 
  select(-param)

# Merge results
data <- data_sp1 %>% 
  inner_join(data_sr1, by="stockid")

# Plot data
g <- ggplot(data, aes(x=est.y, y=est.x)) +
  geom_point() +
  # Labels
  labs(x="SST influence\non recruitment", y="SST influence\non surplus production") +
  # Reference lines
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  geom_abline(slope=1, linetype="solid") +
  # Theme
  theme_bw()
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_sp_sr_theta_comparison.png"), 
       width=4, height=4, units="in", dpi=600)



