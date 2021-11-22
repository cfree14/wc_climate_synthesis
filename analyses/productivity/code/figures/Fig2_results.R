
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
sp_fixed_output <- readRDS(file=file.path(outputdir, "production_0.01p_sst_fixed.Rds"))
sp_random_output <- readRDS(file=file.path(outputdir, "production_0.01p_sst_random.Rds"))
sr_fixed_output <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_fixed.Rds"))
sr_random_output <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_random.Rds"))


# Build data
################################################################################

sp_fixed_results <- splink::get_results(sp_fixed_output)
sp_fixed <- sp_fixed_results %>% 
  filter(param=="theta") %>% 
  mutate(prod_type="Production",
         model_type="Fixed")

sp_random_results <- splink::get_results(sp_random_output)
sp_random <- sp_random_results$stock %>% 
  filter(param=="theta") %>% 
  mutate(prod_type="Production",
         model_type="Random")

# Merge data
data <- bind_rows(sp_fixed, sp_random) %>% 
  # Arrange
  select(prod_type, model_type, stockid, everything()) %>% 
  # Standardize 
  group_by(prod_type, model_type) %>% 
  mutate(est_scaled=est/sd(est))

# Stats
stats <- data %>% 
  group_by(prod_type, stockid) %>% 
  summarize(est_avg=mean(est_scaled)) %>% 
  ungroup() %>% 
  arrange(est_avg)

# ORder data
data_ordered <- data %>% 
  mutate(stockid=factor(stockid, levels=stats$stockid),
         model_type=recode_factor(model_type, "Fixed"="Fixed\neffects\nanalysis", "Random"="Random\neffect\nanalysis"))



# Plot data
################################################################################

# Plot data
ggplot(data_ordered, aes(y=stockid, x=model_type, fill=est_scaled)) +
  facet_wrap(~prod_type) +
  geom_tile(color="grey30", lwd=0.05) +
  # Labels
  labs(x="Approach", y="") + 
  # Legend
  scale_fill_gradient2(name="Relative\neffect") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()






