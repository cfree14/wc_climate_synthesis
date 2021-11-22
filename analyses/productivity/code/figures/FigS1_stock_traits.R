
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
ramdir <- "data/ramldb/processed"
plotdir <- "analyses/productivity/figures"

# Read stock key
stock_key_orig <- read.csv(file=file.path(ramdir, "RAM_stock_key.csv"), as.is=T)

# Read data
data_rec <- readRDS(file.path(ramdir, "RAM_WC_recruitment_data_prepped.Rds")) %>% 
  mutate(dataset="Recruitment")
data_prod <- readRDS(file.path(ramdir, "RAM_WC_production_data_prepped.Rds")) %>% 
  mutate(dataset="Surplus production")


# Setup
################################################################################

# Build data
stock_key <- stock_key_orig %>% 
  # Mark analysis conducted
  mutate(sp_yn=stockid %in% data_prod$stockid,
         sr_yn=stockid %in% data_rec$stockid,
         analysis_catg=ifelse(sp_yn & sr_yn, "Both", 
                              ifelse(!sp_yn & !sr_yn, "Neither", 
                                     ifelse(sp_yn, "Production only", "Recruitment only")))) %>% 
  # Remove neither
  filter(analysis_catg!="Neither")


# Family stats
stats <- stock_key %>% 
  group_by(family, analysis_catg) %>% 
  summarize(n=n()) %>% 
  ungroup()


# Plot data
################################################################################

# Plot coverage by family
g <- ggplot(stats, aes(x=n, y=reorder(family, n), fill=analysis_catg)) +
  geom_bar(stat="identity") + 
  # Labels
  labs(x="Number of stocks", y="") +
  # Theme
  theme_bw()
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_stock_traits.png"), 
       width=4, height=4, units="in", dpi=600)



