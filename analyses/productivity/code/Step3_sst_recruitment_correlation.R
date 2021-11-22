
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(TMB)
library(devtools)
library(freeR)
library(tidyverse)
library(splink)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")

# Directories
datadir <- "data/ramldb/processed"
codedir <- "analyses/productivity/"
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"

# Read data
data_orig <- readRDS(file.path(datadir, "RAM_WC_recruitment_data_prepped.Rds"))

# Problem stocks (in order removed)
perfect_srs <- c("BGROCKPCOAST", "GRSPROCKNCAL", "GRSPROCKSCAL", "LNOSESKAPCOAST", "SPSDOGPCOAST", "YEYEROCKPCOAST")
large_alphas <- c("YSOLEBSAI", "FLSOLEBSAI")
se_fail <- c("ALPLAICBSAI", "DSOLEGA") 
bad_fit <- c("CALSCORPSCAL")
no_peak <- c("PCOD5AB", "SARDPCOAST", "SABLEFPCOAST", "PCODHS") # no peak?
crazy <- "REYEROCKGA"
stocks_ignore <- c(perfect_srs, large_alphas, se_fail, bad_fit, no_peak, crazy)

# Eliminate problem stocks
data <- data_orig %>% 
  filter(!stockid %in% stocks_ignore)



# 
################################################################################


ggplot(data, aes(x=sst_c_scaled, y=r_scaled)) +
  facet_wrap(~stockid, ncol=8, scales="free") +
  geom_smooth(method="lm") +
  geom_point() +
  theme_bw()



  
