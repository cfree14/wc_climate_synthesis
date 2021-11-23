
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
sp_fixed_output <- readRDS(file=file.path(outputdir, "production_0.01p_sst_fixed.Rds"))
sp_random_output <- readRDS(file=file.path(outputdir, "production_0.01p_sst_random.Rds"))
sp_corr_output <- readRDS(file=file.path(outputdir, "RAM_WC_production_sst_correlation.Rds"))
sr_fixed_output <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_fixed.Rds"))
sr_random_output <- readRDS(file=file.path(outputdir, "recruitment_ricker_sst_random.Rds"))
sr_corr_output <- readRDS(file=file.path(outputdir, "RAM_WC_recruitment_sst_correlation.Rds"))


# Build data
################################################################################

# Build production data
####################################

sp_fixed_results <- splink::get_results(sp_fixed_output)
sp_fixed <- sp_fixed_results %>% 
  filter(param=="theta") %>% 
  mutate(prod_type="Production",
         model_type="Fixed") %>% 
  select(prod_type, model_type, stockid, est)

sp_random_results <- splink::get_results(sp_random_output)
sp_random <- sp_random_results$stock %>% 
  filter(param=="theta") %>% 
  mutate(prod_type="Production",
         model_type="Random") %>% 
  select(prod_type, model_type, stockid, est)

sp_corr <- sp_corr_output %>% 
  mutate(prod_type="Production",
         model_type="Correlation") %>% 
  select(prod_type, model_type, stockid, corr) %>% 
  rename(est=corr)

# Build recruitment data
####################################

sr_fixed_results <- splink::get_results(sr_fixed_output)
sr_fixed <- sr_fixed_results %>%
  # Reduce to thetas
  filter(param=="theta") %>% 
  mutate(prod_type="Recruitment",
         model_type="Fixed") %>% 
  select(prod_type, model_type, stockid, est)

sr_random_results <- splink::get_results(sr_random_output)
sr_random <- sr_random_results$stock %>% 
  filter(param=="theta") %>% 
  mutate(prod_type="Recruitment",
         model_type="Random") %>% 
  select(prod_type, model_type, stockid, est)

sr_corr <- sr_corr_output %>% 
  mutate(prod_type="Recruitment",
         model_type="Correlation") %>% 
  select(prod_type, model_type, stockid, corr) %>% 
  rename(est=corr)
 

# Merge data
####################################

# Build data
data <- bind_rows(sp_fixed, sp_random, sp_corr,
                  sr_fixed, sr_random, sr_corr)
  
  
# Export data
saveRDS(data, file=file.path(outputdir, "RAM_WC_sr_sp_sst_influence_results.Rds"))





