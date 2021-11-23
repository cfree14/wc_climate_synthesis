
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
data_orig <- readRDS(file.path(datadir, "RAM_WC_production_data_prepped.Rds"))


# Format data
################################################################################

# Sample size
nyr_req <- 20
stock_2few_yrs <- data_orig %>% 
  group_by(stockid) %>% 
  summarize(nyr=n()) %>%
  ungroup() %>% 
  filter(nyr<nyr_req) %>% 
  pull(stockid)
  
# Problem stocks
large_theta <- "STFLOUNSPCOAST"
problem_stocks <- c(large_theta, stock_2few_yrs)

# Finalize data (remove problem stocks)
data <- data_orig %>% 
  # Rename a few columns
  rename(b_sd=b_scaled, sp_sd=sp_scaled) %>% 
  # Remove problem stocks
  filter(!stockid %in% problem_stocks)

# Export finalized data
saveRDS(data, file.path(datadir, "RAM_WC_production_data_prepped_final.Rds"))


# Fit and examine models
################################################################################

# Fit models
base1 <- splink::fit_sp(data=data, b_col="b_sd", sp_col="sp_sd", p=1)
base2 <- splink::fit_sp(data=data, b_col="b_sd", sp_col="sp_sd", p=0.55)
base3 <- splink::fit_sp(data=data, b_col="b_sd", sp_col="sp_sd", p=0.20)
base4 <- splink::fit_sp(data=data, b_col="b_sd", sp_col="sp_sd", p=0.01)

# Compare models
splink::compare_models(models=list(base1, base2, base3, base4), 
                       names=paste0(c(50, 45, 40, 37), "% MSY"))

# Fit linked models
sst_fixed <- splink::fit_sp(data=data, b_col="b_sd", sp_col="sp_sd", p=0.01, cov_col="sst_c_scaled", cov_effect = "fixed")
sst_random <- splink::fit_sp(data=data, b_col="b_sd", sp_col="sp_sd", p=0.01, cov_col="sst_c_scaled", cov_effect = "random")

# Compare models
splink::compare_models(models=list(base1, base2, base3, base4, sst_fixed, sst_random), 
                       names=c(paste0(c(50, 45, 40, 37), "% MSY"), "Fixed", "Random"))

# Extract results
results_base <- splink::get_results(base4)
results_fixed <- splink::get_results(sst_fixed)
results_random <- splink::get_results(sst_random)

# Plot results
splink::plot_results(results_base)
splink::plot_results(results_fixed)
splink::plot_results(results_random)

# Plot theta
splink::plot_thetas(results_fixed)
splink::plot_thetas(results_random)

# Plot fits
splink::plot_fits(base4, b_col="b_sd", sp_col="sp_sd", 
                  plotdir = plotdir, plotname="AppendixA_production_fits.pdf")

splink::plot_fits(sst_fixed, b_col="b_sd", sp_col="sp_sd", cov_col="sst_c_scaled", 
                  plotdir = plotdir, plotname="AppendixB_production_sst_fixed_fits.pdf")

splink::plot_fits(sst_random, b_col="b_sd", sp_col="sp_sd", cov_col="sst_c_scaled", 
                  plotdir = plotdir, plotname="AppendixC_production_sst_random_fits.pdf")

# Export models
################################################################################

# Export models
saveRDS(base1, file=file.path(outputdir, "production_1.00p.Rds"))
saveRDS(base2, file=file.path(outputdir, "production_0.55p.Rds"))
saveRDS(base3, file=file.path(outputdir, "production_0.20p.Rds"))
saveRDS(base4, file=file.path(outputdir, "production_0.01p.Rds"))
saveRDS(sst_fixed, file=file.path(outputdir, "production_0.01p_sst_fixed.Rds"))
saveRDS(sst_random, file=file.path(outputdir, "production_0.01p_sst_random.Rds"))

