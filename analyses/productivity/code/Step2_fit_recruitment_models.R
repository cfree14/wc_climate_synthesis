
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
  

# Fit and examine models
################################################################################

# Fit models
base <- splink::fit_sr(data=data, b_col="b_scaled", r_col="r_scaled", type="ricker")

# Fit linked models
sst_fixed <- splink::fit_sr(data=data, b_col="b_scaled", r_col="r_scaled", type="ricker", cov_col="sst_c_scaled", cov_effect = "fixed")
sst_random <- splink::fit_sr(data=data, b_col="b_scaled", r_col="r_scaled", type="ricker", cov_col="sst_c_scaled", cov_effect = "random")

# Compare models
splink::compare_models(models=list(base, sst_fixed, sst_random), 
                       names=c("Base", "Fixed", "Random"))

# Extract results
results_base <- splink::get_results(base)
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
splink::plot_fits(base, b_col="b_scaled", r_col="r_scaled", 
                  plotdir = plotdir, plotname="AppendixA_recruitment_fits.pdf")

splink::plot_fits(sst_fixed, b_col="b_scaled", r_col="r_scaled",  cov_col="sst_c_scaled", 
                  plotdir = plotdir, plotname="AppendixB_recruitment_sst_fixed_fits.pdf")

splink::plot_fits(sst_random, b_col="b_scaled", r_col="r_scaled",  cov_col="sst_c_scaled", 
                  plotdir = plotdir, plotname="AppendixC_recruitment_sst_random_fits.pdf")

# Export models
################################################################################

# Export models
saveRDS(base, file=file.path(outputdir, "recruitment_ricker.Rds"))
saveRDS(sst_fixed, file=file.path(outputdir, "recruitment_ricker_sst_fixed.Rds"))
saveRDS(sst_random, file=file.path(outputdir, "recruitment_ricker_sst_random.Rds"))

