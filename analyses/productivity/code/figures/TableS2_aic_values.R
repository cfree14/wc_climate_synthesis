

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


# Models
################################################################################

# Files
files2merge <- list.files(datadir)

# Loop through files
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x) {
  
  # Load data
  output_full <- readRDS(file.path(datadir, x))
  output <- output_full$fit
  
  # Extract values
  k <- length(output[["par"]])
  lik <- output[["objective"]]
  aic_val <- TMBhelper::TMBAIC(output)

  # Record data
  df <- tibble(model=x, 
               k=k,
               nll=lik,
               aic=aic_val)
  
})

# Format data
aic_final <- data_orig %>% 
  # Add delta AIC
  mutate(daic=aic-min(aic)) %>% 
  # Sort by delta AIC
  arrange(daic) %>% 
  # Rename models
  mutate(model=recode(model, 
                      "production_0.01p_sst_fixed.Rds"="SST-linked production (MSY@37%K) - fixed effects",
                      "production_0.01p_sst_random.Rds"="SST-linked production (MSY@37%K) - random effects",
                      "production_0.01p.Rds"="Production (MSY@37%K)",            
                      "production_0.20p.Rds"="Production (MSY@40%)",          
                      "production_0.55p.Rds"="Production (MSY@45%)",            
                      "production_1.00p.Rds"="Production (MSY@50%)"))


# Export table
################################################################################

# Export data
write.csv(aic_final, paste(tabledir, "Table1_model_aic_comparison.csv", sep="/"), row.names=F)

