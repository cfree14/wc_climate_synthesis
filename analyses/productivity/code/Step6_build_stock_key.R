

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/ramldb/processed"
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"

# Read stock key
stock_key_orig <- read.csv(file=file.path(datadir, "RAM_stock_key.csv"), as.is=T)

# Read time series data
data_rec <- readRDS(file.path(datadir, "RAM_WC_recruitment_data_prepped_final.Rds")) %>% 
  mutate(dataset="Recruitment")
data_prod <- readRDS(file.path(datadir, "RAM_WC_production_data_prepped_final.Rds")) %>% 
  mutate(dataset="Surplus production")
data <- bind_rows(data_rec, data_prod)


# Data stats
################################################################################

# SST trend
get_sst_trend <- function(sst, year){
  lmfit <- lm(sst~year)
  coefs <- coef(lmfit)
  slope <- coefs[2]
  return(slope)
}

# Build dataset stats
stats_prod <- data %>% 
  filter(dataset=="Surplus production") %>% 
  group_by(stockid) %>% 
  summarize(yrs_prod=paste(min(year), max(year), sep="-"), 
            nyr_prod=n(),
            sst_c_avg_prod=mean(sst_c),
            sst_c_trend_prod=get_sst_trend(sst_c, year)) %>% 
  ungroup()
stats_rec <- data %>% 
  filter(dataset=="Recruitment") %>% 
  group_by(stockid) %>% 
  summarize(yrs_rec=paste(min(year), max(year), sep="-"), 
            nyr_rec=n(),
            sst_c_avg_rec=mean(sst_c),
            sst_c_trend_rec=get_sst_trend(sst_c, year)) %>% 
  ungroup()

# Analyzed stocks
stocks_used <- sort(unique(c(stats_prod$stockid, stats_rec$stockid)))

# Build data
stock_key1 <- stock_key_orig %>% 
  # Reduce to analyzed stocks
  filter(stockid %in% stocks_used) %>% 
  # Add production stats
  left_join(stats_prod, by="stockid") %>% 
  # Add recruitment stats
  left_join(stats_rec, by="stockid")
  
sort(unique(stock_key1$area))


# Life history
################################################################################

# Build LH data?
build_lh <- F

# Build or read LH data
if(build_lh){
  
  # Species
  spp <- sort(unique(stock_key1$species))
  spp_key <- freeR::taxa(species=spp)
  
  # FishLife
  lh_fl <- freeR::fishlife(species=spp)
  
  # FishBase
  lh_fb <- freeR::fishbase(species=spp, dataset="vonb",  level="family", cleaned = T)
  
  # Process FishBase
  lh_fb_spp <- lh_fb %>% 
    group_by(species) %>% 
    summarize(linf_cm_fb=mean(linf_cm, na.rm=T),
              k_fb=mean(k, na.rm=T),
              tmax_yr_fb=mean(tmax_yr, na.rm=T),
              m_fb=mean(m, na.rm=T))
  lh_fb_gen <- lh_fb %>% 
    group_by(genus) %>% 
    summarize(linf_cm_fb_gen=mean(linf_cm, na.rm=T),
              k_fb_gen=mean(k, na.rm=T),
              tmax_yr_fb_gen=mean(tmax_yr, na.rm=T),
              m_fb_gen=mean(m, na.rm=T))
  lh_fb_fam <- lh_fb %>% 
    group_by(family) %>% 
    summarize(linf_cm_fb_fam=mean(linf_cm, na.rm=T),
              k_fb_fam=mean(k, na.rm=T),
              tmax_yr_fb_fam=mean(tmax_yr, na.rm=T),
              m_fb_fam=mean(m, na.rm=T))
  
  # Build life history
  lh_use <- lh_fl %>% 
    # Simplify
    select(species, linf_cm, tmax_yr, k, m) %>% 
    # Add taxa
    left_join(spp_key %>% select(sciname, genus, family), by=c("species"="sciname")) %>% 
    # Add FB species-specific
    left_join(lh_fb_spp, by="species") %>% 
    # Add FB genus-specific
    left_join(lh_fb_gen, by="genus") %>% 
    # Add FB family-specific
    left_join(lh_fb_fam, by="family") %>% 
    # Final linf
    mutate(linf_cm1=ifelse(!is.na(linf_cm), linf_cm,
                           ifelse(!is.na(linf_cm_fb), linf_cm_fb,
                                  ifelse(!is.na(linf_cm_fb_gen), linf_cm_fb_gen, linf_cm_fb_fam)))) %>% 
    # Final tmax
    mutate(tmax_yr1=ifelse(!is.na(tmax_yr), tmax_yr,
                           ifelse(!is.na(tmax_yr_fb), tmax_yr_fb,
                                  ifelse(!is.na(tmax_yr_fb_gen), tmax_yr_fb_gen, tmax_yr_fb_fam)))) %>% 
    # Final K
    mutate(k1=ifelse(!is.na(k), k,
                     ifelse(!is.na(k_fb), k_fb,
                            ifelse(!is.na(k_fb_gen), k_fb_gen, k_fb_fam)))) %>% 
    # Simplify
    select(species, linf_cm1, tmax_yr1, k1) %>% 
    rename(linf_cm=linf_cm1, tmax_yr=tmax_yr1, k=k1)
  
  # Export data
  write.csv(lh_use, file=file.path(outputdir, "RAM_WC_FL_FB_life_history_data.csv"), row.names = F)
  
}else{
  
  # Read data
  lh_use <- read.csv(file=file.path(outputdir, "RAM_WC_FL_FB_life_history_data.csv"), as.is=T)
  
}
  

# Final key
################################################################################

# Add LH to stock key
stock_key2 <- stock_key1 %>% 
  left_join(lh_use, by="species")


# Export data
################################################################################

# Export
saveRDS(stock_key2, file=file.path(outputdir, "RAM_WC_stock_key.Rds"))










