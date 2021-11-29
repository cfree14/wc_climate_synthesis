

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

# Read status data
data_status <- readRDS(file.path(datadir, "RAM_WC_status_data.Rds"))


# Data stats
################################################################################

# SST trend
get_sst_trend <- function(sst, year){
  lmfit <- lm(sst~year)
  coefs <- coef(lmfit)
  slope <- coefs[2]
  return(slope)
}

# Production dataset stats
stats_prod <- data %>% 
  rename(catch=c) %>% 
  filter(dataset=="Surplus production") %>% 
  group_by(stockid) %>% 
  summarize(yrs_prod=paste(min(year), max(year), sep="-"), 
            nyr_prod=n(),
            sst_c_avg_prod=mean(sst_c),
            sst_c_trend_prod=get_sst_trend(sst_c, year),
            yr2=max(year),
            yr1=yr2-9,
            catch_mt_prod=mean(catch[year %in% yr1:yr2])) %>% 
  ungroup() %>% 
  select(-c(yr1, yr2))

# Recruitment dataset status
stats_rec <- data %>% 
  rename(catch=c) %>% 
  filter(dataset=="Recruitment") %>% 
  group_by(stockid) %>% 
  summarize(yrs_rec=paste(min(year), max(year), sep="-"), 
            nyr_rec=n(),
            sst_c_avg_rec=mean(sst_c),
            sst_c_trend_rec=get_sst_trend(sst_c, year),
            yr2=max(year),
            yr1=yr2-9,
            catch_mt_rec=mean(catch[year %in% yr1:yr2], na.rm=T)) %>% 
  ungroup() %>% 
  select(-c(yr1, yr2))

# Status stats - production dataset
status_prod <- data_status %>% 
  # Mark years w/ prod data
  left_join(data_prod %>% select(stockid, year, sp)) %>% 
  # Reduce to years w/ prod data
  filter(!is.na(sp)) %>% 
  # Average status
  group_by(stockid) %>% 
  summarize(bbmsy_prod=mean(bbmsy, na.rm=T),
            ffmsy_prod=mean(uumsy, na.rm=T)) %>% 
  ungroup()

# Status stats - recruitment dataset
status_rec <- data_status %>% 
  # Mark years w/ rec data
  left_join(data_rec %>% select(stockid, year, r)) %>% 
  # Reduce to years w/ rec data
  filter(!is.na(r)) %>% 
  # Average status
  group_by(stockid) %>% 
  summarize(bbmsy_rec=mean(bbmsy, na.rm=T),
            ffmsy_rec=mean(uumsy, na.rm=T)) %>% 
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
  left_join(stats_rec, by="stockid") %>% 
  # Add production status
  left_join(status_prod, by="stockid") %>% 
  # Add production status
  left_join(status_rec, by="stockid")
  
# Inspect areas - combine?  
sort(unique(stock_key1$area))


# Life history
################################################################################

# Build LH data?
build_lh <- F

# Taxa key
taxa_key <- freeR::taxa(species=stock_key1$species)

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
  # Add taxomy
  left_join(taxa_key %>% select(type, class, order, genus, sciname), by=c("species"="sciname")) %>% 
  rename(taxa_type=type) %>% 
  # Add life history
  left_join(lh_use, by="species") %>% 
  # Arrange
  select(stockid:area, 
         taxa_type, class, order, family, genus, species, comm_name, everything())

# Inspect
freeR::complete(stock_key2)


# Export data
################################################################################

# Export
saveRDS(stock_key2, file=file.path(outputdir, "RAM_WC_stock_key.Rds"))



