
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/ramldb/processed"

# Read RAM Legacy Database v4.491
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAM v4.491 Files (1-14-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].Rdata")


# Build stock key
################################################################################

# Identify most recent assessments
assessments <- assessment %>% 
  # Reduce to most recent
  filter(mostrecent %in% c(999, -1)) %>% 
  # Simplify
  select(stockid, assessid, assessorid, assessmethod, assessyear) %>% 
  # Rename
  rename(assess_range=assessyear) %>% 
  # Extract last year
  mutate(assess_year=substr(assess_range, 6, 10) %>% as.numeric())

# All unique? Yes!
anyDuplicated(assessments$stockid)
anyDuplicated(assessments$assessid)

# Extract recruitment age
rec_age_key <- bioparams %>% 
  # Recruitment age
  filter(bioid=="REC-AGE-yr") %>% 
  # Simplify
  select(stockid, assessid, biovalue) %>% 
  arrange(stockid, assessid) %>% 
  # Rename
  rename(rec_age_yr=biovalue) %>% 
  # Convert
  mutate(rec_age_yr=recode(rec_age_yr,
                           "3+"="3", 
                           "13 to 28 years to 50% commercial selectivity"="20", 
                           "1 to 70"="35"),
         rec_age_yr=as.numeric(rec_age_yr)) %>% 
  # Summarise
  group_by(stockid) %>% 
  summarize(n=sum(!is.na(rec_age_yr)),
            nunique=n_distinct(rec_age_yr, na.rm=T),
            rec_age_yr=mean(rec_age_yr, na.rm=T)) %>% 
  ungroup()

# Reference point key
ref_key <- bioparams_values_views %>% 
  # MSY values
  select(stockid, MSY, Nmsy, MSYbest) %>% 
  rename(msy=MSY, nmsy=Nmsy, msy_best=MSYbest) %>% 
  # MSY units
  left_join(bioparams_units_views %>% select(stockid, MSY, Nmsy, MSYbest)) %>% 
  rename(msy_units=MSY, nmsy_units=Nmsy, msy_best_units=MSYbest) %>% 
  # MSY to use (MSY > NMSY) 
  mutate(msy_use=ifelse(!is.na(msy), msy, nmsy),
         msy_units_use=ifelse(!is.na(msy), msy_units, nmsy_units)) %>% 
  # Simplify
  select(stockid, msy_use, msy_units_use) %>% 
  rename(msy=msy_use, msy_units=msy_units_use) %>% 
  filter(!is.na(msy))
  
# Build stock key
stock_key <- stock %>% 
  # Reduce to most recent assessment
  filter(stockid %in% assessments$stockid) %>% 
  # Elimintate useless columns
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Add assessment info
  left_join(assessments, by="stockid") %>% 
  rename(assessor_id=assessorid, assess_method=assessmethod) %>% 
  # Add family name
  left_join(taxonomy %>% select(family, scientificname)) %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species),
         species=recode(species, 
                        "Chrysophrys auratus"="Pagrus auratus",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                        "Epinephelus niveatus"="Hyporthodus niveatus",
                        "Etrumeus teres"="Etrumeus sadina",
                        "Loligo bleekeri"="Heterololigo bleekeri",
                        "Loligo pealeii"="Doryteuthis pealeii",
                        "Merluccius gayi"="Merluccius gayi gayi",
                        "Mullus barbatus"="Mullus barbatus barbatus",
                        "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Tetrapturus albidus"="Kajikia albida",
                        "Sardinops melanostictus"="Sardinops sagax",
                        "Clupea bentincki"="Strangomera bentincki",
                        "Raja binoculata"="Beringraja binoculata",
                        "Raja rhina"="Beringraja rhina",
                        "Theragra chalcogramma"="Gadus chalcogrammus")) %>% 
  # Add MSY reference points
  left_join(ref_key, by="stockid") %>% 
  # Rearrange columns
  select(stockid, stocklong, 
         assessid, assessor_id, assess_method,
         country, region, area, 
         family, species, comm_name, msy, msy_units) %>% 
  # Add recruitment age
  left_join(rec_age_key %>% select(stockid, rec_age_yr), by="stockid") %>% 
  filter(region%in%c("US West Coast", "Canada West Coast", "US Alaska"))

# Check names
freeR::check_names(stock_key$species)
freeR::suggest_names(stock_key$species)
freeR::complete(stock_key)

# Export stock key
write.csv(stock_key, file=file.path(datadir, "RAM_stock_key.csv"), row.names=F)


# West Coast stocks and data
################################################################################

# Regions
table(stock_key$region)

# West Coast stocks
wc_stocks <- stock_key %>% 
  filter(region%in%c("US West Coast", "Canada West Coast", "US Alaska"))

# West Coast data
wc_data <- timeseries_values_views %>% 
  # Reduce to WC stocks
  filter(stockid %in% wc_stocks$stockid) %>% 
  # Add meta-data
  left_join(wc_stocks %>% select(stockid, region, area, species, comm_name), by="stockid") %>% 
  mutate(species_label=paste0(comm_name, " (", species, ")")) %>% 
  # Arrange
  select(stockid, stocklong, region, area, species_label, comm_name, species, everything())


# Build status series
################################################################################

# Build status data
data_status1 <- wc_data %>% 
  # Reduce to WC stocks
  filter(stockid %in% wc_stocks$stockid) %>% 
  # Select columns of interest
  select(stockid:year, BdivBmsypref:UdivUmgtpref, TBdivTBmsy:ERdivERmsy, CdivMSY, TBdivTBmgt:ERdivERmgt) %>% 
  # Eliminate columns that are always empty
  select(-c(NdivNmsy, NdivNmgt, NdivNmgt, ERdivERmgt)) %>% 
  # Eliminate preference columns since you're going to be the judge of this
  select(-c(BdivBmsypref:UdivUmgtpref)) %>% 
  # Rename columns
  rename(bbmsy_tb=TBdivTBmsy, bbmsy_ssb=SSBdivSSBmsy, ffmsy=FdivFmsy, uumsy=ERdivERmsy, 
         bbmgt_tb=TBdivTBmgt, bbmgt_ssb=SSBdivSSBmgt, ffmgt=FdivFmgt, cdivmsy=CdivMSY)

# Inspect data
freeR::complete(data_status1)

# Build stats
stats_status <- data_status1 %>%
  # Calculate sample size
  group_by(stockid, stocklong) %>% 
  summarize(bbmsy_tb_n=sum(!is.na(bbmsy_tb)),
            bbmsy_ssb_n=sum(!is.na(bbmsy_ssb)), 
            bbmgt_tb_n=sum(!is.na(bbmgt_tb)),
            bbmgt_ssb_n=sum(!is.na(bbmgt_ssb)), 
            ffmsy_n=sum(!is.na(ffmsy)),
            uumsy_n=sum(!is.na(uumsy)),
            ffmgt_n=sum(!is.na(ffmgt))) %>% 
  ungroup() %>% 
  # Select which to use
  mutate(bbmsy_type=ifelse(bbmsy_ssb_n>=bbmsy_tb_n, "SSB", "TB"),
         uumsy_type=ifelse((ffmsy_n+2)>=uumsy_n, "F", "ER"),
         b_n=ifelse(bbmsy_type=="SSB", bbmsy_ssb_n, bbmsy_tb_n),
         f_n=ifelse(uumsy_type=="F", ffmsy_n, uumsy_n)) %>% 
  # Remove stocks without either B/BMSY or F/FMSY data
  filter(b_n > 0 | f_n > 0)

# Finalize data
data_status2 <- data_status1 %>% 
  filter(stockid %in% stats_status$stockid) %>% 
  # Add B/BMSY and F/FMSY to use
  left_join(stats_status %>% select(stockid, bbmsy_type, uumsy_type), by="stockid") %>% 
  mutate(bbmsy=ifelse(bbmsy_type=="SSB", bbmsy_ssb, bbmsy_tb),
         uumsy=ifelse(uumsy_type=="F", ffmsy, uumsy)) %>% 
  # Simplify
  select(stockid:year, bbmsy_type, uumsy_type, bbmsy, uumsy) %>% 
  # Remove years with both values missing
  filter(!is.na(bbmsy) | !is.na(uumsy))


# Build recruitment time series
################################################################################

# Build recruitment data
data_sr1 <- wc_data %>% 
  # Reduce to WC stocks
  filter(stockid %in% wc_stocks$stockid) %>% 
  # Select columns of interest
  select(stockid:year, TBbest, TB, SSB, TN, R, TC, TL) %>% 
  # Remove years without recruitment
  filter(!is.na(R))

# Build stats
stats_sr <- data_sr1 %>%
  # Calculate sample size
  group_by(stockid, stocklong) %>% 
  summarize(tb_best_n=sum(!is.na(TBbest)),
            tb_n=sum(!is.na(TB)), 
            ssb_n=sum(!is.na(SSB)),
            tn_n=sum(!is.na(TN)),
            tc_n=sum(!is.na(TC)),
            tl_n=sum(!is.na(TL))) %>% 
  ungroup() %>% 
  # Add units
  left_join(timeseries_ids_views %>% select(stockid, TBbest)) %>% 
  left_join(timeseries_units_views %>% select(stockid, TB, SSB, TN, R, TC, TL)) %>% 
  # Rename
  rename(tb_best_units=TBbest, tb_units=TB, ssb_units=SSB, tn_units=TN, r_units=R, tc_units=TC, tl_units=TL) %>% 
  # Select which to use
  mutate(b_use=ifelse(ssb_n>0 & ssb_units=="MT", "SSB", "TB"),
         b_units=ifelse(b_use=="SSB", ssb_units, tb_units), 
         c_type=ifelse(tc_n>=(tl_n-10) & tc_units=="MT", "TC", "TL"),
         c_units=ifelse(c_type=="TC", tc_units, tl_units),
         c_n=ifelse(c_type=="TC", tc_n, tl_n)) %>% 
  select(stockid, stocklong, b_use, everything())

# Finalize recruitment data
data_sr2 <- data_sr1 %>% 
  # Add B use
  left_join(stats_sr %>% select(stockid, b_use, b_units, r_units, c_type, c_units), by="stockid") %>% 
  rename(b_type=b_use, r=R) %>% 
  mutate(b=ifelse(b_type=="SSB", SSB, TB), 
         c=ifelse(c_type=='TC', TC, TL)) %>% 
  # Simplify
  select(stockid:year, b_type, b_units, r_units, b, r, c_type, c_units, c) %>% 
  # Drop missing values
  filter(!is.na(b) & !is.na(r))

# Inspect data
freeR::complete(data_sr2)


# Build surplus production time series
################################################################################

# Function to calculate surplus production: 
# SP(t) = TB(t+1) - TB(t) + C(t)
# tb <- subset(bdata, assessid==unique(bdata$assessid)[1])$tb
# catch <- subset(bdata, assessid==unique(bdata$assessid)[1])$catch
calc_sp <- function(tb, catch){
  sp <- c(tb[2:length(tb)] - tb[1:(length(tb)-1)] + catch[1:(length(catch)-1)], NA)
  return(sp)
}

# Build production data
data_sp1 <- wc_data %>% 
  # Reduce to WC stocks
  filter(stockid %in% wc_stocks$stockid) %>% 
  # Select columns of interest (include R for trimming)
  select(stockid:year, TB, SSB, TN, TC, TL, R)

# Build stats
stats_sp <- data_sp1 %>% 
  # Calculate sample size
  group_by(stockid, stocklong) %>% 
  summarize(tb_n=sum(!is.na(TB)), 
            ssb_n=sum(!is.na(SSB)),
            tn_n=sum(!is.na(TN)),
            tc_n=sum(!is.na(TC)),
            tl_n=sum(!is.na(TL))) %>% 
  ungroup() %>% 
  # Add units
  left_join(timeseries_units_views %>% select(stockid, TB, SSB, TN, TC, TL, R)) %>% 
  # Rename
  rename(tb_units=TB, ssb_units=SSB, tn_units=TN, tc_units=TC, tl_units=TL, r_units=R) %>% 
  # Reduce to stocks with required data
  filter((tb_n>0 | ssb_n >0 | tn_n > 0) & (tc_n > 0 | tl_n > 0)) %>% 
  # Select which B to use
  mutate(b_type=ifelse(tb_n>0, "TB", "SSB"),
         b_units=ifelse(b_type=="TB", tb_units, ssb_units),
         b_n=ifelse(b_type=="TB", tb_n, ssb_n),
         c_type=ifelse(tc_n>=(tl_n-10) & tc_units=="MT", "TC", "TL"),
         c_units=ifelse(c_type=="TC", tc_units, tl_units),
         c_n=ifelse(c_type=="TC", tc_n, tl_n)) %>% 
  # Confirm B and C units are same
  mutate(units=ifelse(b_units==c_units, "good", "bad")) %>% 
  # Arrange
  select(stockid, stocklong, b_type, b_units, b_n, c_type, c_units, c_n, units, everything()) %>% 
  # Remove stocks missing required data
  filter(b_n>0 & c_n>0 & units=="good")

# Finalize production data
data_sp2 <- data_sp1 %>% 
  # Subset to stocks with required data
  filter(stockid %in% stats_sp$stockid) %>% 
  # Add B and C to use key
  left_join(stats_sp %>% select(stockid, b_type, b_units, c_type, c_units, r_units), by="stockid") %>% 
  mutate(b=ifelse(b_type=="TB", TB, SSB),
         c=ifelse(c_type=="TC", TC, TL)) %>% 
  # Simplify
  rename(r=R) %>% 
  select(stockid:year, b_type, b_units, c_type, c_units, r_units, b, c, r) %>% 
  # Calculate surplus production
  group_by(stockid) %>% 
  mutate(sp=calc_sp(tb=b, catch=c)) %>% 
  ungroup() %>% 
  # Drop missing values
  filter(!is.na(b) & !is.na(c))
  

# Export data
################################################################################

# Export
saveRDS(data_status2, file=file.path(datadir, "RAM_WC_status_data.Rds"))
saveRDS(data_sr2, file=file.path(datadir, "RAM_WC_recruitment_data.Rds"))
saveRDS(data_sp2, file=file.path(datadir, "RAM_WC_production_data.Rds"))






