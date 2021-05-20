

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)
library(rnaturalearth)

# Directories
ramdir <- "data/ramldb/processed"
pacfindir <- "data/landings/pacfin/processed"
plotdir <- "figures"

# Read RAM data
ram_orig <- readRDS(file.path(ramdir, "RAM_WC_status_data.Rds"))

# Read PACFIN landings data
pacfin_orig <- wcfish::pacfin_all6

# Read FMP data
fmp_key_orig <- readxl::read_excel("data/fmps/CA_FMP_species.xlsx", sheet=1)


# Build data
################################################################################

# Read RAM Legacy Database v4.491
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAM v4.491 Files (1-14-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].Rdata")

# WC species
wc_stocks <- stock %>% 
  # Filter to WC stocks with current assessments
  filter(region %in% c("US West Coast", "US West Coast (Pacific Salmon)", "Pacific Ocean") & state=="Current") %>% 
  # Fix scientific names
  mutate(scientificname=recode(scientificname, 
                               "Raja rhina"="Beringraja rhina"))

# Check names
freeR::check_names(wc_stocks$scientificname)


# Build data
################################################################################

# Check FMP names
freeR::check_names(fmp_key_orig$sci_name)
freeR::suggest_names(fmp_key_orig$sci_name)

# Format PACFIN data
pacfin <- pacfin_orig %>% 
  # Fix scientific names
  mutate(sci_name=recode(sci_name,
                         "Arpisturus brunneus"="Apristurus brunneus",           
                         "Callianassa californiensis"="Neotrypaea californiensis",      
                         "Cancer magister"="Metacarcinus magister",                 
                         # "Cancer spp."="",                    
                         # "Citharichthys spp."="",    
                         "Clupea harengus pallasii"="Clupea pallasii pallasii",    
                         # "Cottidae spp."="",            
                         "Crassostrea gigas kumamoto"="Crassostrea gigas", 
                         # "Decapoda spp."="",            
                         # "Echinodermata spp."="",        
                         # "Elasmobranchii spp."="",        
                         # "Eptatretus spp."="",           
                         "Etrumeus teres"="Etrumeus sadina",     
                         "Galeorhinus zyopterus"="Galeorhinus galeus",
                         # "Hexagrammos spp."="",
                         "Loligo opalescens"="Doryteuthis opalescens",
                         # "Macoma spp."="",
                         "Nuttallia nuttalli"="Nuttallia nuttallii",
                         # "Ophiodon spp."="",
                         # "Paralichthys spp."="",
                         "Pleuronichthys guttulatus"="Hypsopsetta guttulata",
                         "Protothaca staminea"="Leukoma staminea",
                         "Raja binoculata"="Beringraja binoculata",   
                         "Raja inornata"="Beringraja inornata",   
                         "Raja rhina"="Beringraja rhina", 
                         "Raja stellulata"="Beringraja stellulata",       
                         # "Rajidae spp." ="",   
                         "Saxidomus giganteus"="Saxidomus gigantea",
                         "Scorpaena gutatta"="Scorpaena guttata",
                         # "Scorpaena spp."="",
                         # "Scorpaenichthys spp."="",
                         # "Sebastes spp."="",
                         # "Sebastolobus spp."="",
                         # "Semicossyphus spp."="",
                         "Strongylocentrotus franciscanus"="Mesocentrotus franciscanus",
                         # "Surfperch spp."="",               
                         "Tetrapterus audax"="Kajikia audax",   
                         # "Tresus spp."="",
                         "Theragra chalcogramma"="Gadus chalcogrammus"))

# Check PACFIN names
freeR::check_names(pacfin$sci_name)
# freeR::suggest_names(pacfin$sci_name)

# Build data
data <- pacfin %>% 
  # California
  filter(state=="California") %>% 
  # Summarize by year
  group_by(year, comm_name, sci_name) %>% 
  summarize(landings_mt=sum(landings_mt),
            value_usd=sum(revenues_usd)) %>% 
  ungroup() %>% 
  # Label management plan
  mutate(fmp_yn=ifelse(sci_name %in% fmp_key_orig$sci_name, "yes", "no")) %>% 
  # Add stock assessment
  mutate(assessed_yn=ifelse(sci_name %in% wc_stocks$scientificname, "yes", "no")) %>% 
  # Add label
  mutate(status=paste(fmp_yn, assessed_yn, sep="-"),
         status=recode_factor(status, 
                              "no-no"="No FMP/assessment",
                              "no-yes"="FMP but no assessment",
                              "yes-no"="FMP but no assessment",
                              "yes-yes"="Both FMP/assessment"))

# Species without FMPS
spp_no_fmp <- data %>% 
  filter(fmp_yn=="no") %>% 
  select(comm_name, sci_name) %>% 
  unique()

# Status sumamry
status_key <- data %>% 
  group_by(status, comm_name, sci_name) %>% 
  summarize(landings_mt=sum(landings_mt)) %>% 
  ungroup() %>% 
  arrange(status, desc(landings_mt))


# Plot data
################################################################################

# Plot data
g <- ggplot(data %>%  filter(year>=2000), aes(x=year, y=landings_mt/1e3, fill=status)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Catch (1000s mt)") +
  scale_fill_discrete(name="FMP") +
  # Theme
  theme_bw()
g






