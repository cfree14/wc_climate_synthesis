
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rerddap)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/environmental/indices/raw"
outdir <- "data/environmental/indices/processed"

# Source
# https://oceanview.pfeg.noaa.gov/dashboard/
# https://oceanview.pfeg.noaa.gov/erddap/tabledap/index.html?page=1&itemsPerPage=1000

# Marine heatwaves
# 1. Overall daily stats: cciea_OC_MHW
# 2. Regional daily stats: cciea_OC_MHW_regions
# 3. MHW events: cciea_OC_MHW_EV

# Upwelling
# 1. Monthly Coastal Upwelling Transport Index (CUTI): cciea_OC_CUTI
# 2. Monthly Biologically Effective Upwelling Transport Index (BEUTI): cciea_OC_BEUTI
# (3) Length of Upwelling Season Index (LUSI): cciea_OC_LUSI 
# (4) Total Upwelling Magnitude Index (TUMI): cciea_OC_TUMI

# Oscillations
# 1. Pacific Decadal Oscillation Index: cciea_OC_PDO
# 2. Oceanic Nino Index: cciea_OC_ONI
# 3. North Pacific High: cciea_OC_NPH, cciea_OC_NPH_JF
# 4. North Pacific Gyre Oscillation Index: cciea_OC_NPGO
# 5. Extratropical-based Northern Oscillation Index: cciea_OC_NOI
# (6) Multivariate ENSO Index: cciea_OC_MEI

# Other
# Habitat compression index: cciea_EI_HCI
# Spring Transition index: cciea_OC_STI


# Build data
################################################################################

# Look up datasets (to future self: URL must end in /)
datasets_grids <- ed_datasets(which="griddap", url="https://oceanview.pfeg.noaa.gov/erddap/")
datasets_tables <- ed_datasets(which="tabledap", url="https://oceanview.pfeg.noaa.gov/erddap/")


# Heatwaves
#################################################

# Events
#######################

# Get data
data_info <- info("cciea_OC_MHW_EV")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename columns
  rename(max_area_km2=max_area,
         min_coast_dist_km=min_dist_to_coast,
         duration_d=duration,
         mhw_id=blob_id,
         mhw_name=feature_name) %>% 
  # Extact date info
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, 
         mhw_id, mhw_name, 
         duration_d, max_area_km2, max_intensity, mean_intensity, min_coast_dist_km, everything()) %>% 
  select(-time) %>% 
  # Convert to numeric
  mutate(across(.cols=duration_d:min_coast_dist_km, .fns=as.numeric)) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
freeR::complete(data)
range(data$date)

# Export data
saveRDS(data, file.path(outdir, "1983_2021_mhw_events.Rds"))

# Daily stats
#######################

# Get data
data_info <- info("cciea_OC_MHW")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Arrange and gather
  select(time, everything()) %>% 
  gather(key="variable", value="value", 2:ncol(.)) %>% 
  # Add variable type
  mutate(variable_type=ifelse(grepl("intensity", variable), "intensity", "area")) %>% 
  # Extract feature number
  mutate(feature=gsub("intensity_|area_", "", variable) %>% as.numeric()) %>% 
  # Simplify
  select(-variable) %>% 
  # Convert values
  mutate(value=ifelse(value=="-999.0", NA, value), 
         value=as.numeric(value)) %>% 
  # Spread
  spread(key="variable_type", value="value") %>% 
  # Convert intensity
  mutate(intensity=ifelse(is.nan(intensity), NA, intensity)) %>% 
  # Extact date info
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, feature, area, intensity, everything()) %>% 
  select(-time) %>% 
  # Rename
  rename(area_km2=area) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
freeR::complete(data)
range(data$date)

# Export data
saveRDS(data, file.path(outdir, "1982_2020_daily_mhw_stats.Rds"))


# Regional stats
#######################

# Get data
data_info <- info("cciea_OC_MHW_regions")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(deg_day_c_avg=avedegday, 
         deg_day_c_clim_avg=avedegdayclim,
         dist_km=distance,
         coverage_perc=heatwave_cover, 
         intensity_c=intensity) %>% 
  # Extract date info
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(-time) %>% 
  select(year, month, date, region, everything()) %>% 
  # Format region
  mutate(region_id=recode(region, 
                          "wa"=1,
                          "or"=2,
                          "norcal"=3,
                          "cencal"=4,
                          "socal"=5) %>% as.numeric(),
         region=recode(region,
                       "wa"="Washington",
                       "or"="Oregon",
                       "norcal"="Northern California",
                       "cencal"="Central California",
                       "socal"="Southern California")) %>% 
  # Arrange
  select(year, month, date, region_id, region, everything()) %>% 
  # Convert to numeric
  mutate(across(.cols=coverage_perc:deg_day_c_clim_avg, .fns=as.numeric)) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$date)
table(data$region)

# Export data
saveRDS(data, file.path(outdir, "1982_2020_daily_mhw_stats_regional.Rds"))


# Other
#################################################

# HCI
#######################

# Get data
data_info <- info("cciea_EI_HCI")
data_orig <- tabledap(x=data_info)

# Format data
# hci_regn1: Habitat Compression Index, 43.5-48N (fraction below monthly threshold) [0.0, 1.0]
# hci_regn2: Habitat Compression Index, 40-43.5N (fraction below monthly threshold) [0.0, 1.0]
# hci_regn3: Habitat Compression Index, 35.5-40N (fraction below monthly threshold) [0.0, 1.0]
# hci_regn4: Habitat Compression Index, 30-35.5N
data <- data_orig %>% 
  # Gather
  gather(key="region", value="hci", 2:ncol(.)) %>% 
  # Format region
  mutate(region_id=region %>% gsub("hci_regn", "", .) %>% as.numeric(),
         region=recode(region, 
                       "hci_regn1"="43.5-48°N",
                       "hci_regn2"="40-43.5°N",
                       "hci_regn3"="35.5-40°N",
                       "hci_regn4"="30-35.5°N")) %>% 
  # Convert to numeric
  mutate(hci=as.numeric(hci)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(-time) %>% 
  select(year, month, date, region_id, region, hci, everything()) %>% 
  # Convert to tibble
  as_tibble()
  
# Inspect
str(data)
range(data$date)

# Export
saveRDS(data, file.path(outdir, "1980_2021_habitat_compression_index.Rds"))


# HCI
#######################

# Get data
data_info <- info("cciea_OC_STI")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(lat_dd=latitude) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Convert numeric
  mutate(sti=as.numeric(sti),
         lat_dd=as.numeric(lat_dd)) %>% 
  # Arrange
  select(-time) %>% 
  select(lat_dd, year, month, date, sti, everything()) %>% 
  arrange(lat_dd, year) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1967_2020_spring_transition_index.Rds"))


# Large scale
#################################################

# NOI
#######################

# Get data
data_info <- info("cciea_OC_NOI")
data_orig <- tabledap(x=data_info)

# Format data
# NOI = Extratropical-based Northern Oscillation Index
# SOIX = Extratropical-bassed Southern Oscillation Index
# SOI = Southern Oscillation Index
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Convert numeric
  mutate(across(.cols=noi:soi, .fns=as.numeric)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, noi, soix, soi, everything()) %>% 
  select(-time) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1948_2021_noi_soi_soix.Rds"))


# NPGO
#######################

# Get data
data_info <- info("cciea_OC_NPGO")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Convert numeric
  mutate(npgo=as.numeric(npgo)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, npgo, everything()) %>% 
  select(-time) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1950_2021_npgo.Rds"))

# NPH
#######################

# Get data
data_info <- info("cciea_OC_NPH")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(area_million_km2=nph_area) %>% 
  # Convert numeric
  mutate(area_million_km2=as.numeric(area_million_km2)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, area_million_km2, everything()) %>% 
  select(-time) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1967_2021_nph.Rds"))

# NPH-JF
#######################

# Get data
data_info <- info("cciea_OC_NPH_JF")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(area_million_km2=nph_area) %>% 
  # Convert numeric
  mutate(area_million_km2=as.numeric(area_million_km2)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, area_million_km2, everything()) %>% 
  select(-time) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1967_2021_nph_jf.Rds"))

# ONI
#######################

# Get data
data_info <- info("cciea_OC_ONI")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Convert numeric
  mutate(oni=as.numeric(oni)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, oni, everything()) %>% 
  select(-time) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1950_2021_oni.Rds"))


# PDO
#######################

# Get data
data_info <- info("cciea_OC_PDO")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Convert numeric
  mutate(pdo=as.numeric(pdo)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, pdo, everything()) %>% 
  select(-time) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1900_2021_pdo.Rds"))


# ENSO
#######################

# Get data
data_info <- info("cciea_OC_MEI")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(enso=MEI) %>% 
  # Convert numeric
  mutate(enso=as.numeric(enso)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(year, month, date, enso, everything()) %>% 
  select(-time) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1950_2018_enso.Rds"))


# Upwelling
#################################################

# 1. Monthly Coastal Upwelling Transport Index (CUTI): cciea_OC_CUTI
# 2. Monthly Biologically Effective Upwelling Transport Index (BEUTI): cciea_OC_BEUTI
# (3) Length of Upwelling Season Index (LUSI): cciea_OC_LUSI 
# (4) Total Upwelling Magnitude Index (TUMI): cciea_OC_TUMI


# CUTI
#######################

# Get data
data_info <- info("cciea_OC_CUTI")
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(lat_dd=latitude, cuti_m2_s=cuti) %>% 
  # Convert numeric
  mutate(cuti_m2_s=as.numeric(cuti_m2_s),
         lat_dd=as.numeric(lat_dd)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(lat_dd, year, month, date, cuti_m2_s, everything()) %>% 
  select(-time) %>% 
  arrange(lat_dd, date) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1988_2021_cuti.Rds"))


# BEUTI
#######################

# Get data
data_info <- info("cciea_OC_BEUTI")
data_info
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(lat_dd=latitude, beuti_nmol_m_s=beuti) %>% 
  # Convert numeric
  mutate(beuti_nmol_m_s=as.numeric(beuti_nmol_m_s),
         lat_dd=as.numeric(lat_dd)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(lat_dd, year, month, date, beuti_nmol_m_s, everything()) %>% 
  select(-time) %>% 
  arrange(lat_dd, date) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1988_2021_beuti.Rds"))



# LUSI
#######################

# Get data
data_info <- info("cciea_OC_LUSI")
data_info
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(lat_dd=latitude, lusi_days=lusi) %>% 
  # Convert numeric
  mutate(lusi_days=as.numeric(lusi_days),
         lat_dd=as.numeric(lat_dd)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(lat_dd, year, month, date, lusi_days, everything()) %>% 
  select(-time) %>% 
  arrange(lat_dd, date) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1967_2020_lusi.Rds"))



# TUMI
#######################

# Get data
data_info <- info("cciea_OC_TUMI")
data_info
data_orig <- tabledap(x=data_info)

# Format data
data <- data_orig %>% 
  # Rename
  rename(lat_dd=latitude, tumi_m3_s_100m=tumi) %>% 
  # Convert numeric
  mutate(tumi_m3_s_100m=as.numeric(tumi_m3_s_100m),
         lat_dd=as.numeric(lat_dd)) %>% 
  # Extract date
  mutate(year=year(time),
         month=month(time),
         date=date(time)) %>% 
  # Arrange
  select(lat_dd, year, month, date, tumi_m3_s_100m, everything()) %>% 
  select(-time) %>% 
  arrange(lat_dd, date) %>% 
  # Convert to tibble
  as_tibble()

# Inspect
str(data)
range(data$year)

# Export
saveRDS(data, file.path(outdir, "1967_2020_tumi.Rds"))


