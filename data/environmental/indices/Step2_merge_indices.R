
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/environmental/indices/raw"
outdir <- "data/environmental/indices/processed"
plotdir <- "data/environmental/indices/figures"

# Coastwide
pdo <- readRDS(file.path(outdir, "1900_2021_pdo.Rds"))
noi <- readRDS(file.path(outdir, "1948_2021_noi_soi_soix.Rds"))
enso <- readRDS(file.path(outdir, "1950_2018_enso.Rds"))
npgo <- readRDS(file.path(outdir, "1950_2021_npgo.Rds"))
oni <- readRDS(file.path(outdir, "1950_2021_oni.Rds"))
nph <- readRDS(file.path(outdir, "1967_2021_nph.Rds"))
mhw <- readRDS(file.path(outdir, "1982_2020_daily_mhw_stats.Rds"))

# Latitude-specific
sti <- readRDS(file.path(outdir, "1967_2020_spring_transition_index.Rds"))
cuti <- readRDS(file.path(outdir, "1988_2021_cuti.Rds"))
beuti <- readRDS(file.path(outdir, "1988_2021_beuti.Rds"))
lusi <- readRDS(file.path(outdir, "1967_2020_lusi.Rds"))
tumi <- readRDS(file.path(outdir, "1967_2020_tumi.Rds"))

# Region-specific
hci <- readRDS(file.path(outdir, "1980_2021_habitat_compression_index.Rds"))


# Merge coastwide data
################################################################################

# Format individuals
pdo1 <- pdo %>% 
  mutate(index="PDO") %>% 
  rename(value=pdo)

noi1 <- noi %>% 
  select(-c(soi, soix)) %>% 
  mutate(index="NOI") %>% 
  rename(value=noi)

enso1 <- enso %>% 
  mutate(index="ENSO") %>% 
  rename(value=enso)

npgo1 <- npgo %>% 
  mutate(index="NPGO") %>% 
  rename(value=npgo)

oni1 <- oni %>% 
  mutate(index="ONI") %>% 
  rename(value=oni)

sti1 <- sti %>% 
  mutate(index="STI") %>% 
  rename(value=sti)

nph1 <- nph %>% 
  mutate(index="NPH",
         units="Millions sqkm") %>% 
  rename(value=area_million_km2)

# Merge
data <- bind_rows(pdo1, noi1, enso1, npgo1, oni1, nph1) %>% 
  # Format index names
  rename(index_abbrev=index) %>% 
  mutate(index=recode(index_abbrev, 
                      "ENSO"="El Niño-Southern Oscillation",
                      "NOI"="Northern Oscillation",
                      "NPGO"="North Pacific Gyre Oscillation",
                      "NPH"="North Pacific High",
                      "ONI"="Oceanic Niño High",
                      "PDO"="Pacific Decadal Oscillation"),
         index_label=paste0(index, " (", index_abbrev, ")")) %>% 
  # Arrange
  select(index, index_abbrev, index_label,
       year, month, date, value, units, everything())

# Export
saveRDS(data, file=file.path(outdir, "indices_coastwide.Rds"))


# Merge latitudinal data
################################################################################

sti1 <- sti %>% 
  rename(value=sti) %>% 
  mutate(units="day of year", 
         index_abbrev="STI")

cuti1 <- cuti %>% 
  rename(value=cuti_m2_s) %>% 
  mutate(units="m2/s", 
         index_abbrev="CUTI")

beuti1 <- beuti %>% 
  rename(value=beuti_nmol_m_s) %>% 
  mutate(units="nmol/m/s", 
         index_abbrev="BUTI")

lusi1 <- lusi %>% 
  rename(value=lusi_days) %>% 
  mutate(units="days", 
         index_abbrev="LUSI")
  
tumi1 <- tumi %>% 
  rename(value=tumi_m3_s_100m) %>% 
  mutate(units="m3/s/100m of coastline", 
         index_abbrev="TUMI")

# Merge data
data_lat <- bind_rows(sti1, cuti1, beuti1, lusi1, tumi1) %>% 
  # Add index
  mutate(index=recode(index_abbrev,
                      "STI"="Spring Transition Index",
                      "CUTI"="Coastal Upwelling Transport Index",
                      "BEUTI"="Biologically Effective Upwelling Transport Index",
                      "LUSI"="Length of Upwelling Season Index", 
                      "TUMI"="Total Upwelling Magnitude Index"),
         index_label=paste0(index, "\n(", index_abbrev, "; ", units, ")")) %>% 
  # Arrange
  select(index, index_abbrev, index_label,
         lat_dd,
         year, month, date, 
         value, units, everything())

# Export
saveRDS(data_lat, file=file.path(outdir, "indices_latitude.Rds"))
