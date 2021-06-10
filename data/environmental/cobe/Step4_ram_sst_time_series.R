

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)

# Directories
ramdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data"
sstdir <- "data/environmental/cobe/processed"
fgdir <- "data/environmental/cobe/fishing_grounds"

# Read COBE SST annual averages
sst_brick <- raster::brick(file.path(sstdir, "COBE_1891_2021_annual_sst_mean.grd"))

# Read RAM stock boundaries
load(file.path(ramdir, "boundaries_final", "ramldb_boundaries_spdf.Rdata"))
ram <- boundaries_merged
rm(boundaries_merged)

# Read RAM stock boundary key (for getting stock ids)
ram_key <- readxl::read_excel(file.path(ramdir, "ramldb_v3.8_stock_boundary_table_v2_formatted.xlsx"))

# Build annual averages
################################################################################

# RAM assess ids
assessids <- ram$assessid %>% as.character()

# Extract averages
sst_ram <- extract(sst_brick, ram, method="simple", fun=mean, na.rm=T)

# Format data
sst_ram2 <- sst_ram %>% 
  # Convert to data frame
  as.data.frame() %>% 
  # Add assessids
  mutate(assessid=assessids) %>% 
  # Arrange and gather
  dplyr::select(assessid, everything()) %>% 
  gather(key="year", value="sst_c_avg", 2:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X", "", .) %>% as.numeric()) %>% 
  # Add stockid
  left_join(ram_key %>% dplyr::select(assessid, stockid), by="assessid") %>% 
  # Arrange
  dplyr::select(stockid, year, sst_c_avg) %>% 
  # Remov 2021 since incomplete
  filter(year!=2021)

# Inspect
str(sst_ram2)
freeR::complete(sst_ram2)

# Which stockids are missing values?
# 22 stocks don't have SST data -- I bet they are inland
stockids_without <- sst_ram2 %>% 
  group_by(stockid) %>% 
  summarize(nmissing=sum(is.na(sst_c_avg))) %>% 
  filter(nmissing>0)


# Export data
################################################################################

# Export
write.csv(sst_ram2, file=file.path(datadir, "COBE_1981_2020_by_ram_stocks.csv"), row.names=F)

