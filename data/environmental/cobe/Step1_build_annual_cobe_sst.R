

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(tidyverse)

# Directories
indir <- "data/environmental/cobe/raw"
outdir <- "data/environmental/cobe/processed"

# Read data
data_orig <- raster::brick(file.path(indir, "sst.mon.mean.nc"))


# Format monthly data
################################################################################

# Format monthly data
data_mo <- data_orig %>% 
  # Convert from 0-360 to -180-180
  rotate()

# Check
plot(data_orig[[1]])
plot(data[[1]])


# Calculate annual statistics
################################################################################

# Function to calculate an annual statistic
calc_annual_stats <- function(dataset, stat){
  
  # Build a key labelling date years and decades
  dates <- names(dataset) %>% gsub("X", "", .) %>% ymd()
  date_key <- tibble(date=dates, year=year(dates), index=1:length(dates), decade=floor(year/10)*10)
  decades <- sort(unique(date_key$decade))
  
  # Calculate annual stats for years in a decade
  brick_list <- sapply(decades, function(x){
    layers_do <- date_key$index[date_key$decade==x]
    yrs_do <- date_key$year[date_key$decade==x]
    # If fun=sd b/c sd arbitrarily doesn't work in quotes
    if(stat=="sd"){
      brick_do <- stackApply(dataset[[layers_do]], indices=yrs_do, fun=sd, na.rm=T)
    }else{
      brick_do <- stackApply(dataset[[layers_do]], indices=yrs_do, fun=stat, na.rm=T)
    }
    return(brick_do)
  })
  
  # Merge annual stats from each decade file
  data_brick <- brick(brick_list)
  names(data_brick) <- layer_names_annual <- gsub("index_", "", names(data_brick))
  
  # Return brick
  return(data_brick)
  
}

# Calculate annual average
data_yr <- calc_annual_stats(dataset=data_mo, stat="mean")

# Plot check
plot(data_yr[[1]])
plot(data_yr[[100]])

# Export annual data
################################################################################

# Export data
writeRaster(data_yr, file=file.path(outdir, "COBE_1891_2021_annual_sst_mean.grd"), overwrite=T)

