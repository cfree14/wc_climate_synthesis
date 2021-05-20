## Format closure data from ODFW news releases
## April 9, 2021


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/closures/data"
plotdir <- "data/closures/figures"

# Read data
data_orig <- readr::read_csv(file.path(datadir, "ODFW_DungenessCrab_closure.csv"))
# Read season key
season_key <- readxl::read_excel(file.path(datadir, "dcrab_season_key.xlsx"))

# Format data
################################################################################

# Column names
colnames(data_orig)
cols <- c("release_date", "department", "year", "date", "species_parts", "fishery", "action", "assuption", "reason", "where", "region", "lat_s", "lat_n", "source", "link", "notes")

# Step 1. Basic formatting
data1 <- data_orig %>% 
  # Rename columns
  setNames(cols) %>%
  rename(comm_name=species_parts) %>% 
  # Format date
  mutate(date=mdy(date)) %>% 
  # TEMPORARY
  filter(!is.na(reason)) %>% 
  filter(!reason %in% c("season", "summer season", "summer extension")) %>% 
  mutate(year=as.numeric(year),
         region = recode(region,
                       "bays" = "bay",
                       "ocean, bay, estuaries" = "ocean, bay"),
         region = ifelse(is.na(region) & fishery %in% c("commercial", "commercial/recreational"), "ocean", region), ## assuming that all commercial opening/closure are in the ocean
         comm_name = recode(comm_name,
                            "Crabs" = "Dungeness Crab",
                            "Red rock crab, dungeness crab" = "Dungeness Crab"))
  

# Inspect data
str(data1)
range(data1$date, na.rm=T)
range(data1$lat_s, na.rm=T)
range(data1$lat_n, na.rm=T)
sum(data1$lat_n <= data1$lat_s, na.rm=T) ## ASK CHRIS
table(data1$reason)
table(data1$action)
table(data1$fishery)
table(data1$region)
table(data1$comm_name)


# Step 2. One row per fishery (commercial/recreational)

data2 <- purrr::map_df(1:nrow(data1), function(x) {
  
  # Get row
  row <- data1 %>% slice(x)
  
  # Identify species represented in row
  fisheries_in_row_string <- row %>% pull(fishery)
  fisheries_in_row_list <- strsplit(fisheries_in_row_string, split="/")
  fisheries_in_row_cvec <- unlist(fisheries_in_row_list)
  
  # Duplicate row, if necessary
  nfisheries <- length(fisheries_in_row_cvec)
  if(nfisheries>1){
    row_out <- row %>% 
      slice(rep(1:n(), each=nfisheries)) %>% 
      mutate(fishery=fisheries_in_row_cvec)
  }else{
    row_out <- row
  }
  
})


# Step 3. One row per area (ocean, bay)

data3 <- purrr::map_df(1:nrow(data2), function(x) {
  
  # Get row
  row <- data2 %>% slice(x)
  
  # Identify species represented in row
 region_in_row_string <- row %>% pull(region)
  region_in_row_list <- strsplit(region_in_row_string, split=",")
  region_in_row_cvec <- unlist(region_in_row_list)
  
  # Duplicate row, if necessary
  nregion <- length(region_in_row_cvec)
  if(nregion>1){
    row_out <- row %>% 
      slice(rep(1:n(), each=nregion)) %>% 
      mutate(region = region_in_row_cvec)
  }else{
    row_out <- row
  }
  
})


# Step 4. Final formatting (for plotting)
data4 <- data3 %>% 
  # Format fishery
  mutate(comm_name = stringr::str_to_sentence(comm_name),
         fishery = stringr::str_to_sentence(fishery),
         region = stringr::str_to_sentence(region)) %>% 
  # Select and arrange columns
  arrange(comm_name, fishery, date) %>% 
  select(comm_name, fishery, date, action, reason, region, lat_s, lat_n) %>% 
  # Remove non-latitudinal closures (islands, bays, etc) 
  filter(!is.na(lat_s))

# Format for export
# events <- data3 %>% 
#   # Select and arrange columns
#   arrange(comm_name, fishery, date) %>% 
#   select(comm_name, fishery, year, date, action, reason, region, lat_s, lat_n, where, link, notes)



# Build season keys
################################################################################

# Years
years <- 2010:2021

# Build annual open/close date
season_key1 <- purrr::map_df(years, function(x){
  season_key %>% 
    mutate(year1 = x)
})

# Format annual open/close date
season_key2 <- season_key1 %>% 
  mutate(year2=year1+1) %>% 
  select(year1, year2, everything()) %>% 
  # Format open date
  mutate(open=paste(year1, open_short, sep="-"),
         open=ifelse(grepl("NA", open), NA, open),
         open=ymd(open)) %>% 
  # Format close date
  mutate(close=ifelse(state_region!="Oregon (bay)", paste(year2, close_short, sep="-"), paste(year1, close_short, sep="-")),
         close=ifelse(grepl("NA", close), NA, close),
         close=ymd(close)) %>% 
  # Simplify
  select(type, state_region, state, region, lat_n, lat_s, year1, open, close)




# Function to build data
################################################################################

# Function to build grid
# data <- data4; species <- "Dungeness crab"; fishery <- "Commercial"; season_key <- season_key2

build_closure_grid <- function(data, species, fishery, region, season_key){
  
  # Subset data
  fishery_do <- fishery
  region_do <- region
  
  sdata <- data %>% 
    filter(comm_name == species & fishery == fishery_do,
           region == region_do ) %>% 
    mutate(action_use = ifelse(action =="close", reason, action))
  
  # Build empty grid
  date1 <- ymd("2010-09-30")
  date2 <- ymd("2021-09-30")
  dates <- seq(date1, date2, by="1 day")
  lat1 <- 42 # Point Arenas
  lat2 <- 46.15 # WA/OR border
  lats <- seq(lat1, lat2, 0.1)
  
  closure_grid <- expand.grid(date = dates, lat_dd = lats) %>% 
    as.data.frame() %>% 
    mutate(status ="open",
           comm_name = species,
           fishery = fishery_do,
           region = region_do) %>% 
    select(comm_name, fishery, date, lat_dd, status, region) %>% 
    arrange(date, lat_dd)
  
  # Loop through announcements
  for(i in 1:nrow(sdata)){
    
    # Get announcement
    print(i)
    date1 <- sdata %>% slice(i) %>% pull(date)
    lat_s <- sdata %>% slice(i) %>% pull(lat_s)
    lat_n <- sdata %>% slice(i) %>% pull(lat_n)
    status_new <- sdata %>% slice(i) %>% pull(action_use)
    
    # Apply announcement
    closure_grid <- closure_grid %>% 
      mutate(status = ifelse(lat_dd >= lat_s & lat_dd <= lat_n & date >= date1, status_new, status))
    
  }
  
  # Apply out-of-season label
  closure_grid2 <- closure_grid
  regions_do <- c("Washington", "Oregon (ocean)", "California (northern)", "California (central)")
  for(i in 1:length(regions_do)){
    
    region_do <- regions_do[i]
    region_key <- season_key %>% 
      filter(type==fishery_do, state_region==region_do)
    
    dates_in <- purrr::map_df(1:nrow(region_key), function(x){
      dates_in_yr <- seq(region_key$open[x], region_key$close[x], by="1 day")
      df <- tibble(date=dates_in_yr)
    })
    
    dates_out <- dates[!dates%in%dates_in$date]
    
    lat_n <- region_key$lat_n %>% unique()
    lat_s <- region_key$lat_s %>% unique()
    
    closure_grid2 <- closure_grid2 %>% 
      mutate(status=ifelse(lat_dd<=lat_n & lat_dd>lat_s & date %in% dates_out, "out-of-season", status))
    
  }
  
  # Plot closure grid
  g <- ggplot(closure_grid2, aes(x=date, y=lat_dd, fill=status)) +
    # Plot raster
    geom_raster() +
    # Plot events
    # geom_segment(data=sdata, mapping=aes(x=date, xend=date, y=lat_s, yend=lat_n, linetype=action, ), inherit.aes = F) +
    # Plot state lines
    geom_hline(yintercept=c(41.9981226, 46.15), size=1) +
    # Axis
    scale_x_date(date_breaks="1 year", date_labels = "%Y") +
    scale_y_continuous(breaks=seq(32, 50, 2)) +
    # Labels
    labs(x="", y="Latitude (°N)") +
    # Legends
    # scale_fill_manual(name="Season status", values=c("grey40", "grey80", "pink", "darkred", "navy"), drop=F) +
    # Theme
    theme_bw()
  print(g)
  
  # Return
  return(closure_grid2)
  
}

# Plot indidivually
plot_closures <- function(data){
  
  # Species fishery
  # species <- unique(data$comm_name)
  # fishery <- unique(data$fishery)
  
  # Factor statuses
  status_all <- sort(unique(data$status))
  status_other <- status_all[!status_all%in%c("open", "out-of-season")]
  status_order <- c("open", "out-of-season", status_other)
  data_plot <- data %>% 
    mutate(status=factor(status, levels=status_order))
  other_colors <- rainbow(length(status_other))
  
  # Setup theme
  my_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    plot.title=element_text(size=10))
  
  # Plot data
  # title <- paste(species, tolower(fishery), "fishery")
  g <- ggplot(data_plot, aes(x=date, y=lat_dd, fill=status)) +
    # Plot raster
    geom_raster() +
    # Plot state lines
    geom_hline(yintercept=c(41.9981226, 46.15), size=1) +
    # Axis
    scale_x_date(date_breaks="1 year", date_labels = "%Y") +
    scale_y_continuous(breaks=seq(32, 50, 2)) +
    # Labels
    labs(x="", y="Latitude (°N)") +
    # Legends
    scale_fill_manual(name="Season status", values=c("grey80", "white", other_colors), drop=F) +
    # Theme
    theme_bw() + my_theme 
  print(g)
  
  # # Export plot
  # outfig <- paste0(tolower(species) %>% gsub(" ", "_", .), "_",
  #                  tolower(fishery), "_closures.png")
  # ggsave(g, filename=file.path(plotdir, outfig), 
  #        width=6.5, height=3, units="in", dpi=600)
  
}


# Apply individually
################################################################################


comm <- build_closure_grid(data = data4, species ="Dungeness crab", fishery = "Commercial", region = "Ocean", season_key = season_key2)

g <- plot_closures(comm)

##NEEDS A LOT OF CHECKS
comm_bay <- build_closure_grid(data = data4, species ="Dungeness crab", fishery = "Commercial", region = "Bay", season_key = season_key2)

com_bay_plot <- plot_closures(comm_bay)

##Not working
rec_bay <- build_closure_grid(data = data4, species ="Dungeness crab", fishery = "Recreational", region = "Bay", season_key = season_key2)

rec_ocean <- build_closure_grid(data = data4, species ="Dungeness crab", fishery = "Recreational", region = "Ocean", season_key = season_key2)




