
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
data_orig <- readxl::read_excel(file.path(datadir, "PSMFC Dungeness Crab closure.xlsx"), sheet=1, na="NA")


# Format data
################################################################################

# Column names
colnames(data_orig)
cols <- c("release_date", "department", "year", "date", "species_parts", "fishery", "action", "reason", "where", "lat_s", "lat_n", "link", "notes")

# Step 1. Basic formatting
data1 <- data_orig %>% 
  # Rename columns
  setNames(cols) %>%
  rename(comm_name=species_parts) %>% 
  # Format date
  mutate(date=ymd(date))

# Inspect data
str(data1)
range(data1$date)
range(data1$lat_s, na.rm=T)
range(data1$lat_n, na.rm=T)
sum(data1$lat_n <= data1$lat_s, na.rm=T)
table(data1$reason)
table(data1$action)
table(data1$fishery)

# Step 2. One row per fishery
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

# Step 3. Final formatting (for plotting)
data3 <- data2 %>% 
  # Format fishery
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         fishery=stringr::str_to_sentence(fishery)) %>% 
  # Select and arrange columns
  arrange(comm_name, fishery, date) %>% 
  select(comm_name, fishery, date, action, reason, lat_s, lat_n) %>% 
  # Remove non-latitudinal closures (islands, bays, etc) 
  filter(!is.na(lat_s))

# Format for export
events <- data2 %>% 
  # Select and arrange columns
  arrange(comm_name, fishery, date) %>% 
  select(comm_name, fishery, year, date, action, reason, lat_s, lat_n, where, link, notes)
  


# Build season keys
################################################################################

# Dungeness commercial season key
years <- 2004:2012
seasons <- paste(years, years+1-2000, sep="-")
open_wa <- paste0(years, "-12-01") %>% ymd()
close_wa <-  paste0(years+1, "-09-15") %>% ymd()
dcrab_comm_wa <- tibble(comm_name="Dungeness crab", 
                     fishery="Commercial",
                     state="Washington",
                     season=seasons,
                     open=open_wa,
                     close=close_wa)
dcrab_rec_wa <- tibble(comm_name="Dungeness crab", 
                        fishery="Recreational",
                        state="Washington",
                        season=seasons,
                        open=open_wa,
                        close=close_wa)
dcrab_trib_wa <- tibble(comm_name="Dungeness crab", 
                        fishery="Tribal",
                        state="Washington",
                        season=seasons,
                        open=open_wa,
                        close=close_wa)


# Function to build data
################################################################################

# Function to build grid
# data <- data3; species <- "Dungeness crab"; fishery <- "Commercial"; season_key <- dcrab_comm_wa
build_closure_grid <- function(data, species, fishery, season_key){
  
  # Subset data
  fishery_do <- fishery
  sdata <- data %>% 
    filter(comm_name==species & fishery==fishery_do) %>% 
    mutate(action_use=ifelse(action=="close", reason, action))
  
  # Build empty grid
  date1 <- ymd("2003-01-01")
  date2 <- ymd("2012-09-30")
  dates <- seq(date1, date2, by="1 day")
  lat1 <- 46.25
  lat2 <- 48.5
  lats <- seq(lat1, lat2, 0.01)
  closure_grid <- expand.grid(date=dates, lat_dd=lats) %>% 
    as.data.frame() %>% 
    mutate(status="open",
           comm_name=species,
           fishery=fishery_do) %>% 
    select(comm_name, fishery, date, lat_dd, status) %>% 
    arrange(date, lat_dd)
  
  # Loop through announcements
  for(i in 1:nrow(sdata)){
    
    # Get announcement
    date1 <- sdata %>% slice(i) %>% pull(date)
    lat_s <- sdata %>% slice(i) %>% pull(lat_s)
    lat_n <- sdata %>% slice(i) %>% pull(lat_n)
    status_new <- sdata %>% slice(i) %>% pull(action_use)
    
    # Apply announcement
    closure_grid <- closure_grid %>% 
      mutate(status=ifelse(lat_dd>=lat_s & lat_dd <= lat_n & date>=date1, status_new, status))
    
  }
  
  # Apply out-of-season label
  if(species=="Dungeness crab"){
  
    # Subset fishery
    n_key <- season_key %>% 
      filter(fishery==fishery_do & region=="Northern")
    closed_dates_n <- purrr::map_df(2:nrow(n_key), function(x) {
      date1 <- n_key$close[x-1]+1
      date2 <- n_key$open[x]-1
      dates <- tibble(date=seq(date1, date2, by="day"))
    })
    c_key <- season_key %>% 
      filter(fishery==fishery_do & region=="Central")
    closed_dates_c <- purrr::map_df(2:nrow(c_key), function(x) {
      date1 <- c_key$close[x-1]+1
      date2 <- c_key$open[x]-1
      dates <- tibble(date=seq(date1, date2, by="day"))
    })
      
    # Apply out-of-season closures
    closure_grid <- closure_grid %>% 
      # Northern closures
      mutate(status=ifelse(lat_dd >= 38.766488 & date %in% closed_dates_n$date, "out-of-season", status)) %>% 
      # Central closures
      mutate(status=ifelse(lat_dd < 38.766488 & date %in% closed_dates_c$date, "out-of-season", status))
    
  }
  
  # Make factor
  closure_grid <- closure_grid %>% 
    mutate(status=recode_factor(status, 
                                "open"="Season open",
                                "out-of-season"="Out-of-season",
                                "Body condition"="Body condition delay",
                                "Domoic acid"="Domoic acid delay",
                                "Whale entanglement"="Whale entanglement closure")) %>% 
    mutate(status=factor(status, levels=c("Season open",
                                          "Out-of-season",
                                          "Body condition delay",
                                          "Domoic acid delay",
                                          "Whale entanglement closure")))
  
  # Plot closure grid
  title <- paste(species, tolower(fishery_do), "fishery")
  g <- ggplot(closure_grid, aes(x=date, y=lat_dd, fill=status)) +
    # Plot raster
    geom_raster() +
    # Plot events
    geom_segment(data=sdata, mapping=aes(x=date, xend=date, y=lat_s, yend=lat_n, linetype=action, ), inherit.aes = F) +
    # Axis
    scale_x_date(date_breaks="1 year", date_labels = "%Y") +
    scale_y_continuous(breaks=32:42) +
    # Labels
    labs(x="", y="Latitude (°N)", title=title) +
    # Legends
    scale_fill_manual(name="Season status", values=c("grey40", "grey80", "pink", "darkred", "navy"), drop=F) +
    # Theme
    theme_bw()
  print(g)
  
  # Return
  return(closure_grid)
  
}


# Plot indidivually
plot_closures <- function(data){
  
  # Species fishery
  species <- unique(data$comm_name)
  fishery <- unique(data$fishery)
  
  # Setup theme
  my_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    plot.title=element_text(size=10))
  
  # Plot data
  title <- paste(species, tolower(fishery), "fishery")
  g <- ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
    # Plot raster
    geom_raster() +
    # Axis
    scale_x_date(date_breaks="1 year", date_labels = "%Y") +
    scale_y_continuous(breaks=32:42) +
    # Labels
    labs(x="", y="Latitude (°N)", title=title) +
    # Legends
    scale_fill_manual(name="Season status", values=c("grey40", "grey80", "pink", "darkred", "navy"), drop=F) +
    # Theme
    theme_bw() + my_theme 
  print(g)
  
  # Export plot
  outfig <- paste0(tolower(species) %>% gsub(" ", "_", .), "_",
                   tolower(fishery), "_closures.png")
  ggsave(g, filename=file.path(plotdir, outfig), 
         width=6.5, height=3, units="in", dpi=600)
  
}


# Apply individually
################################################################################

# Apply individually
closures_dcrabs_comm <- build_closure_grid(data=data3, species="Dungeness crab", fishery="Commercial", season_key=dcrab_seasons)
closures_dcrabs_rec <- build_closure_grid(data=data3, species="Dungeness crab", fishery="Recreational", season_key=dcrab_seasons)
closures_rcrabs_comm <- build_closure_grid(data=data3, species="Rock crab", fishery="Commercial", season_key=dcrab_seasons)
closures_rcrabs_rec <- build_closure_grid(data=data3, species="Rock crab", fishery="Recreational", season_key=dcrab_seasons)

# Plot individually
plot_closures(closures_dcrabs_comm)
plot_closures(closures_dcrabs_rec)
plot_closures(closures_rcrabs_comm)
plot_closures(closures_rcrabs_rec)


# Merge and export
################################################################################

# Merge together
data <- bind_rows(closures_dcrabs_rec, closures_dcrabs_comm,
                  closures_rcrabs_rec, closures_rcrabs_comm)

# Export data
saveRDS(data, file=file.path(datadir, "CDFW_2015_2020_fishery_closures.Rds"))
write.csv(events, file=file.path(datadir, "CDFW_2015_2020_fishery_closure_announcements.csv"))


