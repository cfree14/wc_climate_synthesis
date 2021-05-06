

##Test

#xbuild_closure_grid <- function(data, species, fishery, region, season_key){
  
  # Subset data
  fishery_do <- fishery
  region_do <- region
  
  sdata <- data4 %>% 
    filter(comm_name == "Dungeness crab" & fishery == "Commercial",
           region == "Ocean" ) %>% 
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
           comm_name = "Dungeness crab",
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

########################################################################
##Trouble shoot Razor Clam


sdata <- data4 %>% 
  filter(comm_name == "Razor clam" & fishery == "recreational") %>% 
  mutate(action_use = ifelse(action =="close", reason, action))

# Build empty grid
date1 <- ymd("2016-01-01")
date2 <- ymd("2021-12-31")
dates <- seq(date1, date2, by="1 day")
lat1 <- 41.9981226 # CA-OR border
lat2 <- 46.15 # WA/OR border
lats <- seq(lat1, lat2, 0.1)

closure_grid <- expand.grid(date = dates, lat_dd = lats) %>% 
  as.data.frame() %>% 
  mutate(status ="open",
         comm_name = "Razor clam",
         fishery = "Recreational") %>% 
  select(comm_name, fishery, date, lat_dd, status) %>% 
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

############################################################################
  
  build_closure_grid <- function(data, species, fishery, region, season_key){
    
    # Subset data
    fishery_do <- "Commercial"
    region_do <- "Ocean"
    species <- "Dungeness crab"
    
    sdata <- data4 %>% 
      filter(comm_name == species & fishery == fishery_do,
             region == region_do ) %>% 
      mutate(action_use = ifelse(action =="close", reason, action))
    
    # Build empty grid
    date1 <- ymd("2016-09-30")
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



