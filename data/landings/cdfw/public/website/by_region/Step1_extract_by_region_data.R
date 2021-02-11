

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(tabulizer)
library(pdftools)

# Directories
rawdir <- "data/landings/cdfw/public/website/raw"
outdir <- "data/landings/cdfw/public/website/by_region/processed"


# Merge data
################################################################################

# Years to merge
years <- 2000:2019

# Merge data
table15_orig <- purrr::map_df(years, function(x){
  
  # File to read
  indir <- file.path(rawdir, x, "other")
  infile <- file.path(indir, "Table15.pdf")
  
  # Read tables
  tables_list <- tabulizer::extract_tables(infile, output = "data.frame", method="stream")
  
  # Merge file data
  fdata <- purrr::map_df(1:length(tables_list), function(y){
  
    # Column names
    col_names <- c("Species", "Eureka", "Fort Bragg", 
                   "Bodega Bay", "San Francisco", "Monterey", "Morro Bay", 
                   "Santa Barbara", "Los Angeles", "San Diego", "Total")
    
    # Extract table
    tdata <- tables_list[[y]] %>% 
      # Set names
      setNames(col_names) %>% 
      # Convert to character to ease merge
      mutate_all(as.character) %>% 
      # Add columns
      mutate(year=x,
             table="Table 15") %>% 
      # Arrange
      select(year, everything())
    
  })
  
  # Return
  fdata
  
})

# Merge data
table15a_orig <- purrr::map_df(2001:2019, function(x){
  
  # File to read
  indir <- file.path(rawdir, x, "other")
  infile <- file.path(indir, "Table15a.pdf")
  
  # Read tables
  tables_list <- tabulizer::extract_tables(infile, output = "data.frame", method="stream")
  
  # Merge file data
  fdata <- purrr::map_df(1:length(tables_list), function(y){
    
    # Column names
    ncols <- ncol(tables_list[[y]])
    if(ncols==5){
      col_names <- c("Species", "Sacramento Delta", "Inland Waters", "Unknown", "Total")
    }else{
      col_names <- c("Species", "Extra", "Sacramento Delta", "Inland Waters", "Unknown", "Total")
    }
    
    # Extract table
    tdata <- tables_list[[y]] %>% 
      # Set names
      setNames(col_names) %>% 
      # Convert to character to ease merge
      mutate_all(as.character) %>% 
      # Add columns
      mutate(year=x,
             table="Table 15a") %>% 
      # Arrange
      select(year, table, everything())
    
  })
  
  # Return
  fdata
  
})


# Format data
################################################################################

# Format Table 15
table15 <- table15_orig %>% 
  # Add id
  # mutate(id=1:n()) %>% 
  # Rename
  rename(comm_name_orig=Species) %>% 
  # Remove empty rows
  filter(Eureka!="" & comm_name_orig!="Species") %>% 
  # Format species
  mutate(comm_name_orig=gsub("\\.", "", comm_name_orig) %>% stringr::str_trim(),
         comm_name_orig=gsub(" pounds| value| Pounds| Value", "", comm_name_orig),
         comm_name_orig=ifelse(comm_name_orig=="", NA, comm_name_orig)) %>% 
  fill(comm_name_orig, .direction="down") %>% 
  # Format totals
  mutate(comm_name_orig=ifelse(grepl("total", tolower(comm_name_orig)), "Totals", comm_name_orig)) %>% 
  # Add source
  mutate(source=paste("CDFW", year+1)) %>%
  # Add metric
  mutate(metric=rep(c("pounds", "value"), nrow(.)/2)) %>%
  # Arrange
  select(source, table, year, comm_name_orig, metric, everything()) %>% 
  # Gather
  gather(key="port_complex", value="value", 6:ncol(.)) %>%
  # Spread
  spread(key="metric", value="value") %>% 
  # Convert to numeric
  mutate(pounds=gsub(",", "", pounds) %>% as.numeric(),
         value=gsub(",|\\$", "", value) %>% as.numeric())

# Inspect
str(table15)
freeR::complete(table15)


# QA/QC data
################################################################################

# Check species totals
# Look great
check_row <- table15 %>% 
  group_by(year, comm_name_orig) %>% 
  summarize(pounds_obs=sum(pounds[port_complex!="Total"]),
            pounds_rep=pounds[port_complex=="Total"],
            pounds_diff=pounds_rep-pounds_obs,
            value_obs=sum(value[port_complex!="Total"]),
            value_rep=value[port_complex=="Total"],
            value_diff=value_rep-value_obs)

# Check port complex totals
check_cols <- table15 %>% 
  group_by(year, port_complex) %>% 
  summarize(pounds_obs=sum(pounds[comm_name_orig!="Totals"]),
            pounds_rep=pounds[comm_name_orig=="Totals"],
            pounds_diff=pounds_rep-pounds_obs,
            value_obs=sum(value[comm_name_orig!="Totals"]),
            value_rep=value[comm_name_orig=="Totals"],
            value_diff=value_rep-value_obs)


# Format Table 15a data
################################################################################

# Format Table 15a
table15a <- table15a_orig %>% 
  # Rename
  rename(comm_name_orig=Species) %>% 
  # Remove extra column
  select(-Extra) %>% 
  # Remove empty rows
  filter(Total!="" & comm_name_orig!="Species") %>% 
  # Format species
  mutate(comm_name_orig=gsub("\\.|:", "", comm_name_orig),
         comm_name_orig=gsub(" Pounds| Value", "", comm_name_orig)) %>% 
  # Format totals
  mutate(comm_name_orig=ifelse(`Sacramento Delta`=="80,460", "Grand Total", comm_name_orig)) %>% 
  # Fill species gaps
  mutate(comm_name_orig=ifelse(comm_name_orig=="", NA, comm_name_orig)) %>% 
  fill(comm_name_orig, .direction="down") %>% 
  # Add source
  mutate(source=paste("CDFW", year+1)) %>%
  # Add metric
  mutate(metric=ifelse(grepl("\\$", Total), "value", "pounds")) %>%
  # Arrange
  select(source, table, year, comm_name_orig, metric, everything()) %>% 
  # Gather
  gather(key="port_complex", value="value", 6:ncol(.)) %>%
  # Spread
  spread(key="metric", value="value") %>% 
  # Convert to numeric
  mutate(pounds=gsub(",", "", pounds) %>% as.numeric(),
         value=gsub(",|\\$", "", value) %>% as.numeric()) %>% 
  # Fill in missing values
  mutate(value=ifelse(pounds==189920, 230590, value),
         value=ifelse(pounds==223601, 260184, value),
         value=ifelse(pounds==33681, 29594, value),
         value=ifelse(pounds==400, 1900, value),
         value=ifelse(is.na(value), 0, value)) %>% 
  # Format name of totasl
  mutate(comm_name_orig=recode(comm_name_orig, "Grand Total"="Totals"))

# Inspect
str(table15a)
freeR::complete(table15a)

# QA/QC TAble 15a data
################################################################################

# Check species totals
# Look great
check_row <- table15a %>% 
  group_by(year, comm_name_orig) %>% 
  summarize(pounds_obs=sum(pounds[port_complex!="Total"]),
            pounds_rep=pounds[port_complex=="Total"],
            pounds_diff=pounds_rep-pounds_obs,
            value_obs=sum(value[port_complex!="Total"]),
            value_rep=value[port_complex=="Total"],
            value_diff=value_rep-value_obs)

# Check port complex totals
check_cols <- table15a %>% 
  group_by(year, port_complex) %>% 
  summarize(pounds_obs=sum(pounds[comm_name_orig!="Totals"]),
            pounds_rep=pounds[comm_name_orig=="Totals"],
            pounds_diff=pounds_rep-pounds_obs,
            value_obs=sum(value[comm_name_orig!="Totals"]),
            value_rep=value[comm_name_orig=="Totals"],
            value_diff=value_rep-value_obs)


# Merge data
################################################################################

# Merge data
data <- bind_rows(table15, table15a) %>% 
  # Remove totals
  filter(comm_name_orig!="Totals" & port_complex!="Total") %>% 
  # Add presentation
  mutate(presentation="not specified",
         presentation=ifelse(grepl("roe on kelp", tolower(comm_name_orig)), "roe on kelp",
                             ifelse(grepl("roe", tolower(comm_name_orig)), "roe", presentation)),
         presentation=ifelse(grepl("claws", tolower(comm_name_orig)), "claws", presentation)) %>% 
  # Clean common names
  mutate(comm_name_reg=wcfish::convert_names(comm_name_orig, to="regular")) %>% 
  mutate(comm_name_reg=recode(comm_name_reg, 
                              'Black tuna skipjack'='Black skipjack tuna', 
                              'Butterfish (Pacific pompano'='Butterfish (Pacific pompano)', 
                              'Claws crab'='Crab', 
                              'Coho salmon roe (chinook'='Chinook/coho salmon', 
                              'Coho) salmon roe (chinook'='Chinook/coho salmon', 
                              'Copper (whitebelly) rockfish'='Copper rockfish', 
                              'Curlfin turbot'='Curlfin sole', 
                              'Dolphin (fish)'='Dolphinfish', 
                              'Giant pacific oyster'='Giant Pacific oyster', 
                              'Group black/blue rockfish'='Black/blue rockfish group', 
                              'Group bocaccio/c rockfish'='Bocaccio/chilipepper rockfish group', 
                              'Group bocaccio/chili rockfish'='Bocaccio/chilipepper rockfish group', 
                              'Group bolina rockfish'='Bolina rockfish group', 
                              'Group canary/verm rockfish'='Canary/vermilion rockfish group', 
                              'Group canary/vermili rockfish'='Canary/vermilion rockfish group', 
                              'Group deep nears rockfish'='Deep nearshore rockfish group', 
                              'Group deep nearshor rockfish'='Deep nearshore rockfish group', 
                              'Group deep nearshore rockfish'='Deep nearshore rockfish group', 
                              'Group deepwater reds rockfish'='Deep nearshore rockfish group', 
                              'Group gopher rockfish'='Gopher rockfish group', 
                              'Group nearshore rockfish'='Nearshore rockfish group', 
                              'Group red rockfish'='Red rockfish group', 
                              'Group rosefish rockfish'='Rosefish rockfish group', 
                              'Group shelf rockfish'='Shelf rockfish group', 
                              'Group slope rockfish'='Slope rockfish group', 
                              'Group small rockfish'='Small rockfish group', 
                              'Hagfishes'='Unspecified hagfish', 
                              'Herring roe on kelp'='Pacific herring', 
                              'Invertebrate Unspecified'='Unspecified invertebrate', 
                              'Monkeyface (ee prickleback'='Monkeyface prickleback', 
                              'Monkeyface (eel) prickleback'='Monkeyface prickleback', 
                              'Monkeyface (pricklebac eel'='Monkeyface prickleback', 
                              'Monkeyface (prickleback) eel'='Monkeyface prickleback', 
                              'Monkeyface eel'='Monkeyface prickleback', 
                              'Ocean (pink) shrimp'='Pink (ocean) shrimp', 
                              'Pacific - roe herring'='Pacific herring', 
                              'Pacific - roe on kelp herring'='Pacific herring', 
                              'Pacific ocean per rockfish'='Pacific ocean perch rockfish', 
                              'Pacific Pomfret'='Pacific pomfret', 
                              'Red urchin'='Red sea urchin', 
                              'Rock unspecified crab'='Rock crab', 
                              'Roe (chinook and co salmon'='Chinook/coho salmon', 
                              'Roe (chinook and coh salmon'='Chinook/coho salmon', 
                              'Roe (chinook and coho salmon'='Chinook/coho salmon', 
                              'Roe herring'='Pacific herring', 
                              'Spider/sheep claws crab'='Spider/sheep crab', 
                              'Spotted cusk- eel'='Spotted cusk-eel', 
                              'True smelts'='True smelt', 
                              'Unspecified jacks'='Unspecified jack', 
                              'Wolf (wolf-eel) eel'='Wolf eel')) %>% 
  # Harmonize common/scientific names
  mutate(comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm"),
         sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Rename
  rename(landings_lb=pounds, value_usd=value) %>% 
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  # Arrange
  select(-c(comm_name_reg)) %>% 
  select(source, table, year, port_complex, 
         comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg, value_usd, everything()) %>% 
  arrange(year, port_complex, comm_name)

# Inspect common names
# wcfish::check_names(data$comm_name_reg)

# Inspect
str(data)
freeR::complete(data)


# Plot data
################################################################################

# Build stats
stats <- data %>% 
  group_by(year, port_complex) %>% 
  summarize(landings_lb=sum(landings_lb))

# Plot
g <- ggplot(stats, aes(x=year, y=landings_lb/1e6, fill=port_complex)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of lbs)") +
  theme_bw()
g

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_2000_2019_landings_by_region_species.Rds"))



