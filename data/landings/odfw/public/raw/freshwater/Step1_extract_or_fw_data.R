

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
inputdir <- "data/landings/odfw/public/raw/freshwater/raw"
outputdir <- "data/landings/odfw/public/raw/freshwater/processed"
plotdir <- "data/landings/odfw/public/raw/freshwater/figures"

# Build file key
################################################################################

# Build file key
file_key <- tibble(filename_orig=list.files(inputdir, pattern=".pdf")) %>% 
  mutate(year=recode(filename_orig, 
                     "63 (1).pdf"="2007",                                                 
                     "63 (2).pdf"="2006",                                                  
                     "63 (3).pdf"="2005",                                                   
                     "63 (4).pdf"="2004",                                                  
                     "63_Freshwater_species_harvested_by_month_by_county (1).pdf"="2010",   
                     "63_Freshwater_species_harvested_by_month_by_county (2).pdf"="2009",  
                     "63_Freshwater_species_harvested_by_month_by_county.pdf"="2011",       
                     "63.pdf"="2008",                                                      
                     "CRAYFISH BRINE SHRIMP 2014.pdf"="2014",                               
                     "CRAYFISH BY COUNTY 2017.pdf"="2017",                                 
                     "CRAYFISH_2013.pdf"="2013",                                            
                     "CRAYFISH_2016.pdf"="2016",                                           
                     "FRESH WATER BY COUNTY.pdf"="2018",                                    
                     "FRESH_WATER_SPECIES_BY_COUNTY.pdf"="2015",                           
                     "Table 63 - Crayfish pounds.pdf"="2012") %>% as.numeric()) %>% 
  arrange(year)



# Merge data
################################################################################

if(F){
  
  # Merge freshwater data
  # for(x in 2018:2004){
  data_orig <- purrr::map_df(2018:2004, function(x) {
    
    # Read data
    print(x)
    infile <- file_key %>% filter(year==x) %>% pull(filename_orig)
    tables_list <- tabulizer::extract_tables(file.path(inputdir, infile), output = "data.frame", method="stream")
    fdata_orig <- tables_list[[1]]
    
    fdata <- fdata_orig
    
    # 2015-209
    if(x>2014){
      fdata <- fdata_orig %>% 
        setNames(c("county", "trash", "type", 
                   "January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December", "Total")) %>% 
        mutate(filename=infile,
               year=x)
    }
    
    # 2013-2014
    if(x %in% 2013:2014){
      fdata <- fdata_orig %>% 
        setNames(c("county", "type", 
                   "January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December", "Total")) %>% 
        mutate(filename=infile,
               year=x)
      
    }
    
    # 2004-2012
    if(x < 2013){
      fdata <- fdata_orig %>% 
        setNames(c("county",
                   "January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December", "Total")) %>% 
        mutate(type="#", 
               filename=infile,
               year=x)
      
    }
    
    # Return
    fdata <- fdata %>% 
      select(year, filename, county, type, everything()) %>% 
      mutate_all(as.character)
  
    # Export file
    # outfile <- paste0(x, "_messy.csv")
    # write.csv(fdata, file=file.path(inputdir, outfile), row.names=F)
    
  })

}

# Format data
################################################################################

# If doing initial format
if(F){

  # Format data
  data1 <- data_orig %>% 
    # Move and delete "trash" column
    mutate(county=ifelse(trash=="Total" & !is.na(trash), "Total", county)) %>% 
    select(-trash) %>% 
    # Fill in country columns
    mutate(county=ifelse(county=="", NA, county)) %>% 
    fill(county, .direction="down") %>% 
    # Fix some county info
    mutate(county=recode(county, 
                         "51-BAKER"="Baker", 
                         "52-BENTON"="Benton", 
                         "53-CLACKAMAS"="Clackamas", 
                         "54-CLATSOP"="Clatsop", 
                         "55-COLUMBIA"="Columbia", 
                         "57-CROOK"="Crook", 
                         "66-JEFFERSON"="Jefferson",
                         "69-LAKE"="Lake", 
                         "74-MARION"="Marion", 
                         "76-MULTNOMAH"="Multnomah", 
                         "79-TILLAMOOK"="Tillamook", 
                         "81-UNION"="Union", 
                         "83-WASCO"="Wasco", 
                         "84-WASHINGTON"="Washington",
                         "86-YAMHILL"="Yamhill", 
                         "ATILLA"="Umatilla", 
                         "ATSOP"="Clatsop", 
                         "CLATSOP"="Clatsop", 
                         "COOS"="Coos", 
                         "Crayfish"="Crayfish", 
                         "EELER"="Wheeler",
                         "FFERSON"="Jefferson", 
                         "JEFFERSON"="Jefferson", 
                         "KE"="Lake", 
                         "LAKE"="Lake", 
                         "LLAMOOK"="Tillamook", 
                         "LTNOMAH"="Multnomah", 
                         "LUMBIA"="Columbia",
                         "MHILL"="Yamhill", 
                         "ODRIVER"="Hood River", 
                         "OD RIVER"="Hood River",
                         "OOK"="Crook", 
                         "OS"="Coos", 
                         "SCO"="Wasco",
                         "SHINGTON"="Washington", 
                         "Shrimp, brine"="Brine shrimp",
                         "Shrimp, Brine"="Brine shrimp",
                         "Total"="Total", 
                         "WASHINGTON"="Washington")) %>% 
    # Add speices
    mutate(species=ifelse(county %in% c("Brine shrimp", "Crayfish"), county, NA)) %>% 
    select(year, filename, species, county, type, everything())
  
  data1[is.na(data1)] <- ""
  
  # Export file
  write.csv(data1, file=file.path(inputdir, "2004_2018_freshwater_landings_messy.csv"), row.names=F)

}

# Format data
################################################################################

# Read data
data2_orig <- readxl::read_excel(file.path(inputdir, "2004_2018_freshwater_landings_messy.xlsx"))

# Fill NAs with zeros
freeR::complete(data2_orig)
data2_orig[is.na(data2_orig)] <- 0

# Check row totals : they are perfect
data2_orig <- data2_orig %>% 
  mutate(Total_obs=January + February + March + April + May + June + July +
           August + September + October + November + December,
         Total_diff=Total_obs-Total)

# Format data
data2_full <- data2_orig %>% 
  # Remove columns
  select(-c(Total, Total_obs, Total_diff)) %>% 
  # Gather columns
  gather(key="month", value="value", 6:ncol(.)) %>% 
  # Spread landings/value
  select(year:county, month, type, value, everything()) %>% 
  mutate(type=recode(type, "#"="landings_lb", "$"="value_usd")) %>% 
  spread(key="type", value="value")

# Inspect data
str(data2_full)
freeR::complete(data2_full)
range(data2_full$year)
table(data2_full$month)
table(data2_full$species)
table(data2_full$county)

# Check data
################################################################################

# Observed totals
tots_obs <- data2_full %>% 
  filter(county!="Total") %>% 
  group_by(year, month, species) %>% 
  summarize(landings_lb_obs=sum(landings_lb, na.rm=T),
            value_usd_obs=sum(value_usd, na.rm=T))

# Report totals
tots_rep <- data2_full %>% 
  filter(county=="Total") %>% 
  select(year, month, species, landings_lb, value_usd) %>% 
  rename(landings_lb_rep=landings_lb, value_usd_rep=value_usd)

# Merge totals
tots_check <- tots_obs %>% 
  left_join(tots_rep) %>% 
  mutate(landings_lb_diff=landings_lb_obs-landings_lb_rep,
         value_usd_diff=value_usd_obs-value_usd_rep)

# Everything checks out perfectly - the only difference is due to a CDFW error

# Export data
################################################################################

# Extarct data only
data2 <- data2_full %>% 
  filter(county!="Total") %>% 
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  select(year:landings_lb, landings_kg, value_usd, everything())

# Export data
write.csv(data2, file=file.path(outputdir, "ODFW_2004_2018_crayfish_brine_shrimp_landings.csv"), row.names = F)







