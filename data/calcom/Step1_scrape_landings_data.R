
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(RSelenium)
library(tidyverse)

# Directories
datadir <- "data/calcom/data/raw/landings"


# Scrape data
################################################################################

# Base URL
base_url <- "https://calcom.psmfc.org/qry_all_landings.asp"

# Firefox profile
# Based on this link:
# https://yizeng.me/2014/05/23/download-pdf-files-automatically-in-firefox-using-selenium-webdriver/
fprof <- makeFirefoxProfile(list(
  browser.download.folderList = 2L,
  browser.download.dir = datadir,
  # browser.helperApps.neverAsk.saveToDisk = "application/pdf",
  browser.helperApps.neverAsk.openFile = "text/csv",
  browser.helperApps.neverAsk.saveToDisk = "text/csv"
  # pdfjs.disabled = TRUE,
  # plugin.scan.plid.all = FALSE,
  # plugin.scan.Acrobat = "99.0"
))

# Launch RSelenium server and driver
rD <- rsDriver(browser=c("firefox"), extraCapabilities = fprof)
remDr <- rD[["client"]]
remDr$open(silent=T)

# Visit website
remDr$navigate(base_url)

# Get species list
spp_menu <- remDr$findElement(using = "name", "COMLANDSP")
spp_menu_options <- spp_menu$getElementText()
spp_vec <- unlist(strsplit(as.character(spp_menu_options), split="\n"))

# Loop through  species
i <- 1
# for(i in 1:10){
for(i in 1:length(spp_vec)){
  
  # Get species
  spp <- spp_vec[i]
  
  # Visit website
  remDr$navigate(base_url)
  
  # Select a species
  spp_menu <- remDr$findElement(using = "name", "COMLANDSP")
  spp_menu$sendKeysToElement(list(spp))
  
  # Press "Submit" button
  submit_button <- remDr$findElement(using="name", "fr1")
  submit_button$clickElement()
  
  # Extract table
  data_page <- XML::htmlParse(remDr$getPageSource()[[1]])
  data_table <- XML::readHTMLTable(data_page)
  
  # Format data
  data_df <- data_table %>% 
    as.data.frame() %>% 
    setNames(c("year", "species_code", "port_complex_code", "gear_code", "live_yn", "source", "landings_lb"))
  
  # Export data
  spp_format <- spp %>% gsub(", ", "_", .) %>% gsub("-", "_", .) %>% gsub("/", "_", .) %>% gsub(" ", "_", .)
  outfile <- paste0(spp_format, ".csv")
  write.csv(data_df, file=file.path(datadir, outfile), row.names=F)

  # I could not figure out how to download a text file
  # Press "download the file" link
  # download_link <- remDr$findElement(using = 'link text', "download the file")
  # download_link$clickElement()
  
}

# Close driver and stop server
remDr$close()
rD[["server"]]$stop()




