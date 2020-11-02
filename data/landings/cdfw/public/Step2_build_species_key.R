
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "data/landings/cdfw/public/processed"
plotdir <- "data/landings/cdfw/public/figures"

# Read data
data <- readRDS(file.path(outputdir, "CDFW_2000_2019_landings_by_port.Rds"))


# Build key
################################################################################

# Unique species
key1 <- data %>% 
  select(comm_name_orig) %>%
  unique() %>% 
  arrange(comm_name_orig) 

# Format common name
# x <- "Anchovy, northern"
format_name <- function(x){
  
  # Comma in name?
  comma_yn <- grepl(",", x)
  if(!comma_yn){
    out_name <- x
  }else{
    name_split <- strsplit(x, split=", ")
    part1 <- stringr::str_to_sentence(name_split[[1]][2]) %>% trimws()
    part2 <- tolower(name_split[[1]][1]) %>% trimws()
    out_name <- paste(part1, part2)
  }
  
  return(out_name)
  
}

# Build species key
key2 <- key1 %>% 
  mutate(comm_name=purrr::map(comm_name_orig, function(x) format_name(x)))

         