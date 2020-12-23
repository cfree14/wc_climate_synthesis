

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
basedir <- "data/landings/odfw/public/raw"
outputdir <- "data/landings/odfw/public/processed"



# Merge data
################################################################################

# Merge data
data1_orig <- purrr::map_df(2015:2019, function(x){
  datadir <- file.path(basedir, x, "by_port") 
  data <- readxl::read_excel(file.path(datadir, paste0(x, "_data_clean.xlsx"))) %>% 
    mutate_all(as.character)
})


# Merge data
data2_orig <- purrr::map_df(2013:2014, function(x){
  datadir <- file.path(basedir, x) 
  data <- readxl::read_excel(file.path(datadir, paste0(x, "_data_clean.xlsx"))) %>% 
    mutate_all(as.character)
})



# Format data 1
################################################################################

# Format
data1 <- data1_orig  %>%
  # Fill in species
  mutate(species=ifelse(is.na(species), "", species)) %>% 
  mutate(group_id=sort(rep(1:(nrow(.)/2), 2))) %>%
  group_by(group_id) %>%
  mutate(species1=species[which.max(nchar(species))]) %>%
  ungroup() %>%
  mutate(species=species1) %>%
  select(-c(species1, group_id)) %>%
  # Gather months
  gather(key="month", value="value", 5:ncol(.)) %>%
  # Replace blanks with zeroes
  mutate(value=ifelse(is.na(value), 0, value),
         value=ifelse(value=="NA", 0, value),
         value=as.numeric(value)) %>% 
  # Spread metrics
  dplyr::select(year, filename, species, month, type, value) %>%
  spread(key="type", value="value") %>% 
  # Add port
  mutate(port=gsub(".pdf|2017|2018|2019", "", filename),
         port=port %>% stringr::str_to_title() %>% stringr::str_trim(),
         port=gsub(" - ", "-", port),
         port=recode(port,
                     "Astoira  Pounds And Values"="Astoria",
                     "Bandon And Port Orford"="Bandon-Port Orford",
                     "Bandon-Port_orford"="Bandon-Port Orford",
                     "Bandon Port Orford"="Bandon-Port Orford",
                     "Gearhart-Seaside-Cannon-Beach-Garibaldi-Nehalem-Bay"="Gearhart-Seaside-Cannon Beach-Nehalem Bay-Garibaldi",
                     "Gearhart-Seaside-Cannonbeach-Garibaldi-Nehalembay"="Gearhart-Seaside-Cannon Beach-Nehalem Bay-Garibaldi",
                     "Gold Beach And Brookings"="Gold Beach-Brookings",
                     "Gold Beach Brookings"="Gold Beach-Brookings",
                     "Goldbeach Brookings"="Gold Beach-Brookings",
                     "Gold-Beach-Brookings"="Gold Beach-Brookings",
                     "Gold_beach-Brookings"="Gold Beach-Brookings",
                     "Netart Pacific City Siletz Salmon River Depoe Bay"="Netarts-Pacific City-Salmon River-Siletz Bay-Depoe Bay",
                     "Netarts-Pacific City-Salmon River-Siletz Bay And Depoe Bay"="Netarts-Pacific City-Salmon River-Siletz Bay-Depoe Bay",
                     "Netarts-Pacific-City-Siletz-Bay-Salmon-River-Depoe-Bay"="Netarts-Pacific City-Salmon River-Siletz Bay-Depoe Bay",
                     "Netarts-Pacificcity-Siletzbay-Salmonriver-Depoebay "="Netarts-Pacific City-Salmon River-Siletz Bay-Depoe Bay",
                     "Gearhart Cannon Beach Garibaldi Nehalem Seaside"="Gearhart-Seaside-Cannon Beach-Nehalem Bay-Garibaldi",
                     "Waldport Yachats Florence Winchester Bay"="Waldport-Yachats-Florence-Winchester Bay",
                     "Waldport-Yachats-Florence-Winchester-Bay"="Waldport-Yachats-Florence-Winchester Bay",
                     "Waldport-Yachats-Florence-Winchester_bay"="Waldport-Yachats-Florence-Winchester Bay")) %>% 
  # Arrange
  select(year, filename, port, everything()) %>% 
  # Format year and month
  mutate(year=as.numeric(year),
         month=recode(month, "Feburary"="February"),
         month=factor(month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))
 
# Inspect 
str(data1)
table(data1$port)
table(data1$month)

# Final formatting
data1_final <- data1 %>% 
  filter(month!="Total" & !species %in% c("Fish", "Crustaceans", "Molluscs", "Other Invertebrates", "Other invertebrates", "Total"))

# Inspect species
sort(unique(data1_final$species))

# QA/QC data 1
################################################################################

# Extract reported totals
data1_tots <-  data1 %>% 
  filter(species=="Total") %>% 
  select(year, port, month, landings, value) %>% 
  rename(landings_rep=landings, value_rep=value)

# Calculate totals based on data and compare to reported totals
stats <- data1_final %>% 
  # Calculated totals
  group_by(year, port, month) %>% 
  summarise(landings=sum(landings),
            value=sum(value)) %>% 
  ungroup() %>% 
  # Reported totals
  left_join(data1_tots) %>% 
  # Inspect difference
  mutate(landings_diff=landings-landings_rep,
         value_diff=value-value_rep)

# Format data 2
################################################################################

# Format
data2 <- data2_orig  %>%
  # Fill in species
  mutate(species=ifelse(is.na(species), "", species)) %>% 
  mutate(group_id=sort(rep(1:(nrow(.)/2), 2))) %>%
  group_by(group_id) %>%
  mutate(species1=species[which.max(nchar(species))]) %>%
  ungroup() %>%
  mutate(species=species1) %>%
  select(-c(species1, group_id)) %>%
  # Gather months
  gather(key="month", value="value", 6:ncol(.)) %>%
  # Replace blanks with zeroes
  mutate(value=ifelse(is.na(value), 0, value),
         value=ifelse(value=="NA", 0, value),
         value=as.numeric(value)) %>%
  # Format port name
  mutate(port=gsub("[[:digit:]]+-", "", port) %>% stringr::str_to_title()) %>% 
  # Format year and month
  mutate(year=as.numeric(year),
         month=recode(month, "Feburary"="February"),
         month=factor(month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
  # Spread metrics
  dplyr::select(year, filename, port, species, month, type, value) %>%
  spread(key="type", value="value")


# Inspect
str(data2)
table(data2$year)
table(data2$port)
table(data2$month)
table(data2$species)

# Final formatting
data2_final <- data2 %>% 
  filter(month!="Total" & !species %in% c("Fish", "Crustaceans", "Molluscs", "Other Invertebrates", "Other invertebrates", "Total"))


# QA/QC data 2
################################################################################

# Extract reported totals
data2_tots <-  data2 %>% 
  filter(species=="Total") %>% 
  select(year, port, month, landings, value) %>% 
  rename(landings_rep=landings, value_rep=value)

# Calculate totals based on data and compare to reported totals
data2_stats <- data2_final %>% 
  # Calculated totals
  group_by(year, port, month) %>% 
  summarise(landings=sum(landings),
            value=sum(value)) %>% 
  ungroup() %>% 
  # Reported totals
  left_join(data2_tots) %>% 
  # Inspect difference
  mutate(landings_diff=landings-landings_rep,
         value_diff=value-value_rep)



  
# Merge and export
################################################################################

# Merge data
data <- bind_rows(data1_final, data2_final) %>% 
  arrange(year, port, species, month)

# Export data
write.csv(data, file=file.path(outputdir, "ODFW_2013_2019_landings_by_port_month.csv"), row.names=F)




