
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/website/by_month/raw"
outdir <- "data/landings/cdfw/public/website/by_month/processed"
plotdir <- "data/landings/cdfw/public/website/by_month"

# Read
data_orig <- readxl::read_excel(file.path(indir, "2000_2019_landings_by_month_messy.xlsx"))

# The QA/QC is going badly

# Format data
################################################################################

# Format data
data_full <- data_orig %>% 
  # Remove total check
  select(-c("Total check", rowid)) %>% 
  # Remove empty rows
  filter(January!="") %>% 
  # Format names
  mutate(comm_name_orig=gsub("[[:digit:]]+", "", comm_name_orig) %>% stringr::str_trim()) %>% 
  mutate(comm_name_orig=gsub(" ,", ",", comm_name_orig)) %>% 
  mutate(comm_name_orig=gsub("  ", " ", comm_name_orig)) %>% 
  mutate(comm_name_orig=gsub(" ,", "", comm_name_orig)) %>%
  mutate(comm_name_orig=stringr::str_trim(comm_name_orig)) %>% 
  mutate(comm_name_orig=recode(comm_name_orig, 
                               "Crab, y ellow rock"="Crab, yellow rock",
                               "Anchovy , northern"="Anchovy, northern",
                               "Crus tacean, unspecified"="Crustacean, unspecified",
                               "Flounder, s tarry"="Flounder, starry",
                               "Fly ingfish"="Flyingfish",
                               "Goby, y ellowfin"="Goby, yellowfin",
                               "Lobs ter, California spiny"="Lobster, California spiny",
                               "Rockfish, oliv e"="Rockfish, olive",
                               "Rockfish, s tarry"="Rockfish, starry",
                               "Rockfish, group s lope"="Rockfish, group slope",
                               "Rockfish, v ermilion"="Rockfish, vermilion",
                               "Rockfish, y ellowtail"="Rockfish, yellowtail",
                               "Rockfish, y elloweye"="Rockfish, yelloweye",
                               "Rockfish, group canary/vermili"="Rockfish, group canary/vermilion",
                               "Rockfish, group canary /vermi"="Rockfish, group canary/vermilion",
                               "Sculpin, s taghorn"="Sculpin, staghorn",
                               "Seaperch, s triped"="Seaperch, striped",
                               "Sea s tars"="Sea stars",
                               "Shark, s ixgill"="Shark, sixgill",
                               "Shark, gray"="Shark, gray smoothhound",
                               "Shrimp, ghos t"="Shrimp, ghost",
                               "Shrimp, coons triped"="Shrimp, coonstriped",
                               "Tuna, y ellowfin"="Tuna, yellowfin",
                               "Butterfish (Pacific"="Butterfish (Pacific pompano", 
                               "Cray fish, red swamp"="Crayfish, red swamp", 
                               "Cray fish, unspecified"="Crayfish, unspecified", 
                               "Eel, monkey face (prickleback"="Eel, monkeyface (prickleback)", 
                               "Eel, monkeyface"="Eel, monkeyface (prickleback)", 
                               "Prickleback, monkeyface (eel"="Eel, monkeyface (prickleback)",  
                               "Rockfish, black-and-"="Rockfish, black-and-yellow",
                               "Rockfish, greens triped"="Rockfish, greenstriped", 
                               "Rockfish, group bocaccio/chi"="Rockfish, group bocaccio/chilipepper", 
                               "Rockfish, group bocaccio/chili"="Rockfish, group bocaccio/chilipepper", 
                               "Rockfish, group canary /vermilion"="Rockfish, group canary/vermilion",
                               "Rockfish, group deep nearsho"="Rockfish, group deep nearshore",
                               "Rockfish, group deep"="Rockfish, group deep nearshore", 
                               "Rockfish, Pacific ocean"="Rockfish, Pacific ocean perch",
                               "Rockfish, s tripetail"="Rockfish, stripetail", 
                               "Salmon, Roe (Chinook and Coho"="Salmon, Roe (Chinook and Coho)",
                               "Salmon, Roe (Chinook,"="Salmon, Roe (Chinook, Coho)",
                               "Sardine, Pacific,"="Sardine, Pacific")) %>% 
  # Reverse common name
  mutate(comm_name1=wcfish::convert_names(comm_name_orig, to="regular")) %>% 
  # Format common name
  mutate(comm_name1=recode(comm_name1, 
                           'Black tuna skipjack'='Black skipjack tuna', 
                           'Brown shark'='Brown smoothhound shark', 
                           'Butterfish (Pacific pompano'='Butterfish (Pacific pompano)', 
                           'Claws crab'='Crab', 
                           'Coho) salmon roe (chinook'='Chinook/coho salmon', 
                           'Copper (whitebelly) rockfish'='Copper rockfish', 
                           'Curlfin turbot'='Curlfin sole', 
                           'Dolphin (fish)'='Dolphinfish', 
                           'Giant pacific oyster'='Giant Pacific oyster', 
                           'Group black/blue rockfish'='Black/blue rockfish group', 
                           'Group bocaccio/chilipepper rockfish'='Bocaccio/chilipepper rockfish group', 
                           'Group bolina rockfish'='Bolina rockfish group', 
                           'Group canary /vermilion rockfish'='Canary/vermilion rockfish group', 
                           'Group canary/vermilion rockfish'='Canary/vermilion rockfish group', 
                           'Group deep nearshore rockfish'='Deep nearshore rockfish group', 
                           'Group deep rockfish'='Deep nearshore rockfish group', 
                           'Group deepwater reds rockfish'='Deepwater reds rockfish group', 
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
                           'Monkeyface (prickleback) eel'='Monkeyface prickleback', 
                           'Ocean (pink) shrimp'='Pink (ocean) shrimp', 
                           'Pacific - roe herring'='Pacific herring', 
                           'Pacific - roe on kelp herring'='Pacific herring', 
                           'Pacific Pomfret'='Pacific pomfret', 
                           'Red urchin'='Red sea urchin', 
                           'Rock unspecified crab'='Unspecified rock crab', 
                           'Roe (chinook and coho) salmon'='Chinook/coho salmon', 
                           'Roe herring'='Pacific herring', 
                           'Smooth shark'='Smooth hammerhead shark', 
                           'Spider/sheep claws crab'='Spider/sheep crab', 
                           'Spotted cusk- eel'='Spotted cusk-eel', 
                           'Total Pounds:'='Totals', 
                           'True smelts'='True smelt', 
                           'Unspecified jacks'='Unspecified jack', 
                           'Wolf (wolf-eel) eel'='Wolf eel')) %>% 
  # Add harmonized names
  mutate(comm_name=wcfish::harmonize_names(comm_name1, "comm", "comm"),
         sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Add presentation
  mutate(presentation="not specified",
         presentation=ifelse(grepl("roe on kelp", tolower(comm_name_orig)), "roe on kelp",
                             ifelse(grepl("roe", tolower(comm_name_orig)), "roe", presentation)),
         presentation=ifelse(grepl("claws", tolower(comm_name_orig)), "claws", presentation)) %>% 
  # Add source/table
  mutate(source=paste("CDFW", year+1),
         table="Table 8") %>%
  # Arrange
  select(-comm_name1) %>% 
  select(source, table, year, comm_name_orig, comm_name, sci_name, presentation, everything()) %>% 
  # Gather
  gather(key="month", value="landings_lb", 8:ncol(.)) %>% 
  # Convert units
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>%
  # Arrange
  arrange(year, comm_name)

# Inspect
freeR::complete(data_full)

# QA/QC data
################################################################################

# Year-month check
check_yr <- data_full %>% 
  group_by(year, month) %>% 
  summarize(tot_rep=landings_lb[comm_name_orig=="Total Pounds:"],
            tot_obs=sum(landings_lb[comm_name_orig!="Total Pounds:"])) %>% 
  mutate(tot_diff=tot_obs-tot_rep) %>% 
  mutate(month=factor(month, levels=c(month.name, "Landings")))

# Plot 
g <- ggplot(check_yr, aes(x=year, y=month, fill=tot_diff/1e6)) +
  geom_tile() +
  scale_fill_gradient2(name="Difference", high="navy", low="darkred", mid="grey90", na.value="grey30") +
  theme_bw()
g


# Format data
################################################################################

# Format
data <- data_full %>% 
  filter(comm_name!="Totals" & month!="Landings")

# Inspect
str(data)
freeR::complete(data)
range(data$year)
table(data$month)
table(data$presentation)

# Plot data
################################################################################

# Stats
stats <- data %>% 
  group_by(year, month) %>% 
  summarize(landings_lb=sum(landings_lb))

# Plot 
g <- ggplot(stats, aes(x=year, y=landings_lb/1e6, fill=month)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of lbs)") +
  theme_bw()
g


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_2000_2019_landings_by_month_species.Rds"))




