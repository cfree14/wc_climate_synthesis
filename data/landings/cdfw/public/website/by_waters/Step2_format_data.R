

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
indir <- "data/landings/cdfw/public/website/by_waters/raw"
outdir <- "data/landings/cdfw/public/website/by_waters/processed"
plotdir <- "data/landings/cdfw/public/website/by_waters"

# Read
data_orig <- readxl::read_excel(file.path(indir, "2000_2019_landings_by_waters_messy.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Remove totals
  select(-c("Total", "Total check")) %>% 
  filter(!grepl("total", tolower(comm_name_orig))) %>% 
  # Gather
  gather(key="area", "value"="landings_lb", 3:ncol(.)) %>% 
  # Format area
  mutate(area=recode(area, 
                     "North"="North-of-State",
                     "South"="South-of-State")) %>% 
  # Convert landings units
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  # Add presentation
  mutate(presentation="not specified",
         presentation=ifelse(grepl("roe on kelp", tolower(comm_name_orig)), "roe on kelp",
                             ifelse(grepl("roe", tolower(comm_name_orig)), "roe", presentation)),
         presentation=ifelse(grepl("claws", tolower(comm_name_orig)), "claws", presentation)) %>% 
  # Format common names
  # 1) Fix typos in original name
  mutate(comm_name_orig=recode(comm_name_orig, 
                               "(eel)"="Prickleback, monkeyface (eel)",
                               "nearshore"="Rockfish, group deep nearshore", 
                               "Rockfish, group deep"="Rockfish, group deep nearshore",
                               "Rockfish, group deepwater"="Rockfish, group deepwater reds")) %>% 
  # 2) Reverse original name
  mutate(comm_name_rev=reverse_names(comm_name_orig),
         comm_name_rev=recode(comm_name_rev, 
                              '(Fish), Dolphin'='Dolphinfish', 
                              # 'Box crab'='Armed box crab', 
                              'Claws crab'='Crab', 
                              'Copper (whitebelly) rockfish'='Copper rockfish',
                              'Cucumber, giant red sea'='Giant red sea cucumber', 
                              'Cucumber, unspecified sea'='Unspecified sea cucumber', 
                              'Cucumber, warty sea'='Warty sea cucumber', 
                              'Curlfin turbot'='Curlfin sole',
                              'Dollar, Sand'='Sand dollar', 
                              'Fish, unspecified trawled'='Unspecified trawled fish',
                              # 'Freshwater snail'='NA', 
                              'Giant pacific oyster'='Giant Pacific oyster', 
                              'Group black/blue rockfish'='Black/blue rockfish group',
                              'Group bocaccio/chili rockfish'='Bocaccio/chilipepper rockfish group',
                              'Group bolina rockfish'='Bolina rockfish group',
                              'Group canary/vermili rockfish'='Canary/vermilion rockfish group',
                              'Group deep nearshore rockfish'='Deep nearshore rockfish group',
                              'Group deepwater reds rockfish'='Deepwater reds rockfish group',
                              'Group gopher rockfish'='Gopher rockfish group',
                              'Group nearshore rockfish'='Nearshore rockfish group',
                              'Group red rockfish'='Red rockfish group',
                              'Group rockfish'='Rockfish',
                              'Group rosefish rockfish'='Rosefish rockfish group',
                              'Group shelf rockfish'='Shelf rockfish group',
                              'Group slope rockfish'='Slope rockfish group',
                              'Group small rockfish'='Small rockfish group',
                              'Hagfishes'='Unspecified hagfish', 
                              'Hare, Sea'='Sea hare', 
                              'Kelp, Herring roe on'='Pacific herring', 
                              'Monkeyface (eel) prickleback'='Monkeyface prickleback', 
                              'Monkeyface (prickleback) eel'='Monkeyface prickleback', 
                              'Monkeyface eel'='Monkeyface prickleback', 
                              'Ocean (pink) shrimp'='Pink (ocean) shrimp', #####
                              'Pacific - roe herring'='Pacific herring', 
                              'Pacific - roe on kelp herring'='Pacific herring', 
                              'Pansy, Sea'='Sea pansy', 
                              'Pomfret, Pacific'='Pacific pomfret', 
                              'Pompano), Butterfish (Pacific'='Butterfish (Pacific pompano)', 
                              'Red urchin'='Red sea urchin', 
                              'Rock unspecified crab'='Unspecified rock crab', 
                              'Roe (chinook and coho) salmon'='Chinook/coho salmon', 
                              'Roe (chinook, coho) salmon'='Chinook/coho salmon', 
                              'Roe herring'='Pacific herring', 
                              'Skipjack, black tuna'='Black skipjack tuna', 
                              'Slug, Sea'='Sea slug', 
                              'Spider crab'='Spider crab', 
                              'Spider/sheep claws crab'='Spider/sheep crab', 
                              'Spotted cusk- eel'='Spotted cusk-eel', 
                              'Star, Bat'='Bat star', 
                              'Stars, Sea'='Sea stars', 
                              'True smelts'='True smelt', 
                              'Unspecified jacks'='Unspecified jack',
                              'Unspecified, Invertebrate'='Unspecified invertebrate', 
                              'Urchin, coronado sea'='Coronado sea urchin', 
                              'Urchin, purple sea'='Purple sea urchin', 
                              'Urchin, red sea'='Red sea urchin', 
                              'Urchin, white sea'='White sea urchin', 
                              'Wolf (wolf-eel) eel'='Wolf eel')) %>% 
  # 3) Harmonize names
  mutate(comm_name=wcfish::harmonize_names(comm_name_rev, "comm", "comm")) %>% 
  # 4) Add scientific name
  mutate(sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  select(year, area, comm_name_orig, comm_name_rev, comm_name, sci_name, presentation, landings_lb, landings_kg, everything()) %>% 
  arrange(year, area, comm_name_orig)

# Check common names
wcfish::check_names(data$comm_name_rev)

# Inspect
str(data)
freeR::complete(data)
range(data$year)
table(data$area)
table(data$presentation)


# Plot data
################################################################################

# Summarize
stats <- data %>% 
  group_by(year, area) %>% 
  summarize(landings_lb=sum(landings_lb))

# Plot data
g <- ggplot(stats, aes(x=year, y=landings_lb/1e6, fill=area)) +
  geom_bar(stat="identity") +
  scale_fill_discrete(name="Waters") +
  labs(x="Year", y="Landings (millions of lbs)") +
  theme_bw()
g

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_2000_2019_landings_by_waters_species.Rds"))






