

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
data_full <- data_orig %>% 
  # Remove totals
  select(-c("Total", "Total check")) %>% 
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
  # 2) Regularize names
  mutate(comm_name_reg=wcfish::convert_names(comm_name_orig, to="regular")) %>% 
  mutate(comm_name_reg=recode(comm_name_reg,
                              'Black tuna skipjack'='Black skipjack tuna', 
                              'Claws crab'='Crab', 
                              'Coho) salmon roe (chinook'='Chinook/coho salmon', 
                              'Curlfin turbot'='Curlfin sole', 
                              'Dolphin (fish)'='Dolphinfish', 
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
                              'Herring roe on kelp'='Pacific herring', 
                              'Invertebrate Unspecified'='Unspecified invertebrate', 
                              'Monkeyface (eel) prickleback'='Monkeyface prickleback', 
                              'Monkeyface (prickleback) eel'='Monkeyface prickleback', 
                              'Monkeyface eel'='Monkeyface prickleback', 
                              'Ocean (pink) shrimp'='Pink (ocean) shrimp', 
                              'Pacific - roe herring'='Pacific herring', 
                              'Pacific - roe on kelp herring'='Pacific herring', 
                              'Pacific Pomfret'='Pacific pomfret', 
                              'Red urchin'='Red sea urchin', 
                              'Rock unspecified crab'='Rock crab', 
                              'Roe (chinook and coho) salmon'='Chinook/coho salmon', 
                              'Roe herring'='Pacific herring', 
                              'Spider/sheep claws crab'='Spider/sheep crab', 
                              'Spotted cusk- eel'='Spotted cusk-eel', 
                              'Total Pounds'='Totals', 
                              'Total Pounds:'='Totals', 
                              'True smelts'='True smelt', 
                              'Unspecifed croaker'='Unspecified croaker', 
                              'Unspecified jacks'='Unspecified jack', 
                              'Wolf (wolf-eel) eel'='Wolf eel',
                              'Group canary/vermilion rockfish'='Canary/vermilion rockfish group', 
                              'Shovelnose guitar'='Shovelnose guitarfish')) %>% 
  # 3) Harmonize names
  mutate(comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm")) %>% 
  # 4) Add scientific name
  mutate(sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  # select(-comm_name_reg) %>% 
  select(year, area, comm_name_orig, comm_name_reg, comm_name, sci_name, presentation, landings_lb, landings_kg, everything()) %>% 
  arrange(year, area, comm_name_orig)

# Check common names
wcfish::check_names(data_full$comm_name_reg)

# Inspect
str(data_full)
freeR::complete(data_full)
range(data_full$year)
table(data_full$area)
table(data_full$presentation)


# QA/QC data
################################################################################

# Extract report totals
tots_rep <- data_full %>% 
  filter(comm_name_reg=="Totals") %>% 
  select(year, area, landings_lb) %>% 
  rename(landings_rep=landings_lb)

# Calculate observed totals
tots_obs <- data_full %>% 
  filter(comm_name_reg!="Totals") %>% 
  group_by(year, area) %>% 
  summarize(landings_obs=sum(landings_lb))

# Check
tots_check <- tots_rep %>% 
  left_join(tots_obs) %>% 
  mutate(tots_diff=landings_rep-landings_obs)
  
# Plot
g <- ggplot(tots_check, aes(x=year, y=area, fill=tots_diff)) +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2(name="Difference (lbs)", low="darkred", high="navy", mid = "white", na.value = "grey30") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g


# Finalize data
################################################################################

# Format data
data <- data_full %>%
  # Remove totals
  filter(comm_name_reg!="Totals") %>%
  # Arrange
  select(-comm_name_reg) %>%
  arrange(year, area, comm_name)

# Inspect
str(data)
freeR::complete(data)


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






