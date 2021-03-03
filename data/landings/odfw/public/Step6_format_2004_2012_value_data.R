

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
workdir <- "data/landings/odfw/public/intermediate"
outputdir <- "data/landings/odfw/public/processed"

# Read data
data_orig <- readxl::read_excel(file.path(workdir, "2004_2012_value_data_messy.xlsx"))


# Format again
################################################################################

# Species key
spp_key <- data_orig %>% 
  select(category, species) %>% 
  unique() %>% 
  arrange(category, species)

# Format data
data_full <- data_orig %>% 
  gather(key="month", value="value", 7:ncol(.)) %>% 
  mutate(value=value %>% as.numeric())


# QA/QC #1: Confirm that all have total rows
################################################################################

# All have total rows?
key <- data_full %>% 
  group_by(year, port, filename) %>% 
  summarize(total_yn=sum(grepl("total", tolower(species)))>0) %>% 
  # Reduce to files without total row
  filter(total_yn==FALSE) %>% 
  arrange(year, filename)
  

# QA/QC #2: Check all row totals
################################################################################

# Calculate observed totals
tots_obs <- data_full %>% 
  filter(month!="Total") %>% 
  group_by(year, filename, port, category, species) %>% 
  summarize(value_obs=sum(value, na.rm=T))

# Extract reported totals
tots_rep <- data_full %>% 
  filter(month=="Total") %>% 
  select(year, filename, port, category, species, value) %>% 
  rename(value_rep=value)

# Compare
tots_check <- tots_rep %>% 
  left_join(tots_obs) %>% 
  mutate(value_diff=value_obs-value_rep) %>% 
  filter(is.na(value_diff) | value_diff!=0)


# QA/QC #3: Check month totals
################################################################################

# Calculate observed totals
tots_obs1 <- data_full %>% 
  filter(!grepl("total", tolower(species))) %>% 
  group_by(year, filename, port, month) %>% 
  summarize(value_obs=sum(value, na.rm=T))

# Extract reported totals
tots_rep1 <- data_full %>% 
  filter(grepl("total", tolower(species)) & species!="Grand total") %>% 
  select(year, filename, port, month, value) %>% 
  rename(value_rep=value)

# Compare
tots_check1 <- tots_rep1 %>% 
  left_join(tots_obs1) %>% 
  mutate(value_diff=value_obs-value_rep)

# Plot comparison
g <- ggplot(tots_check1, aes(x=year, y=month, fill=value_diff)) +
  facet_wrap(~port, ncol=4) +
  geom_raster() +
  # Axis
  scale_x_continuous(breaks=2004:2012) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2(name="Difference in apparent\nand repoted values (1000s of dollars)", 
                       low="darkred", high="navy", mid="grey80", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export data
################################################################################

# Format data for export
data <- data_full %>% 
  # Remove totals 
  filter(month!="Total" & !grepl("total", tolower(category)) & !grepl("total", tolower(species))) %>% 
  # Remove columns
  select(-row_id) %>% 
  # Format category
  mutate(category=gsub(":", "", category),
         month=recode(month, "Feburary"="February"))

# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$port)
table(data$category)
sort(unique(data$species))
table(data$month)

# Export data
saveRDS(data, file=file.path(workdir, "2004_2012_value_data_clean.Rds"))

