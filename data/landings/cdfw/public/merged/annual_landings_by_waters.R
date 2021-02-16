
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/merged/data"
plotdir <- "data/landings/cdfw/public/merged/figures"

# FB old (1936-1976)
data1_orig <- readRDS("data/landings/cdfw/public/fish_bulletins/processed/CDFW_1936_1976_landings_by_waters.Rds")

# FB 173 (1977-1986)
data2_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb173/processed/CDFW_1977_1986_landings_by_port_complex.csv") 

# FB 181 (1987-1999)
data3_orig <- read.csv("data/landings/cdfw/public/fish_bulletins/raw/fb181/processed/FB181_Table4_1987_1999_landings_by_species.csv")

# Website (2000-2019)
data4_orig <- readRDS(file="data/landings/cdfw/public/website/by_waters/processed/CDFW_2000_2019_landings_by_waters_species.Rds")

# Notes
# 1984 - totals don't match
# 1949, 1950, 1970 - too blurry

# Format data
################################################################################

# Column names
colnames(data1_orig)
colnames(data2_orig)
colnames(data3_orig)
colnames(data4_orig)

# Goal dataset
# source, table, year, waters, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg

# Format FB old (1936-1976)
data1 <- data1_orig %>% 
  # Add presentation
  mutate(presentation="not specified") %>% 
  # Arrange
  select(source, table, year, waters, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg) %>% 
  arrange(year, waters, comm_name)

# # Format FB 173 (1977-1986)
data2 <- data2_orig %>%
  # Summarize
  group_by(source, table, year, type, comm_name_orig, comm_name, sci_name) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            landings_kg=sum(landings_kg, na.rm=T)) %>% 
  ungroup() %>% 
  # Add presentation
  mutate(presentation="not specified") %>% 
  # Rename/recode waters
  rename(waters=type) %>% 
  mutate(waters=recode(waters, "Landings"="Not specified")) %>% 
  # Arrange
  select(source, table, year, waters, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg) %>% 
  arrange(year, waters, comm_name)

# Format FB 181 (1987-1999)
data3 <- data3_orig %>% 
  # Add waters
  mutate(waters="Not specified") %>% 
  # Arrange
  select(source, table, year, waters, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg) %>% 
  arrange(year, waters, comm_name)

# Format website (2000-2019)
data4 <- data4_orig %>% 
  # Rename
  rename(waters=area) %>% 
  # Add source
  mutate(source=paste("CDFW", year+1), table="Table 7") %>% 
  # Arrange
  select(source, table, year, waters, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg) %>% 
  arrange(year, waters, comm_name)

# Merge data
data <- bind_rows(data1, data2, data3, data4) %>% 
  # Arrange
  select(source, table, year, waters, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg) %>% 
  arrange(year, waters, comm_name)

# Plot data
################################################################################

# Annual sums
stats <- data %>% 
  group_by(year, waters) %>% 
  summarize(landings_lb=sum(landings_lb))

# Plot data
g <- ggplot(stats, aes(x=year, y=landings_lb/1e6, fill=waters)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of lbs)") +
  scale_x_continuous(breaks=seq(1930,2020,10)) +
  theme_bw()
g


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1936_2019_landings_by_waters_species.Rds"))
write.csv(data, file=file.path(outdir, "CDFW_1936_2019_landings_by_waters_species.csv"), row.names=F)





