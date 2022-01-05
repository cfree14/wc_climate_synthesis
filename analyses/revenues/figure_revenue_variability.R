
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(corrgram)
library(tidyverse)

# Directories
envidir <- "data/environmental/indices/processed"
pacfindir <- "data/landings/pacfin/processed"
plotdir <- "analyses/revenues/figures"

# Read data
landings_orig <- readRDS(file=file.path(pacfindir, "PACFIN_ALL001_1980_2021_all_species_landings.Rds"))


# Build data
################################################################################

# Landings overall
landings_tot <- landings_orig %>% 
  group_by(year) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(period="All years (1981-2021)")

# Landings overall
landings_tot06 <- landings_tot %>% 
  filter(year>=2006) %>% 
  mutate(period="Post-2006 (2006-2021)")

# Merge landings
landings_tot_use <- bind_rows(landings_tot, landings_tot06)

# Landings overall
landings_state <- landings_orig %>% 
  group_by(state, year) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(period="All years (1981-2021)")

# Landings overall
landings_state06 <- landings_state %>% 
  filter(year>=2006) %>% 
  mutate(period="Post-2006 (2006-2021)")

# Merge landings
landings_state_use <- bind_rows(landings_state, landings_state06)



# Plot data
################################################################################

# Plot
g1 <- ggplot(landings_tot_use, aes(x=value_usd/1e6, fill=period)) +
  geom_density(alpha=0.5) +
  # Label
  labs(x="Revenue (USD millions)", y="Density") +
  # Legend
  scale_fill_discrete(name="Period") +
  # Theme
  theme_bw()
g1

# Plot
g1 <- ggplot(landings_tot_use, aes(x=value_usd/1e6, fill=period)) +
  geom_density(alpha=0.5) +
  # Label
  labs(x="Revenue (USD millions)", y="Density") +
  # Legend
  scale_fill_discrete(name="Period") +
  # Theme
  theme_bw()
g1
