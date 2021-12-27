
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
envidir <- "data/environmental/indices/processed"
pacfindir <- "data/landings/pacfin/processed"
plotdir <- "analyses/revenues/figures"

# Read data
indices_orig <- readRDS(file.path(envidir, "environmental_indices.Rds"))
landings_orig <- readRDS(file.path(pacfindir, "PACFIN_ALL001_1980_2021_all_species_landings.Rds"))

# Build data
################################################################################

# Annual average by index
indices <- indices_orig %>% 
  # Mean
  group_by(year) %>% 
  summarize(pdo=mean(pdo, na.rm=T),
            enso=mean(enso, na.rm=T)) %>% 
  ungroup() %>% 
  # Gather
  gather(key="index", value="value", 2:ncol(.)) %>% 
  mutate(index=toupper(index))


# Build landings
landings <- landings_orig %>% 
  # Fix some species
  mutate(comm_name=recode(comm_name,
                          "Black and yellow rockfish"="Black-and-yellow rockfish",
                          "Nom. Calif halibut"="Nom. California halibut",
                          "Nom. Vermillion rockfish"="Nom. Vermilion rockfish",
                          "Nom. Squarespot"="Nom. Squarespot rockfish",
                          "Nom. Chilipepper"="Nom. Chilipepper rockfish",
                          "Nom. Pop"="Nom. Pacific ocean perch",
                          "Nom. Rougheye + blackspotted"="Nom. Rougheye + blackspotted rockfish")) %>% 
  # Mark nominal / not nominal
  mutate(nominal=ifelse(grepl("nom.", tolower(comm_name)), "yes", "no")) %>% 
  # Mark parent species (to merge nominal and true)
  mutate(comm_name_parent=gsub("Nom. ", "", comm_name)) %>% 
  # Expand management group code
  mutate(mgmt_group=recode(mgmt_group_code, 
                           "CPEL"="CPS",
                           "CRAB"="Crabs",
                           "GRND"="Groundfish",
                           "HMSP"="HMS",
                           "OTHR"="Other",
                           "SAMN"="Salmon",
                           "SHLL"="Shellfish",
                           "SRMP"="Shrimp", 
                           "XXXX"="Not specified")) 

# Revenues: coastwide
stats_coast <- landings %>% 
  # Sum values
  group_by(year) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Add indices
  left_join(indices, by="year") %>% 
  # Arrange
  select(index, year, value, everything()) %>% 
  arrange(index, year) %>% 
  # Remove 2021
  filter(year!=2021)

# Revenues: by state
stats_state <- landings %>% 
  # Sum values
  group_by(state, year) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Add indices
  left_join(indices, by="year") %>% 
  # Arrange
  select(index, year, value, everything()) %>% 
  arrange(index, year) %>% 
  # Remove 2021
  filter(year!=2021) %>% 
  # Remove At-Sea
  filter(state!="At-Sea")

# Revenues: by mgmt group
stats_mgmt <- landings %>% 
  # Sum values
  group_by(state, mgmt_group, year) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Standardize values
  group_by(state, mgmt_group) %>% 
  mutate(landings_sd=landings_mt/max(landings_mt),
         value_sd=value_usd/max(value_usd)) %>% 
  ungroup() %>% 
  # Add indices
  left_join(indices, by="year") %>% 
  # Arrange
  select(index, year, value, everything()) %>% 
  arrange(index, year) %>% 
  # Remove 2021
  filter(year!=2021) %>% 
  # Remove At-Sea
  filter(state!="At-Sea")

# Revenues: by mgmt group
stats_spp <- landings %>% 
  # Sum values
  group_by(state, mgmt_group, comm_name_parent, year) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  # Add indices
  left_join(indices, by="year") %>% 
  # Arrange
  select(index, year, value, everything()) %>% 
  arrange(index, year) %>% 
  # Remove 2021
  filter(year!=2021) %>% 
  # Remove At-Sea
  filter(state!="At-Sea") %>% 
  # Calculate correlations
  group_by(index, state, mgmt_group, comm_name_parent) %>% 
  summarize(corr_value=cor(value, value_usd))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot coastwide
g1 <- ggplot(stats_coast, aes(x=value, y=value_usd/1e6)) +
  facet_wrap(~index, scales="free_x") +
  # Regression
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Points
  geom_point(color="grey30") +
  # Labels
  labs(x="Index value", y="Revenues (US millions)") +
  # Theme
  theme_bw() + base_theme
g1

# Plot by state
g2 <- ggplot(stats_state, aes(x=value, y=value_usd/1e6)) +
  facet_grid(index~state, scales="free_x") +
  # Regression
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Points
  geom_point(color="grey30") +
  # Labels
  labs(x="Index value", y="Revenues (US millions)") +
  # Theme
  theme_bw() + base_theme
g2

# Plot by mgmt group: ENSO
g3 <- ggplot(stats_mgmt %>% filter(index=="ENSO"), aes(x=value, y=value_sd)) +
  facet_grid(mgmt_group~state, scales="free") +
  # Regression
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Points
  geom_point(color="grey30") +
  # Labels
  labs(x="Index value", y="Revenues\n(scaled to maximum value)", title="ENSO") +
  # Theme
  theme_bw() + base_theme
g3

# Plot by mgmt group: PDO
g4 <- ggplot(stats_mgmt %>% filter(index=="PDO"), aes(x=value, y=value_sd)) +
  facet_grid(mgmt_group~state, scales="free") +
  # Regression
  geom_smooth(method="lm", fill="grey80", color="black") +
  # Reference line
  geom_vline(xintercept=0, linetype="dotted") +
  # Points
  geom_point(color="grey30") +
  # Labels
  labs(x="Index value", y="Revenues\n(scaled to maximum value)", title="ENSO") +
  # Theme
  theme_bw() + base_theme
g4

# Plot species
g5 <- ggplot(stats_spp, aes(x=corr_value, y=mgmt_group)) +
  facet_wrap(~index) +
  geom_boxplot(alpha=0.5) +
  # Reference line
  geom_vline(xintercept=0) +
  # Labels
  labs(x="Correlation between\nindex and revenues", y="") +
  # Legend
  scale_fill_discrete(name="Management group") +
  # Theme
  theme_bw()+ base_theme
g5


