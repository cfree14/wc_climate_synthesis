

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(rnaturalearth)

# Directories
ramdir <- "data/ramldb/processed"
pacfindir <- "data/landings/pacfin/processed"
plotdir <- "figures"

# Read RAM data
ram_orig <- readRDS(file.path(ramdir, "RAM_WC_status_data.Rds"))

# Read PACFIN landings data
pacfin_orig <- readRDS(file.path(pacfindir, "PACFIN_ALL005_1980_2020_all_species_landings_by_port.Rds"))


# Setup
################################################################################

# Format RAM data
ram <- ram_orig %>% 
  # Reduce to California stocks
  filter(region=="US West Coast" & !area %in% c("Oregon Coast", "Washington")) %>% 
  # Summarize B/BMSY by species/year
  group_by(comm_name, species, year) %>% 
  summarize(nstocks=n_distinct(stockid),
            stocks=sort(unique(stockid)),
            bbmsy=mean(bbmsy, na.rm=T)) %>% 
  ungroup() %>% 
  # Fix some common names
  mutate(comm_name=recode(comm_name, 
                          "Chilipepper"="Chilipepper rockfish",
                          "Pacific hake"="Pacific hake (whiting)"))

# Format landings data
pacfin <- pacfin_orig %>% 
  # Reduce to California
  filter(state=="California") %>% 
  # Summarize landings by species/year
  group_by(year, comm_name, sci_name) %>% 
  summarize(landings_mt=sum(landings_mt, na.rm=T)) %>% 
  ungroup() %>% 
  # Fix some common names
  mutate(comm_name=recode(comm_name,
                          "Cowcod rockfish"="Cowcod",
                          "Pacific whiting"="Pacific hake (whiting)",
                          "Spiny dogfish"="Spotted spiny dogfish",
                          "Chub mackerel"="Pacific chub mackerel"))

# Are all of the RAM common names in the landings common names?
comm_ram <- sort(unique(ram$comm_name))
comm_pacfin <- sort(unique(pacfin$comm_name))
comm_ram[!comm_ram %in% comm_pacfin]

# Merge data
data <- pacfin %>% 
  # Add status
  left_join(ram %>% select(-species)) %>% 
  # Classify status
  mutate(status=cut(bbmsy, breaks = c(0,0.5,1.2,Inf), labels=c("Overfished", "Maximally fished", "Underfished")) %>% as.character(),
         status=ifelse(is.na(status), "Unassessed", status)) %>% 
  # Arrange
  select(comm_name, sci_name, stocks, nstocks, year, landings_mt, status, bbmsy, everything()) %>% 
  arrange(comm_name, year)


# Summarize data
data_sum <- data %>% 
  group_by(status, year) %>% 
  summarize(nspp=n_distinct(comm_name),
            landings_mt=sum(landings_mt)) %>% 
  ungroup() %>% 
  # Fill gaps
  complete(status, year, fill=list(landings_mt=0, nspp=0)) %>% 
  # Factor status
  mutate(status=factor(status, 
                       levels=c("Overfished", "Maximally fished", "Underfished", "Unassessed")))
  

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot status by catch
g1 <- ggplot(data_sum, aes(x=year, y=landings_mt/1e3, fill=status)) +
  geom_area() +
  # Labels
  labs(x="Year", y="Landings (1000s mt)", title="CA fisheries status over time by landings") +
  # Legend
  scale_fill_discrete(name="Stock status") +
  # Theme
  theme_bw() + my_theme
g1

# Plot status by species
g2 <- ggplot(data_sum, aes(x=year, y=nspp, fill=status)) +
  geom_area() +
  # Labels
  labs(x="Year", y="Number of species", title="CA fisheries status over time by number of landed species") +
  # Legend
  scale_fill_discrete(name="Stock status") +
  # Theme
  theme_bw() + my_theme
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=1)

# Export plot
ggsave(g, filename=file.path(plotdir, "CA_1980_2020_fisheries_stock_status.png"), 
       width=6.5, height=4, units="in", dpi=600)


