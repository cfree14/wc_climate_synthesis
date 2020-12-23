

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
basedir <- "data/landings/cdfw/public/raw"
plotdir <- "data/landings/cdfw/public/figures"
outputdir <- "data/landings/cdfw/public/processed"


# Build data
################################################################################


# Years
years <- c(2000:2019); x <- years[1]
data1 <- purrr::map_df(years, function(x){
  
  # Read data
  datadir <- file.path(basedir, as.character(x), "by_month")
  infile <- paste0(x, "_merged_messy.xlsx")
  ydata_orig <- readxl::read_excel(file.path(datadir, infile))
  
  # Format data
  ydata <- ydata_orig %>% 
    # Remove empty rows
    filter(!is.na(species)) %>% 
    # Remove useless columns
    select(year:species, January:Total) %>% 
    # Trim white space
    mutate(area=area %>% stringr::str_trim(),
           category=category %>% stringr::str_trim(),
           species=species %>% stringr::str_trim()) %>% 
    # Delete numbers in common names
    mutate(species=gsub("[[:digit:]]", "", species) %>% stringr::str_trim()) %>% 
    # Convert all to character for merge
    mutate_all(as.character)
    #%>% 
    # Gather
    # gather(key="month", value="landings_lb", 6:ncol(.)) %>% 
    # Convert landings to numeric
    # mutate(landings_lb1=landings_lb %>% stringr::str_trim() %>% as.numeric())
  
  # Inspect
  # freeR::complete(ydata)
  
  # Fill zeros
  # ydata[is.na(ydata)] <- 0
  # ydata
  
})




# Format data
################################################################################

# Port complex order


# Month order
month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December", "Total")

# Format data
data_full <- data1 %>% 
  # Gather
  gather(key="month", value="landings_lb", 7:ncol(.)) %>%
  # Remove empty rows
  filter(!is.na(landings_lb)) %>% 
  # Mark and replace confidential landings
  mutate(confidential=ifelse(grepl("\\*", landings_lb), "yes", ""),
         landings_lb=ifelse(confidential=="yes", NA, landings_lb)) %>% 
  # Convert year/landings to numeric
  mutate(year=as.numeric(year),
         landings_lb=landings_lb %>% stringr::str_trim() %>% as.numeric()) %>% 
  # Remove zeroes
  # filter(landings_lb>0) %>% 
  # Order months
  mutate(month=factor(month, levels=month_order)) %>% 
  # Format areas
  mutate(area=recode(area,
                     "Waters North of State"="Waters North of the State",
                     "Waters South of State"="Waters South of the State",
                     "Waters North Of State"="Waters North of the State",
                     "Waters South Of State"="Waters South of the State")) %>% 
  # Format categories
  mutate(category=stringr::str_to_title(category),
         category=recode(category, 
                         "Fishes"="Finfish",
                         "Crus Taceans"="Crustaceans",
                         "Lants"="Plants",
                         "Urtles"="Turtles",
                         "Worms"="Other invertebrates"),
         category=ifelse(grepl("otal", species), "Total", category)) %>% 
  # Format a few species
  mutate(species=gsub(" ,", ",", species),
         species=gsub("  ", " ", species),
         species=recode(species, 
                        "Crab, y ellow rock"="Crab, yellow rock",
                        "Croaker, unspecifed"="Croaker, unspecified",
                        "Crus tacean, unspecified"="Crustacean, unspecified",
                        "Eel, monkeyface"="Prickleback, monkeyface (eel)",
                        "Eel, monkeyface (prickleback)"="Prickleback, monkeyface (eel)",
                        "Flounder, s tarry"="Flounder, starry",
                        "Fly ingfish"="Flyingfish",
                        "Lobs ter, California spiny"="Lobster, California spiny",
                        "Invertebrate Unspecified"="Invertebrate, unspecified",
                        "Pacific Pomfret"="Pomfret, Pacific",
                        # "Rockfish, copper"="Rockfish, copper (whitebelly)", # can't fix b/c appears 2x in 2011 Monterey
                        "Rockfish, greens triped"="Rockfish, greenstriped",
                        "Rockfish, group bocaccio/ch"="Rockfish, group bocaccio/chilipepper",
                        "Rockfish, group canary/verm"="Rockfish, group canary/vermilion",
                        "Rockfish, group canary/vermili"="Rockfish, group canary/vermilion",
                        "Rockfish, group deep nearshor"="Rockfish, group deep nearshore",
                        "Rockfish, oliv e"="Rockfish, olive",
                        "Rockfish, Pacific ocean perc"="Rockfish, Pacific ocean perch",
                        "Rockfish, s tarry"="Rockfish, starry",
                        "Rockfish, v ermilion"="Rockfish, vermilion",
                        "Rockfish, y ellowtail"="Rockfish, yellowtail",
                        "Salmon, chinook"="Salmon, Chinook",
                        "Salmon, Roe (Chinook and C"="Salmon, Roe (Chinook and Coho)",
                        "Salmon, Roe (Chinook, Coho)"="Salmon, Roe (Chinook and Coho)",
                        "Sea s tars"="Sea stars",
                        "Shrimp, ghos t"="Shrimp, ghost",
                        "Snails, moom"="Snails, moon",
                        "Tuna bluefin"="Tuna, bluefin",
                        "Tuna, y ellowfin"="Tuna, yellowfin",
                        "Tuna, Albacore"="Tuna, albacore",
                        "Urchin, purple sea"="Sea urchin, purple",
                        "Urchin, red"="Sea urchin, red")) %>% 
  # Arrange columns
  select(year:month, landings_lb, confidential)
  
# Inspect data
freeR::complete(data_full)
table(data_full$port_complex)
table(data_full$area)
table(data_full$category)
table(data_full$month)
sort(unique(data_full$species))

# Species key
spp_key <- data_full %>% 
  group_by(species, category) %>% 
  summarize(n=n()) %>% 
  group_by(species) %>% 
  summarise(category1=category[n==max(n)],
            category2=paste(category, collapse=", ")) %>% 
  select(category1, species) %>% 
  arrange(category1, species)

# Add corrected category to data
data_full1 <- data_full %>% 
  left_join(spp_key) %>% 
  select(-category) %>% 
  rename(category=category1) %>% 
  select(year:area, category, everything())

# Check species
spp_key2 <- data_full1 %>% 
  select(category, species) %>% 
  unique() %>% 
  arrange(category, species)

# QA/QC #1: Check row totals
################################################################################

# Check uniqueness
check0 <- data_full1 %>% 
  group_by(year, filename, port_complex, area, species, month) %>% 
  summarize(n=n()) %>% 
  filter(n!=1)

# Check
check1 <- data_full1 %>%
  # Remove columns
  select(-c(category, confidential)) %>%
  # Spread for check
  spread(key="month", value="landings_lb") %>% 
  # Check species totals
  mutate(total=January+February+March+April+May+June+July+August+September+October+November+December,
         total_check=total-Total) %>%
  # Reduce to failures
  filter(abs(total_check)>5) %>% 
  # Mark totals/species
  mutate(type=ifelse(grepl("otal", species), "total", "species")) %>% 
  arrange(type, total_check)


# QA/QC #2: Check section totals
################################################################################

# Extract reported totals
tots_rep <- data_full1 %>% 
  # Remove total month
  filter(month!="Total") %>% 
  # Reduce to rows with totals
  filter(grepl("total", tolower(species))) %>% 
  # Remove Grand totals
  filter(!grepl("grand", tolower(species))) %>% 
  # Simplify dataframe
  select(year, filename, port_complex, area, month, landings_lb) %>% 
  rename(landings_lb_rep=landings_lb)

# Extract data
data <- data_full1 %>% 
  filter(!grepl("total", tolower(species)))

# Calculate observed totals
tots_obs <- data %>% 
  group_by(year, filename, port_complex, area, month) %>% 
  summarize(landings_lb_obs=sum(landings_lb)) %>% 
  ungroup()

# Merge totals to compare
cap <- 100
tots_check <- tots_rep %>% 
  left_join(tots_obs) %>% 
  mutate(tots_diff=(landings_lb_obs-landings_lb_rep),
         tots_diff_cap=pmin(cap, tots_diff),
         tots_diff_cap=pmax(-1*cap, tots_diff_cap))

# Plot check
g <- ggplot(tots_check, aes(x=year, y=month, fill=tots_diff_cap)) +
  # lemon::facet_rep_grid(port_complex~area, 
  #                scales="free_y", repeat.tick.labels = "bottom") +
  facet_grid(port_complex~area,  scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Legends
  scale_fill_gradient2(name="Difference in apparent\nand reported landings",
                       midpoint = 0, low="darkred", high="navy", mid="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
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
        legend.position="bottom")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "CDFW_2000_2019_monthly_landings_qaqc.png"), 
       width=8.5, height=11, units="in", dpi=600)


# Export data
################################################################################

# Final formatting
data_out <- data %>% 
  # Remove totals
  filter(month!="Total") %>% 
  # Rename columns
  rename(comm_name_orig=species) %>% 
  # Add kilograms
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg"),
         landings_mt=landings_kg/1000) %>% 
  # Arrange columns
  select(year:landings_lb, landings_kg, landings_mt, confidential)
  
# Export data
saveRDS(data_out, file=file.path(outputdir, "CDFW_2000_2019_monthly_landings_by_port_complex.Rds"))
write.csv(data_out, file=file.path(outputdir, "CDFW_2000_2019_monthly_landings_by_port_complex.csv"), row.names=F)
