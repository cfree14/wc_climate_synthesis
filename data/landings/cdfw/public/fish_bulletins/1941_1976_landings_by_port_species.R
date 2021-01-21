
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/fish_bulletins/processed"


# Merge data
################################################################################

# Which FBs?
fbs <- c(170, 168, 166, 163, 161, 159, 154, 153, 149, 144, 138, 135, 132, 129, 125, 121, 117, 111,
         108, 105, 102, 95, 89, 86, 80, 74, 67, 63, 59)

# Merge data
data_orig <- purrr::map_df(fbs, function(x){
  
  # Find files
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", paste0("fb", x), "processed")
  infiles <- list.files(indir, pattern="landings_by_port")
  
  # Read and merge data
  fdata <- purrr::map_df(infiles, function(y){
    fdata1 <- read.csv(file.path(indir, y), as.is=T)
  })
  
})


# Format data
################################################################################

# Port complexes
port_complexes <- c("Eureka", "Sacramento Delta", "San Francisco", 
                    "Monterey", "Santa Barbara", "Los Angeles", "San Diego")

# Inspect data
str(data_orig)
freeR::complete(data_orig)
table(data_orig$source)
table(data_orig$year)
table(data_orig$port_complex)
table(data_orig$port)
table(data_orig$type)

# Format data
data_full <- data_orig %>% 
  # Correct a few species names
  mutate(species=recode(species, 
                        "Total"="Totals",
                        'Jacknife clam'='Jackknife clam', 
                        'Pacific Ocean shrimp'='Pacific ocean shrimp')) %>% 
  # Fix port spellings
  rename(port_orig=port) %>% 
  mutate(port_orig=recode(port_orig, 
                          "Areata"="Arcata",
                          "All Other"="All Other Ports",
                          "Avalon Catalina Island"="Avalon (Catalina Island)",
                          "Bay Bodega"="Bay (Bodega)",
                          "Crescent Citv"="Crescent City",
                          "Encinatas"="Encinitas",
                          "Fort Bragg Noyo"="Fort Bragg (Noyo)",
                          "Lone Beach"="Long Beach",
                          "Los Anqeles"="Los Angeles",
                          "Mcrro Bay"="Morro Bay",
                          "Oxnard And Ventura"="Oxnard/Ventura",
                          "Playa Del Ray"="Playa Del Rey",
                          "Point Reyes Drakes Bay"="Point Reyes/Drakes Bay",
                          "Unknown Spelling Error"="Unknown (spelling error)",
                          # Princeton
                          "Halfmoon Bay"="Half Moon Bay",
                          "Princetonbythesea"="Princeton-by-the-sea",
                          "Princeton Halfmoon Bay"='Princeton (Half Moon Bay)',
                          "Princeton By The Sea Halfmoon Bay"="Princeton-by-the-sea (Half Moon Bay)",
                          # Port San Luis
                          "Port San Luis Avila Grover City"="Port San Luis/Avila/Grover City",
                          "Port San Luis Avila Drover City"="Port San Luis/Avila/Grover City",
                          "Port San Luis Avila"="Port San Luis/Avila",
                          # Sausalito
                          "Sausalilo"="Sausalito",
                          "Saiisalito"="Sausalito",
                          # Tomales Bay
                          "Tomalcs Bay Marshall"="Tomales Bay (Marshall)",
                          "Tomalea Bay Marshall"="Tomales Bay (Marshall)",
                          "Tomales Bay Marshall"="Tomales Bay (Marshall)",
                          "San Diego Point Loma"="San Diego/Point Loma")) %>% 
  # Harmonize ports
  mutate(port=recode(port_orig, 
                     "Bay"="Bodega Bay",
                     "Bay (Bodega)"="Bodega Bay",
                     "Fort Bragg (Noyo)"="Fort Bragg",
                     "Newport"="Newport Beach",
                     "San Clemente"="San Clemente Island",
                     "Morro"="Morro Bay",
                     "Pismo"="Pismo Beach",
                     # Avalon (Catalina Island)
                     "Avalon"="Avalon (Catalina Island)",
                     "Catalina Island"="Avalon (Catalina Island)",
                     "Santa Catalina Island"="Avalon (Catalina Island)",
                     # Princeton 
                     "Princeton-by-the-sea"="Princeton",
                     "Princeton-by-the-sea (Half Moon Bay)"="Princeton (Half Moon Bay)",
                     # Avila
                     "Avila"="Avila/Port San Luis/Grover City",
                     "Port San Luis/Avila/Grover City"="Avila/Port San Luis/Grover City",
                     "Port San Luis/Avila"="Avila/Port San Luis/Grover City")) %>% 
  # Make port complex a factor
  mutate(port_complex=factor(port_complex, levels=port_complexes)) %>% 
  # Arrange
  select(source:port_orig, port, type, species, value_usd, landings_lb, everything()) %>% 
  arrange(year, source, table, port_complex, port, type, species)

# Inspect
str(data_full)
freeR::complete(data_full)
range(data_full$year)
table(data_full$year)
table(data_full$source)
table(data_full$table)
table(data_full$port_complex)
table(data_full$port_orig)
table(data_full$port)
table(data_full$year)
table(data_full$type)

# Inspect common names
names2check <- data_full$species[!grepl("total", tolower(data_full$species))]
wcfish::check_names(names2check)


# Inspect coverage
################################################################################

# Coverage
coverage <- data_full %>% 
  # Remove totals
  filter(species!="Totals") %>% 
  # Summarize
  group_by(port_complex, port, year) %>% 
  summarize(landings_lb=sum(landings_lb))

# Plot coverage
g <- ggplot(coverage, aes(x=year, y=port)) +
  facet_grid(port_complex~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(breaks=seq(1940,1980,5)) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g


# QA/QC checks
################################################################################

# Observed totals should match reported totals
# No species should occur twice in a port-year

# Calculate observed totals
tot_obs <- data_full %>% 
  # Remove totals
  filter(species!="Totals") %>% 
  # Calculate totals
  group_by(port_complex, port, year) %>% 
  summarize(value_tot_obs=sum(value_usd, na.rm=T),
            landings_tot_obs=sum(landings_lb, na.rm=T)) %>% 
  ungroup()

# Extract reported totals
tot_rep <- data_full %>% 
  # Extract totals
  filter(species=="Totals") %>% 
  # Simplify
  select(port_complex, port, year, value_usd, landings_lb) %>% 
  # Rename
  rename(value_tot_rep=value_usd, landings_tot_rep=landings_lb)

# Merge
tots <- tot_obs %>% 
  # Add reported
  left_join(tot_rep) %>%
  # Calculate comparison
  mutate(value_tot_diff = value_tot_obs - value_tot_rep,
         landings_tot_diff = landings_tot_obs - landings_tot_rep)

# Plot coverage
g <- ggplot(tots, aes(x=year, y=port, fill=value_tot_diff)) +
  facet_grid(port_complex~., scales="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(breaks=seq(1940,1980,5)) +
  # Legend
  scale_fill_gradient2(name="Difference in\nobserved and reported values",
                       low="darkred", high="navy", mid="grey80", midpoint=0, na.value = "grey50") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")
g
  


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_1941_1976_landings_by_port_species.Rds"))



