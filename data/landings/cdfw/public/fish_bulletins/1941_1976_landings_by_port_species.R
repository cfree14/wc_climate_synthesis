
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
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw/", paste0("fb", x), "processed")
  infiles <- list.files(indir, pattern="landings_by_port")
  
  # Read and merge data
  fdata <- purrr::map_df(infiles, function(y){
    fdata1 <- read.csv(file.path(indir, y), as.is=T)
  })
  
})


# Format data
################################################################################

# Port complexes
port_complexes <- c("Eureka", "Sacramento Delta", "San Francisco", "Monterey", "Santa Barbara", "Los Angeles", "San Diego")

# Inspect data
str(data_orig)
freeR::complete(data_orig)
table(data_orig$source)
table(data_orig$year)
table(data_orig$port_complex)
table(data_orig$port)
table(data_orig$type)

# Format data
data <- data_orig %>% 
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
                          "Point Reyes Drakes Bay"="Point Reyes (Drakes Bay)",
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
                     "Point Reyes"="Point Reyes (Drakes Bay)",
                     "Drakes Bay"="Point Reyes (Drakes Bay)",
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
str(data)
freeR::complete(data)
range(data$year)
table(data$year)
table(data$source)
table(data$table)
table(data$port_complex)
table(data$port_orig)
table(data$port)
table(data$year)
table(data$type)

# Inspect names
# names2check <- data$species[!grepl("total", tolower(data$species))]
# wcfish::check_names(names2check)


# Inspect coverage
################################################################################

# Coverage
coverage <- data %>% 
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


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_1941_1976_landings_by_port_species.Rds"))



