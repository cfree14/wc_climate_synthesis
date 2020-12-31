



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
fbs <- c(170, 168, 166, 163, 161, 159, 154, 153, 149, 144)

# Merge data
data_orig <- purrr::map_df(fbs, function(x){
  
  # Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw/", paste0("fb", x), "processed")
  infile <- list.files(indir, pattern="Tables16-21")
  fdata <- read.csv(file.path(indir, infile), as.is=T)
  
})


# Format data
################################################################################

# Port complexes
port_complexes <- c("Eureka", "San Francisco", "Monterey", "Santa Barbara", "Los Angeles", "San Diego")

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
  # Fix ports
  mutate(port=recode(port, "Playa Del Ray"="Playa Del Rey")) %>% 
  # Make port complex a factor
  mutate(port_complex=factor(port_complex, levels=port_complexes))

# Inspect
table(data$port)

# Inspect names
names2check <- data$species[!grepl("total", tolower(data$species))]
wcfish::check_names(names2check)


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
  # Theme
  theme_bw()
g


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_19XX_1976_landings_by_port_species.Rds"))



