

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/cdfw/public/intermediate/by_port/2cleaned"
outputdir <- "data/landings/public/cdfw/processed"


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(datadir)

# Merge data
data_orig <- purrr::map_df(files2merge, function(x) {
  fdata <- readxl::read_excel(file.path(datadir, x)) %>% 
    mutate_all(.funs=as.character)
})

# Inspect data
table(data_orig$year)
table(data_orig$area) # make sure no unmatched in final
table(data_orig$port) # this is terrible
table(data_orig$species)


# Basic formatting
################################################################################

# Format data
data1 <- data_orig %>% 
  # Reduce to data
  filter(!is.na(landings) & landings!="Pounds") %>% 
  # Format numbers
  mutate(year=as.numeric(year),
         landings=as.numeric(landings),
         value=as.numeric(value)) %>% 
  # Format port
  mutate(port=stringr::str_to_title(port), 
         port=recode(port, 
                     "Guadelupe Beach"="Guadalupe Beach",
                     "Iverness"="Inverness",
                     "Moss Landng"="Moss Landing",
                     "Princeton / Half Moon Bay"="Princeton-Half Moon",
                     "San Leadro"="San Leandro",
                     "Santa Barbara Harbor"="Santa Barbara",
                     "Shelter Cover"="Shelter Cove",)) %>% 
  # Rename columns
  rename(landings_lb=landings, value_usd=value) %>% 
  # Convert landings units
  mutate(landings_kg=measurements::conv_unit(landings_lb, from="lbs", to="kg"),
         landings_mt=landings_kg/1000) %>% 
  # Arrange columns
  select(year, filename, area, port, species, landings_lb, landings_kg, landings_mt, value_usd, everything())

# Extract totals
totals <- data1 %>% 
  # Totals
  filter(grepl("TOTAL", toupper(species)))

# Inspect data
table(data1$year)
table(data1$area)
table(data1$port)
table(data1$species)


# QA/QC number 1: make sure every year-area-port has a total row
################################################################################

check1 <- data1 %>% 
  ungroup() %>% 
  group_by(year, filename, area, port) %>% 
  summarise(n=n(),
            tot_row_yn=ifelse(sum(grepl("TOTAL", toupper(species)))>0, "yes", "no")) %>% 
  ungroup() %>% 
  filter(tot_row_yn=="no") %>% 
  arrange(desc(year), filename)




# Step 1. Make sure every port has a total row
################################################################################

# Check 1
check1 <- data_orig %>% 
  group_by()





# Inspect data
str(data)
table(data$year)


# QA/QC data
################################################################################

# Calculate coverage
stats <- data %>% 
  group_by(year, area, port) %>% 
  summarize(nspp=n_distinct(species))

# Plot coverage
g <- ggplot(stats, aes(x=year, y=port, fill=nspp)) +
  facet_wrap(~area, scales="free_y", ncol=1, strip.position = "right") +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  #Theme
  theme_bw() +
  theme(legend.position="bottom")
g


# Export data
################################################################################

# Export formatted data
write.csv(data, file=file.path(outputdir, "CA_2000_2019_landings_by_year_species_port.csv"), row.names=F)

