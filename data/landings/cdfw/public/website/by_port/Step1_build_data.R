

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/cdfw/public/website/by_port/approach3/intermediate"
outputdir <- "data/landings/cdfw/public/website/by_port/processed"
plotdir <- "data/landings/cdfw/public/website/by_port/figures"

# Read port key
port_key <- readxl::read_excel("data/landings/cdfw/public/website/by_port/port_key/port_key_v1.xlsx")


# Build data
################################################################################

# Merge files
file_merge <- list.files(inputdir)
data_orig <- purrr::map_df(file_merge, function(x){
  df <- readRDS(file.path(inputdir, x))
})

# Merge and format data
data_all <-  data_orig %>% 
  # Format species
  mutate(species=gsub("\\.", "", species)) %>% 
  # Format port
  mutate(port=recode(port, 
                     "Avila / Port San Luis"="Avila/Port San Luis",
                     "Princeton / Half Moon Bay"="Princeton/Half Moon Bay",
                     "Princeton-Half Moon"="Princeton/Half Moon Bay")) %>% 
  # Add port comples
  left_join(port_key, by="port") %>% 
  # Fill in port complex for all other ports
  group_by(year, filename) %>% 
  mutate(port_complex=ifelse(grepl("IW", toupper(filename)), "Inland Waters", port_complex),
         port_complex=ifelse(grepl("21DS|DELTA", toupper(filename)), "Sacramento Delta", port_complex),
         port_complex=ifelse(!is.na(port_complex), port_complex, unique(port_complex) %>% na.omit())) %>% 
  ungroup() %>% 
  # Rename all other ports
  mutate(port=ifelse(port=="All Other Ports", paste("Other", port_complex, "Ports"), port)) %>% 
  # Format a few species
  mutate(species=recode(species, 
                        "Prawn, Spot"="Prawn, spot",
                        "Salmon, Chinook"="Salmon, chinook",
                        "Salmon, Roe (Chinook, Coho)"="Salmon, Roe (Chinook and Coho)",
                        "Shrimp, Brine"="Shrimp, brine")) %>% 
  # Arrange
  select(year, filename, port_complex, port, year, species, landings_lb, value_usd, everything())

# Inspect data
freeR::complete(data_all)

# Which missing values?
missing_values <- data_all %>% 
  filter(is.na(value_usd)) %>% 
  mutate(id=paste(year, port_complex, port, species, sep="-")) %>% 
  pull(id)

# Remove totals
data <- data_all %>% 
  filter(!grepl("TOTAL", toupper(species)))

# Inspect 
sort(unique(data$port))

# Species key
spp_key <- data %>% 
  select(species) %>%
  unique() %>% 
  arrange(species)

# Any duplicated?
freeR::which_duplicated(toupper(spp_key$species))


# QA/QC data
################################################################################

# Calculate observed totals
tots_obs <- data %>% 
  group_by(year, filename, port_complex, port) %>% 
  summarize(landings_lb_obs=sum(landings_lb, na.rm=T),
            value_usd_obs=sum(value_usd, na.rm=T))

# Format reported totals
tots_rep <- data_all %>% 
  # Filter to totals
  filter(grepl("TOTAL", toupper(species))) %>% 
  # Remove area totals
  filter(!grepl("AREA|GRAND", toupper(species)))  %>% 
  # Remove species
  select(-species) %>% 
  rename(landings_lb_rep=landings_lb, value_usd_rep=value_usd)

# Merge reported and observed totls
tots_check <- tots_rep %>% 
  left_join(tots_obs) %>% 
  # Calculate percent difference
  mutate(landings_diff=(landings_lb_obs-landings_lb_rep)/landings_lb_rep*100,
         landings_diff_cap=pmin(3, landings_diff) %>% pmax(-3, .))

# Failed port totals
tots_check_fail <- tots_check %>% 
  filter(landings_diff>=0.5 | landings_diff <=-0.5)

# Plot QA/QC plot
g <- ggplot(tots_check, aes(x=year, y=port, fill=landings_diff_cap)) +
  geom_raster()+
  facet_grid(port_complex~., scales="free_y", space="free_y") +
  # Labels
  labs(x="", y="") +
  scale_x_continuous(breaks=seq(2000,2020,1)) +
  # Legend
  scale_fill_gradient2(name="% difference between\napparent and reported totals", midpoint = 0, low="darkred", high="navy", mid="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
        axis.title=element_blank(),
        plot.title=element_blank(),
        strip.text = element_text(size=6),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "QAQC1_figure.png"), 
       width=4, height=9, units="in", dpi=600)


# Final formatting
################################################################################

# Final formatting
data_out <- data %>% 
  # Rename
  rename(area=port_complex, comm_name_orig=species) %>% 
  # Add
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg") %>% as.numeric()) %>% 
  # Arrange
  select(year:landings_lb, landings_kg, value_usd, everything())
  

# Export data
################################################################################

# Export data
saveRDS(data_out, file.path(outputdir, "CDFW_2000_2019_landings_by_port.Rds"))




