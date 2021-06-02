
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

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_1941_2019_landings_by_port_species.Rds"))

# Read port complex lines
complex_lines_orig <- readxl::read_excel(file.path(outdir, "port_complex_lines.xlsx"))
complex_lines_hi <- complex_lines_orig %>% 
  filter(system_id=="D") %>% 
  select(lat_dd_hi_name, lat_dd_hi, long_dd_hi) %>% 
  rename(line_name=lat_dd_hi_name, lat_dd=lat_dd_hi, long_dd=long_dd_hi)
complex_lines_lo <- complex_lines_orig %>% 
  filter(system_id=="D") %>% 
  arrange(lat_dd_lo) %>% 
  slice(1) %>% 
  select(lat_dd_lo_name, lat_dd_lo, long_dd_lo) %>% 
  rename(line_name=lat_dd_lo_name, lat_dd=lat_dd_lo, long_dd=long_dd_lo)
complex_lines <- bind_rows(complex_lines_hi, complex_lines_lo)

# Projections
wgs84 <- sp::CRS("+proj=longlat +datum=WGS84")

# USA, Mexico, California
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
ca_counties <- tigris::counties(state="California", cb=T) %>% sf::st_transform(wgs84)

# Get WCFISH data
ports <- wcfish::ports
blocks <- wcfish::blocks %>% 
  filter(block_state=="California")


# Build data
################################################################################

# Complexes
complexes <- c("Eureka", "Fort Bragg", "Bodega Bay", "Sacramento Delta", "San Francisco", 
               "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles", "San Diego")

# Calculate recent catch
data <- data_orig %>% 
  group_by(port_complex, port) %>% 
  summarize(landings_mt=mean(landings_kg[year %in% 2010:2019], na.rm=T)/1000,
            value_usd=mean(value_usd[year %in% 2010:2019], na.rm=T)) %>% 
  ungroup() %>% 
  # Add coordinates
  left_join(ports %>% select(port:long_dd), by="port") %>% 
  # Factor port complex
  filter(!port_complex%in%c("Inland Waters", "Unknown")) %>% 
  mutate(port_complex=factor(port_complex, levels=complexes)) %>% 
  # Remove other
  filter(!grepl("Other", port))

# Original data labels: label ports with full data
# Pr
if(F){
  # Full number of years
  nfull <- length(c(1941:1976, 2000:2019)) -1
  
  # Build port labels
  data_labels <- data_orig %>% 
    group_by(port_complex, port) %>% 
    summarize(nyr=n_distinct(year),
              pyr=nyr/nfull) %>% 
    ungroup() %>% 
    mutate(type=cut(pyr, breaks=c(0,0.8,0.9, Inf), labels=c("bad", "good", "full"), right=F)) %>% 
    # Add lat/long
    left_join(ports %>% select(port, long_dd, lat_dd), by="port") %>% 
    # Remove other
    filter(!grepl("Other", port))
  
  # Inspect distribution
  table(data_labels$type)
  hist(data_labels$pyr, breaks=seq(0,1,0.05))
}

data_labels <- data %>% 
  group_by(port_complex) %>% 
  arrange(desc(landings_mt)) %>% 
  slice(1:3) %>% 
  filter(!is.na(landings_mt))

# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_blank(),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5))

# Plot data
g <- ggplot() +
  # Plot blocks
  geom_sf(data=blocks, color="grey60", fill=NA, size=0.1) +
  # Plot land
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=ca_counties, fill="grey85", col="white", size=0.2) +
  # Plot county lines
  geom_segment(data=complex_lines, mapping = aes(x=long_dd, xend=long_dd-1, y=lat_dd, yend=lat_dd), color="black", linetype="solid") +
  # Plot ports (old)
  geom_point(data=data %>% filter(is.na(landings_mt)), 
             mapping=aes(x=long_dd, y=lat_dd, color=port_complex), pch=1, show.legend = F) +
  # Plot ports (recent)
  geom_point(data=data %>% filter(!is.na(landings_mt)), 
             mapping=aes(x=long_dd, y=lat_dd, fill=port_complex, size=landings_mt), pch=21, color="grey30") +
  # Port labels
  ggrepel::geom_text_repel(data_labels, # %>% filter(type=="full"), # when using original data labels method
                           mapping=aes(x=long_dd, lat_dd, label=port), size=2, show.legend = F, 
                           min.segment.length = 0, segment.color="grey30", max.overlaps = 100, force=5) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_discrete(name="Port complex\n(north to south)", drop=F) +
  scale_color_discrete(name="Port complex\n(north to south)", drop=F) +
  scale_size_continuous(name="Annual landings (mt)\n2010-2019 average", range=c(1,8)) +
  guides(fill=guide_legend(order=1), size=guide_legend(order=2)) +
  # Crop plot
  coord_sf(xlim = c(-125.5, -116.6), ylim = c(32, 42)) +
  theme_bw() + base_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigX_port_map.png"), 
       width=6, height=6.25, units="in", dpi=600)





