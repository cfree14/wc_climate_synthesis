
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/landings/canada/ports/raw/GSR_PORTS_TERMINALS_SVW"
outdir <- "data/landings/canada/ports/"
  
# Read data
data_orig <- read.csv(file.path(datadir, "PRT_TRMNAL.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Remove useless columns
  select(-c(cust_org, bus_cat_cl, bus_cat_ds, occpnt_typ, src_id_ind, cont_phone:cont_fax, image_url, dt_update, geocd_ind, shape)) %>% 
  # Rename more
  rename(facility_id1=srcdata_id, 
         facility_name=fclty_nm, 
         description=descriptn, 
         address1=address,
         address2=other_addr, 
         street_addres=st_address, 
         postal_code=postal_cd, 
         lat_dd1=latitude, 
         long_dd1=longitude, 
         storage_capacity=stor_cpcty, 
         data_source=data_srce, 
         berth_description=berth_desc, 
         facility_id2=seq_id, 
         long_dd2=x,
         lat_dd2=y) %>% 
  # Arrange
  select(data_source, authority, operator, facility_name, facility_id1, facility_id2, 
         address1, address2, street_addres, locality, postal_code,
         description, type_use, commodity, storage_capacity, berth_description, keywords, website,
         long_dd1, lat_dd1, long_dd2, lat_dd2, everything()) %>% 
  # Remove redundant lat/long
  select(-c(long_dd2, lat_dd2)) %>% 
  rename(long_dd=long_dd1, lat_dd=lat_dd1)

# Export data
write.csv(data, file=file.path(outdir, "BC_ports_and_terminals.csv"))


# Plot data
################################################################################

# Subset ports
ports <- data %>% 
  filter(grepl("fish", tolower(type_use)))

# Canada
canada <- rnaturalearth::ne_states(country=c("Canada", "United States of America"), returnclass = "sf")

# Plot data
g <- ggplot() +
  geom_sf(data=canada, fill="grey80", color="white", lwd=0.1) +
  # Plot ports
  geom_point(data=ports, mapping = aes(x=long_dd, y=lat_dd, color=description)) +
  coord_sf(xlim=c(-134, -120), ylim=c(48, 57)) +
  # Labels
  labs(x="", y="", title="British Colombia's fishing ports") +
  scale_color_discrete(name="Facility type") +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
        plot.title=element_text(size=8),
        axis.title=element_blank(),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position=c(0.15,0.18))
g

# Export figure
ggsave(g, filename=file.path(outdir, "canada_bc_fishing_ports.png"), 
       width=3.5, height=4, units="in", dpi=600)






