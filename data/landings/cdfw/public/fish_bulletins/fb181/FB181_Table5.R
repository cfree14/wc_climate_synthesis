

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
indir <- "data/landings/cdfw/public/fish_bulletins/fb181/raw"
outdir <- "data/landings/cdfw/public/fish_bulletins/fb181/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/fb181/figures"

# Read data
data_orig <- read.csv(file.path(indir, "tabula-FB181_Table5_annual_landings_by_species_port_complex.csv"), as.is=T) %>% 
  rename(species="Fish.")


# Format data
################################################################################

# Step 1.

# Inspect rows with "Table"
bad_rows <- data_orig %>% 
  filter(grepl("Table", species))
  
# Format data
data1 <- data_orig %>% 
  # Add id
  mutate(row_id=1:n()) %>% 
  select(row_id, everything()) %>% 
  # Add table id
  mutate(table_id=ifelse(grepl("Table", species), species, NA),
         table_id=gsub("Table 5 a", "Table 5a", table_id),
         table_id=recode(table_id, "Table 5m (continued)"="Table 5m (continued) Please fix me:")) %>% 
  select(table_id, row_id, everything()) %>% 
  # Fill table ids
  fill(table_id, .direction="up") %>% 
  # Derive table
  mutate(table=substr(table_id, 1, 8)) %>% 
  # Derive year
  mutate(year=recode(table,
                     "Table 5a"="1987",
                     "Table 5b"="1988",
                     "Table 5c"="1989", 
                     "Table 5d"="1990", 
                     "Table 5e"="1991", 
                     "Table 5f"="1992", 
                     "Table 5g"="1993", 
                     "Table 5h"="1994", 
                     "Table 5i"="1995", 
                     "Table 5j"="1996", 
                     "Table 5k"="1997", 
                     "Table 5l"="1998", 
                     "Table 5m"="1999") %>% as.numeric()) %>% 
  # Derive category
  mutate(category=gsub(".*) ", "", table_id)) %>% 
  # Arrange
  select(row_id, year, table, category, species, Eureka:San.Diego) %>% 
  # Remove useless rows
  filter(!grepl("Table|:", species)) %>% 
  mutate(use=Eureka!="" | Fort.Bragg!="" | Bodega.Bay!="" | San.Francisco!="" | Monterey!="" | Morro.Bay!="" | Santa.Barbara!="" | Los.Angeles!="" | San.Diego!="") %>% 
  filter(use==T) %>% 
  select(-use) %>% 
  # Fill species
  mutate(species=recode(species, "Total ex-vessel value (dollars)"="Total", "Total pounds"="Total")) %>% 
  mutate(species=ifelse(species=="", NA, species)) %>% 
  fill(species, .direction="down") %>% 
  # Fix values 
  mutate(year=ifelse(row_id>=6076, 1999, year),
         table=ifelse(row_id>=6076, "Table 5m", table)) %>% 
  # Fix Table 5i Fish Kelp (row 3524)
  filter(row_id != 3524) %>% 
  # Add landings/value marker
  mutate(metric=rep(c("Landings", "Value"), 5764/2)) %>% 
  select(row_id, species, metric, everything()) %>% 
  # Fix category 
  mutate(category=ifelse(category=="Please fix me:" & row_id <= 6055, "Mollusks:", category),
         category=ifelse(category=="Please fix me:" & row_id <= 6061, "Plants:", category),
         category=ifelse(category=="Please fix me:" | is.na(category) & row_id>6061, "Miscellaneous invertebrates:", category)) 

# Inspect data
str(data1)
table(data1$year)
table(data1$table)
table(data1$category)
table(data1$port_complex)
freeR::complete(data1)

# Export
write.csv(data1, file=file.path(indir, "FB181_Table5_annual_landings_by_species_port_complex_imperfect.csv"), row.names=F)

# Step 1.
#############################

# Read data
data2 <- readxl::read_excel(file.path(indir, "FB181_Table5_annual_landings_by_species_port_complex_imperfect.xlsx"))

# Format
data3 <- data2 %>% 
  # Reshape
  select(-sort) %>% 
  gather(key="port_complex", value="value", 7:ncol(.)) %>% 
  # Format columns
  mutate(value=gsub(",|\\$", "", value) %>% as.numeric(),
         port_complex=gsub("\\.", " ", port_complex),
         category=gsub(":", "", category)) %>% 
  # Reshape again
  select(table, year, port_complex, category, species, metric, value) %>% 
  spread(key="metric", value="value") %>% 
  rename(landings_lb=Landings, value_usd=Value) %>% 
  # Reduce to rows with data
  filter(!is.na(landings_lb) | !is.na(value_usd))
  
  
# QA/QC data
################################################################################

# Get totals
tots_rep <- data3 %>% 
  filter(species=="Total") %>% 
  select(-c(category, species)) %>% 
  rename(landings_lb_rep=landings_lb,
         value_usd_rep=value_usd)

# Compute observed totals
tots_obs <- data3 %>% 
  filter(species!="Total") %>% 
  group_by(table, year, port_complex) %>% 
  summarize(landings_lb_obs=sum(landings_lb),
            value_usd_obs=sum(value_usd)) %>% 
  ungroup()

# Compare totals
tots_check <- tots_rep %>% 
  left_join(tots_obs) %>% 
  mutate(landings_diff=(landings_lb_obs-landings_lb_rep)/landings_lb_rep*100,
         value_diff=(value_usd_obs-value_usd_rep)/value_usd_rep*100)

range(tots_check$landings_diff)

# Plot 
g <- ggplot(tots_check, aes(x=year, y=port_complex, fill=landings_diff)) +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  scale_x_continuous(breaks=1987:1999) +
  # Legend
  scale_fill_gradient2(name="% difference in\nreported vs derived values", 
                       midpoint = 0, low="darkred", high="navy", mid="grey80") +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
g

# Prepare final data
################################################################################

# Final data
data4 <- data3 %>% 
  # Remove totals
  filter(species!="Total") %>% 
  # Rename columns
  rename(comm_name_orig=species) %>% 
  # Add source and landings in kg
  mutate(source="FB 181",
         landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>%
  # Add presentation
  mutate(presentation=ifelse(grepl("claws", comm_name_orig), "claws", 
                             ifelse(grepl("roe on kelp", comm_name_orig), "roe on kelp", 
                                    ifelse(grepl("roe", comm_name_orig), "roe", "whole")))) %>% 
  # Order columns
  select(source, table:comm_name_orig, presentation, 
         landings_lb, landings_kg, value_usd, everything())

# Inspect data
str(data4)
table(data4$year)
table(data4$table)
table(data4$category) # fix this one
table(data4$port_complex)
freeR::complete(data4)

# Export
write.csv(data4, file=file.path(outdir, "FB181_Table5_1987_1999_landings_by_port_complex_species.csv"), row.names=F)


# Plot data
################################################################################

# By complex
############################

# Build stats
stats1 <- data4 %>% 
  group_by(year, port_complex) %>% 
  summarize(landings_lb=sum(landings_lb)/1e6,
            value_usd=sum(value_usd)/1e6) %>% 
  gather(key="metric", value="value", 3:4) %>% 
  mutate(metric=recode(metric, 
                       "landings_lb"="Landings (millions lbs)",
                       "value_usd"="Value (millions USD)"))

# Plot data
g <- ggplot(stats1, aes(x=year, y=value, fill=port_complex)) +
  facet_wrap(~metric, ncol=2, scales="free_y") +
  geom_area() +
  # Labels
  labs(x="", y="", title="Annual landings by port complex, 1987-1999") +
  scale_fill_discrete(name="Port complex") +
  scale_x_continuous(breaks=1987:1999) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_blank(),
        strip.text=element_text(size=8),
        legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        plot.title=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "CDFW_1987_1999_landings_by_port_complex.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


# By category
############################

# Build stats
stats1 <- data4 %>% 
  group_by(year, category) %>% 
  summarize(landings_lb=sum(landings_lb)/1e6,
            value_usd=sum(value_usd)/1e6) %>% 
  gather(key="metric", value="value", 3:4) %>% 
  mutate(metric=recode(metric, 
                       "landings_lb"="Landings (millions lbs)",
                       "value_usd"="Value (millions USD)"))

# Plot data
g <- ggplot(stats1, aes(x=year, y=value, fill=category)) +
  facet_wrap(~metric, ncol=2, scales="free_y") +
  geom_area() +
  # Labels
  labs(x="", y="", title="Annual landings by taxonomic group, 1987-1999") +
  scale_fill_discrete(name="Taxonomic group") +
  scale_x_continuous(breaks=1987:1999) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_blank(),
        strip.text=element_text(size=8),
        legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        plot.title=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "CDFW_1987_1999_landings_by_taxa_group.png"), 
       width=6.5, height=2.5, units="in", dpi=600)
