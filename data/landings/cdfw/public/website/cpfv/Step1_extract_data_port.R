

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
outputdir <- "data/landings/cdfw/public/website/cpfv/processed"
plotdir <- "data/landings/cdfw/public/website/cpfv/figures"

# Build file key
################################################################################

# Build file key
file_key <- purrr::map_df(2000:2019, function(x){
  
  # Get files
  basedir <- "data/landings/cdfw/public/website/raw"
  datadir <- file.path(basedir, x, "cpfv")
  yfiles <- list.files(datadir, pattern=".pdf")
  
  # Build key
  y_key <- tibble(year=x,
                  filename=yfiles)
  
})

# Add region
file_key <- file_key %>% 
  mutate(type=ifelse(grepl("cover", tolower(filename)), "cover", ""),
         type=ifelse(grepl("port", tolower(filename)), "port", type),
         type=ifelse(grepl("state", tolower(filename)), "state", type),
         type=ifelse(grepl("cpfv2|cpfv 1|annual 1|page 2", tolower(filename)), "state", type),
         type=ifelse(grepl("cpfv1|cpfv 2|annual 2|page 1", tolower(filename)), "port", type)) %>% 
  # Manually change a few
  mutate(type=ifelse(filename=="landings08_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings08_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings07_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings07_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings06_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings06_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings05_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings05_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings04_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings04_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings03_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings03_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings02_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings02_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings01_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings01_cpfv2.pdf", "port", type),
         type=ifelse(filename=="landings00_cpfv1.pdf", "state", type),
         type=ifelse(filename=="landings00_cpfv2.pdf", "port", type))

# Check port complexes
file_check <- file_key %>% 
  group_by(year, type) %>% 
  summarize(n=n()) %>% 
  spread(key="type", value="n")


# Extract port-level data
################################################################################

# If extracting
if(F){
  
  # Loop through and export
  for(x in 2019:2000){
    
    # Directory
    basedir <- "data/landings/cdfw/public/raw"
    datadir <- file.path(basedir, x, "cpfv")
    # outdir <- "data/landings/cdfw/public/by_port/processed"
    
    # Identify files
    file_do <- file_key %>% 
      filter(year==x & type=="port") %>% pull(filename)
    
    # Read tables
    tables_list <- tabulizer::extract_tables(file.path(datadir, file_do), output = "data.frame", method="stream")
    if(length(tables_list)==0){
      tables_list <- tabulizer::extract_tables(file.path(datadir, file_do), output = "data.frame", method="lattice")
    }
    
    # Extract dataframe
    ydata_orig <- tables_list[[1]] %>% 
      mutate(year=x,
             filename=file_do) %>% 
      select(year, filename, everything())
    
    # Export data
    outfile <- paste0(x, "_port_messy.csv")
    write.csv(ydata_orig, file=file.path(datadir, outfile), row.names = F)
    
  }
  
}

# Merge and format port-level data
################################################################################

# Merge port-level data
pdata_orig <- purrr::map_df(2000:2019, function(x) {
  
  # Read data
  basedir <- "data/landings/cdfw/public/website/raw"
  datadir <- file.path(basedir, x, "cpfv")
  ydata <- readxl::read_excel(file.path(datadir, paste0(x, "_port_messy.xlsx"))) %>% 
    mutate(year=x)
  
})

# Northern ports:
n_ports <- c("Avila Beach-Morro Bay", "Monterey-Moss Land-Santa Cruz", "Princeton-Bodega Bay", 
             "San Francisco-SF Bay-Delta", "Fort Bragg-Eureka-Crescent City", "Northern Total")
  
# Format data
pdata_full <- pdata_orig %>% 
  # Arrange
  select(year, species, everything()) %>% 
  # Gather
  gather(key="port_complex", value="landings_n", 3:ncol(.)) %>% 
  # Mark region
  mutate(region=ifelse(port_complex=="Total", "All", 
                       ifelse(port_complex %in% n_ports, "Northern", "Southern"))) %>% 
  # Clean species names
  mutate(species=species %>% gsub("\\*|\\^", "", .) %>% stringr::str_trim(),
         species=recode(species, 
                        "Salmon, king (chinook)"="Salmon, king (Chinook)",
                        "LANDINGS"="Total landings:",
                        "Number of Anglers"="Number of anglers:",        
                        "NUMBER OF ANGLERS"="Number of anglers:",
                        "Number of Anglers:"="Number of anglers:",
                        "Number of Fishers:"="Number of anglers:",
                        "Reporting CPFV's:"="Number of reporting CPFVs:",
                        "REPORTING CPFVS"="Number of reporting CPFVs:",
                        "Reporting CPFVs:"="Number of reporting CPFVs:",
                        "Total Landings:"="Total landings:",
                        "TOTALS:"="Total landings:")) %>% 
  # Clean port names
  mutate(port_complex=recode(port_complex, "Monterey-Moss Land-Santa Cruz"="Monterey-Moss Landing-Santa Cruz")) %>% 
  # Add filename (b/c you forgot to export it)
  left_join(file_key %>% filter(type=="port") %>% select(-type), by="year") %>% 
  # Arrange
  select(year, filename, region, port_complex, species, landings_n, everything())

# Inspect data
str(pdata_full)
freeR::complete(pdata_full)
range(pdata_full$year)
table(pdata_full$port_complex)
table(pdata_full$species)

# Extract and format totals
pdata_tots <- pdata_full %>% 
  # Reduce to totals
  filter(species %in% c("Total landings:", "Number of anglers:", "Number of reporting CPFVs:")) %>% 
  # Reshape
  spread(key="species", value="landings_n") %>% 
  # Remove totals
  filter(!grepl("total", tolower(port_complex))) %>% 
  # Rename columns
  rename("anglers_n"="Number of anglers:", "cpfvs_n"="Number of reporting CPFVs:", "landings_n"="Total landings:") %>% 
  # Arrange
  select(year, filename, region, port_complex, everything()) %>% 
  arrange(year, region, port_complex)

# Extract landings data
pdata <- pdata_full %>% 
  # Eliminate totals
  filter(!species %in% c("Total landings:", "Number of anglers:", "Number of reporting CPFVs:")) %>% 
  filter(!grepl("Total", port_complex))

# Inspect data
str(pdata)
freeR::complete(pdata)
range(pdata$year)
table(pdata$port_complex)
table(pdata$species)
  

# QA/QC daa
################################################################################

# Check species totals (north/south)

# Calculate apparent totals
tots_obs <- pdata %>% 
  group_by(year, region, species) %>% 
  summarize(landings_n_obs=sum(landings_n))

# Extract reported totals
tots_rep <- pdata_full %>% 
  filter(port_complex %in% c("Southern Total", "Northern Total")) %>% 
  select(year, region, species, landings_n) %>% 
  rename(landings_n_rep=landings_n) %>% 
  filter(!is.na(landings_n_rep) & !grepl(":", species))

# Compare totals
tots_check <- tots_obs %>% 
  left_join(tots_rep) %>% 
  mutate(landings_n_diff=landings_n_obs-landings_n_rep)

# Check regions totals

# Calculate apparent totals
tots_obs1 <- pdata %>% 
  group_by(year, region, port_complex) %>% 
  summarize(landings_n_obs=sum(landings_n))

# Extract reported totals
tots_rep1 <- pdata_full   %>% 
  filter(species=="Total landings:") %>% 
  select(year, region, port_complex, landings_n) %>% 
  rename(landings_n_rep=landings_n) %>% 
  filter(!grepl("Total", port_complex))
  
# Compare totals
tots_check1 <- tots_obs1 %>% 
  left_join(tots_rep1) %>% 
  mutate(landings_n_diff=landings_n_obs-landings_n_rep)

my_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text=element_text(size=6),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_blank(),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="right")

# Plot
g <- ggplot(tots_check1, aes(x=year, y=port_complex, fill=landings_n_diff)) +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  scale_x_continuous(breaks=2000:2019) +
  # Legend
  scale_fill_gradient2(name="Difference in apparent\nand reported landings") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g  

# Export plot
ggsave(g, filename=file.path(plotdir, "CPFV_port_total_qaqc.png"), 
       width=5.5, height=2, units="in", dpi=600)

# Add species info
################################################################################

# Format more
pdata1 <- pdata %>% 
  # Rename
  rename(comm_name_orig=species) %>% 
  # Format common names
  mutate(comm_name_reg=wcfish::convert_names(comm_name_orig, to="regular"),
         comm_name_reg=recode(comm_name_reg,
                              'Kelp (calico) bass'='Kelp bass', 
                              'King (chinook) salmon'='Chinook salmon', 
                              'Other highly migratory spp'='Other highly migratory species', 
                              'Silver (coho) salmon'='Coho salmon', 
                              'Unspecified fishes'='Unspecified fish', 
                              'Unspecified flatfishes'='Unspecified flatfish', 
                              'Unspecified invertebrates'='Unspecified invertebrate', 
                              'Unspecified rockfishes'='Unspecified rockfish', 
                              'Unspecified sturgeon'='Sturgeon')) %>% 
  # Hamronize names
  mutate(comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm"),
         sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  select(-comm_name_reg) %>% 
  select(year:comm_name_orig, comm_name, sci_name, landings_n, everything())
  
# Check species names
# wcfish::check_names(pdata1$comm_name_reg)

# Inspect
str(pdata1)
freeR::complete(pdata1)
table(pdata1$region)
table(pdata1$port_complex)
table(pdata1$comm_name)

  
# Export port-level data
################################################################################

# Export data
write.csv(pdata1, file=file.path(outputdir, "CDFW_2000_2019_cpfv_landings_by_port_complex_species.csv"), row.names = F)
write.csv(pdata_tots, file=file.path(outputdir, "CDFW_2000_2019_cpfvs_anglers_landings_by_port_complex.csv"), row.names = F)





