
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/environmental/indices/raw"
outdir <- "data/environmental/indices/processed"

# Source
# https://www.ncdc.noaa.gov/teleconnections/pdo/

# Read data
data_orig <- read.table(file=file.path(indir, "ersst.v5.pdo.dat.txt"), fill=T, header=T, skip=1, na.strings = "-99.99")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Names
  janitor::clean_names("snake") %>% 
  # Gather
  gather(key="month", value="pdo", 2:ncol(.)) %>% 
  # Create date dummy
  mutate(month=stringr::str_to_sentence(month),
         date=paste(month, "1,", year) %>% lubridate::mdy(),
         month=lubridate::month(date)) %>% 
  # Arrange
  select(year, month, date, pdo) %>% 
  arrange(date) %>% 
  # Fix PDO
  mutate(pdo=recode(pdo, "-2.71-99.99"="-2.71") %>% as.numeric) 

# Export
saveRDS(data, file.path(outdir, "1850_2021_PDO_index_monthly.Rds"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
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
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Positive/negative text
pos_text <- "SSTs are anomalously cool in the interior North Pacific and warm along the Pacific Coast\nand when sea level pressures are below average over the North Pacific"
neg_text <- "Warm SST anomalies in the interior and cool SST anomalies along the North American coast,\nor above average sea level pressures over the North Pacific"
  
# Plot data
g <- ggplot(data, aes(x=date, y=pdo)) +
  geom_line(color="grey40") +
  # Reference line
  geom_hline(yintercept = 0) +
  # Labels
  labs(x="", y="Pacific Decadal Oscillation (PDO)") +
  # Label sides
  annotate(geom="text", x=ymd("2020-01-01"), 4, hjust=1, label=pos_text, size=2) +
  annotate(geom="text", x=ymd("2020-01-01"), -4, hjust=1, label=neg_text, size=2) +
  # Axes
  scale_x_date(breaks=seq(ymd("1850-01-01"), ymd("2020-01-01"), by="10 years"), date_labels="%Y") +
  # Theme
  theme_bw() + my_theme
g








