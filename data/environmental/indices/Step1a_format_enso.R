
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
# https://psl.noaa.gov/enso/mei/

# Read data
data_orig <- read.table(file=file.path(indir, "meiv2.data"), fill=T, header=F, skip=1, na.strings = "-999.00")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
 # Names
  setNames(c("year", month.name)) %>% 
  # Remove bottom 5 rows
  slice(1:(nrow(.)-5)) %>% 
  # Gather
  gather(key="month", value="enso", 2:ncol(.)) %>% 
  # Add date
  mutate(date=paste(month, "1,", year) %>% mdy()) %>% 
  # Arrange
  select(year, month, date, enso) %>% 
  arrange(date) %>% 
  # Format ENSO
  mutate(enso=as.numeric(enso))

# Export
saveRDS(data, file.path(outdir, "1979_2021_ENSO_index_monthly.Rds"))


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
pos_text <- ""
neg_text <- ""

# Plot data
g <- ggplot(data, aes(x=date, y=enso)) +
  geom_line(color="grey40") +
  # Reference line
  geom_hline(yintercept = 0) +
  # Labels
  labs(x="", y="Multivariate ENSO Index, v2") +
  # Label sides
  # annotate(geom="text", x=ymd("2020-01-01"), 4, hjust=1, label=pos_text, size=2) +
  # annotate(geom="text", x=ymd("2020-01-01"), -4, hjust=1, label=neg_text, size=2) +
  # Theme
  theme_bw() + my_theme
g

  
  
  
  