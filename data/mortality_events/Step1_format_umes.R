
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/mortality_events/raw"
outdir <- "data/mortality_events/processed"
plotdir <- "data/mortality_events"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "NOAA_umes.xlsx"))


# Format data
################################################################################

# Read data
data <- data_orig %>% 
  # Format id
  mutate(number=number %>% stringr::str_trim() %>% as.numeric()) %>% 
  # Add year
  mutate(year1=substr(year, 1, 4) %>% as.numeric()) %>% 
  # Format type
  mutate(cause_orig=stringr::str_to_sentence(cause),
         cause=recode(cause_orig,
                      "Human interaction (vessel strike/rope entanglement)"="Human interaction",
                      "Infectious disease secondary to ecological factors (e.g. change in forage)"="Infectious disease/ecological factors",
                      "Suspect human interaction (entanglement)/infectious disease"="Human interaction/infectious disease",
                      "Suspect human interaction (vessel strike)"="Human interaction",
                      "Undetermined (suspect infectious disease)"="Infectious disease",
                      "Undetermined; secondary ecological factors"="Ecological factors")) %>% 
  # Arrange
  select(type, number, year, year1, everything())

# Inspect data
str(data)
table(data$cause_orig)
table(data$cause)
table(data$species)


# Plot data
################################################################################

# Stats
stats <- count(data, year1, cause)

# Plot data
g <- ggplot(stats, aes(x=year1, y=n, fill=cause)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Number of UMEs") +
  # Legend
  scale_fill_discrete(name="Cause") +
  # Theme
  theme_bw()
g






