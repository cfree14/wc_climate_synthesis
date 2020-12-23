
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir1 <- "data/landings/swfsc/processed"
datadir2 <- "data/landings/cdfw/public/fish_bulletins/fb181/processed"
datadir3 <- "data/landings/cdfw/public/processed"
outputdir <- "data/landings/cdfw/merged"
plotdir <- "data/landings/cdfw/figures"

# Read data
data1_orig <- read.csv(file.path(datadir1, "1928_2002_CA_landings_data_swfsc_longlist_annual.csv"), as.is=T)
data2_orig <- read.csv(file.path(datadir2, "FB181_Table5_1987_1999_landings_by_port_complex_species_expanded.csv"), as.is=T)
data3_orig <- read.csv(file.path(datadir3, "CDFW_2000_2019_landings_by_port_expanded.csv"), as.is=T)


# Species key
################################################################################




# Build data
################################################################################

# 1928-1986 SWFSC Project
# 1987-1999 FB 181
# 2000-2019 CDFW website

# The file should have the following:
# source, port complex, port, year, comm_name_orig, comm_name, sci_name, presentation, landings_lb, landings_kg, value_usd

# Format data
# Email about values
data1 <- data1_orig  %>% 
  filter(year<=1986) %>% 
  mutate(source="Mason 2004",
         port="Not specified") %>% 
  select(source, port_complex, port, year, 
         comm_name_orig, sci_name, level, 
         presentation, landings_lb, landings_kg)

# Format data
data2 <- data2_orig %>% 
  filter(year >= 1987 & year <= 1999) %>%  # this doesn't actually filter anything
  mutate(source="Leos 2014 (Fish Bulletin 181)", 
         port="Not specified") %>% 
  select(source, port_complex, port, year, 
         comm_name_orig, sci_name, level, 
         presentation, landings_lb, landings_kg, value_usd)

# Format data
data3 <- data3_orig %>% 
  filter(year >= 2000) %>%  # this doesn't actually filter anything
  mutate(source="CDFW 2020") %>% 
  rename(port_complex=area) %>% 
  select(source, port_complex, port, year, 
         comm_name_orig, sci_name, level,
         presentation, landings_lb, landings_kg, value_usd)

# Merge data
data <- bind_rows(data1, data2, data3) %>% 
  # Arrange
  select(source, port_complex, port, year, comm_name_orig, sci_name, level,
         presentation, landings_lb, landings_kg, value_usd) %>% 
  arrange(year, port_complex, port, comm_name_orig) %>% 
  # Remove landings equals 0
  filter(landings_lb>0)

# Inspect data
freeR::complete(data)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "1928_2019_CA_landings_by_port_species.Rds"))


# Plot data
################################################################################

# Calculate annual catch
stats <- data %>% 
  group_by(source, year, port_complex) %>% 
  summarize(landings_mt=sum(landings_kg)/1000) %>% 
  mutate(source=recode(source, "Leos 2014 (Fish Bulletin 181)"="Leos 2014"),
         source=factor(source, levels=c("Mason 2004", "Leos 2014", "CDFW 2020"))) %>% 
  ungroup()

# Plot annual catch
g <- ggplot(stats, aes(x=year, y=landings_mt/1e3, fill=port_complex)) +
  facet_grid(.~source, space="free_x", scales = "free_x") +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks=seq(1920,2020,10)) +
  scale_fill_discrete(name="Port complex") +
  labs(x="", y="Landings (1000s mt)") +
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=7),
        legend.text=element_text(size=6),
        legend.title=element_text(size=7),
        strip.text=element_text(size=7),
        plot.title=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "right")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "figure_merged_public_landings.png"), 
       width=6.5, height=3, units="in", dpi=600)

