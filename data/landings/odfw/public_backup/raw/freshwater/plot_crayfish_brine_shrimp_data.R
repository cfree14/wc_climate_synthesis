

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(tigris)

# Directories
inputdir <- "data/landings/odfw/public/raw/freshwater/raw"
outputdir <- "data/landings/odfw/public/raw/freshwater/processed"
plotdir <- "data/landings/odfw/public/raw/freshwater/figures"

# Read data
data_orig <- read.csv(file.path(outputdir, "ODFW_2004_2018_crayfish_brine_shrimp_landings.csv"), as.is = T)

# Get OR counties
or_counties <- tigris::counties(state="Oregon", cb=T)

# Format data
data <- data_orig %>% 
  mutate(month=factor(month, levels=c("January", "February", "March", "April", "May", "June", "July", 
                                      "August", "September", "October", "November", "December")))


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=9),
                   plot.title=element_text(size=11),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Brine shrimp by month time series


# Brine shrimp by month overlay


# Crayfish by county time series
############################################

# Landings by county over time
cray1 <- data %>% 
  filter(species=="Crayfish") %>% 
  group_by(year, county) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  complete(year, county, fill=list(landings_lb=0, value_usd=NA))

# Plot data
g1 <- ggplot(cray1, aes(x=year, y=landings_lb/1e3, fill=county)) +
  geom_area() +
  labs(x="", y="Landings (1000s of lbs)") +
  scale_fill_discrete(name="County") +
  theme_bw() + base_theme
g1


# Crayfish by month overlay
############################################

# Crayfish
cray2 <- data %>% 
  filter(species=="Crayfish") %>% 
  group_by(year, month) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  complete(year, month, fill=list(landings_lb=0, value_usd=NA)) %>% 
  group_by(year) %>% 
  mutate(landings_prop=landings_lb/sum(landings_lb))

# Crayfish by month overlay
g2 <- ggplot(cray2, aes(x=month, y=landings_lb/1e3, color=year, group=year)) +
  geom_line() +
  labs(x="", y="Landings (1000s of lbs)") +
  # Legend
  scale_color_continuous(name="") +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.6,0.4))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "oregon_crayfish_landings_by_county_month.png"), 
       width=8.5, height=4, units="in", dpi=600)


# Crayfish by county map
############################################

# Mean landings by county
cray3 <- cray1 %>% 
  group_by(county) %>% 
  summarize(landings_lb_avg=mean(landings_lb))

# Add to counties sf
sum(!cray3$county %in% or_counties$NAME)
cray3_sf <- or_counties %>% 
  left_join(cray3, by=c("NAME"="county"))

# Plot map
g3 <- ggplot(cray3_sf) +
  geom_sf(mapping=aes(fill=landings_lb_avg/1e3), lwd=0.2, color="grey30") +
  # Labels
  geom_sf_text(mapping=aes(label=NAME), size=3, color="grey10") +
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Average annual landings\nfrom 2004 to 2018 (1000s of lbs)", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom",
        plot.title = element_blank(),
        axis.title = element_blank())
g3

# Export plot
ggsave(g3, filename=file.path(plotdir, "oregon_crayfish_landings_by_county.png"), 
       width=6.5, height=5.5, units="in", dpi=600)





