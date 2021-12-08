

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
sstdir <- "data/environmental/cobe/processed"
datadir <- "data/ramldb/processed"
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"

# Read stock key
# stock_key_orig <- read.csv(file=file.path(datadir, "RAM_stock_key.csv"), as.is=T)

# Read SST data
sst_orig <- read.csv(file.path(sstdir, "COBE_1981_2020_by_ram_stocks.csv"))

# Read time series data
data_prod <- readRDS(file.path(datadir, "RAM_WC_production_data_prepped_final.Rds")) %>% mutate(dataset="Production")
data_rec <- readRDS(file.path(datadir, "RAM_WC_recruitment_data_prepped_final.Rds")) %>% mutate(dataset="Recruitment")


# Build data
################################################################################

# Build data
model_data <- bind_rows(data_prod, data_rec) %>% 
  select(dataset, stockid, year, sst_c) %>% 
  mutate(model_year="yes")

# Stock SST key
stock_sst_key <- model_data  %>% 
  group_by(dataset, stockid) %>% 
  summarize(sst_c_min=min(sst_c),
            sst_c_max=max(sst_c)) %>% 
  ungroup()

# Build SST data
sst <- sst_orig %>% 
  # Rename
  rename(sst_c=sst_c_avg) %>% 
  # Reduce to stocks of interest
  filter(stockid %in% stock_sst_key$stockid) %>% 
  # Add min/max SSTs seen in modeling
  left_join(stock_sst_key, by="stockid") %>% 
  # Arrange
  select(dataset, stockid, year, sst_c, everything()) %>% 
  arrange(dataset, stockid, year) %>% 
  # Mark inside or outside range
  mutate(inside_range=ifelse(sst_c>=sst_c_min & sst_c <=sst_c_max, "inside", "outside"),
         sst_c_diff=ifelse(inside_range=="inside", 0,
                         ifelse(sst_c>sst_c_max, sst_c-sst_c_max, sst_c-sst_c_min))) %>% 
  # Mark model year
  left_join(model_data %>% select(dataset, stockid, year, model_year)) %>% 
  mutate(model_year=ifelse(is.na(model_year), "no", model_year)) %>% 
  # Classify year 
  mutate(year_type=ifelse(model_year=="yes", "Model years",
                          ifelse(sst_c_diff==0, "Inside model",
                                 ifelse(sst_c_diff>0, "Warmer", "Cooler"))))

# Determine sort order
stocks <- model_data %>% 
  # Calculate 1st year
  group_by(dataset, stockid) %>% 
  summarize(year=min(year)) %>% 
  ungroup() %>% 
  # Reshape
  mutate(dataset=tolower(dataset)) %>% 
  spread(key="dataset", value="year") %>% 
  # Sort 
  arrange(production)

# Order data
sst_ordered <- sst %>% 
  mutate(stockid=factor(stockid, levels=stocks$stockid))

# Build stats
stats <- sst %>% 
  # Calc frequency
  group_by(dataset, year, year_type) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Calc percentages
  group_by(dataset, year) %>% 
  mutate(ntot=sum(n),
         perc=n/ntot) %>% 
  ungroup() %>% 
  # Factor year type
  mutate(year_type=factor(year_type, levels=rev(c("Cooler", "Warmer", "Inside model", "Model years"))))


# Plot data
################################################################################

# Hindcast window
hindcast_window <- c(1920, 2010)

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(sst_ordered, aes(x=year, y=stockid, fill=sst_c_diff)) +
  facet_wrap(~dataset) +
  # Plot temp difference
  geom_raster() +
  # Mark model years
  geom_raster(sst_ordered %>% filter(model_year=="yes"), mapping=aes(x=year, y=stockid), fill="grey40", inherit.aes = F) +
  # Reference lines
  geom_vline(xintercept=hindcast_window) +
  # Labels
  labs(x="", y="Stock\n   ") +
  # Axes
  scale_x_continuous(breaks=seq(1880, 2020, 10)) +
  # Legend
  scale_fill_gradient2(name="SST (°C)\ndifference          ", low="navy", high="darkred", mid="white", midpoint = 0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g1

# Plot coverage
g2 <- ggplot(stats, aes(x=year, y=perc, fill=year_type)) +
  facet_wrap(~dataset) +
  geom_bar(stat="identity", color=NA) +
  # Reference lines
  geom_vline(xintercept=hindcast_window) +
  # Labels
  labs(x="", y="Percent of stocks") +
  # Axes
  scale_x_continuous(breaks=seq(1880, 2020, 10)) +
  scale_y_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Type", values=rev(c("blue", "red", "grey95", "grey40"))) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=2, heights=c(0.7, 0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS7_hindcast_window.png"), 
       width=6.5, height=7, units="in", dpi=600)


