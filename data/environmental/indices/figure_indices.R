
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
plotdir <- "data/environmental/indices/figures"

# Read data
data <- readRDS(file=file.path(outdir, "indices_coastwide.Rds"))
data_lat <- readRDS(data, file=file.path(outdir, "indices_latitude.Rds"))
hci <- readRDS(file.path(outdir, "1980_2021_habitat_compression_index.Rds"))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   strip.text=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(x=date, y=value, color=index_label)) +
  facet_wrap(~index_label, ncol=3, scale="free_y") +
  # MHW years
  geom_rect(xmin=ymd("2014-01-01"),
            xmax=ymd("2017-01-01"),
            ymin=-100,
            ymax=100, 
            fill="grey80", color=NA) +
  # Lines
  geom_line(show.legend = F, size=0.2) +
  # Reference line
  geom_hline(yintercept = 0) +
  # Labels
  labs(x="", y="Index value") +
  # Axis
  scale_x_date(lim=c(ymd("1960-01-01"), ymd("2022-01-01")),
               breaks=seq(ymd("1960-01-01"), ymd("2022-01-01"), by="10 years"),
               date_labels="%Y") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_indices_coastwide.png"), 
       width=6.5, height=3.75, units="in", dpi=600)



# Plot data
################################################################################

# Plot
g <- ggplot(data_lat, aes(x=date, y=value, color=lat_dd, group=lat_dd)) +
  facet_wrap(~index_label, ncol=3, scale="free_y") +
  # MHW years
  geom_rect(xmin=ymd("2014-01-01"),
            xmax=ymd("2017-01-01"),
            ymin=-Inf,
            ymax=Inf, 
            fill="grey80", color=NA) +
  # Lines
  geom_line(size=0.2) +
  # Reference line
  geom_hline(yintercept = 0) +
  # Labels
  labs(x="", y="Index value") +
  # Legend
  scale_color_gradientn(name="Latidude (°N)",
                        colors=RColorBrewer::brewer.pal(11, "RdBu")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Axis
  scale_x_date(lim=c(ymd("1960-01-01"), ymd("2022-01-01")),
               breaks=seq(ymd("1960-01-01"), ymd("2022-01-01"), by="10 years"),
               date_labels="%Y") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.25))
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_indices_latidudinal.png"), 
       width=6.5, height=4, units="in", dpi=600)


# Plot data
################################################################################

# Plot
g <- ggplot(hci, aes(x=date, y=hci, color=region, group=region)) +
  # MHW years
  geom_rect(xmin=ymd("2014-01-01"),
            xmax=ymd("2017-01-01"),
            ymin=-Inf,
            ymax=Inf, 
            fill="grey90", color=NA) +
  # Lines
  geom_line(size=0.1) +
  # Reference line
  geom_hline(yintercept = 0) +
  # Labels
  labs(x="", y="Habitat Compression Index (HCI)") +
  # Legend
  scale_color_discrete(name="Region") +
  # Axis
  scale_x_date(lim=c(ymd("1980-01-01"), ymd("2022-01-01")),
               breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="10 years"),
               date_labels="%Y") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_indices_hci.png"), 
       width=4.5, height=2.5, units="in", dpi=600)


