
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outputdir <- "analyses/productivity/output"
plotdir <- "analyses/productivity/figures"

# Read stock key
stock_key_orig <- readRDS(file=file.path(outputdir, "RAM_WC_stock_key.Rds"))


# Setup
################################################################################

# Sample size
sum(!is.na(stock_key_orig$nyr_rec))
sum(!is.na(stock_key_orig$nyr_prod))


# Family stats
stats <- stock_key_orig %>% 
  # Add analysis catg
  mutate(analysis_catg=ifelse(!is.na(nyr_prod) & !is.na(nyr_rec), "Both production/recruitment", 
                             ifelse(!is.na(nyr_prod), "Production only", "Recruitment only"))) %>% 
  # Add family common/label
  mutate(family_comm=recode(family, 
                            "Sebastidae"="Rockfishes",
                            "Pleuronectidae"="Righteye flounders",
                            "Gadidae"="Codfishes",
                            "Clupeidae"="Herrings/sardines",
                            "Lithodidae"="King crabs",
                            "Hexagrammidae"="Greenlings",
                            "Cottidae"="Sculpins",
                            "Anoplopomatidae"="Sablefish",
                            "Oregoniidae"="Snow/tanner crabs",
                            "Squalidae"="Dogfish sharks",
                            "Scorpaenidae"="Scorpionfishes",
                            "Scombridae"="Tunas/mackerels",
                            "Rajidae"="Skates",
                            "Merlucciidae"="Hakes"),
         family_label=paste0(family, "\n(", family_comm, ")")) %>% 
  # N by family
  group_by(family, family_comm, family_label, analysis_catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Calculate n overall
  group_by(family, family_comm, family_label) %>% 
  mutate(n_overall=sum(n)) %>% 
  ungroup()

# Build life history key
lh_key_orig <- stock_key_orig %>% 
  # Simplify
  select(stockid, species, nyr_rec, nyr_prod, linf_cm, tmax_yr1, k) %>% 
  rename(tmax_yr=tmax_yr1)

lh_key_rec <- lh_key_orig %>% 
  filter(!is.na(nyr_rec)) %>% 
  mutate(analysis="Recruitment")

lh_key_prod <- lh_key_orig %>% 
  filter(!is.na(nyr_prod)) %>% 
  mutate(analysis="Production")

lh_key <- bind_rows(lh_key_prod, lh_key_rec) %>% 
  select(analysis, everything()) %>% 
  # Factor for legend
  mutate(analysis=factor(analysis, levels=c("Placeholder", "Production", "Recruitment")))


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

# Plot coverage by family
g1 <- ggplot(stats, aes(x=n, y=reorder(family_label, n_overall), fill=analysis_catg)) +
  geom_bar(stat="identity") + 
  # Labels
  labs(x="Number of stocks", y="") +
  # Legend
  scale_fill_discrete(name="Analysis") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.7,0.2),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot K
g2 <- ggplot(lh_key, aes(k, color=analysis)) +
  geom_density() +
  # Labels
  labs(x="Growth rate, K (1/yr)", y= "Density") +
  scale_color_discrete(drop=F, guide="none") +
  scale_x_continuous(breaks=seq(0, 0.6, 0.1), lim=c(0,NA)) +
  # Theme
  theme_bw() + my_theme
g3

# Plot linf
g3 <- ggplot(lh_key, aes(linf_cm, color=analysis)) +
  geom_density() +
  # Labels
  labs(x="Asymptotic length (cm)", y= "Density") +
  scale_color_discrete(drop=F, guide="none") +
  scale_x_continuous(breaks=seq(0, 150, 25), lim=c(0,NA)) +
  # Theme
  theme_bw() + my_theme 
g3

# Plot tmax
g4 <- ggplot(lh_key, aes(tmax_yr, color=analysis)) +
  geom_density() +
  # Labels
  labs(x="Maximum age (yr)", y= "Density") +
  scale_color_discrete(drop=F, guide="none") +
  scale_x_continuous(breaks=seq(0, 100, 20), lim=c(0,NA)) +
  # Theme
  theme_bw() + my_theme
g4

# Merge
layout_matrix <- matrix(c(1,2,
                          1,3,
                          1,4), ncol=2, byrow = T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4,
                             layout_matrix=layout_matrix, widths=c(0.6, 0.4))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_stock_traits.png"), 
       width=6.5, height=4, units="in", dpi=600)
 


