
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(rgdal)
library(rgeos)
library(lwgeom)
library(ggrepel)
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_apps/wc_landings/data" # when testing
# codedir <- "shiny_apps/wc_landings/code" # when testing

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read port data
ports_ca <- read.csv(file.path(datadir, "port_key_v3.csv"), as.is=T)

# Read landings data
data_ca_orig <- read.csv(file.path(datadir, "CDFW_2000_2019_landings_by_port_expanded.csv"), as.is=T)
data_noaa_orig <- readRDS(file.path(datadir, "NOAA_1950_2019_wc_landings_by_state_species.Rds"))

# Read country shapefiles
load(file.path(file.path(datadir, "country_shapefiles.Rdata")))


# Prepare data
################################################################################

# Ports and order
areas_do <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco", "Monterey", "Morro Bay", "Santa Barbara", "Los Angeles", "San Diego")

# Prepare CA marine landings data
data_ca <- data_ca_orig %>% 
  # Reduce to:
  filter(environment=="marine" & presentation=="whole" & level=="species" & area %in% areas_do) %>% 
  # Add a nice species name
  mutate(species_label=paste0(comm_name, " (", sci_name, ")")) %>% 
  # Arrange port complex
  mutate(area=factor(area, levels=areas_do))


# Parameters
################################################################################

# Species
species_ca <- sort(unique(data_ca$species_label))

# Base theme
base_theme <- theme(axis.text=element_text(size=14),
                    axis.title=element_text(size=16),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=16),
                    strip.text=element_text(size=16),
                    plot.subtitle=element_text(size=18),
                    plot.title=element_text(size=16),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

# User interface
################################################################################

# User interface
ui <- navbarPage("West Coast Fisheries & Climate Change Explorer",
  
  # Explore by species                              
  tabPanel("Explore by species",
   
   # Title 
   h3("Explore landings by species"),    
           
    # Select species
    p("Only landings of marine species with taxonomic identification to the species level are shown here. In other words, freshwater species (from the Sacramento Delta and other Inland Waters) and species in broad taxonomic groups (e.g., unspecified crustacean) are excluded."),
    p("The taxonomic resolution of reported landings has increased through time and the absence of landings can occur not because a fish or invertebrate was not landeded but because its landings were recorded in a broader taxonomic group."),
    selectizeInput(inputId = "species", label = "Select a species:", 
                   choices = species_ca,  multiple = F, options = NULL),
    br(),
    
    # RANK METRICS
    h4("Economic importance of the commercial fishery"),
    
    # Plot recent rank scatterplot
    p("The figure below shows the average annual landings and value over the last 5 years (2015-2019) for the selected species compared to the other species landed in California during this time period. Values (USD) have not been adjusted to account for inflation."),
    plotOutput(outputId = "plot_rank_scatterplot", width=700, height=600),
    br(),
    
    # Plot rank state-wide and by port complex
    p("The figure below shows the rank of the selected species relative to other commercial species over time. The rank represents fisheries from largest to smallest in terms of total landings and total value, i.e., a rank of 1 represents the largest fishery."),
    plotOutput(outputId = "plot_rank_by_species", width=1000, height=500), 
    br(),
    
    # TREND METRICS
    h4("Trends in commercial fishery landings and value"),
    
    # Plot landings time series by port complex
    p("The figure below shows the trends in annual landings and value for the selected species by port complex. Values (USD) have not been adjusted to account for inflation."),
    plotOutput(outputId = "plot_landings_ts_by_area", width=1000, height=500),
    br(),
    
    # Plot landings time series by port complex
    p("The figure below shows the trends in the proportion of annual landings and value for the selected species coming from each port complex. Values (USD) have not been adjusted to account for inflation."),
    plotOutput(outputId = "plot_landings_ts_by_area_prop", width=1000, height=500), 
    br(),
    
    # GEOGRAPHY OF LANDINGS
    h4("Shifting geography of commercial landings"),
    
    # Plot map of landings
   p("The figure below shows the average annual landings over the last 5 years (2015-2019) for the selected species by port. In some years and for some species, landings are attributed to an “All Other Ports” category to maintain confidentiality. These landings are plotted at the centroid of the areas ports."),
   plotOutput(outputId = "plot_landings_by_port_map", width=800, height=1000),
   br(),
   
    # Plot latitude shift through time
   p("The figure below shows how the landings-weighted latitude of the selected species has shifted through time. This calculation excludes the landings attributed to the “All Other Ports” category."),
   plotOutput(outputId = "plot_landings_lat_shift", width=800, height=300),
   br(),
           
  ), # end explore species
  
  # Explore by port
  tabPanel("Explore by port")

)


# Server
################################################################################

# Server
server <- function(input, output){
  
  # Plot rank scatterplot
  output$plot_rank_scatterplot <- renderPlot({
    g <- plot_rank_scatterplot(dataset=data_ca, species_label = input$species, base_theme=base_theme)
    g
  })
  
  # Plot rank state-wide and by port
  output$plot_rank_by_species <- renderPlot({
    g <- plot_rank_by_species(dataset=data_ca, species_label = input$species, base_theme=base_theme)
    g
  })
  
  # Plot landings time series by port complex
  output$plot_landings_ts_by_area <- renderPlot({
    g <- plot_landings_ts_by_area(dataset=data_ca, noaa=data_noaa_orig, species_label = input$species, base_theme=base_theme)
    g
  })
  
  # Plot landings time series by port complex (proprotional)
  output$plot_landings_ts_by_area_prop <- renderPlot({
    g <- plot_landings_ts_by_area_prop(dataset=data_ca, species_label = input$species, base_theme=base_theme)
    g
  })
  
  # Plot landings by port map
  output$plot_landings_by_port_map <- renderPlot({
    g <- plot_landings_by_port_map(dataset=data_ca, ports=ports_ca, species_label = input$species, base_theme=base_theme, usa=usa, mexico=mexico)
    g
  })
  
  # Plot landings lat shift
  output$plot_landings_lat_shift <- renderPlot({
    g <- plot_landings_lat_shift(dataset=data_ca, ports=ports_ca, species_label = input$species, base_theme=base_theme)
    g
  })
  
}

shinyApp(ui = ui, server = server)
