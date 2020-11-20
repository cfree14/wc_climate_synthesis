

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/mexico/datamares/public/raw"
outputdir <- "data/landings/mexico/datamares/public/processed"

# Read data
data_orig <- readxl::read_excel(file.path(inputdir, "bb4167625s_1.xlsx"), sheet="data")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(id=main_id,
         year=ano,
         month=mes,
         state=entidad,
         office=oficina_nombre,
         type=descripcion,
         comm_name=nombre_comun,
         family=familia,
         sci_name=nombre_cientifico,
         species_group=nombreprin,
         landings_kg=peso_vivo,
         processed_kg=peso_desembarcado,
         value_mxn=valor) %>% 
  # Format state
  mutate(state=stringr::str_to_title(state),
         state=recode(state, 
                      "Michoacan"="Michoacán",
                      "Nuevo Leon"="Nuevo León",
                      "Queretaro"="Querétaro",
                      "San Luis Potosi"="San Luis Potosí",
                      "Yucatan"="Yucatán")) %>% 
  # Format office
  mutate(office=stringr::str_to_title(office)) %>% 
  # Format type
  mutate(type=recode(type, "CAPTURA"="capture", "CULTIVO"="culture")) %>% 
  # Format taxonomic info
  mutate_each(comm_name:species_group, funs = stringr::str_to_sentence) %>% 
  # Add landings info
  mutate(landings_mt=landings_kg/1e3) %>% 
  # Arrange
  select(id, year:type, species_group, family, comm_name, sci_name, landings_mt, landings_kg, everything())
  
# Inspect
str(data)
freeR::complete(data)
range(data$year)
table(data$month)
table(data$state)
table(data$type)
sort(unique(data$office))
sort(unique(data$comm_name))

# Export
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "2006_2014_mexico_landings_datamares.Rds"))



# Plot check
################################################################################

# Annual stats
stats <- data %>% 
  group_by(year, state) %>% 
  summarize(landings_mt=sum(landings_mt))

# Plot
g <- ggplot(stats, aes(x=year, y=landings_mt/1e3, fill=state)) +
  geom_area() +
  labs(x="Year", y="Landings (1000s mt)") +
  theme_bw()
g





