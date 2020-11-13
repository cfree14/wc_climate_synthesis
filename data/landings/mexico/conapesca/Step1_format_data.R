

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/mexico/conapesca/raw"
outputdir <- "data/landings/mexico/conapesca/processed"

# Read data
data_orig <- readRDS(file.path(inputdir, "baja_data.rds"))


# Format data
################################################################################

# Ent fco = whole fresh
# Ent fca = whole fresh
# ind. = individual whole
# 2001-2019
# Desv = no guts
# Desc = no head
# s.c. foo = fresh

# Inspect data
str(data_orig)

# Format data
data <- data_orig %>% 
  # Rename columns
  rename(disembarkment_site=NombreActivo,
         landing_site=SitioDesembarque,
         economic_unit=UnidadEconomica, # person/business
         state=Estado, 
         office=Oficina,
         place_of_capture=LugarDeCaptura,
         year=Ano,
         month=Mes,
         species_group=NombrePrincipal,
         comm_name_long=NombreComun,
         sci_name=NombreCientifico,
         landings_kg=PesoDesembarcado, # confirm units
         processed_kg=PesoVivo, # confirm units
         price_mxn_kg=Precio,
         value_mxn=Valor) %>% # confirm units %>% 
  # Format month
  mutate(month=as.character(month),
         month=recode(month, 
                      "Abril"="April",    
                      "Agosto"="August",  
                      "Diciembre"="December",  
                      "Enero"="January",  
                      "Febrero"="February",  
                      "Julio"="July",  
                      "Junio"="June",  
                      "Marzo"="March",  
                      "Mayo"="May",  
                      "Noviembre"="November",  
                      "Octubre"="October",  
                      "Septiembre"="September")) %>% 
  # Format taxonomic info to character 
  mutate_at(vars(species_group, comm_name_long, sci_name), .funs=as.character) %>% 
  # Convert location into to character
  mutate_at(vars(disembarkment_site, landing_site, economic_unit, state, office, place_of_capture), .funs=as.character) %>% 
  # Format state
  mutate(state=stringr::str_to_title(state)) %>% 
  # Format office
  mutate(office=stringr::str_to_title(office),
         office=recode(office, 
                       "Bahia Asuncion"="Bahía Asunción",
                       "Bahia De Los Angeles"="Bahía de los Ángeles",
                       "Bahia Tortugas"="Bahía Tortugas",
                       "Isla Cedros"="Isla de Cedros",
                       "Pto. Adolfo Lopez Mateos"="Puerto Adolfo López Mateos",
                       "Cd. Constitucion"="Ciudad Constitución",
                       "San Quintin"="San Quintín",
                       "Santa Rosalia"="Santa Rosalía",
                       "Villa De Jesus Maria"="Villa Jesús María")) %>% 
  # Extract info from common name
  # filete de fco., de cultivo vivo, carne de seca, aleta de, concha de, colas de, carne de, de cultivo, hueva de, piel de
  # fca./fco., ent./desv./desc./desv. y desc., filete, ind., s.c. fco.
  # mutate(type=ifelse(grepl("cultiv", comm_name_long), "cultured", "wild"),
  #        live_yn=ifelse(grepl("vivo", comm_name_long), "live", "dead")) %>% 
  # Format common name
  mutate(comm_name=gsub("filete de fco.|de cultivo vivo|carne de seca|aleta de|concha de|colas de|carne de|fca.|fco.|ent.|desv.|desc.|desv. y desc.|filete|ind.|s.c.|fco.|de cultivo|hueva de|piel de", "", comm_name_long),
         comm_name=stringr::str_trim(comm_name)) %>% 
  # Arrange
  select(year, month, state, office, landing_site, place_of_capture, 
         disembarkment_site, economic_unit, 
         species_group, comm_name_long, comm_name, sci_name, #type, live_yn,
         landings_kg, processed_kg, price_mxn_kg, value_mxn, 
         everything())

# Inspect
str(data)
freeR::complete(data)

# Common names
table(data$comm_name)

# Date
table(data$year)
table(data$month)

# Vessel, business, person
table(data$disembarkment_site)
table(data$economic_unit)
n_distinct(data$disembarkment_site)
n_distinct(data$economic_unit)

# Location
table(data$state)
table(data$office)
n_distinct(data$office)

# Place of landing/capture
table(data$landing_site)
table(data$place_of_capture)
n_distinct(data$place_of_capture)
n_distinct(data$landing_site)

# Species
n_distinct(data$species_group)
n_distinct(data$sci_name)
n_distinct(data$comm_name_long)
table(data$comm_name_long)
table(data$comm_name)


# Format data
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "2000_2015_baja_landings_translated.Rds"))

