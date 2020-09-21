

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/landings/pacfin/raw/codes"
outputdir <- "data/landings/pacfin/processed"

# Read data
port_key_orig <- read.csv(file.path(inputdir, "pacfin_port_codes_alpha.csv"), as.is=T)


# Format data
################################################################################

# Format data
port_key <- port_key_orig %>% 
  # Columns names
  janitor::clean_names("snake") %>% 
  rename(port_code=pacfin_port_code,
         port_name=pacfin_port_name,
         port_desc=pacfin_port_description,
         agency=agency_name) %>% 
  # Add state
  mutate(state=recode(agency, 
                      "ADFG"="Alaska",
                      "AFSC"="Alaska",
                      "AKR"="Other",
                      "CDFW"="California",
                      "DFO - CANADA"="Other",
                      "GLOB"="Other",
                      "NWAFC"="Foreign",
                      "ODFW"="Oregon",
                      "WDFW"="Washington",
                      "XXXXX"="Other")) %>% 
  # Format port name/description
  mutate(port_name=stringr::str_to_title(port_name),
         port_name=stringr::str_trim(port_name),
         port_desc=stringr::str_to_title(port_desc),
         port_desc=stringr::str_trim(port_desc)) %>% 
  # Format port description
  # Fix spaces before (Catcher Processor)
  mutate(cp_note=grepl("(Catcher Processor)", port_desc),
         port_desc=gsub("\\(Catcher Processor\\)", "", port_desc) %>% stringr::str_trim(),
         port_desc=ifelse(cp_note, paste(port_desc, "(Catcher Processor)"), port_desc)) %>% 
  select(-cp_note) %>% 
  # Format port names (generic)
  mutate(port_name=gsub("Cp-", "CP-", port_name),
         port_name=gsub("Cp/", "CP/", port_name),
         port_name=gsub("Adfg", "ADFG", port_name),
         port_name=gsub("-Jv", "-JV", port_name),
         port_name=gsub("Cdfg", "CDFG", port_name),
         port_name=gsub("Nmfs", "NMFS", port_name),
         port_name=gsub("Nwafc", "NWAFC", port_name),
         port_name=gsub("Odfw", "ODFW", port_name),
         port_name=gsub("Sf", "SF", port_name),
         port_name=gsub("U.s.", "U.S.", port_name),
         port_name=gsub("Ussr", "USSR", port_name),
         port_name=gsub("Wdfw", "WDFW", port_name),
         port_name=gsub("Woc", "WOC", port_name)) %>% 
  # Format port names (with "Area Ports")
  mutate(port_name=ifelse(grepl("Area Ports", port_desc), port_desc, port_name),
         port_name=gsub("Area Ports", "Area ports", port_name)) %>% 
  # Make port names unique (makes cleaning easier)
  mutate(port_name=make.unique(port_name, sep=" ")) %>% 
  # Format California port names
  mutate(port_name=ifelse(port_desc=="San Simeon", "San Simeon", port_name),
         port_name=recode(port_name, 
                          "Avila"="Avila/Port San Luis",
                          "At Sea" = "California At Sea 1",
                          "At Sea 3" = "California At Sea 2",
                          "CDFG"="All California ports",
                          "Crescent"="Crescent City",
                          "Fields Ldg"="Fields Landing",
                          "Pnt Arena"="Point Arena",
                          "Moss Lndg"="Moss Landing",
                          "Newport B."="Newport Beach",
                          "O D Norte"="Other Del Norte County ports",
                          "O Humboldt"="Other Humboldt County ports",
                          "O La/Org"="Other Los Angeles/Orange County ports",
                          "O Mendocno"="Other Mendocino County ports",
                          "O S Diego"="Other San Diego County ports",
                          "O S Luis"="Other San Luis Obispo County ports",
                          "O Sb/Ven"="Other Santa Barbara/Ventura County ports",
                          "O Scrz/Mtr"="Other Santa Cruz/Monterey County ports",
                          "O SF/Smteo"="Other San Franciscio Bay/San Mateo County ports",
                          "O Snma/Mrn"="Other Sononoma/Marin County Outer Coast ports",
                          "Other Cal"="Other/Unknown California ports 1",
                          "Princeton"="Princeton-Half Moon Bay",
                          "Pt Angeles"="Port Angeles",
                          "Pt Hueneme"="Port Hueneme",
                          "Pt. Reyes"="Point Reyes",
                          "SF"="San Francisco",
                          "Tomales"="Tomales Bay",
                          "Unkn Cal"="Other/Unknown California ports 2",
                          "S. Barbara"="Santa Barbara",
                          "Terminal I"="Terminal Island",
                          "Willmngtn"="Willmington")) %>% 
  # Format Oregon port names
  mutate(port_name=recode(port_name,
                          "Abv Bonnev"="Columbia River-above Bonneville Dam",
                          "At Sea 1"="Oregon At Sea 1",
                          "At Sea 4"="Oregon At Sea 2",
                          "Blw Bonnev"="Columbia River-below Bonneville Dam",
                          "Cannon Bch"="Cannon Beach",
                          "Col River"="Columbia River",
                          "Col R Or."="Columbia River Area ports - Oregon",
                          "Nehalem"="Nehalem Bay",
                          "Netarts"="Netarts Bay",
                          "ODFW"="All Oregon ports",
                          "Orford"="Port Orford",
                          "Pacific"="Pacific City",
                          "Samn River"="Salmon River",
                          "Siletz"="Siletz Bay",
                          "Tlmk/Grbld"="Tillamook/Garibaldi",
                          "Unk Oregon"="Other/Unknown Oregon ports 1", 
                          "Unk Oregon 1"="Other/Unknown Oregon ports 2",
                          "Wash-Land"="Landed in WA, transported to OR 1",
                          "Wash-Land 1"="Landed in WA, transported to OR 2",
                          "Winchester"="Winchester Bay")) %>% 
  # Format Washington port names
  mutate(port_name=recode(port_name,
                          "At Sea 2"="Washington At Sea 1",
                          "At Sea 5"="Washington At Sea 2",
                          "Bellingham"="Bellingham Bay",
                          "Copalis"="Copalis Beach",
                          "Coast Wa."="Coastal Washington ports",
                          "Col R Wa."="Columbia River Area ports - Washington",
                          "CP/Wdf"="CP-Washington 1",
                          "CP/Wdf 1"="CP-Washington 2",
                          "Friday H."="Friday Harbor",
                          "Grays Hrbr"="Grays Harbor",
                          "Ilwco/Chnk"="Ilwaco/Chinook",
                          "N Puget S"="North Puget Sound ports",
                          "O Col Wa"="Other Columbia River ports",
                          "O N Puget"="Other North Puget Sound ports",
                          "O S Puget"="Other South Puget Sound ports",
                          "O Wa Coast"="Other Washingtion Coastal ports",
                          "S Puget S"="South Puget Sound ports",
                          "Townsend"="Port Townsend",
                          "Unkn Wash"="Other/Unknown Washington ports 1",
                          "Unkn Wash 1"="Other/Unknown Washington ports 2",
                          "WDFW"="All Washington ports",
                          "Willapa"="Willapa Bay")) %>%
  # Format Alaska port names
  mutate(port_name=recode(port_name,
                          "A-Y-K"="Arctic-Yukon-Kuskokwim (A-Y-K) Region ports",
                          "ADFG"="All Alaska ports",
                          "ADFG-JV"="JV-Alaska 1",
                          "ADFG-JV 1"="JV-Alaska 2",
                          "Bait-Dlv"="Floating Bait Deliveries 1",
                          "Bait-Dlv 1"="Floating Bait Deliveries 2",
                          "Bering"="Bering Sea", 
                          "Bristol-C"="Bristol Bay (Central)", 
                          "Bristol-W"="Bristol Bay",
                          "Central"="Central Region ports",
                          "Cook"="Cook Inlet",
                          "CP-Bering"="CP-Bering Sea",
                          "CP-Bristol"="CP-Bristol Bay",
                          "CP-Brstl-C"="CP-Bristol Bay (Central)",
                          "CP-Cook"="CP-Cook Inlet",
                          "CP-Dutch"="CP-Dutch Harbor",
                          "CP-Ketchkn"="CP-Ketchikan",
                          "CP-Kotzebu"="CP-Kotzebue",
                          "CP-Kuskok"="CP-Kuskokwim",
                          "CP-Norton"="CP-Norton Sound",
                          "CP-Peninsu"="CP-Alaska Peninsula",
                          "CP-William"="CP-Prince William Sound", 
                          "CP-Ptrsbrg"="CP-Wrangell-Petersburg",
                          "CP-Yukon"="CP-Yukon River", 
                          "CP/At Sea"="CP-Alaska At Sea 1", 
                          "CP/At Sea 1"="CP-Alaska At Sea 2",
                          "Dutch"="Dutch Harbor",
                          "Floating"="FP-Alaska Domestic 1", 
                          "Floating 1"="FP-Alaska Domestic 2", 
                          "Inshr Flt"="FP-Alaska Inshore 1", 
                          "Inshr Flt 1"="FP-Alaska Inshore 2", 
                          "Norton"="Norton Sound", 
                          "Ntrb Atsea"="Alaska At Sea, non-tribal", 
                          "Peninsula"="Alaska Peninsula", 
                          "Petersburg"="Wrangell-Petersburg",
                          "Pr W Snd"="Prince William Sound",
                          "Southeast"="Southeastern Region ports", 
                          "Trbl Atsea"="Alaska At Sea, tribal", 
                          "Unkn Alas"="Other/Unknown Alaska ports 1", 
                          "Unkn Alas 1"="Other/Unknown Alaska ports 2", 
                          "Westward"="Westward Region ports",
                          "WOC At Sea"="Alaska At Sea, PFMC (CP/FP) ", 
                          "WOC At Sea 1"="Alaska At Sea, PFMC", 
                          "WOC-Cp"="CP-PFMC", 
                          "WOC-Float"="FP-PFMC", 
                          "Yukon"="Yukon River")) %>% 
  # Format foreign
  mutate(port_name=recode(port_name, 
                          "Bulgria-JV"="JV-Bulgaria",
                          "E. Germany"="East Germany",
                          "Foreign"="All Countries 1",
                          "Greece-JV"="JV-Greece",
                          "Jvp"="JV-All",
                          "Korea-JV"="JV-Korea",
                          "NWAFC"="All Countries 2",
                          "Oth-Cntry"="Other Countries",
                          "Other-JVs"="JV-Other",
                          "Poland-JV"="JV-Poland",
                          "U.S."="USSR",
                          "U.S.-JV"="JV-USSR",
                          "W. Germany"="West Germany")) %>% 
  # Format other
  mutate(port_name=recode(port_name, 
                          "All Source"="All landings (US+JV+foreign)",
                          "Canada"="All Canada landings (US+Canada ports)",
                          "Canadian"="Canada Group 1 ports 1",
                          "Canadian 1"="Canada Group 1 ports 2",
                          "CP/Akr"="CP-AKR 1",
                          "CP/Akr 1"="CP-AKR 2",
                          "Domestic"="All US landings",
                          "Fl/Akr"="FP-AKR 1",
                          "Fl/Akr 1"="FP-AKR 2",
                          "NMFS/Akr"="All NMFS/AKR landings",
                          "Ss/Akr"="SS-AKR 1",
                          "Ss/Akr 1"="SS-AKR 2",
                          "Tribal"="All tribal landings",
                          "U.S. Ports"="Canada Group 2 (US) ports 1",
                          "U.S. Ports 1"="Canada Group 2 (US) ports 2",
                          "Unkn Dfo"="Other/Unknown Canada ports 1",
                          "Unkn Dfo 1"="Other/Unknown Canada ports 2",
                          "Unknown"="Unknown")) %>% 
  # Add port long
  mutate(port_name_long=ifelse(state%in%c("California", "Oregon", "Washington", "Alaska"), 
                               paste(port_name, state, sep=", "), port_name)) %>% 
  # Add port type
  mutate(port_yn=ifelse(grepl("ports|Other|Unknown|At Sea|Float|CP|JV|FP|SS|landings|All|transported", port_name) | state%in%c("Other", 'Foreign'), "no", "yes")) %>% 
  arrange(port_yn, port_name)
  
# Inspect
head(port_key)

# Confirm codes are unique
anyDuplicated(port_key$port_code)
anyDuplicated(port_key$port_name)



# Add lat/longs
################################################################################

# Packages
library(sf)
library(ggmap)
library(mapview)

# Google Maps API key
source("data/landings/pacfin/ggmap_api_key.R")
register_google(key=ggmap_api_key)

# Geocode locations
port_key_xy <- mutate_geocode(port_key, port_name_long)

# Convert to SF
port_key_xy_sf <- port_key_xy %>%
  filter(port_yn=="yes") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Plot to check
mapview(port_key_xy_sf)


# Final formatting
################################################################################

# Final formatting
port_key_final <- port_key_xy %>% 
  # Rename
  rename(state1=state,
         port_name_long1=port_name_long,
         long_dd=lon,
         lat_dd=lat) %>% 
  # Add new columns
  mutate(state2=recode(state1, 
                       "California"="CA",
                       "Oregon"="OR",
                       "Washington"="WA",
                       "Alaska"="AK"),
         port_name_long2=ifelse(state2%in%c("CA", "OR", "WA", "AK"), 
                               paste(port_name, state2, sep=", "), port_name)) %>% 
  # Overwrite lat/longs for ports that aren't single ports
  mutate(long_dd=ifelse(port_yn=="yes", long_dd, NA),
         lat_dd=ifelse(port_yn=="yes", lat_dd, NA)) %>% 
  # Arrange
  select(port_code, agency, state1, state2, 
         port_name, port_name_long1, port_name_long2, everything()) %>% 
  arrange(state1, port_name)


# Export
################################################################################

# Export
write.csv(port_key_final, file=file.path(outputdir, "pacfin_port_codes_clean.csv"), row.names = F)





