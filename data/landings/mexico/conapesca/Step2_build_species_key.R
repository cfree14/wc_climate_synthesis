

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
data_orig <- readRDS(file.path(outputdir, "2000_2015_baja_landings_translated.Rds"))

# Build keys
################################################################################

# Group key
#############################################

# Group key
group_key <- data_orig %>% 
  select(species_group_orig) %>% 
  unique() %>% 
  arrange(species_group_orig)

# Export
write.csv(group_key, file=file.path(inputdir, "CONAPESCA_species_group_key.csv"), row.names=F)

# Species key
#############################################

# Species key
spp_key <- data_orig %>% 
  # Unique species
  group_by(sci_name) %>% 
  summarize(species_groups=paste(sort(unique(species_group)), collapse = ", ")) %>% 
  ungroup() %>% 
  arrange(sci_name) %>% 
  # Rename
  rename(sci_name_orig=sci_name) %>% 
  # Recode species
  mutate(sci_name=recode(sci_name_orig,
                         "0"="NA",
                         "Alectis cirialis"="Alectis ciliaris",
                         "Alectis crinitus"="Alectis ciliaris",
                         "Amphilophus macracanthus"="Astatheros macracanthus",
                         "Anadara mulicostata/anadara grandis"="Anadara multicostata/Anadara grandis",
                         "Argopecten circularis/argopecten ventricosus"="Argopecten irradians/Argopecten ventricosus",
                         "Arothron setosus"="Arothron meleagris",
                         "Astraea undosa/astraea turbanica"="Megastraea undosa/Megastraea turbanica",
                         "Bairdiella chrysoleuca"="Stellifer chrysoleuca",
                         "Bathystoma rimator"="Haemulon aurolineatum",
                         "Callinectes spp."="Callinectes spp",
                         "Callinectus sapidus"="Callinectes sapidus",
                         "Cambarellus sp."="Cambarellus spp",
                         "Cancer anntenarius"="Cancer antennarius",
                         "Chione fluctifraga"="Chionista fluctifraga",
                         "Chirostoma estor estor"="Chirostoma estor",
                         "Cichlasoma bifasciatum"="Vieja bifasciata",
                         "Citula dorsalis"="Carangoides otrynter",
                         "Coriphaena hippurus"="Coryphaena hippurus",
                         "Crassostrea iridescens"="Striostrea prismatica",
                         "Cynocsion sp"="Cynocsion spp",
                         "Dasyatis dipterur"="Hypanus dipterurus",
                         "Dasyatis dipterura"="Hypanus dipterurus",
                         "Dasyatis sabina"="Hypanus sabinus",
                         "Diplerctrum pacificum"="Diplectrum pacificum",
                         "Engraulis\xa0spp"="Engraulis spp",
                         "Epinephelus acanthistius"="Hyporthodus acanthistius",
                         "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                         "Epinephelus niphobles"="Hyporthodus niphobles",
                         "Etrumeus teres"="Etrumeus sadina",
                         "Eucheuma gelidium"="Meristotheca gelidium",
                         "Gigartina acicularis/gigartina elegans" = "Chondracanthus acicularis/Chondracanthus elegans",
                         "Gracilaropsis lemaneiformis"="Gracilariopsis lemaneiformis",
                         "Haemulon parrai"="Haemulon parra",
                         "Haemulon plumieri"="Haemulon plumierii",
                         "Haemulon scudderi"="Haemulon scudderii",
                         "Haliots sorenseni"="Haliotis sorenseni",
                         "Hemirhamphus brasiliensis"="Hemiramphus brasiliensis",
                         "Hoplopagrus guentheri"="Hoplopagrus guentherii",
                         "Informaci\xf3n no disponible"="NA",
                         "Lepisosteus osseus/atractosteus spatula"="Lepisosteus osseus/Atractosteus spatula",
                         "Leukoma antiqua"="Ameghinomya antiqua",
                         "Loliolopsis diomedeae"="Lolliguncula diomedeae",
                         "Lutjanus\xa0spp"="Lutjanus spp",
                         "Lutjanus spp."="Lutjanus spp",
                         "Lyropecten (nodipecten) subnodosus"="Lyropecten nodosus",
                         "Makaira indica"="Istiompax indica",
                         "Makaira nigricans/makaira mazara"="Makaira nigricans/Makaira mazara",
                         "Muricantus nigritus"="Hexaplex nigritus",
                         "Muricanthus nigritus"="Hexaplex nigritus",
                         "Mustelus henlei/mustelus lunulatus"="Mustelus henlei/Mustelus lunulatus",
                         "Mylopharingodon piceus"="Mylopharyngodon piceus",
                         "Mytilus\xa0spp"="Mytilus spp",
                         "N/d"="NA",
                         "NANA"="NA",
                         "No disponible"="NA",
                         "Octopus sp"="Octopus spp",
                         "Oreochromis\xa0spp"="Oreochromis spp",
                         "Oreochromis spp."="Oreochromis spp",
                         "Panulirus sp"="Panulirus spp",
                         "Paralichthyidae spp/pleuronectidae spp"="Paralichthyidae spp/Pleuronectidae spp",
                         "Pecten vogdesi/lyropecten (nodipecten) subnodosus"="Euvola vogdesi/Nodipecten subnodosus",
                         "Penaeus sp"="Penaeus spp",
                         "Pescara stereolepis gigas"="Stereolepis gigas",
                         "Phaeophyceae"="Phaeophyceae spp",
                         "Phyllonothus brassica"="Phyllanthus brassica",
                         "Platyrhinoides triseriatus"="Platyrhinoidis triseriata", #####
                         "Polyprion sp."="Polyprion spp",
                         "Pristis pectinatus"="Pristis pectinata",
                         "Rachycentrum canadum"="Rachycentron canadum",
                         "Rangia flexuosa /spondylus calcifer"="Rangia flexuosa/Spondylus limbatus",
                         "Rhambdia sp."="Rhamdia spp",
                         "Rhinobatos productus"="Pseudobatos productus",
                         "Rhomboplites aurorubens/aplodinotus grunniens"="Rhomboplites aurorubens/Aplodinotus grunniens",
                         "Sardinops caeruleus"="Sardinops sagax",
                         "Sanguinolaria nutalli"="Nuttallia nuttallii",
                         "Scarops perrico"="Scarus perrico",
                         "Sectator ocyurus"="Kyphosus ocyurus",
                         "Selar crumenophtalmus"="Selar crumenophthalmus",
                         "Selene brownii/selene setapinnis/selene vomer/selene brevoortii"="Selene brownii/Selene setapinnis/Selene vomer/Selene brevoortii",
                         "Seriola dumeri"="Seriola dumerili",
                         "Sphoeriodes testudinaeus"="Sphoeroides testudineus",
                         "Spondylus calcifer"="Spondylus limbatus",
                         "Strongylocentrotus\xa0spp"="Strongylocentrotus spp",
                         "Strombus galateus"="Titanostrombus galeatus",
                         "Strombus gigas"="Lobatus gigas",
                         "Strongylocentrotus franciscanus"="Mesocentrotus franciscanus",
                         "Tapes philippinarum"="Ruditapes philippinarum",
                         "Tetrapturus albidus"="Kajikia audax",
                         "Tetrapturus audax"="Kajikia audax",
                         "Tetrapturus spp/makaira spp"="Tetrapturus spp/Makaira spp",
                         "Trachinotus\xa0spp"="Trachinotus spp",
                         "Trachypenaeus faoe"="Rimapenaeus faoe",
                         "Trachypenaeus pacificus"="Rimapenaeus pacificus",
                         "Turbo fluctuosa"="Turbo fluctuosus",
                         "Urolophus maculatus"="Urobatis maculatus",
                         "Venus antiqua"="Leukoma antiqua",
                         "Xenoteca variata"="Xenotoca variata")) %>% 
  # Fix generic names
  mutate(sci_name=gsub("spp", "spp.", sci_name))

# Check names
####################

# All names
spp_all <- sort(unique(spp_key$sci_name))

# Species-specific names
spp_single <- spp_all[!grepl("spp", spp_all) & !grepl("/", spp_all)]
freeR::suggest_names(spp_single)

# Multiple species names
spp_mult <- spp_all[grepl("/", spp_all) & !grepl("spp", spp_all)]
spp_mult_vec <- purrr::map(spp_mult, function(x) strsplit(x, "/")) %>% unlist() %>% unique() %>% sort()
freeR::suggest_names(spp_mult_vec)

# Generic species names
spp_gen <- spp_all[grepl("spp", spp_all)]


# Get additional info
####################

# Taxanomic info
spp_key_taxa <- freeR::taxa(spp_all)

# Common name
spp_info <- freeR::fishbase(species=spp_single, dataset = "species", cleaned=T)
spp_comm_names <- freeR::fb_comm_name(spp_key$sci_name)

# Build out key
spp_key1 <- spp_key %>% 
  # Add common name
  left_join(spp_comm_names %>% select(species, comm_name), by=c("sci_name"="species")) %>% 
  rename(comm_name_eng=comm_name) %>% 
  # Add taxonomic info
  left_join(spp_key_taxa %>% select(-species), by=c("sci_name"="sciname"))




