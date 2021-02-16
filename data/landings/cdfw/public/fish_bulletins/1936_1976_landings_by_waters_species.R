
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/fish_bulletins/processed"
plotdir <- "data/landings/cdfw/public/fish_bulletins/figures"


# Merge data
################################################################################

# Which FBs?
# 154, 86 -- too blurry to use
fbs <- c(170, 168, 166, 163, 161, 159, 153, 149, 144, 138, 135, 132, 129, 125,
         121, 117, 111, 108, 105, 102, 95, 89, 80, 74, 67, 63, 59, 58, 57) # 154 (1970), 86 (1950), 80-Table 31 (1949)

# Merge data
data_orig <- purrr::map_df(fbs, function(x){
  
  # Find files
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", paste0("fb", x), "raw")
  infiles <- list.files(indir, pattern="by_waters")
  
  # Read and merge data
  fdata <- purrr::map_df(infiles, function(y){
    fdata1 <- readxl::read_excel(file.path(indir, y)) %>% 
      # Convert to character to ease merge
      mutate_all(as.character) %>% 
      # Add FB and table
      mutate(source=paste("FB", x), 
             table=y)
  })
  
})


# Format data
################################################################################

# Format data
data_full <- data_orig %>% 
  # Rename
  rename(comm_name_orig=Species) %>% 
  # Remove useless rows
  mutate(comm_name_orig=stringr::str_trim(comm_name_orig)) %>% 
  filter(!grepl("check", comm_name_orig)) %>% 
  # Remove useless columns
  select(-c("Category", "Total", "Total check 1", "Total check 2")) %>% 
  # Arrange
  select(source, table, comm_name_orig, everything()) %>% 
  # Gather regions
  gather(key="waters", value="landings_lb", 4:ncol(.)) %>% 
  # Format table
  mutate(table=table %>% gsub("_by_waters.xlsx", "", .) %>% gsub("Table", "Table ", .)) %>% 
  # Format landings
  mutate(landings_lb=gsub("[[:punct:]]", "", landings_lb) %>% as.numeric(),
         landings_lb=ifelse(is.na(landings_lb), 0, landings_lb)) %>% 
  # Add year
  mutate(source_table=paste(source, table, sep="-"), 
         year=recode(source_table, 
                     "FB 57-Table 7"=1936,
                     "FB 57-Table 17"=1937,
                     "FB 57-Table 27"=1938,
                     "FB 57-Table 37"=1939,
                     "FB 58-Table 13"=1940,
                     "FB 59-Table 21"=1941,  
                     "FB 59-Table 31"=1942,  
                     "FB 63-Table 28"=1943,    
                     "FB 63-Table 38"=1944,   
                     "FB 67-Table 28"=1945,   
                     "FB 67-Table 38"=1946,    
                     "FB 74-Table 51"=1947,   
                     "FB 80-Table 14"=1948, 
                     "FB 80-Table 31"=1949,
                     "FB 86-Table 9"=1950,
                     "FB 89-Table 15"=1951, 
                     "FB 95-Table 11"=1952, 
                     "FB 102-Table 12"=1953,   
                     "FB 102-Table 30"=1954,   
                     "FB 105-Table 15"=1955,   
                     "FB 105-Table 33"=1956,   
                     "FB 108-Table 7"=1957,  
                     "FB 108-Table 25"=1958, 
                     "FB 111-Table 7"=1959,    
                     "FB 117-Table 7"=1960,    
                     "FB 121-Table 7"=1961,   
                     "FB 125-Table 7"=1962,    
                     "FB 129-Table 8"=1963,    
                     "FB 132-Table 7"=1964,   
                     "FB 135-Table 7"=1965,    
                     "FB 138-Table 7"=1966,  
                     "FB 144-Table 7"=1967,  
                     "FB 149-Table 7"=1968,  
                     "FB 153-Table 7"=1969, 
                     "FB 154-Table 7"=1970, 
                     "FB 159-Table 7"=1971,  
                     "FB 161-Table 7"=1972,  
                     "FB 163-Table 7"=1973,  
                     "FB 166-Table 7"=1974,
                     "FB 168-Table 7"=1975,  
                     "FB 170-Table 7"=1976) %>% as.numeric()) %>% 
  # Format common names
  mutate(comm_name_orig=recode(comm_name_orig,
                               '( lam, jacknife'='Clam, jackknife', 
                               "('ahezon"='Cabezon', 
                               '(‘abrilla spotted'='Cabrilla, spotted', 
                               '(’lam, gaper'='Clam, gaper', 
                               '(’lam, jackknife'='Clam, jackknife', 
                               '(Mam gaper'='Clam, gaper', 
                               '(Mam, jackknife'='Clam, jackknife', 
                               '\\ cllowtail'='Yellowtail', 
                               '\\V hitefish ocean'='Whitefish, ocean', 
                               '•Sable fish'='Sablefish', 
                               '•Salmon'='Salmon', 
                               '^ ellowtail, California'='Yellowtail, California', 
                               '< irrenling kelp'='Greenling, kelp', 
                               '8melt'='Smelt', 
                               'Abalonc'='Abalone', 
                               'Abalone -'='Abalone', 
                               'Abalone black'='Abalone, black', 
                               'Abalone green'='Abalone, green', 
                               'Abalone pink'='Abalone, pink', 
                               'Abalone pinto'='Abalone, pinto', 
                               'Abalone red'='Abalone, red', 
                               'Abalone, black'='Abalone, black', 
                               'Abalone, green'='Abalone, green', 
                               'Abalone, hreaded'='Abalone, threaded', 
                               'Abalone, pink'='Abalone, pink', 
                               'Abalone, pinto'='Abalone, pinto', 
                               'Abalone, threaded'='Abalone, threaded', 
                               'Abalone, white'='Abalone, white', 
                               'Abalooe pink'='Abalone, pink', 
                               'Abalooe red'='Abalone, red', 
                               'Abalooe threaded'='Abalone, threaded', 
                               'Abalooe white'='Abalone, white', 
                               'Abalooe, black'='Abalone, black', 
                               'Abdone red'='Abalone, red', 
                               'Abdone white'='Abalone, white', 
                               'Abdooe threaded'='Abalone, threaded', 
                               'Ablone, flat'='Abalone, flat', 
                               'Ahalone'='Abalone', 
                               'Anchovy, northern'='Anchovy, northern', 
                               'Barracuda California'='Barracuda, California', 
                               'Barraruda, California'='Barracuda, California', 
                               'Bass, rock'='Bass, rock', 
                               'Black fish, Sacramento'='Blackfish, Sacramento', 
                               'Bon i to Pacific —'='Bonito, Pacific', 
                               'Bonilo'='Bonito', 
                               'Bonito Pacific'='Bonito, Pacific', 
                               'Booito Pacific'='Bonito, Pacific', 
                               'Boruto, Pacific'='Bonito, Pacific', 
                               'Butter fish Pacific'='Butterfish, Pacific', 
                               'Cabcsonc'='Cabezon', 
                               'Cabcxon'='Cabezon', 
                               'Cabczon'='Cabezon', 
                               'Cabeiooc'='Cabezon', 
                               'Cabeson'='Cabezon', 
                               'Cabexon'='Cabezon', 
                               'Cabexone'='Cabezon', 
                               'Cabezone'='Cabezon', 
                               'Cabrilla spotted'='Cabrilla, spotted', 
                               'Cabrilla, spotted'='Cabrilla, spotted', 
                               'CabriUa'='Cabrilla', 
                               'Cabzeon'='Cabezon', 
                               'Caezon'='Cabezon', 
                               'Caheron'='Cabezon', 
                               'Cahrilla'='Cabrilla', 
                               'Car?--'='Carp', 
                               'Carp————'='Carp', 
                               'Cbm, jackknife'='Clam, jackknife', 
                               'Cbm, pismo'='Clam, pismo', 
                               'Cbm, Pismo'='Clam, pismo', 
                               'Cbm, softshell'='Clam, softshell', 
                               'Cbm, Washington'='Clam, Washington', 
                               'Cbnt gaper'='Clam, gaper', 
                               'Clam Japanese’'='Clam, Japanese', 
                               'Clam, Cockle'='Clam, cockle', 
                               'Clam, I’isino'='Clam, pismo', 
                               'Clam, jackknife'='Clam, jackknife', 
                               'Clam, jacknife'='Clam, jackknife', 
                               'Clam, Japanese1'='Clam, Japanese', 
                               'Clam, jraper'='Clam, gaper', 
                               'Clam, Jtaper'='Clam, gaper', 
                               'Clam, paper'='Clam, gaper',
                               'Clam, Raior'='Clam, razor', 
                               'Clam, Razor'='Clam, razor', 
                               'Clam, rismo'='Clam, pismo', 
                               'Clam, soft-shell'='Clam, softshell', 
                               'Clam, Soft-shell'='Clam, softshell', 
                               'Clam, Softahell'='Clam, softshell', 
                               'Clam, Softehell'='Clam, softshell', 
                               'Clam, softshell'='Clam, softshell', 
                               'Clam, Softshell'='Clam, softshell', 
                               'Clam, unclassified'='Clam, unclassified', 
                               'Clam, unehissified'='Clam, unclassified', 
                               'Clam, Washingon'='Clam, Washington', 
                               'Clam, washington'='Clam, Washington', 
                               'Clam5'='Clam', 
                               'Corbina Mexican'='Corbina, Mexican', 
                               'Corvina, Mexican'='Corbina, Mexican', 
                               'Corvina, shortfin'='Corvina, shortfin', 
                               'Crab, king'='Crab, king', 
                               'Crab, market'='Crab, market', 
                               'Crab, pelagic red'='Crab, pelagic red', 
                               'Crab, rock'='Crab, rock', 
                               'Crab, spider'='Crab, spider', 
                               'Crab,rock'='Crab, rock',  
                               'Croaker, white'='Croaker, white', 
                               'Croaker, white (Kingfish)'='Croaker, white', 
                               'Croaker, white (Kinjtfish)'='Croaker, white', 
                               'Crocker, white'='Croaker, white', 
                               'Crustacean: Crab'='Crab', 
                               'Crustacean: Crab Dungeness'='Crab, Dungeness', 
                               'Crustacean: Crab, market'='Crab, market', 
                               'Cult us, Pacific'='Cultus, Pacific', 
                               'Cultus Pacific'='Cultus, Pacific', 
                               'Dolphin'='Dolphinfish', 
                               'Dolphin fish'='Dolphinfish', 
                               'Dolphinfish, common'='Dolphinfish, common', 
                               'Dolphmfish'='Dolphinfish', 
                               'Edunodcnn:'='Echinoderm:', 
                               'Fi*h:'='Fish:', 
                               'Fish: Anchovy'='Anchovy', 
                               'Flounder, arrow tooth'='Flounder, arrowtooth', 
                               'Flounder, arrowtooth'='Flounder, arrowtooth', 
                               'Flounder, starry'='Flounder, starry', 
                               'Flying fish'='Flyingfish', 
                               'Flying Fish'='Flyingfish', 
                               'Flyingfihs'='Flyingfish', 
                               'Flyingfisb'='Flyingfish', 
                               'Grab, pelagic red'='Crab, Pelagic red', 
                               'Grecnling, kelp'='Greenling, kelp', 
                               'Greenling, kelp'='Greenling, kelp', 
                               'Hake Pacific'='Hake, Pacific', 
                               'HakelPacific'='Hake, Pacific', 
                               'Half moon'='Halfmoon', 
                               'Halibut California'='Halibut, California', 
                               'Halibut Pacific'='Halibut, Pacific', 
                               'Halibut, arrowtooth'='Halibut, arrowtooth', 
                               'Halibut, California*'='Halibut, California', 
                               'Hardhead-'='Hardhead', 
                               'Herrin* Pacific'='Herring, Pacific', 
                               'Herrin*, Pacific'='Herring, Pacific', 
                               'Herring Pacific'='Herring, Pacific', 
                               'Herring Pacific round'='Herring, Pacific round', 
                               'Herrins, Pacific'='Herring, Pacific', 
                               'Hock fish'='Rockfish', 
                               'Honito, Pacific'='Bonito, Pacific', 
                               'I una, Yellowfin'='Tuna, yellowfin', 
                               'Iing cod'='Lingcod', 
                               'Iingcod'='Lingcod', 
                               'Ilalfinoon'='Halfmoon', 
                               'Ilalfmnon'='Halfmoon', 
                               'Kingfisn'='Kingfish', 
                               'Kockfish'='Rockfish', 
                               'langcod'='Lingcod', 
                               'lobster, spiny'='Lobster, spiny', 
                               'Lobster, spiny'='Lobster, spiny', 
                               'Loleter, spiny'='Lobster, spiny', 
                               'Lolwtcr, spiny'='Lobster, spiny', 
                               'M iscellaneous Fish'='Miscellaneous fish', 
                               'Mackerel Horae'='Mackerel, horse', 
                               'Mackerel Jack'='Mackerel, jack', 
                               'Mackerel Pacific'='Mackerel, Pacific', 
                               'Mackerel, bullet'='Mackerel, bullet', 
                               'Mackerel, horse'='Mackerel, horse', 
                               'Mackerel, jack'='Mackerel, jack', 
                               'Mackerel, jock'='Mackerel, jack', 
                               'Mackerel, Spanidh'='Mackerel, Spanish', 
                               'Mackerel, Spanish'='Mackerel, Spanish', 
                               'Miscellaneous (animalhood)'='Miscellaneous (animal food)', 
                               'Miscellaneous Fish'='Miscellaneous fish',
                               'Miscellaneous fish _'='Miscellaneous fish',
                               'Miscellaneous Pish'='Miscellaneous fish', 
                               'Ml hitefish, ocean'='Whitefish, ocean', 
                               'MoJIusk:'='Molliisk:', 
                               'Molliisk:'='Molliisk:', 
                               "Mollus'c"='Molliisk:', 
                               'Mollusk'='Molliisk:', 
                               'Mollusk:'='Molliisk:', 
                               'Mudsuckcr longjaw'='Mudsucker, longjaw', 
                               'Mudsucker longfaw'='Mudsucker, longjaw', 
                               'Mudsucker longjaw'='Mudsucker, longjaw', 
                               'Mudsucker longjaw———'='Mudsucker, longjaw', 
                               'Mudsucker, longjaw'='Mudsucker, longjaw', 
                               'Mullet1*'='Mullet', 
                               'Mulletb'='Mullet', 
                               'MulletT'='Mullet', 
                               'Mussle'='Mussel', 
                               'Opaleyc'='Opaleye', 
                               'Opaleyr'='Opaleye', 
                               'Oyiter giant Pacific'='Oyster, giant Pacific', 
                               'Oyster, eastern'='Oyster, eastern', 
                               'Oyster, giant Pacific'='Oyster, giant Pacific', 
                               'Oyster, giant Pacifiic'='Oyster, giant Pacific', 
                               'Oysters, eastern'='Oyster, eastern', 
                               'Oysters, Pacific'='Oyster, Pacific', 
                               'Pike Sacramento'='Pike, Sacramento', 
                               'Pom|»no Pacific'='Pompano, Pacific', 
                               'Pompano California'='Pompano, California', 
                               'Prawn, ndgeback'='Prawn, ridgeback', 
                               'Prawn, ridgcback'='Prawn, ridgeback', 
                               'Prawn, ridgeback'='Prawn, ridgeback', 
                               'Prawn, sj>ot'='Prawn, spot', 
                               'Queen fish'='Queenfish', 
                               'Rcckfish'='Rockfish', 
                               'Rock Bass'='Rock bass', 
                               'Rock bass—'='Rock bass', 
                               'Rock fish'='Rockfish', 
                               'Rock Hass'='Rock bass', 
                               'Rock\"fish'='Rockfish', 
                               'Rockbass'='Rock bass', 
                               'Sabiefish'='Sablefish', 
                               'Sablcfish'='Sablefish', 
                               'Sable fish'='Sablefish', 
                               'Sahlefish'='Sablefish', 
                               'Sand dab'='Sanddab', 
                               'Sand Dab'='Sanddab', 
                               'Sanddah'='Sanddab', 
                               'Sareo'='Sargo', 
                               'SaWefish'='Sablefish', 
                               'Scabass, white'='Seabass, white', 
                               'Scat rout, greenling'='Seatrout, greenling', 
                               'Scatrout greenling'='Seatrout, greenling', 
                               'Sculpin, staghorn'='Sculpin, staghorn', 
                               'Sculpin, staghorn—'='Sculpin, staghorn', 
                               'Sea bass black'='Sea bass, black', 
                               'Sea bass, (riant'='Sea bass, giant', 
                               'Sea bass, giant'='Sea bass, giant', 
                               'Sea bass, white'='Seabass, white', 
                               'Sea bass, White'='Seabass, white', 
                               'Sea loss, giant'='Sea bass, giant', 
                               'Sea Snail'='Sea snail', 
                               'Sea star'='Sea stars', 
                               'Sea Urchin'='Sea urchin', 
                               'Sea-bass, Black'='Sea bass, black', 
                               'Sea-bass, Short-fin'='Sea bass, shortfin', 
                               'Sea-bass, White'='Seabass, white', 
                               'Sea-trout, California'='Seatrout, California', 
                               'Seabase, White'='Seabass, white', 
                               'Seabass, black'='Sea bass, black', 
                               'Seabass, Black'='Sea bass, black', 
                               'Seabass, Shortfin'='Sea bass, shortfin', 
                               'Seabass, white'='Seabass, white', 
                               'Seaboss, White'='Seabass, white', 
                               'Seabss, white'='Seabass, white', 
                               'Seahaas, white'='Seabass, white', 
                               'Sealiass, white'='Seabass, white', 
                               'Seatrout greenling'='Seatrout, greenling', 
                               'Seatrout, grcenling'='Seatrout, greenling', 
                               'Seatrout, greenling'='Seatrout, greenling', 
                               'Seulpin staghorn'='Sculpin, staghorn', 
                               'Sguid market'='Squid, market', 
                               'Shad—'='Shad', 
                               'Shccphcad California'='Sheephead, California', 
                               'Shccphcad, California'='Sheephead, California', 
                               'Shccphead, California'='Sheephead, California', 
                               'Shccpshcad'='Sheephead', 
                               'Shccpshead'='Sheephead', 
                               'Shcepshead'='Sheephead', 
                               'Shecphead, California'='Sheephead, California', 
                               'Sheed-head'='Sheephead', 
                               'Sheep head, California'='Sheephead, California', 
                               'Sheep-head'='Sheephead', 
                               'Sheepahead'='Sheephead', 
                               'Sheephead'='Sheephead', 
                               'Sheephead California'='Sheephead, California', 
                               'Sheepsdhead'='Sheephead', 
                               'Shnmp Pacific Ocean'='Shrimp, Pacific ocean', 
                               'Shrimp Pacific Ocean'='Shrimp, Pacific ocean', 
                               'Shrimp, lay'='Shrimp, bay', 
                               'Shrimp, ocean'='Shrimp, ocean', 
                               'Sierra —'='Sierra', 
                               'Siuid'='Squid', 
                               'Smeit'='Smelt', 
                               'Smelt Whitehall'='Smelt, whitebait', 
                               'Snail sea'='Snail, sea', 
                               'Soabass, white'='Seabass, white', 
                               'Sole Dover'='Sole, Dover', 
                               'Sole English'='Sole, English', 
                               'Sole Knglish'='Sole, English', 
                               'Sole miscellaneous'='Sole, miscellaneous', 
                               'Sole rex'='Sole, rex', 
                               'Sole, and'='Sole, sand', 
                               'Sole, Knglish'='Sole, English', 
                               'Sole, l)over'='Sole, Dover', 
                               'Sole, miscellaneous'='Sole, miscellaneous', 
                               'Sole, petrale'='Sole, petrale', 
                               'Sole, petrales'='Sole, petrale', 
                               'SpJitUil'='Splittail', 
                               'Split tail'='Splittail', 
                               'Split-tail'='Splittail', 
                               'Swordfish Broadbill'='Swordfish, broadbill', 
                               'Swordfish, Broad bill'='Swordfish, broadbill', 
                               'Swordfish, broad!ill'='Swordfish, broadbill', 
                               'Swordfish, Broadbil!'='Swordfish, broadbill', 
                               'Swordfish, broadbill'='Swordfish, broadbill', 
                               'Swordfish, Broodbill'='Swordfish, broadbill', 
                               'Swordfish, Marlin'='Swordfish, marlin', 
                               'Tana, skipjack, black'='Tuna, black skipjack', 
                               'Totacl check'='Total check', 
                               'Total pounds'='Totals', 
                               'Tuna Albacore'='Tuna, albacore', 
                               'Tuna, Alba core'='Tuna, albacore', 
                               'Tuna, albacorc'='Tuna, albacore', 
                               'Tuna, Albacorc'='Tuna, albacore', 
                               'Tuna, albacore'='Tuna, albacore', 
                               'Tuna, alhacorc'='Tuna, albacore', 
                               'Tuna, bigeyc'='Tuna, bigeye', 
                               'Tuna, bigeye'='Tuna, bigeye', 
                               'Tuna, biwyc'='Tuna, bigeye', 
                               'Tuna, black skipjack'='Tuna, black skipjack', 
                               'Tuna, blackfin'='Tuna, blackfin', 
                               'Tuna, blucfin'='Tuna, bluefin', 
                               'Tuna, Blucfin'='Tuna, blackfin', 
                               'Tuna, blucfm'='Tuna, bluefin', 
                               'Tuna, blue fin'='Tuna, bluefin', 
                               'Tuna, bluefin'='Tuna, bluefin', 
                               'Tuna, Bon ito'='Tuna, bonito', 
                               'Tuna, bonito'='Tuna, bonito', 
                               'Tuna, hluefin'='Tuna, bluefin', 
                               'Tuna, Iongtail'='Tuna, longtail', 
                               'Tuna, oriental'='Tuna, oriental', 
                               'Tuna, Rluefin'='Tuna, bluefin', 
                               'Tuna, skipjack'='Tuna, skipjack', 
                               'Tuna, skipjack, black'='Tuna, black skipjack', 
                               'Tuna, vellowfin'='Tuna, yellowfin', 
                               'Tuna, ycliowfin'='Tuna, yellowfin', 
                               'Tuna, ycllowfin'='Tuna, yellowfin', 
                               'Tuna, Ycllowfin'='Tuna, yellowfin', 
                               'Tuna, yellofin'='Tuna, yellowfin', 
                               'Tuna, yellow fin'='Tuna, yellowfin', 
                               'Tuna, yellowfin'='Tuna, yellowfin', 
                               'Tuna, yellowfn'='Tuna, yellowfin', 
                               'Tuna, Yeuowfin'='Tuna, yellowfin', 
                               'Tuna,Skipjack'='Tuna, skipjack', 
                               'W hitebait'='Whitebait', 
                               'W hitefish Ocean'='Whitefish, ocean', 
                               'W hitobait'='Whitebait', 
                               'Waboo'='Wahoo', 
                               'Wahoo-'='Wahoo', 
                               'Whitcfish, ocean'='Whitefish, ocean', 
                               'White fish Ocean'='Whitefish, ocean', 
                               'White fish, ocean'='Whitefish, ocean', 
                               'White fish, Ocean'='Whitefish, ocean', 
                               'Whitefish ocean'='Whitefish, ocean', 
                               'Whitefish Orran'='Whitefish, ocean', 
                               'Whitefish, ocean'='Whitefish, ocean', 
                               'Whitofish, ocean'='Whitefish, ocean', 
                               'Ycllowtail'='Yellowtail', 
                               'Ycllowtail, California'='Yellowtail, California', 
                               'Yellow tail'='Yellowtail', 
                               'Yellow tail, California'='Yellowtail, California', 
                               'Yellowtaii'='Yellowtail', 
                               'Yollowtail'='Yellowtail',
                               'Anchovy, Northern'='Anchovy, northern', 
                               'Bass, Rock'='Bass, rock', 
                               'Blackfish, Sacramento'='Blackfish, sacramento', 
                               'Clam, Gaper'='Clam, gaper', 
                               'Clam, Japanese'='Clam, Japanese', 
                               'Clam, Pisino'='Clam, pismo', 
                               'Clam, Pismo'='Clam, pismo', 
                               'Corvina, Shortfin'='Corvina, shortfin', 
                               'Crab, King'='Crab, king', 
                               'Crab, Pelagic red'='Crab, pelagic red', 
                               'Crab, Rock'='Crab, rock', 
                               'Croaker, White'='Croaker, white', 
                               'Crustacean:'='Unspecified crustacean', 
                               'Dolphinfish, Common'='Dolphinfish, common', 
                               'Echinoderm:'='Unspecified echinoderm', 
                               'Ed'='Armed box crab', 
                               'Fish:'='Fish, unspecified', 
                               'Flounder, Starry'='Flounder, starry', 
                               'Halibut, Arrowtooth'='Halibut, arrowtooth', 
                               'Halibut, Northern'='Halibut, northern', 
                               'Lobster, Spiny'='Lobster, spiny', 
                               'Mackerel, Horse'='Mackerel, horse', 
                               'Mackerel, Jack'='Mackerel, jack', 
                               'Make'='Market crab', 
                               'Midshipman'='Plainfin midshipman', 
                               'Miscellaneous'='Miscellaneous sole', 
                               'Oyster, Eastern'='Oyster, eastern', 
                               'Oyster, Giant Pacific'='Oyster, giant Pacific', 
                               'Oyster, Japanese'='Oyster, Japanese', 
                               'Oyster, Native'='Oyster, native', 
                               'Pike'='Unspecified perch-like', 
                               'Saury'='Pacific saury', 
                               'Scallop'='Rock scallop', 
                               'Seabass, White'='Seabass, white',
                               'Shail'='Sailfish', 
                               'Sheephead'='California sheephead', 
                               'Sierra'='Pacific sierra', 
                               'Snail'='Sailfish', 
                               'Sole, Dover'='Sole, Dover', 
                               'Sole, English'='Sole, English', 
                               'Sole, Miscellaneous'='Sole, miscellaneous', 
                               'Swordfish, Broadbill'='Swordfish, broadbill', 
                               'Tuna, Albacore'='Tuna, albacore', 
                               'Tuna, Bluefin'='Tuna, bluefin', 
                               'Tuna, bluefin (Atlantic)'='Tuna, Atlantic bluefin', 
                               'Tuna, Bonito'='Tuna, bonito', 
                               'Tuna, Oriental'='Tuna, oriental', 
                               'Tuna, Skipjack'='Tuna, skipjack', 
                               'Tuna, Yellowfin'='Tuna, yellowfin', 
                               'Whitefish, Ocean'='Whitefish, ocean',
                               'Sole, dover'='Sole, Dover', 
                               'Sole, english'='Sole, English',
                               'Corvina, Shortfin'='Corvina, shortfin',
                               'Crab, King'='Crab, king', 
                               'Crab, Pelagic red'='Crab, pelagic red',
                               'Croaker, White'='Croaker, white',
                               'Croaker, white (kingfish)'='Croaker, white',
                               'Dolphinfish, Common'='Dolphinfish, common',
                               'Seabass, White'='Seabass, white',
                               'Seabass, shortfin'='Sea bass, shortfin',
                               'Oyster, Giant Pacific'='Oyster, giant Pacific', 
                               'Oyster, japanese'='Oyster, Japanese', 
                               'Oyster, Native'='Oyster, native',
                               'Halibut, Arrowtooth'='Halibut, arrowtooth',
                               'Swordfish, marlin'="Marlin, striped",
                               'Tuna, Mebachi'='Tuna, mebachi',
                               'Sea-bass, Totuava'='Seabass, totuava',
                               'Seabass, Totuava'='Seabass, totuava',
                               "Total pounds"="Totals")) %>% 
  # Remove categories
  filter(!grepl(":", comm_name_orig) & !is.na(comm_name_orig))  %>% 
  # Harmonize names
  mutate(comm_name_reg=wcfish::convert_names(comm_name_orig, to="regular"),
         comm_name=wcfish::harmonize_names(comm_name_reg, "comm", "comm"),
         sci_name=wcfish::harmonize_names(comm_name, "comm", "sci")) %>% 
  # Arrange
  select(source, table, source_table, year, waters, comm_name_orig, comm_name_reg, comm_name, sci_name, landings_lb)


# Inspect common names
wcfish::check_names(data_full$comm_name_orig) 
wcfish::check_names(data_full$comm_name_reg) 

# Inspect data
str(data_full)
freeR::complete(data_full)
range(data_full$year)
table(data_full$source)
table(data_full$table)
sort(unique(data_full$waters))
sort(unique(data_full$source_table))


# QA/QC species totals
################################################################################

# Extract reported totals
tots_rep <- data_full %>% 
  filter(waters=="Grand Total") %>% 
  select(-waters) %>% 
  rename(landings_rep=landings_lb)

# Calculate observed totals
tots_obs <- data_full %>% 
  filter(waters!="Grand Total") %>% 
  group_by(source, table, comm_name_orig) %>% 
  summarize(landings_obs=sum(landings_lb))

# Merge
# MUST BE ZERO ROWS
tots_check <- tots_rep %>% 
  left_join(tots_obs) %>% 
  mutate(landings_diff=landings_rep-landings_obs) %>% 
  filter(landings_diff!=0)


# QA/QC waters totals
################################################################################

# Extract reported totals
tots_rep2 <- data_full %>% 
  filter(comm_name_orig=="Totals") %>% 
  select(-comm_name_orig) %>% 
  rename(landings_rep=landings_lb)

# Calculate observed totals
tots_obs2 <- data_full %>% 
  filter(comm_name_orig!="Totals") %>% 
  group_by(source, table, waters) %>% 
  summarize(landings_obs=sum(landings_lb))

# Merge
# MUST BE ZERO ROWS
tots_check2 <- tots_rep2 %>% 
  left_join(tots_obs2) %>% 
  mutate(landings_diff=landings_rep-landings_obs) %>% 
  filter(landings_diff!=0) %>% 
  arrange(source)


# Final format
################################################################################

# Format data
data <- data_full %>% 
  # Remove totals
  filter(waters!="Grand Total" & comm_name_orig!="Totals") %>% 
  # Arrange waters
  mutate(waters=recode(waters, 
                       "North"="North-of-State",
                       "South"="South-of-State"),
         waters=factor(waters, levels=c("California", "North-of-State", "South-of-State", "Continental",
                                        "Central Pacific", "South Pacific", "Japan", "Africa", "Shipments"))) %>% 
  # Add kilograms
  mutate(landings_kg=measurements::conv_unit(landings_lb, "lbs", "kg")) %>% 
  # Arrange
  select(-c(source_table, comm_name_reg)) %>% 
  select(source, table, year, waters, comm_name_orig, comm_name, sci_name, landings_lb, everything()) %>% 
  arrange(year, waters, comm_name_orig)

# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$source)
table(data$table)
sort(unique(data$waters))


# Plot data
################################################################################

# Annual sums
stats <- data %>% 
  group_by(year, waters) %>% 
  summarize(landings_lb=sum(landings_lb))

# Plot annual sums
g <- ggplot(stats, aes(x=year, y=landings_lb/1e6, fill=waters)) +
  geom_bar(stat="identity",position=position_stack(reverse = TRUE)) +
  # Labels
  labs(x="", y=" Landings (millions of lbs)") +
  # Theme
  theme_bw()
g


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1936_1976_landings_by_waters.Rds"))








