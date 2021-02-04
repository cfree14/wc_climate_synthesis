##Cleans and wrangles commercial fishing vessels data from California historic Fish Bulletins

##################################################################################

## Clear workspace
rm(list = ls())

################################################################################
# Setup
## Packages
library(tidyverse)

# Directories
outdir <- "data/landings/cdfw/public/fish_bulletins/processed"
################################################################################
# Read and format data for years 1936-1938 (by home port, no lenght)

nvessels_36_38 <- readxl::read_excel("data/landings/cdfw/public/fish_bulletins/raw/fb57/raw/Table5.xlsx") %>% 
  pivot_longer(cols = 2:ncol(.),
               names_to = "year",
               values_to = "nvessels") %>% 
  rename(region = "Region of home port") %>%
  filter(!(region == "Totals" | region == "Total check"),
         year != "1939-1940") %>% 
  mutate(source = "FB 57",
         region_type = "port complex",
         table_name = "Table5",
         region = recode(region,
                         "Alaska Washington and Oregon" = "OR, WA, AK",
                         "Mexico and Panama" = "Mexico",
                         "Del Norte-Eureka" = "Del Norte Eureka"),
         year = recode(year,
                       "1936-1937" = "1936-37",
                       "1937-1938" = "1937-38",
                       "1938-1939" = "1938-39")) %>% 
  select(source, table_name, year, nvessels, region_type, region)

##Checked totals and sum matches to Total. All good!

################################################################################
# Read and merge all tables by length

## Set 1: Table number varies. Data reported by port
fbs_1 <- c(44, 49, 57, 58, 59, 59, 63, 63, 67, 67, 74, 80, 80, 86, 89, 95, 102, 102, 105, 105)

table_name <- c("Table30", "Table141", "Table6", "Table12", "Table18", "Table19", "Table25", "Table26", "Table25", "Table26", "Table31", "Table7", "Table8", "Table7", "Table13", "Table9", "Table9", "Table10", "Table12", "Table13")

year <- c("1934", "1935", "1939-40", "1940- 41", "1941-42", "1942-43", "1943-44", "1944-45", "1945-46", "1946-47", "1947-48", "1948-49", "1949-50", "1950-51", "1951-52", "1952-53", "1953-54", "1954-55", "1955-56", "1956-57")

##Data frame with path to read each table and corresponding year
fb_table_key <- tibble(fbs_1, table_name, year) %>% 
  mutate(source = paste("FB", fbs_1), 
         path = paste0("fb", fbs_1, "/", "raw", "/", table_name, ".xlsx"))


## Merging
data_orig_older <- purrr::map_df(fb_table_key$path, function(x){ 

  # Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", x)
  fdata <- readxl::read_excel(file.path(indir))
  
  fdata_1 <- fdata %>% 
    # pivot_longer
    pivot_longer(2:ncol(.),
                 names_to = "length_class_orig",
                 values_to = "nvessels") %>%
    # Rename
    setNames(c("region", "length_class_orig", "nvessels")) %>% 
    # Add and arrange source
    mutate(region_type = "port complex",
           path = x) %>% 
    # Convert to character
    mutate_all(as.character) %>% 
    left_join(fb_table_key, by = "path") %>% 
    select(source, table_name, year, length_class_orig, nvessels, region_type, region)

})


## Set 2: All tables are Table 5. Data reported statewise
fbs_2 <- c(108, 111, 117, 121, 125, 132, 135, 138, 144, 149, 153, 154, 159, 161, 163, 166, 168, 170)

data_orig_tb5 <- purrr::map_df(fbs_2, function(x){
  
  # Read data
  indir <- file.path("data/landings/cdfw/public/fish_bulletins/raw", paste0("fb", x), "raw")
  infile <- list.files(indir, pattern="Table5")
  fdata <- readxl::read_excel(file.path(indir, infile))
  
  # Format
  ncols <- ncol(fdata)
  if(ncols==3){
    fdata1 <- fdata %>% 
      # pivot_longer
      pivot_longer(2:ncol(.),
                   names_to = "length_class_orig",
                   values_to = "nvessels") %>%
      # Rename
      setNames(c("length_class_orig", "year", "nvessels")) %>% 
      # Add and arrange source
      mutate(source=paste("FB", x),
             table_name = "Table5",
             region_type = "state",
             region = "statewide",
             length_class_orig = str_remove(length_class_orig, "//.")) %>% 
      select(source, table_name, everything()) %>% 
      # Convert to character
      mutate_all(as.character)
  }
  
  if(ncols>3){
    fdata1 <- fdata %>% 
      # pivot_longer
      pivot_longer(2:ncol(.),
                   names_to = "length_class_orig",
                   values_to = "nvessels") %>%
      # Rename
      setNames(c("year", "length_class_orig", "nvessels")) %>% 
      # Convert to character
      mutate_all(as.character) %>% 
      # Add and arrange source
      mutate(source=paste("FB", x),
             table_name = "Table5",
             region_type = "state",
             region = "statewide") %>% 
      select(source, table_name, everything())
     
  }
  
  # Return
  fdata1
  
})

## FB129. Table6
nvessels_fb129 <- readxl::read_excel("data/landings/cdfw/public/fish_bulletins/raw/fb129/raw/Table6.xlsx") %>% 
  pivot_longer(2:ncol(.),
               names_to = "length_class_orig",
               values_to = "nvessels") %>%
  # Rename
  setNames(c("year", "length_class_orig", "nvessels")) %>% 
  # Add and arrange source
  mutate(source= "FB 129",
         table_name = "Table6",
         region_type = "state",
         region = "statewide") %>% 
  select(source, table_name, everything()) %>% 
  # Convert to character
  mutate_all(as.character)

## All fb with table5 format
data_orig_tb5 <- rbind(data_orig_tb5, nvessels_fb129)

#################################################################################
## DF with totals 
# Set 1: Totals for each season/yr
totals_older <- data_orig_older %>% 
  filter(grepl("number", tolower(length_class_orig)),
         grepl("number", tolower(region))) %>% 
  mutate(nvessels_total = as.numeric(nvessels)) %>% 
  select(source, year, nvessels_total)


# Set 2 + fb129: Total by season/yr
totals_tb5 <- data_orig_tb5 %>% 
  filter(length_class_orig == "Total") %>% 
  mutate(year = gsub("\\.|\\_|\\,", "", year),
         nvessels_total = as.numeric(nvessels)) %>% 
  select(source, year, nvessels_total)

################################################################################

## Format data
# Set 1
data_older <- data_orig_older %>% 
  filter(!grepl("total", tolower(length_class_orig)),
         !grepl("total", tolower(region)),
         !grepl("number", tolower(length_class_orig)),
         !grepl("number", tolower(region))) %>% 
  # Format nvessels
  mutate(nvessels=as.numeric(nvessels)) %>% 
  # Format length group
  mutate(length_class_orig=gsub("\\.|\\_|\\,", "", length_class_orig),
         length_class_orig=gsub("- | -", "-", length_class_orig),
         length_class_orig=stringr::str_trim(length_class_orig),
         region = gsub("\\.|\\_|\\-", "", region),
         length_class_orig = recode(length_class_orig, 
                                "Up to 24 feet"="0-24",
                                "Up to 24'" = "0-24",
                                "25 to 39 feet"="25-39",
                                "25 feet to 39 feet" = "25-39",
                                "25' to 39'" = "25-39",
                                "40 to 64 feet"="40-64",
                                "40 feet to 64 feet" = "40-64",
                                "40' to 64'" = "40-64",
                                "65 to 84 feet"="65-84",
                                "65 feet to 84 feet" = "65-84",
                                "65' to 84'" = "65-84",
                                "65 to 84" = "65-84",
                                "85 to 99 feet"="85-99",
                                "85 to 99" = "85-99",
                                "85 feet and over" = "85+",
                                "85' and over" = "85+",
                                "100 feet and over"="100+",
                                "Up to 21 feet" = "0-24", ##fixing numbers that were incorrect in the xlsx file
                                "10 to 61 feet" = "40-64"))


# Set2
data_tb5 <- data_orig_tb5 %>% 
  # Remove totals and total checks
  filter(!grepl("total", tolower(length_class_orig))) %>% 
  # Format nvessels
  mutate(nvessels=as.numeric(nvessels)) %>% 
  # Format length group
  mutate(length_class_orig = gsub("\\.|\\_|\\,", "", length_class_orig),
         length_class_orig = gsub("- | -", "-", length_class_orig),
         length_class_orig = stringr::str_trim(length_class_orig),
         year = gsub("\\.|\\_|\\,", "", year),
         length_class_orig = recode(length_class_orig, 
                                "Up to 24 fcet"="0-24",
                                "Up to 24 feet"="0-24",
                                "25 to 39 feet"="25-39",
                                "40 to 64 feet"="40-64",
                                "65 to 84 feet"="65-84",
                                "85 to 99 feet"="85-99",
                                "100 feet and over"="100+",
                                "181 and over"="181+",
                                "40 to 61 feet"="40-64", ##fixing numbers that were incorrect in the xlsx file
                                "25 to 29 feet" = "25-39",
                                "15-11" = "11-15",
                                "150-160" = "156-160",
                                "20-25" = "21-25"))

# Inspect
unique(data_tb5$length_class_orig)
unique(data_older$length_class_orig)

#################################################################################
# Check Totals
total_check_older <- data_older %>% 
  group_by(source, year) %>% 
  summarise(total_sum = sum(nvessels, na.rm = T)) %>% 
  left_join(totals_older, by = c("source", "year")) %>% 
  mutate(check_diff = total_sum - nvessels_total) ## All 0! Totals match

total_check_tb5 <- data_tb5 %>% 
  group_by(source, year) %>% 
  summarise(total_sum = sum(nvessels, na.rm = T)) %>% 
  left_join(totals_tb5, by = c("source", "year")) %>% 
  mutate(check_diff = total_sum - nvessels_total) ## All 0! Totals match

##################################################################################
## Combining all data and removing duplicate years

nvessels_complete <- rbind(data_older, data_tb5) %>% 
  mutate(region = recode(region,
                         "Oregon Washington and Alaska boats fishing in California" = "OR, WA, AK",
                         "Alaska Washington and Oregon" = "OR, WA, AK",
                         "Alaska Oregon and Washington" = "OR, WA, AK",
                         "Alaska Oregon and Washington" = "OR, WA, AK",
                         "Kureka" = "Eureka",
                         "Del Norte Eurecka" = "Del Norte Eureka",
                         "i/os Angeles" = "Los Angeles",
                         "Saciamento" = "Sacramento"),
         length_class_group = recode(length_class_orig,
                                           "0-10" = "0-24" ,
                                           "11-15" = "0-24",
                                           "16-20" = "0-24",
                                           "21-25" = "0-24",
                                           "26-30" = "25-39",
                                           "31-35" = "25-39",
                                           "36-40" = "25-39",
                                           "41-45" ="40-64",
                                           "46-50" = "40-64",
                                           "51-55" = "40-64",
                                           "56-60" ="40-64",
                                           "61-65" = "40-64",
                                           "66-70" = "65-84",
                                           "71-75" ="65-84",
                                           "76-80" ="65-84",
                                           "81-85" = "65-84",
                                           "85-99" = "85+",
                                           "86-90" = "85+",
                                           "91-95" = "85+",
                                           "100+" = "85+",
                                           "96-100" = "85+",
                                           "101-105" = "85+",
                                           "106-110" = "85+",
                                           "111-115" = "85+",
                                           "116-120" = "85+",
                                           "121-125" = "85+",
                                           "126-130" = "85+",
                                           "131-135" = "85+",
                                           "136-140" ="85+",
                                           "141-145" = "85+", 
                                           "146-150" = "85+",
                                           "151-155" = "85+", 
                                           "156-160" = "85+",
                                           "161-165" = "85+",
                                           "166-170" = "85+",
                                           "171-175" = "85+",
                                           "176-180" = "85+",
                                           "181+" = "85+"),
         year = recode(year,
                       "1957-1958"= "1957-58",
                       "1958-1959" = "1958-59",
                       "1959-1960" = "1959-60",
                       "1960-1961" = "1960-61",
                       "1961-1962" = "1961-62",
                       "1962-1963" = "1962-63",
                       "1963-1964" = "1963-64",
                       "1964-1965" = "1964-65",
                       "1965-1966" = "1965-66")) %>% 
  distinct(year, length_class_orig, nvessels, region_type, region, .keep_all = T)

##Data up untill 1947-48 only reports up to the lenght group 85+

################################################################################
## Testing group with year 1970-71

nvessels_grouped_test <- nvessels_complete %>%
  filter(source == "FB 159",
         year == "1970-71") %>% 
  group_by(length_class_group) %>% 
  summarise(nvessels_group_fb159 = sum(nvessels))


test_compare <- nvessels_complete %>% 
  filter(source == "FB 154",
         year == "1970-71") %>% 
  left_join(nvessels_grouped) %>% 
  select(-region, -region_type)

################################################################################
## Grouped df to plot

nvessels_grouped <- nvessels_complete %>% 
  filter(!(year == "1970-71" & source == "FB 159")) %>% 
  group_by(year, length_class_group) %>% 
  summarise(nvessels_group_yr = sum(nvessels, na.rm = T)) %>% 
  mutate(length_class_group = as_factor(length_class_group))

#mutate(iso3_code = fct_relevel(iso3_code, rev(c("CHN", "RUS"


################################################################################
# Plot
pal_antique <- c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C")

nvessels_ts <- ggplot(nvessels_grouped %>% 
                        mutate(length_class_group = 
                                 fct_relevel(length_class_group, c("85+", "65-84", "40-64", "25-39", "0-24"))))+
  geom_bar(aes(x = year, y = nvessels_group_yr, fill = length_class_group), 
           stat = "identity")+
  theme_classic()+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = pal_antique)+
  labs(fill = element_text("Vessel Length Group (ft)"),
       title = "Commercial Fishing Vessels registered in CA",
       x= "Year",
       y = "Nº of Vessels")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1))
  
##############################################################################

##Saving data
nvessels_save <- nvessels_complete %>% 
  filter(!(year == "1970-71" & source == "FB 154")) ##removing the less detailed info for year 1970

#saveRDS(nvessels_save, file = file.path(outdir, "CDFW_1934_1977_nvessels_by_length.Rds"))

#saveRDS(nvessels_36_38, file = file.path(outdir, "CDFW_1936_1938_nvessels_by_port.Rds"))

###############################################################################
##Data questions
## What happens with data on years 1936-1938? A: There is data by port not by lenght in FB57, Table 5
## Is there any data for years before 1936? A: No, there is one graph with data from year 1928 but it contains averages and probably predicting numbers from the graph is not as accurate.
## What happens with data after 1976

## Plot notes
## group 85-89 and 100+ in 85+ group? A:YES!
## format years for them to be just the one year and add note that season goes from April 1st to March 31 of the following year?

##Saving data
## save complete table, where and what format? A: Save in outdir (see line 14) as a Rsd file
## in complete table, what to do with year 1970, leave both fomrats? A: just leave the finner scale

###################################################################################


