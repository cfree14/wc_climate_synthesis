
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/disasters/raw"
outdir <- "data/disasters/processed"
plotdir <- "data/disasters/figures"

# Read data
# data_orig <- readxl::read_excel(file.path(indir, "FisheryDisasters_1989-2021.xlsx"), col_types = "text")
data_orig <- read.csv(file.path(indir, "FisheryDisasters_1989-2021.csv"), as.is=T)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(disaster_id=disaster_number, 
         disaster_years=year,
         disaster_year1_orig=year_1,
         region=management_zone, 
         area_season=area_season_affected, 
         fishery_type1=state_federal, 
         fishery_type2=comm_rec_tribal,
         requesters=requester_s,
         request_dates=request_date,
         request_year_lag_yrs=request_year_disaster_year,
         determination=secretary_of_commerce_determination, 
         determination_dates=determination_date,
         determination_dates_lag_days=determination_lag_d,
         determination_year_lag_yrs=state_years,
         appropriation_amount_usd=appropriation_amount,
         appropriation_amount_usd_2019=appropriation_amount_2019_usd,
         correction_factor_2019usd=us_bls_correction_factor_jan_oct_2019,
         cause=formal_federally_stated_cause,
         cause_simple=cause_of_disaster,
         cause_catg=cause_simplified,
         cause_catg_simple=cause_more_simplified,
         damages_est_usd=total_est_damages) %>% 
  # Remove useless columns
  select(-freq) %>% 
  # Remove useless rows
  filter(state!="REPEAT OF NO. 110") %>% 
  # Format region
  mutate(region=gsub(" Region", "", region)) %>% 
  # Format state
  mutate(state=stringr::str_trim(state),
         state=recode(state, 
                      "Alaska"="AK",
                      "American Samoa"="AS",
                      "California"="CA",
                      "Florida"="FL",
                      "Florida, USVI, Puerto Rico"="FL, VI, PR",
                      "Georgia"="GA",
                      "Georgia and South Carolina"="GA, SC",
                      "Guam and Northern Mariana Islands"="GU, MP",
                      "Hawaii"="HI",
                      "Louisiana"="LA",
                      "Louisiana, Mississippi, Alabama"="LA, MS, AL",
                      "LA, MS, AL, and FL"="LA, MS, AL, FL",
                      "MA, ME, NH, CT, RI, and NY"="MA, ME, NH, CT, RI, NY",
                      "Maine"="ME",
                      "Massachusetts"="MA",
                      "Mississippi"="MS", 
                      "New Hampshire"="NH",
                      "New Jersey and New York"="NJ, NY", 
                      "New York"="NY",
                      "North Carolina"="NC", 
                      "Oregon"="OR", 
                      "Oregon and California"="OR, CA",
                      "Rhode Island"="RI",
                      "Texas"="TX",
                      "Virginia"="VA", 
                      "Washington"="WA")) %>% 
  # Format fishery type,
  mutate(fishery_type1=recode(fishery_type1, 
                              "Both"="Federal/State",
                              "Federal and State"="Federal/State", 
                              "Federal, State"="Federal/State", 
                              "Federal, Tribal"="Federal/Tribal")) %>% 
  # Add taxa group
  mutate(taxa_group=ifelse(grepl("salmon|chum|coho|chinook", tolower(fishery)), "Salmon",
                           ifelse(grepl("crab|urchin", tolower(fishery)), "Invertebrate", "Other"))) %>% 
  # Format disaster year
  mutate(disaster_year1=substr(disaster_year1_orig, 1, 4) %>% as.numeric(.)) %>% 
  # Format request date
  # mutat
  # Format request year
  mutate(request_year=as.numeric(request_year)) %>% 
  # Convert appropriation amounts
  mutate(appropriation_amount_usd=appropriation_amount_usd %>% gsub("\\(cont. from 2008-2009\\)", "", .) %>% stringr::str_trim(), 
         appropriation_amount_usd=appropriation_amount_usd %>% gsub("\\$", "", .) %>% gsub(",", "", .) %>% as.numeric(),
         appropriation_amount_usd_2019=appropriation_amount_usd_2019 %>% gsub("\\$", "", .) %>% gsub(",", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(disaster_id, 
         disaster_years, disaster_year1_orig, disaster_year1,
         region, state, area_season,
         fishery_type1, fishery_type2, 
         fishery, 
         requesters, request_year, request_dates, request_year_lag_yrs, request_letter, 
         determination, determination_year, determination_dates, determination_dates_lag_days, determination_year_lag_yrs,
         determination_authority, determination_letter, press_release,
         funding_authority, correction_factor_2019usd, appropriation_amount_usd, appropriation_amount_usd_2019, damages_est_usd,
         cause, cause_simple, cause_catg, cause_catg_simple,
         notes, extra_notes,
         everything()) %>% 
  # Arrange
  arrange(disaster_id)

# Is ID unique?
anyDuplicated(data$disaster_id)

# Inspect data
str(data)
colnames(data)
table(data$region)
table(data$state)
table(data$fishery_type1)
table(data$fishery_type2)
table(data$determination)
table(data$determination_authority)
table(data$funding_authority)
sort(unique(data$cause_simple))
sort(unique(data$cause_simple))
table(data$cause_catg) # could be majorly improved
table(data$cause_catg_simple)


# Export data
saveRDS(data, file=file.path(outdir, "1989_2021_federal_disaster_database.Rds"))

# Subset data
################################################################################

sdata <- data #%>%
  # filter(region %in% c("Alaska", "West Coast")) 
nfisheries <- n_distinct(sdata$fishery)

g <- ggplot(sdata, aes(x=disaster_year1, y=fishery, fill=cause_catg_simple)) +
  # Facet
  facet_grid(taxa_group~., space="free", scales="free_y") +
  # Plot MHW years
  geom_rect(xmin=2014.5, xmax=2018.5, ymin=1, ymax=nfisheries, fill="grey90", alpha=0.1) +
  # Plot disasters
  geom_tile() +
  # Axis
  scale_x_continuous(breaks=seq(1990,2020,5)) +
  # Labels
  labs(x="Year", y="") +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw()+
  theme(legend.position = "bottom")
g
