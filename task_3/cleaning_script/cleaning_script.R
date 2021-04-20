library(tidyverse)
library(readxl)


excel_sheets("raw_data/seabirds.xls") # 4 sheets

raw_birds_recordID <- read_excel(here("raw_data/seabirds.xls"), sheet = "Bird data by record ID")%>% 
  janitor::clean_names()
raw_birds_codes <- read_excel(here("raw_data/seabirds.xls"), sheet = "Bird data codes")%>% 
  janitor::clean_names()
raw_ships_recordID <- read_excel(here("raw_data/seabirds.xls"), sheet = "Ship data by record ID") %>% 
  janitor::clean_names()
raw_ships_codes <- read_excel(here("raw_data/seabirds.xls"), sheet = "Ship data codes")%>% 
  janitor::clean_names()


# Each recordID is unique in the ships_raw. So they are one report from an observer of all the birds they saw.

# what do I want from each table?
# BIRDS: [definitely] record(renamed to bird_sighting_id), record_id, species_common, species_scientific, species_abbreviation, count; 
      # [perhaps] the other columns depending on where analysis leads - only boolean columns?

# SHIPS: [definitely] record_id (foregin key), date, time, latitude, longitude, hemisphere
      # [perhaps] all the rest

cleaned_birds <- raw_birds_recordID %>% 
  select(record, record_id, species_common_name_taxon_age_sex_plumage_phase, species_scientific_name_taxon_age_sex_plumage_phase, species_abbreviation, count) %>% 
  rename(bird_sighting_id = record,
         species_common = species_common_name_taxon_age_sex_plumage_phase,
         species_scientific = species_scientific_name_taxon_age_sex_plumage_phase)

# should be able to replace NA's in scientific and abbreviated species names
# Also clean up common names


# drop [NO BIRDS RECORDED] observations? yes

# replace NA's with 1??? - decided 2 columns, one with NA's; one with NA = 1


cleaned_ships <- raw_ships_recordID %>% 
  select(record_id, date, time, lat, long, ew) %>% 
  rename(latitude = lat,
         longitude = long, 
         east_west_hemisphere = ew)


# NA's don't bother me

# Remove false date in the time column

# Other than that quite happy with this table


#BIRDS TABLE 1st

 cleaned_birds <- cleaned_birds %>% 
   mutate(species_common = str_remove_all(species_common, "sensu lato"),
          species_common = paste0(species_common, "_"),
          species_common = str_remove_all(species_common, " [A-Z]+[0-9]*_"),
          species_common = paste0(species_common, "_"),
          species_common = str_remove_all(species_common, " [A-Z]+[_ ]+"),
          species_common = str_remove_all(species_common, " [_]+"),
          species_common = str_remove_all(species_common, "[_]+"),
          species_common = na_if(species_common, "[NORECORDED]")) %>% 
   drop_na(species_common) %>% 
   mutate(species_scientific = str_remove_all(species_scientific, "sensu lato"),
          species_scientific = paste0(species_scientific, "_"),
          species_scientific = str_remove_all(species_scientific, " [A-Z]+[0-9]*_"),
          species_scientific = paste0(species_scientific, "_"),
          species_scientific = str_remove_all(species_scientific, " [A-Z]+[_ ]+"),
          species_scientific = str_remove_all(species_scientific, " [_]+"),
          species_scientific = str_remove_all(species_scientific, "[_.]+")) %>% 
   mutate(species_abbreviation = str_remove_all(species_abbreviation, " [A-Z,0-9]+")) %>% 
   mutate(count_coerced = replace_na(count, 1))
          


  
# SHIPS TABLE
 
 cleaned_ships <- cleaned_ships %>% 
   mutate(time = str_remove_all(time, "[0-9]+-[0-9]+-[0-9]+ "))

 # JOIN and WRITE
 
 birds_ships_clean <- left_join(cleaned_birds, cleaned_ships, by = "record_id")
 
 write_csv(birds_ships_clean, "clean_data/birds_sightings_clean_data.csv")
  

 
 
 
 
 
  