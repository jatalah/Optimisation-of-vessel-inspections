library(tidyverse)
library(clipr)
# get taxa fro NIWa datasets--------------

dat1 <- 
bind_rows(
  Passenger = read_csv('cleaned_data/Golder07_passenger_data_raw.csv'),
  NZDS07 = read_csv('cleaned_data/nzds07_clean_crms.csv'),
  # Caw17 = read_csv('cleaned_data/cawthron_2017_vessel_biofouling_data_clean.csv', col_types = cols(vessel_code = col_character())),
  # Caw21 =  read_csv('cleaned_data/caw21_raw_clean.csv'),
  .id = "dataset"
) %>% 
  filter(vessel_code %in% distinct(read_csv('tables/misclasified_vessels.csv'), vessel_code)$vessel_code) %>% 
  mutate(Taxa = if_else(is.na(all_taxa), Taxa, all_taxa)) %>% 
  select(vessel_code, Taxa, TRich:WM) 

dat1 %>% 
  select(vessel_code, Taxa) %>% 
  bind_cols(dat1 %>% select(-c(1:2)) %>% select_if(colSums(.) != 0)) %>% 
  pivot_longer(cols = AG:HY) %>% 
  group_by(vessel_code) %>% 
  filter(value>0) %>% 
  summarise(Taxa1 = first(paste0(name, collapse = ","))) %>% 
  left_join(dat1 %>% select(vessel_code, Taxa, TRich)) %>% 
  write_clip()





# get taxa fro Cawthron datasets -------
mis <- read_csv('tables/misclasified_vessels.csv')

bind_rows(
  Caw17 = read_csv(
    'cleaned_data/cawthron_2017_vessel_biofouling_data_clean.csv',
    col_types = cols(vessel_code = col_character())
  ) %>% mutate(vessel_code = str_extract(id, "(\\d)+")) ,
  Caw21 =  read_csv('cleaned_data/caw21_raw_clean.csv'),
  .id = "dataset"
) %>% 
select(
 vessel_code,
  `Visible taxa (select from list. e.g. Al, Sp, Se, Is etc.)`:`Continuous strips > 50 mm in width?`   ,
  -c(`Video notes:`:`Typical fouling rep image #?`)
) %>%
filter(vessel_code %in% distinct(read_csv('tables/misclasified_vessels.csv'), vessel_code)$vessel_code) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% 
  pivot_longer(cols = `Algae % coverage`:`Total number of species?`) %>% 
  group_by(vessel_code) %>% 
  drop_na(value) %>% 
  filter(value>0) %>% 
  summarise(Taxa1 = first(paste0(name, collapse = ","))) %>% 
  left_join(dat1 %>% select(vessel_code, Taxa, TRich)) 


# caw21---


filter(read_csv('cleaned_data/caw21_raw.csv') %>% rename(vessel_code = "vessel"), vessel_code %in% mis$vessel_code) %>% 
  select(vessel_code, gooseneck:algae_patch_50mm_width, - mobiles_write_in, -area_32 ) %>% 
  pivot_longer(-vessel_code) %>% 
  drop_na() %>% 
  filter(value>0) %>% 
  group_by(vessel_code) %>% 
  summarise(Taxa1 = first(paste0(unique(name), collapse = ",")))
