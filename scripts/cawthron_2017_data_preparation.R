library(tidyverse)
library(readxl)
library(purrr)
library(clipr)
library(janitor)

setwd(
  'C:/Users/javiera/Cawthron/17745 - Optimisation of Vessel Inspections for Biofouling - General/data_analyses/data/Cawthron 2017'
)
# setwd("C:/Users/javiera/Cawthron/17745 - Optimisation of Vessel Inspections for Biofouling - General/data_analyses/data/Cawthron 2017")
file.list <- list.files(pattern = '*.xlsx')
file.list <- setNames(file.list, file.list)

caw17_raw <-
  map_df(file.list,
         ~ read_excel(
           path = .x,
           skip = 1,
           na = c('x', '-', 'xx', 'na', '?'),
           trim_ws = T,
           col_types = "text"
         ),
         .id = "id") %>%
  fill(`New grouping`, `LoF (0-5)...6`) %>%
  mutate(Type = if_else(is.na(Type), `Type  - general (grey) or niche`, Type)) %>%
  mutate(
    number = str_extract(id, "(\\d)+"),
    number = as.numeric(number),
    Side = fct_collapse(
      Side,
      Portside = c('Portside', 'Port'),
      Starboard = c('Starboard side', 'Star'),
      Middle = c('Mid', 'mid')
    ),
    Depth = if_else(is.na(Depth), "BWL", Depth),
    Depth  = fct_collapse(
      Depth ,
      WL = c('WL', 'WL?'),
      BWL = c('BWL', 'BWL?', 'BWL/WL', 'WL/BWL')
    )
  ) %>%
  mutate(across(
    c(
      `Algae % coverage`:`Total number of species?`,
      `Algae frond length (mm) ?`
    ),
    as.numeric
  )) %>%
  rename(mean_LoF = `LoF (0-5)...6`,
         LoF = `LoF (0-5)...9`,
         Area = `New grouping`) %>%
  mutate(
    Area = fct_collapse(
      Area,
      `Block mark` = c("Block mark", "Block marks"),
      `Bow thruster` = c("Bow thruster", "Bow thrusters"),
      `Discharge` = c('Discharge pipe', 'Discharge pipes'),
      `Draft mark` = c('Draft mark', 'Draft marks', 'Draft numbers'),
      `Propeller and shaft` = c('Propeller & shaft', 'Propellar & shaft'),
      Instrument = c('Instrument', 'Instruments'),
      `Sea chest gratings` = c('Sea chest', 'Sea chests'),
      Stabiliser = c('Stabiliser', 'Stabilizer', 'Stabilisers'),
      Hull = c('BWL', 'WL'),
      # Other = c(
      #   'Bumper bracing',
      #   'Damaged AF coating',
      #   'Fixing',
      #   'Pole opening',
      #   'Instruments',
      #   'Sounder'),
      `Flat bottom` = c('Flat bottom', 'Flat keel', 'Centre keel')
    ),
    `Algal overgrowth?` = fct_collapse(
      `Algal overgrowth?`,
      N = c('n', 'N'),
      Y = c('Y', 'y')
    ),
    `Clustering ?` = fct_collapse(`Clustering ?`, N = c('n', 'N'), Y = c('Y', 'y')),
    `Algae colour?` = fct_collapse(`Algae colour?`, Green = c('green', 'Green')),
    `Continuous strips > 50 mm in width?` = fct_collapse(`Continuous strips > 50 mm in width?`, N = c('n', 'N'))
  ) %>%
  select(where( ~ !all(is.na(.x)))) %>%
  mutate(
    across(`Algae % coverage`:`Sponges no. species`, ~ replace_na(., 0)),
    `Other overgrowth?` = if_else(`Algal overgrowth?` == 'O', "Y", "N"),
    `Algal overgrowth?` = fct_recode(`Algal overgrowth?`, N = 'O'),
    Region = fct_collapse(
      Region,
      Mid = c("Amidship", "Amidships (Am)", "Mid", "Bottom"),
      Stern = c("Stern", "Stern (St)"),
      Bow = c("Bow", "Bow (Bo)")
    ),
    Side = fct_recode(Side, Starboard = "Bottom"),
    Side = replace_na(Side, "Middle"),
    Region = replace_na(Region, "Amidship")
  ) %>%
  select(
    id,
    number,
    Type,
    Side,
    Region,
    Depth,
    Area,
    CRMS,
    mean_LoF,
    LoF,
    `Algae % coverage`:`Continuous strips > 50 mm in width?`
  ) %>% 
  mutate(LoF = as.numeric(LoF),
         mean_LoF = as.numeric(mean_LoF)) %>% 
  clean_names() %>% 
  rename(mean_LoF = mean_lo_f,
         LoF = lo_f) %>% 
  mutate_at(vars(area, region), as.character) %>% 
  mutate(area = if_else(area=="Hull", region, area)) 
 
tabyl(caw17_raw, area)
names(caw17_raw)
tabyl(caw17_raw, side)
tabyl(caw17_raw, region)

# read summary data ----
summary_caw17 <- 
  read_excel("C:/Users/javiera/Cawthron/17745 - Optimisation of Vessel Inspections for Biofouling - General/data_analyses/data/summary4_DM.xlsx")


caw17_crms <- 
  summary_caw17 %>% 
  select(Nr, CRMS_Sh_fail) %>% 
  rename(vessel_code = Nr,
         crms = CRMS_Sh_fail) %>% 
  mutate(crms = str_to_lower(crms)) %>% 
  mutate(vessel_code = as.character(vessel_code)) 

tabyl(caw17_crms, crms)

caw17_wide <-
  caw17_raw %>%
  filter(depth == "BWL") %>%
  pivot_wider(
    id_cols = id,
    names_from = area,
    values_from = mean_LoF,
    values_fn = function(x) {
      mean(x, na.rm = T)
    }
  ) %>%
  mutate(vessel_code = str_extract(id, "(\\d)+")) %>%
  select(-id) %>% 
  left_join(caw17_crms)

setwd(
  "C:/Users/javiera/Cawthron/17745 - Optimisation of Vessel Inspections for Biofouling - General/data_analyses"
)

caw17_long <- 
caw17_wide %>% 
  pivot_longer(cols = Bow:Fixing , names_to = 'area', values_to = 'LoF') %>% 
  write_csv('cleaned_data/caw17_long.csv')

tabyl(caw17_long, area)
missing_plot(caw17_wide)


# exploration -----
glimpse(caw17_raw)
names(caw17_raw)
tabyl(caw17_raw, side)
tabyl(caw17_raw, depth)
tabyl(caw17_raw, type)
tabyl(caw17_raw, LoF)
tabyl(caw17_raw, mean_LoF)

glimpse(caw17_raw)
distinct(caw17_raw, area)
tabyl(caw17_raw, area) %>% 
  adorn_pct_formatting(affix_sign = F) 

tabyl(caw17_raw, CRMS)
tabyl(caw17_raw, `Total number of species?`)
tabyl(caw17_raw, `Algal overgrowth?`)
tabyl(caw17_raw, `Total number of species?`)
tabyl(caw17_raw, `Clustering ?`)
tabyl(caw17_raw, `Algae colour?`)
tabyl(caw17_raw, `Algae frond length (mm) ?`)
tabyl(caw17_raw, `Continuous strips > 50 mm in width?`)


# write datasets--------
# setwd(
#   "C:/Users/javiera/Cawthron/Optimisation of Vessel Inspections for Biofouling - General/data_analyses"
# )
# 

write_csv(
  caw17_raw,
  'C:/Users/javiera/Cawthron/Optimisation of Vessel Inspections for Biofouling - General/data_analyses/cleaned_data/cawthron_2017_vessel_biofouling_data_clean.csv'
)
