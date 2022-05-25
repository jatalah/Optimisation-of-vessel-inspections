library(tidyverse)
library(janitor)
library(readxl)

# get vessel type from imo dataset---
imo_data <-
  read_csv(
    "C:/Users/javiera/Cawthron/17443 - MPI Vessel Biofouling Risk Profiling - Documents/Data analyses/cleaned_data/imo_data.csv",
    locale = locale(encoding = "ASCII")
  ) %>%
  select(imo, type) %>% 
  mutate(type = str_trim(type, side = "right"))

# get CRMS compliance assessment -----
crms_caw21 <- 
  read_excel("C:/Users/javiera/Cawthron/17443 - MPI Vessel Biofouling Risk Profiling - Documents/1. Vessel sampling communications/VesselSelection_and_Tracking.xlsx", "Risk level") %>% 
  select(Name, 'Pass/Fail') %>% 
  rename(vessel = Name,
         crms = "Pass/Fail") %>% 
  mutate(vessel = str_to_title(vessel),
         vessel = fct_recode(vessel, `Maersk Garonne` = "Maersk Garrone"))

# get vessel raw data----
caw21_raw <- 
  read_excel("C:/Users/javiera/Cawthron/17443 - MPI Vessel Biofouling Risk Profiling - Documents/2. Field data/VesselBiofoulingMaster.xlsx") %>% 
  clean_names() %>% 
  rename(LoF = lo_f,
         side = side_port_stbd,
         region = region_st_mid_bow) %>%
  mutate(region = fct_recode(region, 
                             Mid = "Midships",
                             Stern = "Sterm"))

write_csv(caw21_raw, 'cleaned_data/caw21_raw.csv')
  
# get vessel type---
caw21_type <- 
caw21_raw %>%
  distinct(vessel, imo) %>% 
  left_join(imo_data, by = 'imo') %>% 
  mutate(vessel = str_to_title(vessel)) %>% 
  mutate(type = fct_recode(type,  `Vehicles Carrier` =  "Ro-Ro/Passenger Ship")) %>% 
  select(-imo) %>% 
  rename(vessel_code = vessel,
         vessel_type = type) %>% 
  write_csv('cleaned_data/caw21_type.csv')

caw21_raw_clean <- 
  caw21_raw %>% 
  filter(area_8 != "Endoscope Seachest",
        area_32!='Waterline') %>%
  mutate(area = fct_recode(area_8, 
                         `Bilge keel`	=	"Bilge keel"	,
                         `Bow thruster`	=	"Bow thruster"	,
                         `Bulbous bow`	=	"Bulbous bow"	,
                         # Other	=	"Discharge"	,
                         # Other	=	"Anode"	,
                         Anode =	"ICCP"	,
                         DDSS	=	"Dock block"	,
                         DDSS	=	"Dock blocks"	,
                         Hull	=	"Draft marks"	,
                         Hull	=	"Hull",
                         Other	=	"Transducer",
                         `Propeller and shaft`	=	"Propeller"	,
                         `Propeller and shaft`	=	"Propeller blade"	,
                         `Propeller and shaft`	=	"Propeller boss"	,
                         `Propeller and shaft`	=	"Propeller shad"	,
                         `Propeller and shaft`	=	"Propeller shaft"	,
                         `Propeller and shaft`	=	"Rope guard"	,
                         `Rudder and shaft`	=	"Rudder"	,
                         `Rudder and shaft`	=	"Rudder B"	,
                         `Rudder and shaft`	=	"Rudder H"	,
                         `Rudder and shaft`	=	"Rudder L"	,
                         `Rudder and shaft`	=	"Rudder S"	,
                         `Rudder and shaft`	=	"Rudder T"	,
                         `Rudder and shaft`	=	"Rudder Tr"	,
                         `Sea chest gratings`	=	"Sea chest"	,
                         `Propeller and shaft`	=	"Stern arch"	,
                         `Bow thruster`	=	"Thruster"),
         vessel = str_to_title(vessel)) %>% 
  mutate(across(green_algae_50mm_length:other_organism_45, ~as.numeric(na_if(., "NA")))) %>% 
  rowwise() %>%
  mutate(crms_area = if_else(sum(c_across(green_algae_50mm_length:other_organism_45), na.rm = T)>0,0, 1
  )) %>% 
  ungroup() %>% 
  group_by(vessel, type, area, area_8, crms_area, region, side) %>% 
  summarise_at(vars(LoF), mean, na.rm =T) %>% 
  ungroup() %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  mutate(area1 = if_else(area=="Hull", region , area)) %>% 
  left_join(crms_caw21, by = 'vessel') %>% 
  mutate(crms = if_else(vessel =="Seaspan Hamburg", "fail", crms)) %>% 
  rename(vessel_code = vessel) %>% 
  write_csv('cleaned_data/caw21_raw_clean.csv')
  
caw21_crms <- 
caw21_raw1 %>% 
  select(vessel_code, crms, area, LoF) %>% 
  write_csv('cleaned_data/caw21_crms.csv')


dim(caw21)
tabyl(caw21, region)
tabyl(caw21, area)

