# load libraries and clean env.----
library(tidyverse)
library(readxl)
library(janitor)
library(finalfit)
rm(list=ls())

# read the revised data provide by MPI https://mpinz.sharefile.com/d-s48a2080126b74f2dbd941b7947334d9c on Wed 19/05/2021 14:26---------
pass_raw <-
  read_excel('data/MPI 2007/passenger/Passenger Vessel Data_revised 05192021.xlsx', 1)

codes <-
  read_excel('data/MPI 2007/passenger/MPI vessel sampling codes.xlsx') %>% 
  rename(Area_name =  "Opportunistic collections - niche areas",
         Area = "Code") %>% 
  mutate(Area = fct_recode(Area,  K = "FK, K" ))

# read taxonomic data codes----
taxa_codes <- read_csv('data/MPI 2007/passenger/taxa codes.csv')

# read vessel summary from Inglis et al. 2010-------
vessel_summary <- read_csv('cleaned_data/nzds07_vessel_summarised_data_niwa.csv')

# clean passenger data ------
pass <-
  pass_raw %>%
  mutate(
    Area = fct_recode(
      `Hull Location`, # change code to "MPI vessel sampling codes.csv'
      OV	=	"Overall vessel",
      SU	=	"BO - surface",
      SU	=	"AM - surface",
      AM	=	"Amidships below water line",
      SU	=	"ST - surface",
      BO	=	"BOs", # within 0.5 m depth of the hull
      DS	=	"BOd",
      BO	=	"BOn",
      AM	=	"AMs", # within 0.5 m depth of the hull
      DS	=	"AMd",
      AM	=	"AMn",
      DS	=	"STd",
      ST	=	"STn",
      BB	=	"Bow- BO",
      BT	=	"Bow thruster - BT",
      HA	=	"Hull below waterline - BW",
      WA	=	"Waterline - WA",
      K	=	"Flat bottom keel - K",
      DS	=	"DDSS - DS",
      BK	=	"Bilge keels - BK",
      GR	=	"Sea chest gratings - GR",
      ST	=	"Stern - ST",
      PS	=	"Propeller & Shaft - PS",
      RS	=	"Rudder & Shaft - RS",
      HA	=	"Hull Area - HA",
      PS	=	"Rope guard",
      SB	=	"Stabilisers - SB",
      BB	=	"Bulbous bow - BB",
      SU	=	"STs"
    ),
    LoF = as.numeric(ifelse(LoF == "-", NA, LoF))
  ) %>% 
  left_join(codes, by = "Area") %>% 
  filter(Area != "OV" & Area != "SU" & Area != "WA") %>%   # remove overall vessel and surface samples
  droplevels() %>% 
  clean_names() %>% 
  rename(LoF = "lo_f") %>% 
  left_join(taxa_codes, by = "species_group") %>% 
  left_join(vessel_summary, by = 'vessel_code') %>% 
  select(-c(vessel_name:source_bioregion),  -hull_location,  - paint_conditon, -area, - VesselType)

# get distinct taxa for each vessel ------------
taxa_pass <- 
  pass %>% 
  select(vessel_code, Taxa) %>% 
  group_by(vessel_code) %>% 
  distinct(Taxa)  %>% 
  drop_na(Taxa) %>%
  summarise(Taxa = first(paste0(Taxa, collapse = ",")))


# Assign CRMS compliance at the vessel level based on average LoF and taxa present by vessel area-----
# To enable use of the Golder 2007 passenger vessel dataset for this project, retrospective assignments of CRMS compliance for each vessel area were made using criteria that align with the definitions of the CRMS. This included determinations that could be made based on occurrence records of different types of taxa.
pass_crms <- 
  pass %>%
  pivot_wider(
    id_cols = vessel_code,
    names_from = area_name,
    values_from = LoF,
    values_fn = function(x) {
      mean(x, na.rm = T)
    }
  ) %>% 
  rowwise() %>% 
  mutate(max_lof = max(c_across(DDSS :Stern ), na.rm = T)) %>% 
  left_join(taxa_pass) %>% 
  left_join(vessel_summary) %>% 
  mutate(crms =
           case_when(BV == 1 ~ 'fail',
                     AN == 1 ~ 'fail',
                     AM == 1 ~ 'fail',
                     CB == 1 ~ 'fail',
                     DP == 1 ~ 'fail',
                     EC == 1 ~ 'fail',
                     FW == 1 ~ 'fail',
                     GP == 1 ~ 'fail',
                     IS == 1 ~ 'fail',
                     MU == 1 ~ 'fail',
                     PY == 1 ~ 'fail',
                     SN == 1 ~ 'fail',
                     SP == 1 ~ 'fail',
                     str_detect(Taxa, "Bivalves") ~ 'fail',
                     str_detect(Taxa, "Ascidians") ~ 'fail',
                     str_detect(Taxa, "Amphipods") ~ 'fail',
                     TRich > 2 ~ 'fail',
                     max_lof < 3  ~ 'pass',
                     TRUE ~ 'fail')) %>% 
  write_csv('cleaned_data/Golder07_passenger_data_raw.csv') 

# convert into long format and save it  ---------
pass_crms_long <- 
pass_crms %>% 
  select(vessel_code,  crms, DDSS:Stern) %>% 
  pivot_longer(cols = DDSS:Stern, names_to = 'area', values_to = 'LoF') %>% 
  write_csv('cleaned_data/pass_crms_long.csv')

# data quality check -------
names(pass_crms)
dim(pass)
tabyl(pass_crms, crms)
tabyl(pass, Taxa)
tabyl(pass, area_name)
tabyl(pass_crms_long, area)

# missing values---------
missing_plot(pass_crms)

# descriptive tables ------
tabyl(pass, Taxa) %>%
  adorn_pct_formatting(digits = 1, affix_sign = F)
 
# areas
tabyl(pass, area_name) %>% 
  adorn_pct_formatting(digits = 1, affix_sign = F) 

# vessel codes
distinct(pass, vessel_code)

# LoF
tabyl(pass,LoF)

# check biomass by LoF 
ggplot(pass,aes(LoF, biomass_g )) +
  geom_point() +
  scale_y_log10()