# Data preparation of commercial vessels surveyed by New Zealand Diving and Salvage Ltd (NZDS) between 2005 and 2007. See Inglis et al. 2010 for details.

# load libraries and clean env.----
library(tidyverse)
library(readxl)
library(janitor)
library(finalfit)
filter <- dplyr::filter
rm(list=ls())

# Read each of the three spreadsheets-------
nzds07 <-
  read_excel("data/MPI 2007/commercial/ZBS2004-03 NZDS final data set 2007.xlsx",
             sheet = "LoF hull locations data") %>%
  clean_names() %>%
  rename(LoF = "lo_f") %>%
  mutate(hull_area = str_to_sentence(hull_area),
         hull_area = fct_recode(hull_area, DDSS = "Ddss")) %>%
  rename(area = "hull_area") %>%
  filter(area != "Waterline") %>%
  droplevels() %>%
  mutate(biomass = if_else(is.na(sample_wet_weight_g) &
                             LoF < 2, 0, sample_wet_weight_g))


# read summarized data NZDS and passenger data---------
vessel_summary <-
  read_excel('data/MPI 2007/commercial/HF_vessel_summary_voyage_history.xls',
             sheet = 'Raw data by vessel') %>%
  filter(Provider %in% c('NZD', "KML")) %>%
  mutate(vessel_code = str_remove_all(QuesCode, '-')) %>%
  select(vessel_code,
         VesselType,
         TRich,
         AG:WM,
         Biomass) %>%
  write_csv('cleaned_data/nzds07_vessel_summarised_data_niwa.csv')

# get auxiliary variables ----------
taxa <-
  nzds07 %>%
  select(vessel_code, sample_type) %>%
  group_by(vessel_code) %>%
  distinct(sample_type)  %>%
  drop_na(sample_type) %>%
  summarise(all_taxa = first(paste0(sample_type, collapse = ",")))

# Assign CRMS compliance at the vessel level based on average LoF and taxa present by vessel area----------
# To enable use of the 2007 NZDS dataset for this project, retrospective assignments of CRMS compliance for each vessel area were made using criteria that align with the definitions of the CRMS. This included determinations that could be made based on occurrence records of different types of taxa.

nzds07_crms <-
  nzds07 %>%
  drop_na(LoF) %>%
  group_by(vessel_code, quadrat_number, area) %>%
  summarise(
    LoF = mean(LoF),
    taxa = first(paste0(sample_type, collapse = ",")),
    taxa = na_if(taxa, "NA"),
    biomass = sum(biomass),
    .groups = 'drop'
  ) %>% 
  pivot_wider(
    id_cols = c(vessel_code),
    names_from = area,
    values_from = LoF,
    values_fn = function(x) {
      mean(x, na.rm = T)
    }
  ) %>% 
  clean_names() %>% 
  rowwise() %>% 
  mutate(max_lof = max(c_across(bow:hull_below_waterline), na.rm = T)) %>% 
  left_join(taxa, by = "vessel_code") %>% 
  left_join(vessel_summary, by = "vessel_code") %>% 
  mutate(crms =
           case_when(BV == 1 ~ 'fail', # Bivalve
                     AN == 1 ~ 'fail', # Ascidian
                     CB == 1 ~ 'fail', # Crabs
                     DP == 1 ~ 'fail', # Decapod
                     EC == 1 ~ 'fail', # Echinoderm
                     FW == 1 ~ 'fail', # Flatworm
                     GP == 1 ~ 'fail', # Gastropod
                     MU == 1 ~ 'fail', # Molluscs
                     PY == 1 ~ 'fail', # Pycnogonid
                     SN == 1 ~ 'fail', # Sea anemone
                     SP == 1 ~ 'fail', # Sponge
                     str_detect(all_taxa, "barnacles with hydroid") ~ 'fail',
                     str_detect(all_taxa, "barnacles with algal") ~ 'fail',
                     str_detect(all_taxa, "gastropods") ~ 'fail',
                     str_detect(all_taxa, "mussels") ~ 'fail',
                     str_detect(all_taxa, "other crustaceans") ~ 'fail',
                     str_detect(all_taxa, "sponges") ~ 'fail',
                     str_detect(all_taxa, "ascidians") ~ 'fail',
                     str_detect(all_taxa, "anemones") ~ 'fail',
                     str_detect(all_taxa, "pycnogonids") ~ 'fail',
                     str_detect(all_taxa, "other bivalves") ~ 'fail',
                     str_detect(all_taxa, "hydroid") ~ 'fail',
                     str_detect(all_taxa, "barnacles with algal and hydroid growth") ~ 'fail',
                     str_detect(all_taxa, "bivalves") ~ 'fail',
                     TRich > 2 ~ 'fail',
                     max_lof < 3  ~ 'pass',
                     TRUE ~ 'fail')) %>% 
  # select(vessel_code,  crms, bow:hull_below_waterline) %>% 
  write_csv('cleaned_data/nzds07_clean_crms.csv')

missing_plot(nzds07_crms)
tabyl(nzds07_crms, crms)

# convert into long format and save it ------
nzds07_crms_long <-
  nzds07_crms %>%
  select(vessel_code,  crms, bow:hull_below_waterline) %>%
  pivot_longer(
    cols = bow:hull_below_waterline,
    names_to = 'area',
    values_to = 'LoF'
  ) %>%
  write_csv('cleaned_data/nzds07_crms_long.csv')

# CRMS by areas data prep-----------
# nzds07_crms_by_area <- 
#   nzds07 %>%
#   drop_na(LoF) %>% 
#   group_by(vessel_code, quadrat_number, area) %>% 
#   summarise(LoF = mean(LoF), 
#             taxa = first(paste0(sample_type, collapse = ",")),
#             taxa = na_if(taxa, "NA"),
#             biomass = sum(biomass)) %>% 
#   ungroup() %>% 
#   mutate(crms_area =
#            case_when(str_detect(taxa, "barnacles with hydroid") ~ '1',
#                      str_detect(taxa, "barnacles with algal") ~ '1',
#                      str_detect(taxa, "gastropods") ~ '1',
#                      str_detect(taxa, "mussels") ~ '1',
#                      str_detect(taxa, "other crustaceans") ~ '1',
#                      str_detect(taxa, "sponges") ~ '1',
#                      str_detect(taxa, "ascidians") ~ '1',
#                      str_detect(taxa, "anemones") ~ '1',
#                      str_detect(taxa, "pycnogonids") ~ '1',
#                      str_detect(taxa, "other bivalves") ~ '1',
#                      str_detect(taxa, "barnacles with algal and hydroid growth") ~ '1',
#                      str_detect(taxa, "bivalves") ~ '1',
#                      LoF < 3  ~ '0',
#                      is.na(taxa) & LoF > 3~ '1',
#                      TRUE ~ '1'),
#          crms_area = as.numeric(crms_area)) %>% 
#   pivot_wider(
#     id_cols = c(vessel_code, quadrat_number),
#     names_from = area,
#     values_from = crms_area,
#     values_fn = function(x) {
#       mean(x, na.rm = T)
#     }
#   ) %>%  
#   clean_names() %>% 
#   rowwise() %>% 
#   mutate(sum_crms_area = sum(c_across(bow:hull_below_waterline), na.rm = T)) %>% 
#   left_join(vessel_summary) %>% 
#   mutate(crms =
#            case_when(BV == 1 ~ 'fail',
#                      AN == 1 ~ 'fail',
#                      CB == 1 ~ 'fail',
#                      DP == 1 ~ 'fail',
#                      EC == 1 ~ 'fail',
#                      FW == 1 ~ 'fail',
#                      GP == 1 ~ 'fail',
#                      MU == 1 ~ 'fail',
#                      PY == 1 ~ 'fail',
#                      SN == 1 ~ 'fail',
#                      SP == 1 ~ 'fail',
#                      TRich > 2 ~ 'fail',
#                      sum_crms_area < 1  ~ 'pass',
#                      TRUE ~ 'fail')) 