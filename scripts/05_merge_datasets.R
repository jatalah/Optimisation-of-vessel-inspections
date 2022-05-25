library(tidyverse)
library(caret)
library(finalfit)
library(visdat)
library(janitor)
library(readxl)
library(naniar)
library(ggpubr)
rm(list=ls())
source('scripts/theme_javier.R')

# vessel type --------
vessel_type_data <-
  read_csv('cleaned_data/nzds07_vessel_summarised_data_niwa.csv') %>%
  select(vessel_code, VesselType) %>%
  clean_names() %>%
  bind_rows(
    read_excel(
      "C:/Users/javiera/Cawthron/17745 - Optimisation of Vessel Inspections for Biofouling - General/data_analyses/data/summary4_DM.xlsx"
    ) %>%
      select(Nr, Type) %>%
      rename(vessel_code = Nr,
             vessel_type = Type) %>%
      mutate(vessel_code = as.character(vessel_code))
  ) %>% 
  bind_rows( read_csv('cleaned_data/caw21_type.csv')) %>% 
  write_csv('cleaned_data/vessel_type_data_all.csv')

anti_join(read_csv('cleaned_data/nzds07_crms_long.csv'), vessel_type_data) %>%
  distinct(vessel_code) # 10 vessels without vessel type ------


tabyl(vessel_type_data, vessel_type)


# merge all four datasets-----------
d <-
  bind_rows(
    Passenger = read_csv('cleaned_data/pass_crms_long.csv'),
    NZDS07 = read_csv('cleaned_data/nzds07_crms_long.csv'),
    Caw17 = read_csv('cleaned_data/caw17_long.csv', col_types = cols(vessel_code = col_character())),
    Caw21 =  read_csv('cleaned_data/caw21_crms.csv'),
    .id = "dataset"
  ) %>% 
  rename(Area = area) %>% 
  mutate(area = str_replace_all(Area, "&", "and")) %>% 
  mutate(area = fct_collapse(Area,
                             Other = c(
                               'Bumper bracing',
                               'Damaged AF coating',
                               'Fixing',
                               'Instrument',
                               'Pole opening',
                               'Sounder'))) %>% 
  mutate(area = fct_recode(area, 
                           Other = "Anode", 
                           Other = "bow_thruster",
                           Other = "Bow thruster",
                           # Other = "Discharge pipe",
                           Other = "Stabilisers",
                           Other = "Stabiliser",
                           Other = "Stern thruster",
                           Other = "Bilge keels",
                           Other = "Bilge keel",
                           Other	=	"Discharge"	,
                           ddss = 'Block mark',
                           ddss = 'DDSS',
                           Hull = "Draft mark",
                           Hull = 'Bulbous bow',
                           Hull = "Flat bottom",
                           Hull = "flat_bottom_keel",
                           Hull = "Flat bottom keel",
                           Hull = "hull_area",
                           Hull = "Hull Area",
                           Hull = "hull_below_waterline",
                           Hull = 'Mid',
                           propeller_and_shaft  = "Propeller & Shaft",
                           propeller_and_shaft = "Propeller and shaft",
                           Hull = 'Bow',
                           Hull = "Stern",
                           Hull = "stern",
                           Hull = "Amidship",
                           Hull = "bow",
                           rudder_and_shaft  = "Rudder & Shaft",
                           rudder_and_shaft  = "Rudder & shaft",
                           rudder_and_shaft  = "Rudder and shaft",
                           sea_chest_gratings   = "Sea chest gratings"
                           )) %>% 
  left_join(vessel_type_data, by = 'vessel_code') %>%
  mutate(crms = str_to_lower(crms)) %>%
  mutate(
    crms = factor(crms),
    vessel_type = fct_collapse(
      vessel_type,
      RoRo = c("RoRo / Car carrier", "RoRo", "Vehicles Carrier"),
      Cargo = c(
        "Container / General cargo",
        "General Cargo",
        "Container",
        "Container Ship"
      ),
      Bulk = c("Bulk Carrier", "Bulk")
    ),
  ) %>%
  filter(
    vessel_type != "Fishing" &
      vessel_type != "Heavy Lift Ship" &
      vessel_type != "Research Vessel"
  ) %>%
  droplevels()

write_csv(d, 'cleaned_data/global_dataset_raw.csv')


tabyl(d, area)
tabyl(d, Area)

d %>% 
  group_by(dataset) %>% 
  distinct(vessel_code, vessel_type) %>% 
  count()

vessel_type_data 
  

# Investigate Other category ------
other_dat <- 
  d %>% 
  filter(area == 'Other') %>% 
  droplevels() %>% 
  mutate(Area = fct_collapse(Area,  
                             `Bilge keel` = c("Bilge keel", "Bilge keels"),
                             `Bow thruster` = c("Bow thruster", "bow_thruster"),
                             Stabiliser  = c("Stabiliser", "Stabilisers"))) %>% 
  group_by(dataset, vessel_code, vessel_type, Area) %>% 
  summarise(LoF = mean(LoF, na.rm = T), .groups = 'drop') %>% 
  tabyl(Area) %>% 
  write_csv('cleaned_data/other_dat.csv')

other_prop_plot <- 
  ggplot(other_dat, aes(percent *100, fct_reorder(Area, percent))) +
  geom_col(fill = 'gray50', color = 1) +
  labs(y = 'Area', x = "Percent (%)")


other_prop_plot

distinct(d, vessel_code)
tabyl(d, area)
tabyl(d, area, dataset)
tabyl(d, vessel_type)
distinct(d, vessel_code, dataset) %>% 
  tabyl(dataset)

model_dat %>% 
  group_by(vessel_code) %>% 
  filter(n()>1)

# average and write global model data --------------
model_dat <-
  d %>%
  pivot_wider(
    id_cols = c(dataset, vessel_code, crms, vessel_type),
    names_from = area,
    values_from = LoF,
    values_fn = function(x) {
      mean(x, na.rm = T)
    }
  ) %>%
  clean_names() %>% 
  write_csv('cleaned_data/global_model_dat.csv')

model_dat_median <-
  d %>%
  pivot_wider(
    id_cols = c(dataset, vessel_code, crms, vessel_type),
    names_from = area,
    values_from = LoF,
    values_fn = function(x) {
      median(x, na.rm = T)
    }
  ) %>%
  clean_names() %>% 
  write_csv('cleaned_data/global_model_dat_median.csv')

# Data imputation---------
model_dat <- read_csv('cleaned_data/global_model_dat.csv') %>% 
  mutate(dataset = fct_recode(dataset, Golder07 = 'Passenger'))

impute_para <- preProcess(as.data.frame(model_dat[,5:10]), method = "bagImpute")
model_dat_imp <- predict(impute_para, model_dat) %>% write_csv('cleaned_data/global_dataset_imputed.csv')

model_dat <- read_csv('cleaned_data/global_model_dat.csv') %>% 
  mutate(dataset = fct_recode(dataset, Golder07 = 'Passenger'))

# using median data -----
impute_para_median <- preProcess(as.data.frame(model_dat_median[,5:10]), method = "bagImpute")
model_dat_imp_median <- predict(impute_para_median, model_dat_median) %>% 
  write_csv('cleaned_data/global_dataset_imputed_median.csv')


# Missing values plot-----------
missing_plot(model_dat[, -1])
p_mis <- 
  vis_miss(model_dat[, -c(1:2)]) +
  theme(axis.text.x = element_text(angle = 70))

p_mis1 <- 
  gg_miss_var(model_dat[, -c(2:3)], facet = dataset, show_pct = TRUE ) +
  theme_javier()

p_mis1
ggsave(ggarrange(p_mis, p_mis1, labels = 'auto', nrow = 2),
       filename = 'figures/missing_plot.png',
       width = 8,
       dpi = 300,
       height = 10)

ggarrange(p_mis, p_mis1)
  
gg_miss_fct(model_dat[, -c(2:3)], dataset) +
  scale_fill_gradient(low = "white", high = "gray20", name = ) +
  labs(x = 'Dataset', y = "Predictor variable")