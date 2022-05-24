library(tidyverse)
library(readxl)
library(janitor)
library(finalfit)

# read the revised data provide by MPI https://mpinz.sharefile.com/d-s48a2080126b74f2dbd941b7947334d9c on Wed 19/05/2021 14:26----
pass_raw <-
  read_excel('data/MPI 2007/passenger/Passenger Vessel Data_revised 05192021.xlsx', 1)

codes <-
  read_excel('data/MPI 2007/passenger/MPI vessel sampling codes.xlsx') %>% 
  rename(Area_name =  "Opportunistic collections - niche areas",
         Area = "Code") %>% 
  mutate(Area = fct_recode(Area,  K = "FK, K" ))

taxa_codes <- read_csv('data/MPI 2007/passenger/taxa codes.csv')
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


taxa_pass <- 
  pass %>% 
  select(vessel_code, Taxa) %>% 
  group_by(vessel_code) %>% 
  distinct(Taxa)  %>% 
  drop_na(Taxa) %>%
  summarise(Taxa = first(paste0(Taxa, collapse = ",")))


# Assign CRMS compliance at the vessel level based on avearge LoF and taxa present by vessel area---
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

# convert into long format ---
pass_crms_long <- 
pass_crms %>% 
  select(vessel_code,  crms, DDSS:Stern) %>% 
  pivot_longer(cols = DDSS:Stern, names_to = 'area', values_to = 'LoF') %>% 
  write_csv('cleaned_data/pass_crms_long.csv')



# explore ----
tabyl(pass_crms, crms)
tabyl(pass, Taxa)
tabyl(pass, area_name)
tabyl(pass_crms_long, area)

missing_plot(pass_crms)


names(pass_crms)
dim(pass)

dim(pass)


tabyl(pass, Taxa) %>%
  adorn_pct_formatting(digits = 1, affix_sign = F)
 
# clipr::write_clip()

tabyl(pass, area_name) %>% 
  adorn_pct_formatting(digits = 1, affix_sign = F) 

distinct(pass, vessel_code)


tabyl(pass,LoF)
tabyl(pass,biomass_g)

bp_pass <- 
pass %>% 
 group_by(vessel_code, area_name, area) %>% 
  summarise(LoF = mean(LoF, na.rm = T)) %>% 
  ggplot(aes(area_name, LoF)) +
  geom_boxplot(aes(fill = area), alpha = .8, width = .3, outlier.shape = NA, na.rm = T, notchwidth = 0.5, notch = F) +
  geom_point(position = position_jitter(width = .25, height = .2), alpha = .2)  +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(breaks = 0:6) +
  theme(axis.title.y = element_blank()) +
  scale_fill_viridis_d(guide = F, option = 'A') +
  geom_hline(yintercept = 3, lty = 3) +
  geom_hline(yintercept = 2, lty = 3) +
  annotate("text", x = 15.3, y = c(1.5,2.5,3.5), label = c("Pass","Uncertain", "Fail")) +
  labs(subtitle = 'Passengers vessel')


ggplot(pass,aes(LoF, biomass_g )) +
  geom_point() +
  scale_y_log10()


pass_crms %>% 
  ggplot(aes(factor(crms), biomass_g)) +
  geom_boxplot(alpha = .8, width = .3, outlier.shape = NA, na.rm = T, notchwidth = 0.5, notch = F) +
  geom_point(position = position_jitter(width = .25, height = .2), alpha = .2)  +
  theme_bw()
