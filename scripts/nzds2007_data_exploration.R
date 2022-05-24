library(tidyverse)
library(janitor)
library(finalfit)

nzds07_crms <- read_csv('cleaned_data/nzds07_crms.csv')
nzds07_crms_long <- read_csv('cleaned_data/nzds07_crms_long.csv')
# Explore ---
tabyl(nzds07_crms, crms)
tabyl(nzds07_crms_long, area)
nzds07_crms %>% missing_plot()


tabyl(nzds07, LoF)
tabyl(nzds07, port_in_nz,         )
sum(is.na(nzds07$sample_wet_weight_g))

tabyl(nzds07, area) %>% 
  adorn_pct_formatting(affix_sign = F) 

# write_clip()

tabyl(nzds07, vessel_code, port_in_nz )

tabyl(nzds07, sample_type) %>%
  adorn_pct_formatting(affix_sign = F) %>% 
  write_clip()

ggplot(nzds07, aes(sample_wet_weight_g)) +
  geom_histogram()

ggplot(nzds07, aes(LoF)) +
  geom_histogram() + coord_flip()


bp_com_dat <- 
  nzds07 %>% 
  group_by(vessel_code, area) %>% 
  summarise(LoF = mean(LoF, na.rm = T),
            biomass = sum(biomass))


bp_pass_dat <-
  pass %>%
  group_by(vessel_code, area_name) %>%
  summarise(LoF = mean(LoF, na.rm = T),
            biomass = sum(biomass_g, na.rm = T)) %>%
  select(-area) %>%
  rename(area = "area_name")

bp_d <-
  bind_rows(Commercial = bp_com_dat,
            Passenger = bp_pass_dat,
            .id = "Type") %>%
  mutate(
    area = str_to_sentence(area),
    area = fct_recode(area, DDSS = "Ddss"),
    area = str_replace_all(area, "and", "&")
  )

bp_lof <-
  ggplot(bp_d, aes(area, LoF)) +
  geom_boxplot(
    aes(fill = area),
    alpha = .8,
    width = .3,
    outlier.shape = NA,
    na.rm = T,
    notchwidth = 0.5,
    notch = F
  ) +
  geom_point(
    size = .8,
    position = position_jitter(width = .25, height = .2),
    alpha = .2
  )  +
  coord_flip() +
  theme_bw(base_size = 12) +
  facet_wrap( ~ Type) +
  scale_y_continuous(breaks = 0:6) +
  theme(axis.title.y = element_blank()) +
  scale_fill_viridis_d(guide = F, option = 'A') 

bp_lof

ggsave(bp_lof,
       filename = 'figures/boxplots_lof_2007_data.jpg',
       width = 8.5, height = 5)

# Notches are used to compare groups; if the notches of two boxes do not overlap, this suggests that the medians are significantly different.

# Biomass---
bp_biomass <- 
  bp_d %>% 
  filter(biomass>0) %>% 
  ggplot(aes(area, biomass)) +
  geom_boxplot(
    aes(fill = area),
    alpha = .8,
    width = .3,
    outlier.shape = NA,
    na.rm = T,
    notchwidth = 0.5,
    notch = F
  ) +
  geom_point(
    size = .8,
    position = position_jitter(width = .25, height = .2),
    alpha = .2
  )  +
  coord_flip() +
  theme_bw(base_size = 12) +
  facet_wrap( ~ Type, scales = 'free_x') +
  scale_y_log10(labels = scales::comma_format(accuracy= 1)) +
  theme(axis.title.y = element_blank()) +
  scale_fill_viridis_d(guide = F, option = 'D') +
  labs(y = "Biomass (g)")


ggsave(bp_biomass,
       filename = 'figures/boxplots_biomass_2007_data.jpg',
       width = 8.5, height = 5)

sum(is.na(nzds07$LoF))


wide_07 <- 
  nzds07 %>% 
  select(- paint_condition  , -sample_type, -sample_wet_weight_g) %>% 
  pivot_wider(values_from = "LoF", names_from = "hull_area", values_fn = mean)


map(wide_07, ~ sum(is.na(.)))

nzds07 %>%
  filter(!is.na(sample_wet_weight_g)) %>% 
  group_by(vessel_code, date_of_survey, hull_area) %>%
  summarise(n = n()) %>%
  arrange(-n)  %>% View

nzds07 %>% 
  distinct(vessel_code,date_of_survey) %>% 
  group_by(vessel_code) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

nzds07 %>% 
  distinct(vessel_code) 

nzds07 %>% 
  filter(is.na(sample_wet_weight_g) & LoF>1)

nzds07 %>% 
  filter(!is.na(sample_wet_weight_g) & LoF<2) 
