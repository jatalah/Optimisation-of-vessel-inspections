library(tidyverse)

hull_area_dat <- 
  bind_rows(read_csv('cleaned_data/caw17_long.csv', col_types = cols(vessel_code = col_character())),
            read_csv('cleaned_data/caw21_raw_clean.csv') %>% 
              filter(area == "Hull") %>% 
              select(vessel_code, LoF, region) %>% 
              rename(area = "region"),
read_csv('cleaned_data/pass_crms_long.csv')) %>% 
  filter(area %in% c("Bow", "Mid", "Stern"))

hull_area_dat %>% distinct(vessel_code) %>% dim()

vessel_hull_area_boxplot <- 
  ggplot(hull_area_dat, aes(area, LoF)) +
  labs(x = "Hull Area", y = "Level of biofouling (LoF)") +
  geom_boxplot(
    alpha = .8,
    width = .5,
    outlier.shape = NA,
    na.rm = T,
    notchwidth = 0.5,
    notch = F
  ) +
  geom_point(position = position_jitter(width = .25, height = .2), alpha = .1)  +
  scale_y_continuous(breaks = 0:6) +
  theme_javier()

vessel_hull_area_boxplot


ggsave(vessel_hull_area_boxplot,
       filename = 'figures/vessel_hull_area_boxplot.png',
       dpi = 300,
       width = 3,
       height = 3)


hull_area_dat %>% 
  group_by(area) %>% 
  summarise_at(
    "LoF",
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      n = ~n(),
      Q1 = ~quantile(., probs = 0.25,na.rm = T),
      Q3 = ~quantile(., probs = 0.75,na.rm = T),
      se = ~ sd / sqrt(n),
      ci = ~ se * 1.96
    ),
    na.rm = T
  ) 
