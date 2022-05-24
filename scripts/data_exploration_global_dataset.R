library(tidyverse)
library(clipr)
library(janitor)
source('scripts/theme_javier.R')
theme_set(theme_javier())
dat <- 
  # read_csv('cleaned_data/global_dataset_imputed_median.csv') %>% # changed to median
  read_csv('cleaned_data/global_dataset_imputed.csv')  # changed to median



# table of no. of vessel per dataset------
tabyl(dat, dataset) %>% 
  adorn_pct_formatting() %>% 
  write_clip()

# table of no. of vessel per type------
tabyl(dat, vessel_type) %>% 
  adorn_pct_formatting() 

# Number of vessels by dataset and type------ 
# vessel_type_plot1 <- 
#   tabyl(dat, vessel_type, dataset) %>% 
#   as_tibble() %>% 
#   pivot_longer(cols = -vessel_type) %>% 
#   ggplot() +
#   geom_col(aes(x = name, fill = vessel_type, y = value), alpha = 0.9) +
#   coord_flip() +
#   labs(x = 'Dataset', y = 'Number of vessels') + 
#   scale_fill_brewer(name = "Vessel type", palette = 'Spectral') 
# 
# vessel_type_plot1
# 
# ggsave(vessel_type_plot1,
#        filename = 'figures/vessel_type_plot1.png',
#        dpi = 300,
#        width = 5,
#        height = 4)

# Number of vessels by type and dataset------ 
vessel_type_plot <- 
tabyl(dat, dataset, vessel_type) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -dataset) %>% 
  ggplot() +
  geom_col(aes(x = name, fill = dataset, y = value), alpha = 0.9, color = 'gray20') +
  coord_flip() +
  labs(x = 'Vessel type', y = 'Number of vessels') + 
  scale_fill_brewer(name = "Dataset", palette = 'Spectral') 
vessel_type_plot

ggsave(vessel_type_plot,
       filename = 'figures/vessel_type_plot.png',
       dpi = 300,
       width = 4,
       height = 3)

# LoF summaries-------
dl <- 
  dat %>% 
  pivot_longer(cols = c(ddss:sea_chest_gratings), names_to = 'area', values_to = 'LoF') %>% 
  mutate(Area = str_to_sentence(str_replace_all(area, "_", " ")),
         Area = fct_recode(Area, DDSS = "Ddss"))

dl %>%
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

# LoF summaries by type and area----
dl %>%
  group_by(vessel_type, area) %>%
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
  ) %>% 
  View()
  write_csv('tables/LoF_summary_type_area.csv')

# LoF summaries by type ---- 
dl %>%
  group_by(vessel_type) %>%
  summarise_at(
    "LoF",
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      Q1 = ~quantile(., probs = 0.25,na.rm = T),
      Q3 = ~quantile(., probs = 0.75,na.rm = T),
      n = ~n(),
      se = ~ sd / sqrt(n),
      ci = ~ se * 1.96
    ),
    na.rm = T
  )

# LoF summaries by area ---- 
dl %>%
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
      se = ~ sd / sqrt(n()),
      ci = ~ se * 1.96
    ),
    na.rm = T
  ) %>% 
    arrange(-median)
  
  
# LoF boxplot -----
lof_boxplot <- 
ggplot(dl, aes(Area, LoF)) +
  geom_boxplot(aes(fill = Area), alpha = .8, width = .3, outlier.shape = NA, na.rm = T, notchwidth = 0.5, notch = F) +
  geom_point(position = position_jitter(width = .25, height = .2), alpha = .1)  +
  coord_flip() +
  theme_javier() +
  scale_y_continuous(breaks = 0:6) +
  theme(axis.title.y = element_blank()) +
  scale_fill_viridis_d(guide = 'none', option = 'A') +
  facet_wrap(~vessel_type)

lof_boxplot  

ggsave(lof_boxplot,
       filename = 'figures/lof_boxplot.png',
       dpi = 300,
       width = 6.5,
       height = 4.5)

# Violin plot as an alternative ---
violin_lof_plot <- 
  ggplot(dl, aes(Area, LoF)) +
  geom_violin(aes(fill = Area), alpha = .8) +
  coord_flip() +
  theme_javier() +
  scale_y_continuous(breaks = 0:6) +
  theme(axis.title.y = element_blank()) +
  scale_fill_viridis_d(guide = , option = 'A') +
  facet_wrap(~vessel_type)

ggsave(lof_boxplot,
       filename = 'figures/lof_boxplot.png',
       dpi = 300,
       width = 6,
       height = 4)


# Other area plot--------------
other_dat <- read_csv('cleaned_data/other_dat.csv')

other_prop_plot <- 
  ggplot(other_dat, aes(percent *100, fct_reorder(Area, percent))) +
  geom_col(fill = 'gray50', color = 1) +
  labs(y = 'Area', x = "Percent (%)")

ggsave(other_prop_plot,
       filename = 'figures/other_prop_plot.png',
       dpi = 300,
       width = 4,
       height = 3)

# CRMS summary -----
tabyl(dat, crms)

dat %>%
  group_by(vessel_type, crms) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  ggplot( aes(fill=crms, y=freq, x=vessel_type)) + 
  geom_bar(position="fill", stat="identity")+
  labs(y = "Proportion", x = "vessel type") +
  coord_flip() +
  theme_javier() +
  scale_fill_discrete(name = 'CRMS')

crms_plot <- 
dat %>%
  group_by(vessel_type, crms) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n),
         crms = str_to_title(crms)) %>% 
  ggplot( aes(fill=crms, y=n, x=vessel_type)) + 
  geom_bar(position="dodge", stat="identity", alpha = 0.9, color = 'gray50') +
  labs(y = "Number of vessels", x = "Vessel type") +
  coord_flip() +
  theme_javier() +
  scale_fill_discrete(name = 'CRMS') +
  geom_text(
    aes(
      x = vessel_type,
      y = n + 7,
      group = crms,
      label = paste0(round(freq, 2)*100,"%")),
      size = 3,
      position = position_dodge(width = 1)
    ) +
  theme(legend.position = c(.8,.8))

crms_plot
# CRMS status bar plot------
ggsave(crms_plot,
       filename = 'figures/crms_plot.png',
       dpi = 300,
       width = 6,
       height = 4)
