library(tidyverse)
library(janitor)
library(readxl)
theme_set(theme_bw())

nzds07_s1 <- 
  read_excel("data/MPI 2007/commercial/ZBS2004-03 NZDS final data set 2007.xlsx", sheet = 1) %>% 
  clean_names() %>% 
  rename(LoF = "lo_f",
         overall_lof = "overall_lo_f")

summary(nzds07_s1)
dim(nzds07_s2)
glimpse(nzds07_s2)
names(nzds07_s2)

tabyl(nzds07_s1, LoF)
tabyl(nzds07_s1, port_in_nz)
distinct(nzds07_s1, vessel_code)
distinct(nzds07_s1, hull_area)
distinct(nzds07_s2, hull_area_section   )
