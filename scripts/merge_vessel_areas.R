x <- bind_rows(
  Passenger = read_csv('cleaned_data/pass_crms_long.csv'),
  NZDS07 = read_csv('cleaned_data/nzds07_crms_long.csv'),
  Caw17 = read_csv('cleaned_data/caw17_long.csv', col_types = cols(vessel_code = col_character())),
  Caw21 =  read_csv('cleaned_data/caw21_crms.csv'),
  .id = "dataset"
) %>% 
  filter(area == 'Other') 


tabyl(x, area, dataset)


