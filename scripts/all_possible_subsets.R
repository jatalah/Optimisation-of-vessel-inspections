library(tidyverse)
library(caret)

model_dat <-
  read_csv('cleaned_data/global_dataset_imputed_median.csv') %>%
  mutate(crms = factor(crms))

vars <- names(model_dat)[-c(1:3)]

models <- list()

for (i in 1:7){
  vc <- combn(vars,i)
  for (j in 1:ncol(vc)){
    model <- as.formula(paste0("crms ~", paste0(vc[,j], collapse = "+")))
    models <- c(models, model)
  }
}

all_subsets <- 
tibble(models) %>% 
  mutate(model_names = map(models, ~toString(.x))) 


# run all RF models 5
all_models <- 
  all_subsets %>% 
  mutate(model_names = map(models, ~toString(.x))) %>% 
  slice(8:127) %>% 
  mutate(mod = map(models, ~train(
    .x,
    data = model_dat,
    preProcess = c("center", "scale", "YeoJohnson"),
    method = "rf",
    ntree = 1000,
    tuneGrid = expand.grid(.mtry = c(2:4)),
    trControl = train_control
  ))) %>% 
  mutate(preds = map(
    mod,
    ~ model_dat %>%
      bind_cols(
        predict(.x$finalModel, type = "response") %>%
          as_tibble_col(column_name = "pred_crms")
      ) %>%
      mutate(crms = factor(crms))
  )) %>% 
  mutate(preds_all =  map(
    .x = mod,
    ~ bind_cols(
      predict(.x, type = "raw", newdata = all_d) %>% as_tibble_col(column_name = "pred_crms"),
      predict(.x, type = "prob", newdata = all_d) %>% as_tibble(),
      all_d
    ) %>%
      mutate(fail = round(fail, 2),
             pass = round(pass, 2))
  )) %>% 
  mutate(conf = map(.x = mod, confusionMatrix)) %>% 
  mutate(acc = map(preds, ~postResample(pred = .x$pred_crms, obs = .x$crms))) %>% 
  mutate(
    best_tune = map(mod, ~.x$bestTune),
    acc_se = map(mod, ~.x$results),
    se_best= map2(.x = best_tune, .y = acc_se, ~filter(.y, mtry == .x$mtry)))


write_rds(all_models, 'outputs/all_subset_models_median.RDS', "xz", compression = 9L)
all_models <- read_rds('outputs/all_subset_models.RDS')

all_model_summary <- 
all_models %>% 
  select(model_names, se_best) %>% 
  unnest_wider(col = se_best) %>% 
  unnest(model_names) %>% 
  mutate(vessel_type = if_else(str_detect(model_names, "vessel_type"), 1, 0),
         ddss = if_else(str_detect(model_names, "ddss"), 1, 0),
         hull = if_else(str_detect(model_names, "hull"), 1, 0),
         other = if_else(str_detect(model_names, "other"), 1, 0),
         propeller_and_shaft = if_else(str_detect(model_names, "propeller_and_shaft"), 1, 0),
         rudder_and_shaft = if_else(str_detect(model_names, "rudder_and_shaft"), 1, 0),
         sea_chest_gratings = if_else(str_detect(model_names, "sea_chest_gratings"), 1, 0),
         nvars = rowSums(across(vessel_type:sea_chest_gratings))) %>% 
  arrange(-Accuracy) %>% 
  mutate(AccuracySE = AccuracySD/sqrt(15),
         KappaSE = KappaSD /sqrt(15),
         low = Accuracy - AccuracySE,
         high = Accuracy + AccuracySE) %>% 
  write_csv('outputs/all_models_subset_summary.csv')

all_model_summary %>% 
  mutate(model_predictors = str_remove(model_names, "~, crms, ")) %>% 
  select(model_predictors, nvars, Accuracy, AccuracySE) %>% 
  arrange(nvars) %>% 
  mutate(across(Accuracy:AccuracySE, round, 3)) %>% 
  write_csv('tables/all_models_subset_summary_table_median.csv')

# all_model_summary <- read_csv('tables/all_models_subset_summary_table.csv')
all_model_summary <- read_csv('outputs/all_models_subset_summary_median.csv')

# save model data for app-------
model_only <- 
  all_models %>% 
  select(model_names, mod) %>% 
  unnest(cols = c(model_names)) %>% 
  # mutate(model_names = str_remove_all(model_names, "~, crms, ")) %>% 
  # full_join(all_model_summary, by = c('model_names'= 'model_predictors')) %>% 
  full_join(all_model_summary, by = 'model_names') %>% 
  mutate(model_names = str_remove_all(model_names, "~, crms, ")) %>%
  write_rds('scripts/CRMS_app/models_only_median.rds', "xz", compression = 9L)

# describe model accuracy for all subsets-----------
all_model_summary %>% 
 group_by(nvars) %>% 
  summarise(across(c(Accuracy,AccuracySE), mean))


all_model_summary %>% 
  filter(Accuracy<=.625) %>% 
  select(model_names:Kappa)


all_model_summary %>% 
  filter(Accuracy<=.75 & Accuracy>=.625) %>% 
  select(model_names:Kappa)

all_model_summary %>% 
  filter(Accuracy>=.75 & Accuracy>=.875) %>% 
  select(model_names:Kappa)


# all possible subset plot ----------
all_subset_accurary_plot <- 
  ggplot(all_model_summary, aes(nvars, Accuracy, color = factor(nvars))) +
  geom_point(position = position_jitter(width = .2, seed = 1), alpha = .5) +
  geom_errorbar(
    aes(ymin = low,  ymax = high),
    width = 0.1,
    position = position_jitter(width = .2, seed = 1), alpha = .9
  ) +
  theme_javier() +
  scale_color_discrete(guide = 'none') +
  labs(x = 'Number of predictor variables', y = "Accuracy ± SE")

ggsave(all_subset_accurary_plot,
       filename = 'figures/all_subset_accurary_plot.png',
       dpi = 300,
       width = 5,
       height = 3.5)

# Kappa for all subsets -----
ggplot(all_model_summary, aes(nvars, Kappa, color = factor(nvars))) +
  geom_point(position = position_jitter(width = .2, seed = 1), alpha = .5) +
  geom_errorbar(
    aes(ymin = Kappa - KappaSE,  ymax = Kappa + KappaSE),
    width = 0.1,
    position = position_jitter(width = .2, seed = 1), alpha = .9
  ) +
  theme_javier() +
  scale_color_discrete(guide = 'none') +
  labs(x = 'Number of predictor variables', y = "Kappa ± SE")


# Get predictions for all subset----=
all_subset_preds <- 
  read_rds('outputs/all_subset_models.RDS') %>% 
  select(model_names, preds_all, se_best)  %>% 
  unnest(c(preds_all, model_names, se_best)) %>%
  # mutate(vessel_type = if_else(str_detect(model_names, "vessel_type"), 1, 0),
  #        ddss = if_else(str_detect(model_names, "ddss"), 1, 0),
  #        hull = if_else(str_detect(model_names, "hull"), 1, 0),
  #        other = if_else(str_detect(model_names, "other"), 1, 0),
  #        propeller_and_shaft = if_else(str_detect(model_names, "propeller_and_shaft"), 1, 0),
  #        rudder_and_shaft = if_else(str_detect(model_names, "rudder_and_shaft"), 1, 0),
  #        sea_chest_gratings = if_else(str_detect(model_names, "sea_chest_gratings"), 1, 0),
  #        nvars = rowSums(across(vessel_type:sea_chest_gratings))) %>% 
  mutate(AccuracySE = AccuracySD/sqrt(15),
         KappaSE = KappaSD /sqrt(15),
         low_ci = Accuracy - (1.96* AccuracySE),
         high_ci = Accuracy + (1.96 * AccuracySE))

# write_csv(all_subset_preds, 'outputs/all_subset_preds.csv')
# all_subset_preds <- read_csv( 'outputs/all_subset_preds.csv')
write_rds(all_subset_preds, 'outputs/all_subset_preds.rds',"xz", compression = 9L)



all_subset_preds <- read_rds('outputs/all_subset_preds.rds')

selected_preds <- 
  all_subset_preds %>% 
  filter(str_detect(model_names, "hull")) %>%
  filter(str_detect(model_names, "vessel_type")) %>% 
  mutate(model_names = str_remove_all(model_names, "~, crms, ")) %>% 
  select(-mtry, -c(Kappa:KappaSE)) %>% 
  write_rds('outputs/selected_preds.rds',"xz", compression = 9L)


selected_preds <- read_rds('outputs/selected_preds.rds')

selected_preds %>% 
  slice(1:1000) %>% 
  write_rds('outputs/selected_preds1.rds',"xz", compression = 9L)

DT::datatable(selected_preds)


selected_preds  %>% 
  filter(str_detect(model_names, "rudder_and_shaft")) %>%
  filter(str_detect(model_names, "propeller_and_shaft"))

all_subsets$model_names %>% 
  unlist() %>% 
  as_tibble_col(column_name = 'name') %>% 
  slice(8:127) %>% 
  mutate(name = str_remove_all(name, "~, crms, ")) %>% 
  filter(str_detect(name, "vessel_type"))
