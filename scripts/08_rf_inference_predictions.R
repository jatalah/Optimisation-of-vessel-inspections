# load libraries and clean env.----
library(tidyverse)
library(caret)
library(yardstick)
library(randomForest)
library(probably)
source('scripts/theme_javier.R')
theme_set(theme_minimal())
rm(list=ls())

# read the model data ----------------
model_dat <-
read_csv('cleaned_data/global_dataset_imputed_median.csv') %>%
mutate(crms = factor(crms))

# best model based on CV fitted ---------
m_rf <- readRDS('outputs/rf_model.RDS')

m_rf
m_rf$finalModel

# see parameter tuning----------
ggplot(m_rf, highlight = TRUE) +
  theme_minimal()

# Use RF for inference and predictions-------------
m_rf$finalModel

predictors(m_rf$finalModel)

m_rf
# summary of resemple performance---------
mean(m_rf$resample$Accuracy)
m_rf$finalModel
predictors(m_rf)
plot(m_rf$finalModel)
m_rf$pred

# Variable importance-----
importance(m_rf$finalModel)

var_imp_rf <-
randomForest::varImpPlot(m_rf$finalModel) %>%
as_tibble(rownames = "Variable") %>%
mutate(Variable = str_replace_all(Variable, "vessel_type", ""),
Variable = str_to_sentence(str_replace_all(Variable, "_", " ")),
Variable = fct_recode(Variable, DDSS = "Ddss", RoRo = "Roro"))

# plot variable importance--------
imp_plot <-
ggplot(var_imp_rf, aes(x = reorder(Variable, MeanDecreaseAccuracy ), y = MeanDecreaseAccuracy )) +
geom_point() +
geom_segment(aes(
x = Variable,
xend = Variable,
y = 0,
yend = MeanDecreaseAccuracy
)) +
ylab("Variable importance") +
xlab("Predictor") +
coord_flip() +
  theme_javier()

imp_plot

ggsave(imp_plot,
filename = 'figures/rf_imp_plot.png',
dpi = 300,
width = 5,
height = 4)

# Confusion matrix for full dataset--------
confusionMatrix(m_rf)

d1 <-
  model_dat %>%
  bind_cols(
    predict(m_rf$finalModel, type = "response") %>%
      as_tibble_col(column_name = "pred_crms")
  ) %>%
  mutate(crms = factor(crms)) %>%
  bind_cols(
    predict(m_rf$finalModel, type = "prob") %>%
      as_tibble() %>%
      transmute(fail_prob = as.numeric(fail), pass_prob = as.numeric(pass))
  )

# confusion matrix stats---------
cm <- conf_mat(d1, crms, pred_crms)
summary(cm) %>% write_csv('tables/confusion_matrix_summary_stats.csv')

# confusion matrix plot -----------
confusion_plot <-
  autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "#D6EAF8", high = "#2E86C1") +
  labs(x = 'Observed CRMS status', y = "Predicted CRMS status")


misclasified_vessels <-
  d1 %>%
  filter(crms != pred_crms) %>%
  select(vessel_code,
         vessel_type,
         crms,
         pred_crms,
         ddss:sea_chest_gratings) %>%
  mutate_if(is.numeric, round, 1) %>%
  write_csv('tables/misclasified_vessels.csv')

# misclasified_vessels <- read_csv('tables/misclasified_vessels.csv')

# summary of misclassified vessels by type------
full_join(
  misclasified_vessels %>%
    group_by(vessel_type) %>%
    tally(),
  d1 %>%
    group_by(vessel_type) %>%
    tally(),
  by = 'vessel_type'
) %>%
  mutate(prop = n.x / n.y * 100)


# ROC curves --------
# for resamples prediction results ------
resample_res <- m_rf$pred %>% as_tibble()
metrics(resample_res, obs, pred)

resample_res %>%
roc_auc(obs, fail)

resample_res %>%
group_by(Resample) %>%
roc_curve(obs, fail) %>%
autoplot() +
  theme_javier()

# for final model predictions --------
auc <- d1 %>% roc_auc(crms, fail_prob)

roc_plot <- 
  d1 %>%
  roc_curve(crms, fail_prob) %>%
  autoplot() +
  theme_javier() +
  annotate(
    geom = 'text',
    x = .75,
    y = .2,
    label = paste0("AUC = ", round(auc$.estimate, 2))
  )

roc_plot
ggsave(roc_plot,
       filename = 'figures/roc_plot.png',
       dpi = 300,
       width = 4,
       height = 4)

# LoF thresholds---------------
d1 %>%
  pr_curve(crms, other) %>%
  filter(precision < 1) %>% top_n(1, precision)

d1 %>%
  threshold_perf(crms, other, seq(0, 5, by = 0.1)) %>%
  filter(.metric == 'spec' & .estimate < 1) %>%
  top_n(1, .estimate)


d1 %>%
  pr_curve(crms, sea_chest_gratings) %>%
  filter(precision < 1) %>% top_n(1, precision)


d1 %>%
  threshold_perf(crms, sea_chest_gratings, seq(0, 5, by = 0.1)) %>%
  filter(.metric == 'spec' & .estimate < 1) %>%
  top_n(1, .estimate)


d1 %>%
  roc_curve(crms, propeller_and_shaft) %>%
  autoplot() +
  theme_javier()

d1 %>%
  pr_curve(crms, sea_chest_gratings) %>%
  ggplot(aes(x =  .threshold, y = precision)) +
  geom_path() +
  theme_bw()


# data predictions -------------
all_d <-
expand_grid(
vessel_type = unique(model_dat$vessel_type),
ddss = 0:5,
hull = 0:5,
propeller_and_shaft = 0:5,
rudder_and_shaft = 0:5,
sea_chest_gratings = 0:5,
other = 0:5
) %>% 
  write_csv('cleaned_data/all_d.csv')

all_pred <-
bind_cols(
predict(m_rf, type = "raw", newdata = all_d) %>% as_tibble_col(column_name = "pred_crms"),
predict(m_rf, type = "prob", newdata = all_d) %>% as_tibble(),
all_d
) %>%
mutate(fail = round(fail, 2),
pass = round(pass, 2))


# save all predictions-----------
all_pred
write_csv(all_pred, 'cleaned_data/all_pred.csv')


# feature selection using recursive feature selection-----------------
set.seed(1)
ctrl <- rfeControl(
functions = rfFuncs,
method = "cv",
number = 5,
returnResamp = "all",
functions = caretFuncs,
saveDetails = TRUE
)
ctrl <-
rfeControl(
functions = rfFuncs,
method = "cv",
number = 5,
returnResamp = "all",
functions = caretFuncs,
saveDetails = TRUE
)
ctrl <-
rfeControl(
functions = rfFuncs,
method = "cv",
number = 5,
returnResamp = "all",
saveDetails = TRUE
)
rfProfile <- rfe(model_dat[,2:8],  model_dat[[1]],  sizes = c(1:7), rfeControl = ctrl)
model_dat
rfProfile <- rfe(model_dat[,4:10],  model_dat[[3]],  sizes = c(1:7), rfeControl = ctrl)


# summarize the RFE results------------
print(rfProfile)
rfe_preds <- predictors(rfProfile)
rfProfile$fit
head(rfProfile$resample)

# plot results 
rfe_performance_plot <- ggplot(rfProfile)
ggplot(rfProfile, metric = "Kappa") + theme_bw()
ggplot(rfProfile, metric = "Accuracy") + theme_bw()
ggsave(rfe_performance_plot,
filename = 'figures/rfe_performance_plot.png',
dpi = 300,
width = 5,
height = 4)
pickSizeBest(rfProfile$results, metric = "Accuracy", maximize = T)

# # Other feature selection algorithms---------
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 3, number = 5)
rfWithFilter <- sbf(model_dat[,4:10],  model_dat[[3]], sbfControl = filterCtrl)
rfWithFilter

# all possible subsets--
library(CAST)
select(model_dat, head(rfe_preds,7))
bssmodel <- bss(select(model_dat, head(rfe_preds,7)),  model_dat[[3]])
all_subsets <- bssmodel$perf_all
