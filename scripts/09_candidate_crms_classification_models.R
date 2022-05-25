library(tidyverse)
library(caret)
library(gbm)
theme_set(theme_javier())
set.seed(123)

# read data------
model_dat <- 
  read_csv('cleaned_data/global_dataset_imputed_median.csv') %>% 
  select(-c(dataset, vessel_code)) %>% 
  mutate(crms = factor(crms))

# set train control -------
train_control <-
  trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    savePredictions =  TRUE,
    selectionFunction = "tolerance",
    classProbs = TRUE
  )

# random forest------------
set.seed(123)
m_rf <-
  train(
    crms  ~ .,
    data = select(model_dat, crms:sea_chest_gratings),
    # preProcess = c("center", "scale","YeoJohnson"),
    method = "rf",
    ntree = 1000,
    tuneGrid = expand.grid(.mtry=c(2)),
    trControl=train_control,
    importance = T
  )
saveRDS(m_rf, 'outputs/rf_model_median.RDS')

m_rf
m_rf$finalModel


# neural networks----------------
m_nn <-
  train(
    crms  ~ .,
    data = model_dat,
    preProcess = c("center", "scale","YeoJohnson"),
    method = "nnet",
    trControl=train_control
  )
m_nn

# Boosted regression trees----------
m_brt <-
  train(
    crms  ~ .,
    data = model_dat,
    method = "gbm",
    trControl=train_control,
    preProcess = c("center", "scale","YeoJohnson"),
    tuneGrid =  expand.grid(
      .n.trees = c(100, 500, 1000),
      .interaction.depth = c(1:3),
      .shrinkage = c(0.1),
      .n.minobsinnode = (10)
    ) 
  )
m_brt


# Extreme boosting
m_xgbTree <-
  train(
    crms  ~ .,
    data = model_dat,
    method = "xgbTree",
    trControl=train_control,
    preProcess = c("center", "scale","YeoJohnson")
  )
m_xgbTree

# logistic regression ------
m_logistic <-
  train(
    crms  ~ .,
    data = model_dat,
    preProcess = c("center", "scale","YeoJohnson"),
    method = "bayesglm",
    trControl=train_control
  )
m_logistic

# Support vector machine------
m_svm <-
  train(
    crms  ~ .,
    data = model_dat,
    preProcess = c("center", "scale","YeoJohnson"),
    method = "lssvmRadial",
    trControl=train_control
  )
m_svm


# collect resamples and compare models-------------
results <-
  resamples(list(
    RF = m_rf,
    GLM = m_logistic,
    SVM = m_svm,
    BRT = m_brt,
    NN = m_nn,
    Xboost = m_xgbTree
  ))
# summarize the distributions
summary(results)

# Report figure 7 with box-plots of results for all five candidate model--------------
candidate_models_performance_plot <-
  ggplot(results, metric = c("Accuracy", "Kappa")) +
  labs(y = "Cross-validation performance")

candidate_models_performance_plot

ggsave(candidate_models_performance_plot,
    filename = 'figures/candidate_models_performance_plot.png',
         dpi = 300,
         width = 5,
         height = 4)
