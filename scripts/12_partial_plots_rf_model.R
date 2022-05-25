library(tidyverse)
library(ggpubr)
library(randomForest)
library(plotmo)
library(pdp)

model_dat <-
  read_csv('cleaned_data/global_dataset_imputed.csv') %>%
  mutate(crms = factor(crms))

# fir model without vessel type to avoid problems with partial plots----
rf_model <- randomForest(crms  ~ ., data = select(model_dat, crms, ddss:sea_chest_gratings), mty = 2, ntree = 1000)
plot(rf_model)

# interaction partial repsonse curves----
##All in one plot
tiff(
  "figures/two_way_partial_plots.tiff",
  width = 6000,
  height = 6000,
  res = 600,
  type = 'cairo',
  compression = 'lzw'
)

plotmo(
  rf_model,
  type = 'prob',
  degree1 = F,
  all2 = T,
  persp.ticktype = "detailed",
  nresponse = "fail",
  caption = '',
  main = c(
    'DDSS x Hull',
    'DDSS x Other',
    'DDSS x Propeller',
    'DDSS x rudder',
    "DDSS x Seachest",
    "Hull x Other",
    "Hull x Propeller",
    "Hull x Rudder",
    "Hull x Seachest",
    "Other x Propeller",
    "Other x Rudder",
    "Other x Seachest",
    "Propeller x Rudder",
    "Propeller x Seachest",
    "Rudder x Seachest"
  )
)

dev.off()

# main effects -----
plotmo(rf_model,
       type = 'prob',
       degree2 = F, 
       nresponse="fail", caption = '') 


p1 <- 
  rf_model %>%
  partial(pred.var = "other", prob = TRUE) %>%
  autoplot(smooth = TRUE, ylab = 'Probability', xlab = "Other LoF") +
  scale_y_continuous(limits = c(0.25,1))

p2 <- 
  rf_model %>%
  partial(pred.var = "sea_chest_gratings", prob = TRUE) %>%
  autoplot(smooth = TRUE, ylab = 'Probability', xlab = "Sea chest gratings LoF") +
  scale_y_continuous(limits = c(0.25,1)) 

p3 <- 
  rf_model %>%
  partial(pred.var = "rudder_and_shaft", prob = TRUE) %>%
  autoplot(smooth = TRUE, ylab = 'Probability', xlab = "Rudder and shaft LoF") +
  scale_y_continuous(limits = c(0.25,1))

p4 <- 
  rf_model %>%
  partial(pred.var = "hull", prob = TRUE) %>%
  autoplot(smooth = TRUE, ylab = 'Probability', xlab = "Hull LoF") +
  scale_y_continuous(limits = c(0.25,1)) +
  scale_x_continuous(limits = c(0,5))

p5 <- 
  rf_model %>%
  partial(pred.var = "propeller_and_shaft", prob = TRUE) %>%
  autoplot(smooth = TRUE, ylab = 'Probability', xlab = "Propeller and shaft LoF") +
  scale_y_continuous(limits = c(0.25,1))

p6 <- 
  rf_model %>%
  partial(pred.var = "ddss", prob = TRUE) %>%
  autoplot(smooth = TRUE, ylab = 'Probability', xlab = "DSS LoF") +
  scale_y_continuous(limits = c(0.25,1))


all_pdp <- ggarrange(p1, p2, p3, p4, p5, p6, labels = "AUTO")


ggsave(all_pdp,
       filename = 'figures/pdp_all_d1.png',
       width = 6,
       height = 4)


# pdp interaction----
other_scg_cont <-
partial(rf_model,
pred.var = c("other", "sea_chest_gratings"),
prob = TRUE) %>%
plotPartial(xlab = "Other LoF", ylab = "Sea chest grating LoF")


other_scg <-
  partial(rf_model,
          pred.var = c("other", "sea_chest_gratings"),
          prob = TRUE) %>%
  plotPartial(levelplot = F, xlab = "Other LoF", ylab = "Sea chest grating LoF")

other_scg


rf_model %>%
  partial(pred.var = c("other", "sea_chest_gratings"), prob = TRUE) %>%
  autoplot(contour = T, legend.title = "Probability")
