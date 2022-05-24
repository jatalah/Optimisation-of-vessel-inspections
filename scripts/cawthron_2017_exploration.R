library(tidyverse)
library(finalfit)
caw17 <- read_csv('cleaned_data/cawthron_2017_vessel_biofouling_data_clean.csv')


caw17 %>% 
  distinct(Region)

bp_lof_caw17 <- 
  caw17 %>% 
  group_by(id, Area) %>% 
  summarise(LoF = mean(mean_LoF, na.rm = T)) %>% 
  ggplot(aes(Area, LoF)) +
  geom_boxplot(aes(fill = Area), alpha = .8, width = .3, outlier.shape = NA, na.rm = T, notchwidth = 0.5, notch = F) +
  geom_point(position = position_jitter(width = .25, height = .2), alpha = .2)  +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(breaks = 0:6) +
  theme(axis.title.y = element_blank()) +
  scale_fill_viridis_d(guide = F, option = 'A') 

ggsave(bp_lof_caw17,
       filename = 'figures/boxplots_lof_caw2017_data.jpg',
       width = 4.5, height = 5)


x <- 
  caw17 %>%
  drop_na(CRMS) %>%
  # mutate(CRMS = fct_recode(factor(CRMS), Pass = "0", Fail = "1")) %>% 
  left_join(caw17sum %>% rename(id = "Nr") %>% select(id, CRMS_Sh_fail), by = "id")
  


ggplot(x, aes(x = Area,
             fill = factor(CRMS))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion") +
  coord_flip()


y <- 
  x %>% 
  select(id, Area, CRMS, CRMS_Sh_fail) %>% 
  pivot_wider(names_from = Area, values_from = CRMS, values_fn = mean) %>% 
  select(-`Stern thruster`, -c(`Bulbous bow`:Other, -hull))

library(caret)
map(y, ~sum(is.na(.)))


trans_para <- preProcess(clean_names(y) , method = "medianImpute")
pred_t <- predict(trans_para, clean_names(y)) %>% select(crms_sh_fail, draft_mark, sea_chest, rudder_shaft, discharge_pipe    )
library(gbm)

m1 <- train(factor(crms_sh_fail)  ~ ., data = pred_t, method = "rf")
m1
varImp(m1)
confusionMatrix(m1)

randomForest(Species ~ ., data = iris, ntree=100, mtry=2, importance=TRUE)

