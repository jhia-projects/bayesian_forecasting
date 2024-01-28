############################################
# Task: Statistical learning - neural networks + gradient descent

# Last edited: 31/7/2023
############################################
rm(list=ls())
options(scipen=10)
library(tidyverse)
library(dplyr)
library(lubridate)
library(broom)
library(tidyr)
library(purrr)

# install.packages("rpart.plot")
library(rpart.plot)

CONFIG = list(
  input_temp = file.path(dirname(getwd()), "python", "output", "temp"),
  temp = file.path("out", "temp"),
  out = file.path("out")
)

# Scenario 1: historical database -----------------------------------------

# Training data
train = read_csv(file.path(CONFIG$out, "train_s1.csv")) %>% mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% select(-1)

# Test data
test = read_csv(file.path(CONFIG$out, "test_s1.csv")) %>% mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% select(-1)

# Decision trees
library(e1071)
library(rpart)
set.seed(111)
fit_tune = tune.rpart(Weekend_Gross ~ ., data=train, cp = c(0.01,0.3,0.01))
summary(fit_tune)
fit <- rpart(Weekend_Gross ~ ., data=train, cp=0.01)
rpart.plot(fit)
summary(fit)
printcp(fit) ## overall R^2 is 0.151

# In sample
insample_rpart_p = train %>% mutate(pred_y = predict(fit))
write_csv(insample_rpart_p, file.path(CONFIG$temp, "dt_s1_insample.csv"))

# Outsample
outsample_rpart_p = test %>% mutate(pred_y = predict(fit, test))
write_csv(outsample_rpart_p, file.path(CONFIG$temp, "dt_s1_outsample.csv"))

# Random forests (Bagging - bootstrap aggregation)
require(randomForest)
set.seed(71)
rf.fit <- randomForest(Weekend_Gross ~ ., data=train)
print(rf.fit)

# In sample
insample_rf_p = train %>% mutate(pred_y = predict(rf.fit))
write_csv(insample_rf_p, file.path(CONFIG$temp, "rf_s1_insample.csv"))

# Outsample
outsample_rf_p = test %>% mutate(pred_y = predict(rf.fit, test))
write_csv(outsample_rf_p, file.path(CONFIG$temp, "rf_s1_outsample.csv"))

# Nerual netowrk
library(caret)
set.seed(111)
my.grid <- expand.grid(.decay = seq(0.4, 0.5, 0.1), .size = 6:7)
nnet.fit <- train(Weekend_Gross ~ ., data = train,
                  method = "nnet", maxit = 100, tuneGrid = my.grid, trace = F, linout = 1)
summary(nnet.fit)

# In sample
insample_nn_p = train %>% mutate(pred_y = predict(nnet.fit))
write_csv(insample_nn_p, file.path(CONFIG$temp, "nn_s1_insample.csv"))

# Outsample
outsample_nn_p = test %>% mutate(pred_y = predict(nnet.fit, test))
write_csv(outsample_nn_p, file.path(CONFIG$temp, "nn_s1_outsample.csv"))

# Scenario 2 --------------------------------------------------------------

# Training data
train = read_csv(file.path(CONFIG$out, "train_s2.csv")) %>% mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% select(-1)

# Test data
test = read_csv(file.path(CONFIG$out, "test_s2.csv")) %>% mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% select(-1)

# Decision trees
set.seed(111)
fit_tune = tune.rpart(Weekend_Gross ~ ., data=train, cp = c(0.01,0.3,0.01))
summary(fit_tune)
fit <- rpart(Weekend_Gross ~ ., data=train, cp=0.01)
rpart.plot(fit)
summary(fit)
printcp(fit) ## overall R^2 is 0.148

# In sample
insample_rpart_p = train %>% mutate(pred_y = predict(fit))
write_csv(insample_rpart_p, file.path(CONFIG$temp, "dt_s2_insample.csv"))

# Outsample
outsample_rpart_p = test %>% mutate(pred_y = predict(fit, test))
write_csv(outsample_rpart_p, file.path(CONFIG$temp, "dt_s2_outsample.csv"))

# Random forests (Bagging - bootstrap aggregation)
set.seed(71)
rf.fit <- randomForest(Weekend_Gross ~ ., data=train)
print(rf.fit)

# In sample
insample_rf_p = train %>% mutate(pred_y = predict(rf.fit))
write_csv(insample_rf_p, file.path(CONFIG$temp, "rf_s2_insample.csv"))

# Outsample
outsample_rf_p = test %>% bind_rows(tibble("Country" = as.factor("US"))) %>% mutate(Country = as.factor(Country))
outsample_rf_p = outsample_rf_p %>% mutate(pred_y = predict(rf.fit, outsample_rf_p)) %>% filter(Country != "US")
write_csv(outsample_rf_p, file.path(CONFIG$temp, "rf_s2_outsample.csv"))

# Nerual netowrk
set.seed(111)
my.grid <- expand.grid(.decay = seq(0.4, 0.5, 0.1), .size = 6:7)
nnet.fit <- train(Weekend_Gross ~ ., data = train,
                  method = "nnet", maxit = 100, tuneGrid = my.grid, trace = F, linout = 1)
summary(nnet.fit)

# In sample
insample_nn_p = train %>% mutate(pred_y = predict(nnet.fit))
write_csv(insample_nn_p, file.path(CONFIG$temp, "nn_s2_insample.csv"))

# Outsample
outsample_nn_p = test %>% mutate(pred_y = predict(nnet.fit, test))
write_csv(outsample_nn_p, file.path(CONFIG$temp, "nn_s2_outsample.csv"))

# others ------------------------------------------------------------------
# 
# # Gradient boost (Adaboost)
# library(gbm)
# library(caret)
# 
# model_gbm = gbm(Weekend_Gross ~.,
#                 data = train,
#                 distribution = "gaussian",
#                 cv.folds = 10,
#                 shrinkage = .01,
#                 n.minobsinnode = 10,
#                 n.trees = 500)
# 
# print(model_gbm)
# summary(model_gbm)
# 
# test_x = test[, -1] # feature and target array
# gb_p = test %>% mutate(fitted = predict.gbm(model_gbm, test_x))
# mean((gb_p$Weekend_Gross-gb_p$fitted)^2) ## 0.970122 - very good performance
