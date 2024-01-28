############################################
# Task: Perform baseline regression

# Last edited: 1/8/2023
############################################
rm(list=ls())
options(scipen=10)
library(tidyverse)
library(dplyr)
library(lubridate)
library(broom)
library(tidyr)
library(purrr)

library(stargazer)

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

# Fit OLS model
lm_fit = lm(Weekend_Gross ~ ., data = train)
summary(lm_fit)

stargazer(lm_fit, title = "Regression results", align = T, 
          dep.var.labels=c("OLS"),
          covariate.labels=c("Country (US)", "Time", "Number of Cinemas", "ln Cumulative revenue",
                             "Thriller","Romance","Action", "Drama", "Comedy", "Star presence"),
          out=file.path(CONFIG$out, "ols", "baseline_insample.html"))

# In sample
insample_lm_p = train %>% mutate(pred_y = predict(lm_fit))
write_csv(insample_lm_p, file.path(CONFIG$temp, "lm_s1_insample.csv"))

# Outsample
outsample_lm_p = test %>% mutate(pred_y = predict(lm_fit, test))
write_csv(outsample_lm_p, file.path(CONFIG$temp, "lm_s1_outsample.csv"))

# Scenario 2 --------------------------------------------------------------

# Training data
train = read_csv(file.path(CONFIG$out, "train_s2.csv")) %>% mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% select(-1)

# Test data
test = read_csv(file.path(CONFIG$out, "test_s2.csv")) %>% mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% select(-1)

# Fit OLS model
lm_fit = lm(Weekend_Gross ~ ., data = train)
summary(lm_fit)

stargazer(lm_fit, title = "Regression results", align = T, 
          dep.var.labels=c("OLS"),
          covariate.labels=c("Country (US)", "Time", "Number of Cinemas", "ln Cumulative revenue",
                             "Thriller","Romance","Action", "Drama", "Comedy", "Star presence"),
          out=file.path(CONFIG$out, "ols", "baseline_s2_insample.html"))

# In sample
insample_lm_p = train %>% mutate(pred_y = predict(lm_fit))
write_csv(insample_lm_p, file.path(CONFIG$temp, "lm_s2_insample.csv"))

# Outsample
outsample_lm_p = test %>% mutate(pred_y = predict(lm_fit, test))
write_csv(outsample_lm_p, file.path(CONFIG$temp, "lm_s2_outsample.csv"))

