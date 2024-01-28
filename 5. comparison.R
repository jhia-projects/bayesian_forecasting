############################################
# Task: Produce tables for comparison

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

CONFIG = list(
  input_temp = file.path(dirname(getwd()), "python", "output", "temp"),
  temp = file.path("out", "temp"),
  out = file.path("out")
)

# Insample
insample_fit = function(s1, s2) {
  df_s1 = read_csv(file.path(CONFIG$temp, s1))
  
  insample_s1_lm_st = tibble(
    rmse_uk = sqrt(mean((df_s1$Weekend_Gross[df_s1$Country=="UK"]-df_s1$pred_y[df_s1$Country=="UK"])^2)), 
    rmse_us = sqrt(mean((df_s1$Weekend_Gross[df_s1$Country=="US"]-df_s1$pred_y[df_s1$Country=="US"])^2)), 
    mae_uk = mean(abs(df_s1$Weekend_Gross[df_s1$Country=="UK"]-df_s1$pred_y[df_s1$Country=="UK"])), 
    mae_us = mean(abs(df_s1$Weekend_Gross[df_s1$Country=="US"]-df_s1$pred_y[df_s1$Country=="US"])) 
  ) %>% pivot_longer(cols = everything()) %>% mutate(scenario = "s1") %>% 
    separate(name, sep = "_", into = c("metric", "country"))
  
  df_s2 = read_csv(file.path(CONFIG$temp, s2))
  insample_s2_lm_st = tibble(
    rmse_uk = sqrt(mean((df_s2$Weekend_Gross[df_s2$Country=="UK"]-df_s2$pred_y[df_s2$Country=="UK"])^2)), 
    rmse_us = sqrt(mean((df_s2$Weekend_Gross[df_s2$Country=="US"]-df_s2$pred_y[df_s2$Country=="US"])^2)), 
    mae_uk = mean(abs(df_s2$Weekend_Gross[df_s2$Country=="UK"]-df_s2$pred_y[df_s2$Country=="UK"])), 
    mae_us = mean(abs(df_s2$Weekend_Gross[df_s2$Country=="US"]-df_s2$pred_y[df_s2$Country=="US"])) 
  ) %>% pivot_longer(cols = everything()) %>% mutate(scenario = "s2") %>% 
    separate(name, sep = "_", into = c("metric", "country"))
  
  df_all = bind_rows(insample_s1_lm_st, insample_s2_lm_st) %>% 
    pivot_wider(names_from = c("scenario", "country"), values_from = "value")
  
  # Get name of model
  df_all = df_all %>% mutate(model = strsplit(s1, "_")[[1]][1], .before = everything())
  return(df_all)
}
insample_fit = map2_dfr(
  list.files(file.path(CONFIG$temp), pattern = "s1_insample.csv"),
  list.files(file.path(CONFIG$temp), pattern = "s2_insample.csv"),
  insample_fit
)
insample_fit = insample_fit %>% mutate(across(where(is.numeric), ~round(., 3)))
write_csv(insample_fit, file.path(CONFIG$out, "insample_fit.csv"))

# Outsample
outsample_fit = function(s1, s2) {
  df_s1 = read_csv(file.path(CONFIG$temp, s1))
  
  outsample_s1_st = tibble(
    rmse_uk = sqrt(mean((df_s1$Weekend_Gross[df_s1$Country=="UK"]-df_s1$pred_y[df_s1$Country=="UK"])^2)), 
    rmse_us = sqrt(mean((df_s1$Weekend_Gross[df_s1$Country=="US"]-df_s1$pred_y[df_s1$Country=="US"])^2)), 
    mae_uk = mean(abs(df_s1$Weekend_Gross[df_s1$Country=="UK"]-df_s1$pred_y[df_s1$Country=="UK"])), 
    mae_us = mean(abs(df_s1$Weekend_Gross[df_s1$Country=="US"]-df_s1$pred_y[df_s1$Country=="US"])) 
  ) %>% pivot_longer(cols = everything()) %>% mutate(scenario = "s1") %>% 
    separate(name, sep = "_", into = c("metric", "country"))
  
  df_s2 = read_csv(file.path(CONFIG$temp, s2))
  
  outsample_s2_st = tibble(
    rmse_uk = sqrt(mean((df_s2$Weekend_Gross[df_s2$Country=="UK"]-df_s2$pred_y[df_s2$Country=="UK"])^2)), 
    mae_uk = mean(abs(df_s2$Weekend_Gross[df_s2$Country=="UK"]-df_s2$pred_y[df_s2$Country=="UK"]))
  ) %>% pivot_longer(cols = everything()) %>% mutate(scenario = "s2") %>% 
    separate(name, sep = "_", into = c("metric", "country"))
  
  df_all = bind_rows(outsample_s1_st, outsample_s2_st) %>% 
    pivot_wider(names_from = c("scenario", "country"), values_from = "value")
  
  # Get name of model
  df_all = df_all %>% mutate(model = strsplit(s1, "_")[[1]][1], .before = everything())
  return(df_all)
}

outsample_fc = map2_dfr(
  list.files(file.path(CONFIG$temp), pattern = "s1_outsample.csv"),
  list.files(file.path(CONFIG$temp), pattern = "s2_outsample.csv"),
  outsample_fit
)
outsample_fc = outsample_fc %>% mutate(across(where(is.numeric), ~round(., 3)))
write_csv(outsample_fc, file.path(CONFIG$out, "outsample_fc.csv"))


# Outsample
var_fc = function(s1, s2) {
  df_s1 = read_csv(file.path(CONFIG$temp, s1))
  outsample_s1_var_st = tibble(
    var_uk = var(df_s1$Weekend_Gross[df_s1$Country=="UK"]-df_s1$pred_y[df_s1$Country=="UK"]), 
    var_us = var(df_s1$Weekend_Gross[df_s1$Country=="US"]-df_s1$pred_y[df_s1$Country=="US"])
  ) %>% pivot_longer(cols = everything()) %>% mutate(scenario = "s1") %>% 
    separate(name, sep = "_", into = c("metric", "country"))
  
  df_s2 = read_csv(file.path(CONFIG$temp, s2))
  outsample_s2_var_st = tibble(
    var_uk = var(df_s2$Weekend_Gross[df_s2$Country=="UK"]-df_s2$pred_y[df_s2$Country=="UK"])
  ) %>% pivot_longer(cols = everything()) %>% mutate(scenario = "s2") %>% 
    separate(name, sep = "_", into = c("metric", "country"))
  
  df_all = bind_rows(outsample_s1_var_st, outsample_s2_var_st) %>% 
    pivot_wider(names_from = c("scenario", "country"), values_from = "value")
  
  # Get name of model
  df_all = df_all %>% mutate(model = strsplit(s1, "_")[[1]][1], .before = everything())
  return(df_all)
}

var_fc_all = map2_dfr(
  list.files(file.path(CONFIG$temp), pattern = "s1_outsample.csv"),
  list.files(file.path(CONFIG$temp), pattern = "s2_outsample.csv"),
  var_fc
)
var_fc_all = var_fc_all %>% mutate(across(where(is.numeric), ~round(., 3)))
write_csv(var_fc_all, file.path(CONFIG$out, "var_fc_all.csv"))

# Factor change - RMSE and MAE
insample = read_csv(file.path(CONFIG$out, "insample_fit.csv")) %>% 
  pivot_longer(cols = c("s1_uk", "s1_us", "s2_uk", "s2_us"), values_to = "insample")
outsample = read_csv(file.path(CONFIG$out, "outsample_fc.csv")) %>% 
  pivot_longer(cols = c("s1_uk", "s1_us", "s2_uk"), values_to = "outsample")
factor_change = inner_join(insample, outsample) %>% 
  mutate(factor = outsample/insample) %>% 
  select(-c(insample, outsample)) %>% 
  pivot_wider(names_from = name, values_from = factor) %>% 
  mutate(model = as.factor(model)) %>% 
  mutate(model = fct_relevel(model, c("lm", "bayesian", "dt", "rf", "nn"))) %>% 
  arrange(model) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
write_csv(factor_change, file.path(CONFIG$out, "factor_change.csv"))

# RMSE for UK pre-international, by genres
rmse_genre = function(s2) {
  df = read_csv(file.path(CONFIG$temp, s2))
  
  outsample_s2_genre = tibble(
    thriller = sqrt(mean((df$Weekend_Gross[df$Country=="UK" & df$thriller_factor==1]-
                            df$pred_y[df$Country=="UK" & df$thriller_factor==1])^2)),
    romance = sqrt(mean((df$Weekend_Gross[df$Country=="UK" & df$romance_factor==1]-
                           df$pred_y[df$Country=="UK" & df$romance_factor==1])^2)),
    action = sqrt(mean((df$Weekend_Gross[df$Country=="UK" & df$action_factor==1]-
                          df$pred_y[df$Country=="UK" & df$action_factor==1])^2)),
    drama = sqrt(mean((df$Weekend_Gross[df$Country=="UK" & df$drama_factor==1]-
                         df$pred_y[df$Country=="UK" & df$drama_factor==1])^2)),
    comedy = sqrt(mean((df$Weekend_Gross[df$Country=="UK" & df$comedy_factor==1]-
                          df$pred_y[df$Country=="UK" & df$comedy_factor==1])^2))
  )
  df_all = outsample_s2_genre %>% mutate(model = strsplit(s2, "_")[[1]][1], .before = everything())
}
rmse_genre_all = map_dfr(list.files(file.path(CONFIG$temp), pattern = "s2_outsample.csv"), rmse_genre)
rmse_genre_all = rmse_genre_all %>% mutate(across(where(is.numeric), ~round(., 3))) %>% 
  mutate(model = as.factor(model)) %>% 
  mutate(model = fct_relevel(model, c("lm", "bayesian", "dt", "rf", "nn"))) %>% 
  arrange(model)

write_csv(rmse_genre_all, file.path(CONFIG$out, "rmse_genre_all.csv"))





