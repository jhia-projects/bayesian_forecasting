############################################
# Task: Perform bayesian hierarchical regression

# Last edited: 23/7/2023
############################################
rm(list=ls())
options(scipen=10)
library(tidyverse)
library(dplyr)
library(lubridate)
library(broom)
library(tidyr)
library(purrr)

library(rjags) ## Bayesian hierarchical model
library(MCMCvis) ## Output for MCMC
library(yardstick)

CONFIG = list(
  input_temp = file.path(dirname(getwd()), "python", "output", "temp"),
  temp = file.path("out", "temp"),
  out = file.path("out")
)

# Scenario 1: historical database -----------------------------------------

set.seed(111)

# Training data
train = read_csv(file.path(CONFIG$out, "train_s1.csv")) %>% 
  mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% arrange(Country)
# Test data
test = read_csv(file.path(CONFIG$out, "test_s1.csv")) %>% 
  mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% arrange(Country)

# Run hierarchical model in JAGS
dat<-list(
  Weekend_Gross=train$Weekend_Gross, 
  weekend_id=train$weekend_id, 
  Number_of_cinemas=train$Number_of_cinemas, 
  lag_total_Gross_to_date=train$lag_total_Gross_to_date, 
  thriller_factor=train$thriller_factor, 
  romance_factor=train$romance_factor, 
  action_factor=train$action_factor, 
  drama_factor=train$drama_factor, 
  comedy_factor=train$comedy_factor, 
  star_factor=train$star_factor, 
  s = train$Country, 
  n=length(train$Weekend_Gross), # Number of observations
  ncountry=length(unique(train$Country)) # Number of levels
)

## Hierarchical model spec in jags
model1<-"
model {

for ( i in 1:n ) {

# Likelihood
Weekend_Gross[i] ~ dnorm(y.hat[i], invsigma2[s[i]]) ## Note: RJAGs parameterises variance as precision (1/sigma^2)

# Expected value and variance (precision) - note that s[i] is nested indexing in bugs
y.hat[i] <- b[s[i], 1] + b[s[i], 2] * weekend_id[i] + b[s[i], 3] * Number_of_cinemas[i] + b[s[i], 4] * lag_total_Gross_to_date[i] + b[s[i], 5] * thriller_factor[i] + b[s[i], 6] * romance_factor[i] + b[s[i], 7] * action_factor[i] + b[s[i], 8] * drama_factor[i] + b[s[i], 9] * comedy_factor[i] + b[s[i], 10] * star_factor[i]
}

# Prior (2nd stage - for each country)
for(j in 1:ncountry) { 
b[j, 1:10] ~ dmnorm(meanu[], prec.Sigma[,]) 
invsigma2[j] ~ dgamma(0.01, 0.01)
sigma2[j] <- sqrt(1/invsigma2[j])
}

# Hyperpriors (3rd stage - meanu and prec.Sigma)
for(p in 1:10) {meanu[p]~dnorm(0, .0001)}

prec.Sigma[1:10, 1:10] ~ dwish(Omega[,], 10)  #Wishart is MV form of gamma on precision
Sigma[1:10, 1:10]<-inverse(prec.Sigma[,]) #get the covariance matrix on the variance scale - note that this is needed to ge inverse Wishart

for (p in 1:10 ) { sqSigma[p] <- sqrt(Sigma[p,p]) }
for ( p in 1:10 ) { for (q in 1:10 ) {
rho[p,q] <- ( Sigma[p,q] / (sqSigma[p]*sqSigma[q]) )
}}

#Set some initial values for the covariance matrix
for (j in 1:10){ for (k in 1:10){  Omega[j,k] <-equals(j,k)*1 } }

}
"

init.rng1<-list(".RNG.seed" = 1234, ".RNG.name" = "base::Mersenne-Twister")
mod1<-jags.model(file=textConnection(model1), data=dat, n.chains=1, inits =list(init.rng1))
#next, we update the model, this is the "burn in" period
update(mod1, 5000)

# Take 5000 samples
samps<-coda.samples(mod1, variable.names=c("b", "invsigma2", ## Stage 2 - report precision
                                           "meanu", "prec.Sigma" ## Stage 3 - report precision
                                           ), n.iter=5000)
summary(samps, quantiles =  c(.025, .05, .95, .975))
samps_df = data.frame(samps[[1]])

# Evaluate the mixing
MCMCtrace(samps, filename = "MCMC trace plots  - Scenario 1.pdf")

# Export the posterior estimates B
model1_est = rownames_to_column(data.frame(MCMCsummary(samps, round=2, Rhat = F, n.eff = F))) %>% 
  mutate("Percentile" = paste0("(", X2.5., ", ", X97.5., ")")) %>% 
  select(-c(X2.5., X50., X97.5.))
write_csv(model1_est, file.path(CONFIG$out,"bayesian", "bhm_s1_post.csv"))

rownames_to_column(data.frame(MCMCsummary(samps, round=2, Rhat = F, n.eff = F))) %>% 
  filter(grepl("b", rowname)) %>% 
  mutate(country = str_sub(rowname, start = 3, end=3)) %>% 
  mutate(variable = str_extract(gsub("]", "", rowname), '\\b[^,]+$') ) %>% 
  select(country, variable, mean) %>% 
  pivot_wider(names_from = country, values_from = mean) %>% 
  write_csv(., file.path(CONFIG$out, "bayesian", "bhm_s1_post_country.csv"))

# Model evaluation
s1_insample = bind_rows(
  # Prediction for UK
  train %>% 
    filter(Country=="UK") %>% 
    rowwise() %>% 
    mutate(pred_y = mean(
      rnorm(5000, 
            samps_df$b.1.1. +  weekend_id*samps_df$b.1.2. + Number_of_cinemas*samps_df$b.1.3. + lag_total_Gross_to_date*samps_df$b.1.4. + 
              thriller_factor*samps_df$b.1.5. + romance_factor*samps_df$b.1.6. + action_factor*samps_df$b.1.7. + drama_factor*samps_df$b.1.8. + 
              comedy_factor*samps_df$b.1.9. + star_factor*samps_df$b.1.10.,
            1/samps_df$invsigma2.1.
      )
    ), .after = Weekend_Gross),
  # Prediction for US
  train %>% 
    filter(Country=="US") %>% 
    rowwise() %>% 
    mutate(pred_y = mean(
      rnorm(5000, 
            samps_df$b.2.1. +  weekend_id*samps_df$b.2.2. + Number_of_cinemas*samps_df$b.2.3. + lag_total_Gross_to_date*samps_df$b.2.4. + 
              thriller_factor*samps_df$b.2.5. + romance_factor*samps_df$b.2.6. + action_factor*samps_df$b.2.7. + drama_factor*samps_df$b.2.8. + 
              comedy_factor*samps_df$b.2.9. + star_factor*samps_df$b.2.10.,
            1/samps_df$invsigma2.2.
      )
    ), .after = Weekend_Gross)
)
write_csv(s1_insample, file.path(CONFIG$temp, "bayesian_s1_insample.csv"))

# outsample
s1_outsample = bind_rows(
  # Prediction for UK
  test %>% 
    filter(Country=="UK") %>% 
    rowwise() %>% 
    mutate(pred_y = mean(
      rnorm(5000, 
            samps_df$b.1.1. +  weekend_id*samps_df$b.1.2. + Number_of_cinemas*samps_df$b.1.3. + lag_total_Gross_to_date*samps_df$b.1.4. + 
              thriller_factor*samps_df$b.1.5. + romance_factor*samps_df$b.1.6. + action_factor*samps_df$b.1.7. + drama_factor*samps_df$b.1.8. + 
              comedy_factor*samps_df$b.1.9. + star_factor*samps_df$b.1.10.,
            1/samps_df$invsigma2.1.
      )
    ), .after = Weekend_Gross),
  # Prediction for US
  test %>% 
    filter(Country=="US") %>% 
    rowwise() %>% 
    mutate(pred_y = mean(
      rnorm(5000, 
            samps_df$b.2.1. +  weekend_id*samps_df$b.2.2. + Number_of_cinemas*samps_df$b.2.3. + lag_total_Gross_to_date*samps_df$b.2.4. + 
              thriller_factor*samps_df$b.2.5. + romance_factor*samps_df$b.2.6. + action_factor*samps_df$b.2.7. + drama_factor*samps_df$b.2.8. + 
              comedy_factor*samps_df$b.2.9. + star_factor*samps_df$b.2.10.,
            1/samps_df$invsigma2.2.
      )
    ), .after = Weekend_Gross)
)
write_csv(s1_outsample, file.path(CONFIG$temp, "bayesian_s1_outsample.csv"))

rm(list=setdiff(ls(), "CONFIG"))


# Scenario 2 --------------------------------------------------------------

# Training data
train = read_csv(file.path(CONFIG$out, "train_s2.csv")) %>% 
  mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% arrange(Country)
# Test data
test = read_csv(file.path(CONFIG$out, "test_s2.csv")) %>% 
  mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id)) %>% arrange(Country)

# Run hierarchical model in JAGS
dat<-list(
  Weekend_Gross=train$Weekend_Gross, 
  weekend_id=train$weekend_id, 
  Number_of_cinemas=train$Number_of_cinemas, 
  lag_total_Gross_to_date=train$lag_total_Gross_to_date, 
  thriller_factor=train$thriller_factor, 
  romance_factor=train$romance_factor, 
  action_factor=train$action_factor, 
  drama_factor=train$drama_factor, 
  comedy_factor=train$comedy_factor, 
  star_factor=train$star_factor, 
  s = train$Country, 
  n=length(train$Weekend_Gross), # Number of observations
  ncountry=length(unique(train$Country)) # Number of levels
)

## Hierarchical model spec in jags
model1<-"
model {

for ( i in 1:n ) {

# Likelihood
Weekend_Gross[i] ~ dnorm(y.hat[i], invsigma2[s[i]]) ## Note: RJAGs parameterises variance as precision (1/sigma^2)

# Expected value and variance (precision) - note that s[i] is nested indexing in bugs
y.hat[i] <- b[s[i], 1] + b[s[i], 2] * weekend_id[i] + b[s[i], 3] * Number_of_cinemas[i] + b[s[i], 4] * lag_total_Gross_to_date[i] + b[s[i], 5] * thriller_factor[i] + b[s[i], 6] * romance_factor[i] + b[s[i], 7] * action_factor[i] + b[s[i], 8] * drama_factor[i] + b[s[i], 9] * comedy_factor[i] + b[s[i], 10] * star_factor[i]
}

# Prior (2nd stage - for each country)
for(j in 1:ncountry) { 
b[j, 1:10] ~ dmnorm(meanu[], prec.Sigma[,]) 
invsigma2[j] ~ dgamma(0.01, 0.01)
sigma2[j] <- sqrt(1/invsigma2[j])
}

# Hyperpriors (3rd stage - meanu and prec.Sigma)
for(p in 1:10) {meanu[p]~dnorm(0, .0001)}

prec.Sigma[1:10, 1:10] ~ dwish(Omega[,], 10)  #Wishart is MV form of gamma on precision
Sigma[1:10, 1:10]<-inverse(prec.Sigma[,]) #get the covariance matrix on the variance scale - note that this is needed to ge inverse Wishart

for (p in 1:10 ) { sqSigma[p] <- sqrt(Sigma[p,p]) }
for ( p in 1:10 ) { for (q in 1:10 ) {
rho[p,q] <- ( Sigma[p,q] / (sqSigma[p]*sqSigma[q]) )
}}

#Set some initial values for the covariance matrix
for (j in 1:10){ for (k in 1:10){  Omega[j,k] <-equals(j,k)*1 } }

}
"

init.rng1<-list(".RNG.seed" = 1234, ".RNG.name" = "base::Mersenne-Twister")
mod1<-jags.model(file=textConnection(model1), data=dat, n.chains=1, inits =list(init.rng1))
#next, we update the model, this is the "burn in" period
update(mod1, 5000)

# Take 5000 samples
samps<-coda.samples(mod1, variable.names=c("b", "invsigma2", ## Stage 2 - report precision
                                           "meanu", "prec.Sigma" ## Stage 3 - report precision
), n.iter=5000)
summary(samps, quantiles =  c(.025, .05, .95, .975))
samps_df = data.frame(samps[[1]])

# Evaluate the mixing
MCMCtrace(samps, filename = "MCMC trace plots  - Scenario 2.pdf")

# Export the posterior estimates B
model1_est = rownames_to_column(data.frame(MCMCsummary(samps, round=2, Rhat = F, n.eff = F))) %>% 
  mutate("Percentile" = paste0("(", X2.5., ", ", X97.5., ")")) %>% 
  select(-c(X2.5., X50., X97.5.))
write_csv(model1_est, file.path(CONFIG$out,"bayesian", "bhm_s2_post.csv"))

rownames_to_column(data.frame(MCMCsummary(samps, round=2, Rhat = F, n.eff = F))) %>% 
  filter(grepl("b", rowname)) %>% 
  mutate(country = str_sub(rowname, start = 3, end=3)) %>% 
  mutate(variable = str_extract(gsub("]", "", rowname), '\\b[^,]+$') ) %>% 
  select(country, variable, mean) %>% 
  pivot_wider(names_from = country, values_from = mean) %>% 
  write_csv(., file.path(CONFIG$out, "bayesian", "bhm_s2_post_country.csv"))

# Model evaluation
s2_insample = bind_rows(
  # Prediction for UK
  train %>% 
    filter(Country=="UK") %>% 
    rowwise() %>% 
    mutate(pred_y = mean(
      rnorm(5000, 
            samps_df$b.1.1. +  weekend_id*samps_df$b.1.2. + Number_of_cinemas*samps_df$b.1.3. + lag_total_Gross_to_date*samps_df$b.1.4. + 
              thriller_factor*samps_df$b.1.5. + romance_factor*samps_df$b.1.6. + action_factor*samps_df$b.1.7. + drama_factor*samps_df$b.1.8. + 
              comedy_factor*samps_df$b.1.9. + star_factor*samps_df$b.1.10.,
            1/samps_df$invsigma2.1.
      )
    ), .after = Weekend_Gross),
  # Prediction for US
  train %>% 
    filter(Country=="US") %>% 
    rowwise() %>% 
    mutate(pred_y = mean(
      rnorm(5000, 
            samps_df$b.2.1. +  weekend_id*samps_df$b.2.2. + Number_of_cinemas*samps_df$b.2.3. + lag_total_Gross_to_date*samps_df$b.2.4. + 
              thriller_factor*samps_df$b.2.5. + romance_factor*samps_df$b.2.6. + action_factor*samps_df$b.2.7. + drama_factor*samps_df$b.2.8. + 
              comedy_factor*samps_df$b.2.9. + star_factor*samps_df$b.2.10.,
            1/samps_df$invsigma2.2.
      )
    ), .after = Weekend_Gross)
)
write_csv(s2_insample, file.path(CONFIG$temp, "bayesian_s2_insample.csv"))

# outsample
s2_outsample = bind_rows(
  # Prediction for UK
  test %>% 
    filter(Country=="UK") %>% 
    rowwise() %>% 
    mutate(pred_y = mean(
      rnorm(5000, 
            samps_df$b.1.1. +  weekend_id*samps_df$b.1.2. + Number_of_cinemas*samps_df$b.1.3. + lag_total_Gross_to_date*samps_df$b.1.4. + 
              thriller_factor*samps_df$b.1.5. + romance_factor*samps_df$b.1.6. + action_factor*samps_df$b.1.7. + drama_factor*samps_df$b.1.8. + 
              comedy_factor*samps_df$b.1.9. + star_factor*samps_df$b.1.10.,
            1/samps_df$invsigma2.1.
      )
    ), .after = Weekend_Gross),
  # Prediction for US
  test %>% 
    filter(Country=="US") %>% 
    rowwise() %>% 
    mutate(pred_y = mean(
      rnorm(5000, 
            samps_df$b.2.1. +  weekend_id*samps_df$b.2.2. + Number_of_cinemas*samps_df$b.2.3. + lag_total_Gross_to_date*samps_df$b.2.4. + 
              thriller_factor*samps_df$b.2.5. + romance_factor*samps_df$b.2.6. + action_factor*samps_df$b.2.7. + drama_factor*samps_df$b.2.8. + 
              comedy_factor*samps_df$b.2.9. + star_factor*samps_df$b.2.10.,
            1/samps_df$invsigma2.2.
      )
    ), .after = Weekend_Gross)
)
write_csv(s2_outsample, file.path(CONFIG$temp, "bayesian_s2_outsample.csv"))

rm(list=setdiff(ls(), "CONFIG"))