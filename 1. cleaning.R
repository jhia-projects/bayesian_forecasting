############################################
# Task: Clean the dataset for analysis

# Last edited: 2/7/2023
############################################
rm(list=ls())
library(tidyverse)
library(dplyr)
library(lubridate)
library(broom)
library(tidyr)
library(purrr)
library(stargazer)

CONFIG = list(
  input_temp = file.path(dirname(getwd()), "python", "output", "temp"),
  out = file.path("out")
)
# Clean the movies dataset ------------------------------------------------

# Load in movie datasets
uk_movies = read_csv(file.path(CONFIG$input_temp, "uk_movies_attributes.csv")) 
us_movies = read_csv(file.path(CONFIG$input_temp, "us_movies_attributes.csv")) %>% 
  filter(us_release_date >= "2012-01-01" & us_release_date <= "2018-01-01")
# Identify common movies
common_movies = inner_join(uk_movies %>% distinct(Release), us_movies %>% distinct(Release))
movies = bind_rows(
  uk_movies %>% inner_join(common_movies), 
  us_movies %>% inner_join(common_movies)
) %>% 
  # Classify movies according to 'historic' or 'forecast' set
  mutate(year = year(us_release_date)) %>% 
  mutate(cat = if_else(year <= 2015, "Historic", "Forecast"))

# Check that the data is on a release-country-weekend-id level
exclude = movies %>% group_by(Release, Country, weekend_id) %>% summarise(count = n()) %>% ungroup() %>% filter(count > 1) %>% distinct(Release)
## Mama, Noah, Non-Stop, The Master, Wild - exclude them from analysis, not sure whether the movie pairing is correct

# Exclude ambiguous movies from analysis
movies_2 = anti_join(movies, exclude) %>% 
  # Select necessary columns
  select(Release, Country, 
         Weekend_Gross, weekend_id, Number_of_cinemas, Total_Gross_to_date, 
         thriller_factor, romance_factor, action_factor, drama_factor, comedy_factor, star_factor,
         cat, us_release_date) %>%
  arrange(Release, Country, weekend_id)

rm(uk_movies, us_movies, common_movies, movies, exclude)

# Create clean dataset with lag variables for cumulative revenue
movies_clean = movies_2 %>% 
  group_by(Country, Release) %>% 
  # Generate lagged cumulative variable 
  mutate(lag_total_Gross_to_date = lag(Total_Gross_to_date, 1), .after=Total_Gross_to_date) %>% 
  select(-Total_Gross_to_date) %>% 
  # Log transform the revenue variables
  mutate(across(c(Weekend_Gross, lag_total_Gross_to_date), ~log(.))) %>% 
  mutate(lag_total_Gross_to_date = if_else(is.na(lag_total_Gross_to_date), 0, lag_total_Gross_to_date)) %>% 
  ungroup() %>% 
  # Generate movie id
  mutate(movie_id = as.numeric(as.factor(Release)), .before = everything()) %>% 
  # Get rid of unnecessary variables
  select(-c(Release, us_release_date))
write_csv(movies_clean, file.path(CONFIG$out, "movies_clean.csv"))

# Descriptive statistics --------------------------------------------------
descriptive = movies_2 %>% 
  group_by(Country) %>% 
  summarise(
    "Number of movies" = n_distinct(Release),
    "Mean weekly sales (million £)" = round(mean(Weekend_Gross/10^6), 2),
    "Mean first-week sales (million £)" = round(mean(Weekend_Gross[weekend_id==1]/10^6), 2),
    "Mean screens per week" = round(mean(Number_of_cinemas), 0),
    "Mean running weeks per movie" = round(mean(weekend_id), 0),
    "Longest movie run" = max(weekend_id)
  )
write_csv(descriptive, file.path(CONFIG$out, "descriptive.csv"))

range(movies_2$us_release_date) ## 2012 to 2017
movies_2 %>% distinct(Country, Release) %>% nrow() ## 1086 country-release combinations
movies_2 %>% group_by(cat) %>% summarise(count = n_distinct(Release)) ## 374 historic, 168 forecast

# Prepare dataset for 2 information scenarios -----------------------------

# Scenario 1: train on all historical data, forecast on 1st week UK and US
movies_clean = read_csv(file.path(CONFIG$out, "movies_clean.csv")) %>% 
  relocate(Country, .after = Weekend_Gross) %>% 
  mutate(Country = as.factor(Country), weekend_id = as.numeric(weekend_id))

# Training data
train = movies_clean %>% filter(cat=="Historic") %>% select(-cat)
write_csv(train, file.path(CONFIG$out, "train_s1.csv"))

# Test data
test = movies_clean %>% filter(cat=="Forecast" & weekend_id == 1) %>% select(-cat)
write_csv(test, file.path(CONFIG$out, "test_s1.csv"))
rm(train, test)

# Scenario 2: train on all historical data, US first week. Forecast on 1st week UK
# Training data
train = movies_clean %>% 
  filter(cat=="Historic" | (cat=="Forecast" & Country == "US" & weekend_id == 1)) %>% 
  select(-cat)
write_csv(train, file.path(CONFIG$out, "train_s2.csv"))

# Test data
test = movies_clean %>% filter(cat=="Forecast" & weekend_id == 1 & Country == "UK") %>% select(-cat)
write_csv(test, file.path(CONFIG$out, "test_s2.csv"))
rm(train, test)




