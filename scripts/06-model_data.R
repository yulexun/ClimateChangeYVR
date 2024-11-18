#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(brms)
library(car)
library(modelsummary)

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/cleaned_data.parquet")

### Model data ####

# MLR


m1 <- lm(mean_temp ~ wind_speed + total_precipitation + snow + 
  pressure_station + max_temp + min_temp + total_rain + gust_speed_km_h, data=analysis_data)
summary(m1)

m2 <- lm(mean_temp ~ wind_speed + total_precipitation + snow + 
  pressure_station + total_rain + gust_speed_km_h, data=analysis_data)
summary(m2)

m3 <- lm(mean_temp ~ wind_speed + total_precipitation +
  pressure_station + total_rain + gust_speed_km_h, data=analysis_data)
summary(m3)
AIC(m3)

# Bayesian Model
formula <- bf(mean_temp ~ wind_speed + pressure_station + total_rain + gust_speed_km_h)

priors <- c(
  prior(normal(0, 5), class = "b"),
  prior(normal(0, 10), class = "Intercept")
)

model <- brm(
    formula = formula,
    data = analysis_data,
    family = gaussian(),
    prior = priors,
    chains = 4, 
    iter = 2000, 
    warmup = 500, 
    cores = 4
)

summary(model)

pp_check(model)
bayes_R2(model)
summary(m1)$r.squared
summary(m1)$adj.r.squared

# GLM
glm_model <- glm(mean_temp ~ wind_speed + total_precipitation + pressure_station + 
  total_rain + gust_speed_km_h,
  data = analysis_data,
  family = gaussian())

summary(glm_model)
modelsummary(glm_model)

glm_model_log <- glm(log(mean_temp) ~ wind_speed + total_precipitation + pressure_station +
                 total_rain + gust_speed_km_h,
                 data = analysis_data,
                 family = gaussian())

summary(glm_model_log)
modelsummary(glm_model_log)

#### Save model ####
saveRDS(
  model,
  file = "models/first_model_bayesian.rds"
)
