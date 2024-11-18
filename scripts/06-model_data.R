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
library(boot)


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
bayesian_model <- brm(mean_temp ~ wind_speed + total_precipitation + pressure_station +
  total_rain + gust_speed_km_h,
  data = analysis_data,
  family = gaussian(),
  prior = c(prior(normal(0, 10), class = "b"),
            prior(normal(0, 10), class = "Intercept")),
  chains = 4, iter = 2000, cores = 4)

# Summary of results
summary(bayesian_model)

# Posterior predictive checks
pp_check(bayesian_model)

analysis_data$log_mean_temp <- log(analysis_data$mean_temp + 3)


bayesian_model_log <- brm(
  formula = log_mean_temp ~ wind_speed + total_precipitation + pressure_station + 
             total_rain + gust_speed_km_h,
  data = analysis_data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),       # Priors for coefficients
    prior(normal(0, 10), class = "Intercept") # Prior for the intercept
  ),
  chains = 4, iter = 2000, cores = 4
)

# Summary of the model
summary(bayesian_model_log)

# Posterior predictive checks
pp_check(bayesian_model_log)

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

# Model Comparison
cv_glm <- cv.glm(data = analysis_data, glmfit = glm_model_log, K = 10)

# RMSE for GLM
rmse_glm <- sqrt(cv_glm$delta[1])  # Extract cross-validation error
print(rmse_glm)


#### Save model ####
saveRDS(
  model,
  file = "models/first_model_bayesian.rds"
)
