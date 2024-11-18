#### Preamble ####
# Purpose: Building GLM model and BRM model
# Author: Lexun Yu
# Date: 18 November 2024
# Contact: lx.yu@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(brms)
library(car)
library(modelsummary)
library(boot)
library(arrow)
library(MASS)


#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/cleaned_data.parquet")

### Model data ####

# MLR


m1 <- lm(mean_temp ~ wind_speed + total_precipitation + snow +
  pressure_station + max_temp + min_temp + total_rain + gust_speed_km_h, data = analysis_data)
summary(m1)

m2 <- lm(mean_temp ~ wind_speed + total_precipitation + snow +
  pressure_station + total_rain + gust_speed_km_h, data = analysis_data)
summary(m2)

m3 <- lm(mean_temp ~ wind_speed + total_precipitation +
  pressure_station + total_rain + gust_speed_km_h, data = analysis_data)
summary(m3)
AIC(m3)

# Bayesian Model
bayesian_model <- brm(
  mean_temp ~ wind_speed + total_precipitation + pressure_station +
    total_rain + gust_speed_km_h,
  data = analysis_data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 10), class = "Intercept")
  ),
  chains = 4, iter = 2000, cores = 4
)

# Summary of results
summary(bayesian_model)

# Posterior predictive checks
pp_check(bayesian_model)

hist(analysis_data$log_mean_temp)

bayesian_model_log <- brm(
  formula = log_mean_temp ~ wind_speed + total_precipitation + pressure_station +
    total_rain + gust_speed_km_h,
  data = analysis_data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"), # Priors for coefficients
    prior(normal(0, 10), class = "Intercept") # Prior for the intercept
  ),
  chains = 4, iter = 2000, cores = 4
)

# Summary of the model
summary(bayesian_model_log)

# Posterior predictive checks
pp_check(bayesian_model_log)
plot(residuals(bayesian_model_log))

# GLM
glm_model <- glm(
  mean_temp ~ wind_speed + total_precipitation + pressure_station +
    total_rain + gust_speed_km_h,
  data = analysis_data,
  family = gaussian()
)

summary(glm_model)
modelsummary(glm_model)

glm_model_log <- glm(
  log_mean_temp ~ wind_speed + total_precipitation + pressure_station +
    total_rain + gust_speed_km_h,
  data = analysis_data,
  family = gaussian()
)

summary(glm_model_log)
modelsummary(glm_model_log)


#### Validation Testing ####
set.seed(123)
train_indices <- sample(1:nrow(analysis_data), size = 0.7 * nrow(analysis_data))
train_data <- analysis_data[train_indices, ]
test_data <- analysis_data[-train_indices, ]

# GLM
glm_model <- glm_model_log

# BRM
brm_model <- bayesian_model_log

# GLM predictions
glm_predictions <- predict(glm_model, newdata = test_data)

# BRM predictions
brm_predictions <- colMeans(posterior_predict(brm_model, newdata = test_data))

# RMSE
rmse_glm <- sqrt(mean((test_data$mean_temp - glm_predictions)^2))
rmse_brm <- sqrt(mean((test_data$mean_temp - brm_predictions)^2))

# MAE
mae_glm <- mean(abs(test_data$mean_temp - glm_predictions))
mae_brm <- mean(abs(test_data$mean_temp - brm_predictions))

# Compare results
print(c(GLM_RMSE = rmse_glm, BRM_RMSE = rmse_brm))
print(c(GLM_MAE = mae_glm, BRM_MAE = mae_brm))

plot(glm_model)

#### Save model ####
saveRDS(
  brm_model,
  file = "models/brm_model.rds"
)

saveRDS(
  glm_model,
  file = "models/glm_model.rds"
)
