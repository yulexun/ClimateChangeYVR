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

set.seed(123)
train_indices <- sample(1:nrow(analysis_data), size = 0.7 * nrow(analysis_data))
train_data <- analysis_data[train_indices, ]
test_data <- analysis_data[-train_indices, ]

# MLR

m1 <- lm(mean_temp_F ~ wind_speed + total_precipitation + snow +
  pressure_station + max_temp + min_temp + total_rain + gust_speed_km_h, data = train_data)
summary(m1)

m2 <- lm(mean_temp_F ~ wind_speed + total_precipitation + snow +
  pressure_station + total_rain + gust_speed_km_h, data = train_data)
summary(m2)

m3 <- lm(mean_temp_F ~ wind_speed + total_precipitation +
  pressure_station + total_rain + gust_speed_km_h, data = train_data)
summary(m3)

m4 <- lm(mean_temp_F ~ wind_speed +
  pressure_station + total_rain + gust_speed_km_h, data = train_data)
summary(m4)

AIC(m3)
AIC(m4)
# Bayesian Model
hist(train_data$log_mean_temp)


bayesian_model_log <- brm(
  formula = log_mean_temp ~ wind_speed + total_precipitation + pressure_station +
    total_rain + gust_speed_km_h,
  data = train_data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"), # Priors for coefficients
    prior(normal(0, 10), class = "Intercept") # Prior for the intercept
  ),
  chains = 4, iter = 2000, cores = 4
)

# Summary of the model
summary(bayesian_model_log)
modelsummary(bayesian_model_log)

# Posterior predictive checks
pp_check(bayesian_model_log)
plot(residuals(bayesian_model_log))

# GLM
glm_model <- glm(
  mean_temp_F ~ wind_speed + total_precipitation + pressure_station +
    total_rain + gust_speed_km_h,
  data = train_data,
  family = gaussian()
)

summary(glm_model)
modelsummary(glm_model)

glm_model_log <- glm(
  log_mean_temp ~ wind_speed + total_precipitation + pressure_station +
    total_rain + gust_speed_km_h,
  data = train_data,
  family = gaussian()
)

summary(glm_model_log)
modelsummary(glm_model_log)


#### Validation Testing ####
#### Validation Testing ####

# GLM
glm_model <- glm_model_log

# BRM
brm_model <- bayesian_model_log

# GLM predictions (reverse log transformation and convert to Celsius)
glm_predictions_log <- predict(glm_model, newdata = test_data)
glm_predictions_F <- exp(glm_predictions_log)  # Predicted in Fahrenheit
glm_predictions <- (glm_predictions_F - 32) * 5 / 9  # Convert to Celsius

# BRM predictions (posterior predictive mean, reverse log transformation and convert to Celsius)
brm_predictions_log <- colMeans(posterior_predict(brm_model, newdata = test_data))
brm_predictions_F <- exp(brm_predictions_log)  # Predicted in Fahrenheit
brm_predictions <- (brm_predictions_F - 32) * 5 / 9  # Convert to Celsius

# Ensure consistency: Predicted and actual values in Celsius
test_data <- test_data %>%
  mutate(
    actual_mean_temp = mean_temp,      # Actual values (Celsius)
    glm_predicted = glm_predictions,  # GLM predictions (Celsius)
    brm_predicted = brm_predictions   # Bayesian predictions (Celsius)
  )

# Calculate RMSE
rmse_glm <- sqrt(mean((test_data$actual_mean_temp - test_data$glm_predicted)^2))
rmse_brm <- sqrt(mean((test_data$actual_mean_temp - test_data$brm_predicted)^2))

# Calculate MAE
mae_glm <- mean(abs(test_data$actual_mean_temp - test_data$glm_predicted))
mae_brm <- mean(abs(test_data$actual_mean_temp - test_data$brm_predicted))

# Output Results
print(c(GLM_RMSE = rmse_glm, BRM_RMSE = rmse_brm))
print(c(GLM_MAE = mae_glm, BRM_MAE = mae_brm))

# Visualize actual vs predicted
library(ggplot2)

# GLM vs Actual
ggplot(test_data, aes(x = actual_mean_temp)) +
  geom_point(aes(y = glm_predicted, color = "GLM Predictions")) +
  geom_point(aes(y = actual_mean_temp, color = "Actual Values")) +
  labs(
    title = "GLM Model: Actual vs Predicted Mean Temperature",
    x = "Actual Mean Temperature (°C)",
    y = "Predicted Mean Temperature (°C)",
    color = "Legend"
  ) +
  theme_minimal()

# BRM vs Actual
ggplot(test_data, aes(x = actual_mean_temp)) +
  geom_point(aes(y = brm_predicted, color = "BRM Predictions")) +
  geom_point(aes(y = actual_mean_temp, color = "Actual Values")) +
  labs(
    title = "Bayesian Model: Actual vs Predicted Mean Temperature",
    x = "Actual Mean Temperature (°C)",
    y = "Predicted Mean Temperature (°C)",
    color = "Legend"
  ) +
  theme_minimal()

# Combined Visualization for GLM and BRM
ggplot(test_data, aes(x = actual_mean_temp)) +
  geom_point(aes(y = glm_predicted, color = "GLM Predictions")) +
  geom_point(aes(y = brm_predicted, color = "BRM Predictions")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "GLM vs BRM: Actual vs Predicted Mean Temperature",
    x = "Actual Mean Temperature (°C)",
    y = "Predicted Mean Temperature (°C)",
    color = "Legend"
  ) +
  theme_minimal()

# Tabular Comparison
model_comparison <- tibble(
  Model = c("GLM", "Bayesian"),
  RMSE = c(rmse_glm, rmse_brm),
  MAE = c(mae_glm, mae_brm)
)

print(model_comparison)
