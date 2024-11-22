#### Preamble ####
# Purpose: Replicated graphs from... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(rstantools)
library(brms)

validate_data <- read_parquet("data/02-analysis_data/cleaned_data_validate.parquet")
bayesian_model_log <- readRDS("models/brm_model.rds")
glm_model_log <- readRDS("models/glm_model.rds")

# #### BRM ####
# # Extract relevant predictor variables from `validate_data`
# predictors <- validate_data %>%
#   select(wind_speed, total_precipitation, pressure_station, total_rain, gust_speed_km_h)

# # Predict log-transformed mean temperature (log_mean_temp in Fahrenheit)
# predicted_log_mean_temp <- posterior_epred(
#   bayesian_model_log,
#   newdata = predictors
# )

# # Convert log-transformed predictions to the original scale (mean temperature in Fahrenheit)
# predicted_mean_temp_F <- exp(predicted_log_mean_temp)

# # Convert predicted mean temperature from Fahrenheit to Celsius
# predicted_mean_temp_C <- (predicted_mean_temp_F - 32) * 5 / 9

# # Calculate the mean prediction (in Celsius) for each observation
# predicted_summary <- apply(predicted_mean_temp_C, 2, function(x) {
#   mean(x)
# })

# # Add the real response (mean_temp in Celsius) and predictions to the validation dataset
# comparison <- validate_data %>%
#   mutate(
#     predicted_mean_temp = predicted_summary
#   ) %>%
#   select(date, mean_temp, predicted_mean_temp)

# # Compare real vs predicted responses
# print(comparison)

# comparison_brm <- comparison

# # Visualize the comparison between actual and predicted mean temperatures (without confidence interval)
# library(ggplot2)

# ggplot(comparison, aes(x = date)) +
#   geom_line(aes(y = mean_temp, color = "Actual")) +
#   geom_line(aes(y = predicted_mean_temp, color = "Predicted")) +
#   labs(
#     title = "Comparison of Actual and Predicted Mean Temperatures",
#     x = "Date",
#     y = "Mean Temperature (°C)",
#     color = "Legend"
#   ) +
#   theme_minimal()

#### GLM ####
# Extract relevant predictor variables from the validation dataset
predictors <- validate_data %>%
  select(wind_speed, total_precipitation, pressure_station, total_rain, gust_speed_km_h)

# Predict log_mean_temp (in Fahrenheit) using the GLM model
predicted_log_mean_temp <- predict(
  glm_model_log,
  newdata = predictors,
  type = "response"
)

# Reverse the log transformation to get mean temperature in Fahrenheit
predicted_mean_temp_F <- exp(predicted_log_mean_temp)

# Convert the predicted mean temperature from Fahrenheit to Celsius
predicted_mean_temp_C <- (predicted_mean_temp_F - 32) * 5 / 9

# Add the predictions to the validation dataset for comparison
comparison <- validate_data %>%
  mutate(
    predicted_mean_temp = predicted_mean_temp_C
  ) %>%
  select(date, mean_temp, predicted_mean_temp)

# Compare actual vs predicted mean temperatures
print(comparison)

comparison_glm <- comparison
# Visualize the comparison between actual and predicted mean temperatures
library(ggplot2)

ggplot(comparison, aes(x = date)) +
  geom_line(aes(y = mean_temp, color = "Actual")) +
  geom_line(aes(y = predicted_mean_temp, color = "Predicted")) +
  labs(
    title = "Comparison of Actual and Predicted Mean Temperatures (GLM)",
    x = "Date",
    y = "Mean Temperature (°C)",
    color = "Legend"
  ) +
  theme_minimal()

#### BRM reverse ####

