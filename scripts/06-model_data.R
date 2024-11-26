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
write_parquet(train_data, "data/02-analysis_data/train_data.parquet")
write_parquet(test_data, "data/02-analysis_data/test_data.parquet")

# MLR

m1 <- lm(mean_temp_F ~ wind_speed + total_precipitation + snow +
  pressure_station + max_temp + min_temp + total_rain + gust_speed_km_h, data = train_data)
summary(m1)

m4 <- lm(mean_temp_F ~ wind_speed +
  pressure_station + total_precipitation + gust_speed_km_h, data = train_data)
summary(m4)

AIC(m4)
# Bayesian Model

bayesian_model_log <- brm(
  formula = log_mean_temp ~ wind_speed + pressure_station +
    total_precipitation_boxcox + log_gust_speed,
  data = train_data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"), # Priors for coefficients
    prior(normal(0, 10), class = "Intercept") # Prior for the intercept
  ),
  chains = 4, iter = 2000, cores = 4
)

# GLM

glm_model_log <- lm(log_mean_temp ~ log_wind_speed + log_pressure +
  total_precipitation_boxcox + log_gust_speed, data = train_data)

# Adding polynomial transformations to the model
glm_model_poly <- lm(
  log_mean_temp ~
    poly(log_wind_speed, 2) + # Quadratic polynomial for wind_speed
    poly(log_pressure, 2) + # Quadratic polynomial for pressure_station
    poly(total_precipitation_boxcox, 2) + # Quadratic polynomial for total_precipitation_boxcox
    poly(log_gust_speed, 2), # Quadratic polynomial for log_gust_speed
  data = train_data
)

#### Save Model ####
saveRDS(glm_model_poly, "models/glm_poly.rds")
saveRDS(glm_model_log, "models/glm_log.rds")
saveRDS(bayesian_model_log, "models/brm.rds")
saveRDS(m1, "models/m1.rds")
saveRDS(m4, "models/m4.rds")
