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

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/cleaned_data.parquet")

### Model data ####
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

#### Save model ####
saveRDS(
  model,
  file = "models/first_model_bayesian.rds"
)
