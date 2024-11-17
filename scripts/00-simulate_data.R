#### Preamble ####
# Purpose: Simulates a dataset of YUL weather data
# Author: Lexun Yu
# Date: 17 November 2024
# Contact: lx.yu@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(arrow)
set.seed(853)

# Generate simulated data based on the cleaned_data structure
simulated_data <- tibble(
  date = seq(as.Date("1960-01-01"), as.Date("1970-12-01"), by = "month"), # Simulated dates
  wind_speed = runif(n = 132, min = 10, max = 30), # Random wind speed between 10 and 30
  total_precipitation = runif(n = 132, min = 50, max = 250), # Random precipitation between 50 and 250 mm
  snow = runif(n = 132, min = 0, max = 200), # Random snow between 0 and 200 cm
  pressure_station = runif(n = 132, min = 990, max = 1030), # Random pressure between 990 and 1030 hPa
  max_temp = runif(n = 132, min = -10, max = 35), # Random max temperature between -10°C and 35°C
  min_temp = runif(n = 132, min = -20, max = 20), # Random min temperature between -20°C and 20°C
  mean_temp = runif(n = 132, min = -15, max = 25), # Random mean temperature between -15°C and 25°C
  total_rain = runif(n = 132, min = 0, max = 200), # Random total rain between 0 and 200 mm
  total_snow = runif(n = 132, min = 0, max = 150) # Random total snow between 0 and 150 cm
)

write_parquet(simulated_data, "data/00-simulated_data/simulated_data.parquet")
