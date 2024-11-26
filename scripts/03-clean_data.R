#### Preamble ####
# Purpose: Cleans the raw data
# Author: Lexun Yu
# Date: 16 November 2024
# Contact: lx.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(lubridate)
library(arrow)
library(MASS)

#### Clean data ####
raw_data_climate <- read_csv("data/01-raw_data/climateyvr.csv")
raw_data_ahccd <- read_csv("data/01-raw_data/ahccdyvr.csv")

raw_data_climate <-
  raw_data_climate |>
  janitor::clean_names()

raw_data_ahccd <-
  raw_data_ahccd |>
  janitor::clean_names()

# convert time

raw_data_ahccd$date_time <- ymd(paste0(raw_data_ahccd$date, "-01"))
raw_data_climate$date_time <- ymd(paste0(raw_data_climate$date_time, "-01"))

# keep data from 1960 to 2010

data_ahccd <- raw_data_ahccd[
  raw_data_ahccd$date_time >= as.Date("1959-08-01") &
    raw_data_ahccd$date_time <= as.Date("2010-08-01"),
]

data_climate <- raw_data_climate[
  raw_data_climate$date_time >= as.Date("1959-08-01") &
    raw_data_climate$date_time <= as.Date("2010-08-01"),
]


# only keep variables related to this research

# For the first dataset (data_ahccd)
cleaned_data_ahccd <- data_ahccd %>%
  dplyr::select("date_time", "wind_speed_vitesse_vent", "total_precip_precip_totale",
  "snow_neige", "pressure_station_pression_station")


# For the second dataset (data_climate)
variables_to_keep_climate <- c(
  "date_time", "mean_max_temp_c", "mean_min_temp_c",
  "mean_temp_c", "total_rain_mm", "spd_of_max_gust_km_h"
)

cleaned_data_climate <- data_climate %>%
  dplyr::select(all_of(variables_to_keep_climate))

cleaned_data <- full_join(cleaned_data_ahccd, cleaned_data_climate, by = "date_time")

cleaned_data <- cleaned_data %>%
  rename(
    date = date_time,
    wind_speed = wind_speed_vitesse_vent,
    total_precipitation = total_precip_precip_totale,
    snow = snow_neige,
    pressure_station = pressure_station_pression_station,
    max_temp = mean_max_temp_c,
    min_temp = mean_min_temp_c,
    mean_temp = mean_temp_c,
    total_rain = total_rain_mm,
    gust_speed_km_h = spd_of_max_gust_km_h
  )


cleaned_data <-
  na.omit(cleaned_data)


cleaned_data$mean_temp_F <- (cleaned_data$mean_temp * 1.8) + 32
cleaned_data$log_mean_temp <- log(cleaned_data$mean_temp_F)


cleaned_data$log_total_precipitation <- log(cleaned_data$total_precipitation)
cleaned_data$log_gust_speed <- log(cleaned_data$gust_speed_km_h)

# Add a constant to ensure all values are positive
cleaned_data$mean_temp_f_adj <- cleaned_data$mean_temp_F
cleaned_data$total_precipitation_adj <- cleaned_data$total_precipitation
cleaned_data$gust_speed_adj <- cleaned_data$gust_speed_km_h

cleaned_data$log_wind_speed <- log(cleaned_data$wind_speed)
cleaned_data$log_pressure <- log(cleaned_data$pressure_station)


# Fit a simple intercept-only model
precipitation_model <- lm(total_precipitation_adj ~ 1, data = cleaned_data)

# Find the optimal lambda for Box-Cox
boxcox_precipitation <- boxcox(precipitation_model, lambda = seq(-2, 2, by = 0.1))
lambda_precipitation <- boxcox_precipitation$x[which.max(boxcox_precipitation$y)]
cat("Optimal lambda for total_precipitation:", lambda_precipitation, "\n")

# Apply the transformation
if (lambda_precipitation == 0) {
  cleaned_data$total_precipitation_boxcox <- log(cleaned_data$total_precipitation_adj)
} else {
  cleaned_data$total_precipitation_boxcox <- (cleaned_data$total_precipitation_adj^lambda_precipitation - 1) / lambda_precipitation
}

write_parquet(cleaned_data, "data/02-analysis_data/cleaned_data.parquet")
