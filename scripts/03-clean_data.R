#### Preamble ####
# Purpose: Cleans the raw data
# Author: Lexun Yu
# Date: 16 November 2024
# Contact: lx.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(arrow)

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

data_ahccd_validate <- raw_data_ahccd[
  raw_data_ahccd$date_time >= as.Date("2010-09-01") &
    raw_data_ahccd$date_time <= as.Date("2013-04-01"),
]

data_climate_validate <- raw_data_climate[
  raw_data_climate$date_time >= as.Date("2010-09-01") &
    raw_data_climate$date_time <= as.Date("2013-04-01"),
]

# only keep variables related to this research
# Define the variables to keep for both datasets
variables_to_keep <- c(
  "date_time", "wind_speed_vitesse_vent", "total_precip_precip_totale",
  "snow_neige", "pressure_station_pression_station"
)

# For the first dataset (data_ahccd)
cleaned_data_ahccd <- data_ahccd %>%
  select(all_of(variables_to_keep))

cleaned_data_ahccd_validate <- data_ahccd_validate %>%
  select(all_of(variables_to_keep))

# For the second dataset (data_climate)
variables_to_keep_climate <- c(
  "date_time", "mean_max_temp_c", "mean_min_temp_c",
  "mean_temp_c", "total_rain_mm", "spd_of_max_gust_km_h"
)

cleaned_data_climate <- data_climate %>%
  select(all_of(variables_to_keep_climate))

cleaned_data_climate_validate <- data_climate_validate %>%
  select(all_of(variables_to_keep_climate))

cleaned_data <- full_join(cleaned_data_ahccd, cleaned_data_climate, by = "date_time")
cleaned_data_validate <- full_join(cleaned_data_ahccd_validate, cleaned_data_climate_validate, by = "date_time")

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

  cleaned_data_validate <- cleaned_data_validate %>%
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

cleaned_data_validate <-
  na.omit(cleaned_data_validate)

cleaned_data$mean_temp_F <- (cleaned_data$mean_temp * 1.8) + 32
cleaned_data$log_mean_temp <- log(cleaned_data$mean_temp_F)

cleaned_data_validate$mean_temp_F <- (cleaned_data_validate$mean_temp * 1.8) + 32
cleaned_data_validate$log_mean_temp <- log(cleaned_data_validate$mean_temp_F)

cleaned_data$log_total_precipitation <- log(cleaned_data$total_precipitation)
cleaned_data$log_gust_speed <- log(cleaned_data$gust_speed_km_h)

# Add a constant to ensure all values are positive
cleaned_data$mean_temp_f_adj <- cleaned_data$mean_temp_F
cleaned_data$total_precipitation_adj <- cleaned_data$total_precipitation
cleaned_data$gust_speed_adj <- cleaned_data$gust_speed_km_h

library(MASS)

# Fit a simple intercept-only model
mean_temp_model <- lm(mean_temp_f_adj ~ 1, data = cleaned_data)

# Find the optimal lambda for Box-Cox
boxcox_mean_temp <- boxcox(mean_temp_model, lambda = seq(-2, 2, by = 0.1))
lambda_mean_temp <- boxcox_mean_temp$x[which.max(boxcox_mean_temp$y)]
cat("Optimal lambda for mean_temp_f:", lambda_mean_temp, "\n")

# Apply the transformation
if (lambda_mean_temp == 0) {
  cleaned_data$mean_temp_f_boxcox <- log(cleaned_data$mean_temp_f_adj)
} else {
  cleaned_data$mean_temp_f_boxcox <- (cleaned_data$mean_temp_f_adj^lambda_mean_temp - 1) / lambda_mean_temp
}

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

# Fit a simple intercept-only model
gust_speed_model <- lm(gust_speed_adj ~ 1, data = cleaned_data)

# Find the optimal lambda for Box-Cox
boxcox_gust_speed <- boxcox(gust_speed_model, lambda = seq(-2, 2, by = 0.1))
lambda_gust_speed <- boxcox_gust_speed$x[which.max(boxcox_gust_speed$y)]
cat("Optimal lambda for gust_speed:", lambda_gust_speed, "\n")

# Apply the transformation
if (lambda_gust_speed == 0) {
  cleaned_data$gust_speed_boxcox <- log(cleaned_data$gust_speed_adj)
} else {
  cleaned_data$gust_speed_boxcox <- (cleaned_data$gust_speed_adj^lambda_gust_speed - 1) / lambda_gust_speed
}

ggplot(cleaned_data, aes(x = log_mean_temp)) +
  geom_histogram(fill = "blue", alpha = 0.7) +
  labs(title = "Log-Transformed Mean Temp", x = "Log(Mean Temp)", y = "Frequency") +
  theme_minimal()

# Plot for total_precipitation
ggplot(cleaned_data, aes(x = total_precipitation_boxcox)) +
  geom_histogram(binwidth = 0.1, fill = "green", alpha = 0.7) +
  labs(title = "Box-Cox Transformed Total Precipitation", x = "Transformed Total Precipitation", y = "Frequency") +
  theme_minimal()

# Plot for gust_speed
ggplot(cleaned_data, aes(x = log_gust_speed)) +
  geom_histogram(binwidth = 0.1, fill = "purple", alpha = 0.7) +
  labs(title = "Log-Transformed Gust Speed", x = "Log(Gust Speed)", y = "Frequency") +
  theme_minimal()

write_parquet(cleaned_data, "data/02-analysis_data/cleaned_data.parquet")
write_parquet(cleaned_data_validate, "data/02-analysis_data/cleaned_data_validate.parquet")