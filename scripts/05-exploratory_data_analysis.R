#### Preamble ####
# Purpose: Graph and check the cleaned raw data
# Author: Lexun Yu
# Date: 18 November 2024
# Contact: lx.yu@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/cleaned_data.parquet")

#### Plot ####
attach(analysis_data)
summary(analysis_data)
hist(mean_temp, breaks = 100, main = "Mean Temperature", col = "red")
hist(mean_temp_F, breaks = 100, main = "Mean Temperature", col = "red")
hist(log_mean_temp, breaks = 100, main = "Mean Temperature", col = "red")

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
hist(wind_speed, breaks = 100, main = "Wind Speed")
hist(total_precipitation, breaks = 100, main = "Total Precipitation")
hist(snow, breaks = 100, main = "total snow")
hist(pressure_station, breaks = 100, main = "Pressure")
hist(max_temp, breaks = 100, main = "Max Temperature")
hist(min_temp, breaks = 100, main = "Min Temperature")
hist(mean_temp, breaks = 100, main = "Mean Temperature")
hist(total_rain, breaks = 100, main = "Rain")
hist(gust_speed_km_h, breaks = 100, main = "Max Gust Speed")



# Set layout for multiple plots
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1)) # Adjust margins for better fit

# Log-transformed histograms
hist(log(analysis_data$wind_speed), breaks = 100, main = "Log(Wind Speed)", xlab = "Log(Wind Speed)", col = "blue")
hist(log(analysis_data$total_precipitation + 1), breaks = 100, main = "Log(Total Precipitation)", xlab = "Log(Total Precipitation + 1)", col = "green")
hist(log(analysis_data$snow + 1), breaks = 100, main = "Log(Total Snow)", xlab = "Log(Total Snow + 1)", col = "purple")
hist(log(analysis_data$pressure_station), breaks = 100, main = "Log(Pressure)", xlab = "Log(Pressure)", col = "red")
hist(log(analysis_data$max_temp + abs(min(analysis_data$max_temp)) + 1), breaks = 100, main = "Log(Max Temperature)", xlab = "Log(Max Temp Adjusted)", col = "orange")
hist(log(abs(analysis_data$min_temp) + 1), breaks = 100, main = "Log(Min Temperature)", xlab = "Log(Abs(Min Temp) + 1)", col = "cyan")
hist(log(analysis_data$mean_temp + abs(min(analysis_data$mean_temp)) + 1), breaks = 100, main = "Log(Mean Temperature)", xlab = "Log(Mean Temp Adjusted)", col = "pink")
hist(log(analysis_data$total_rain + 1), breaks = 100, main = "Log(Total Rain)", xlab = "Log(Total Rain + 1)", col = "yellow")
hist(log(gust_speed_km_h), breaks = 100, main = "Log(Max Gust Speed)")

pairs(analysis_data[, 2:8], main = "Pairwise Scatterplots (Columns 1-8)")

selected_data <- analysis_data[, c("wind_speed", "pressure_station", "snow", "mean_temp", "total_rain", "gust_speed_km_h")]
pairs(as.data.frame(selected_data), main = "Pairwise Scatterplots (Excluding Snow, Max Temp, and Min Temp)")

selected_data <- analysis_data[, c("wind_speed", "pressure_station", "mean_temp", "total_rain", "gust_speed_km_h")]
pairs(as.data.frame(selected_data), main = "Pairwise Scatterplots (Excluding Snow, Max Temp, and Min Temp)")
