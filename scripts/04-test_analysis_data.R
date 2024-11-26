#### Preamble ####
# Purpose: Tests the structure and validity of the cleaned data
# Author: Lexun Yu
# Date: 17 November 2024
# Contact: lx.yu@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

data <- read_parquet("data/02-analysis_data/cleaned_data.parquet")

#### Test data ####

test_that("No NA values in the dataset", {
  # Check if any column contains NA values
  expect_false(any(is.na(cleaned_data)), info = "Dataset contains NA values")
})

test_that("Simulated dataset has correct structure", {
  # Test the number of rows
  expect_equal(nrow(cleaned_data), 610)

  # Test the number of columns
  expect_equal(ncol(cleaned_data), 20)

  # Test the column names
  expected_colnames <- c(
    "date", "wind_speed", "total_precipitation", "snow", "pressure_station",
    "max_temp", "min_temp", "mean_temp", "total_rain", "gust_speed_km_h",
    "mean_temp_F", "log_mean_temp", "log_total_precipitation", "log_gust_speed",
    "mean_temp_f_adj", "total_precipitation_adj", "gust_speed_adj", "log_wind_speed",
    "log_pressure", "total_precipitation_boxcox"
  )
  expect_equal(colnames(cleaned_data), expected_colnames)
})

test_that("Date column is correctly formatted", {
  # Test if the date column is of Date type
  expect_s3_class(cleaned_data$date, "Date")

  # Test if the dates are in the expected range
  expect_true(all(cleaned_data$date >= as.Date("1959-08-01") & cleaned_data$date <= as.Date("2010-08-01")))
})

test_that("Numeric columns have expected ranges", {
  # Test wind speed range
  expect_true(all(cleaned_data$wind_speed >= 0 & cleaned_data$wind_speed <= 30))

  # Test total precipitation range
  expect_true(all(cleaned_data$total_precipitation >= 0 & cleaned_data$total_precipitation <= 400))

  # Test snow range
  expect_true(all(cleaned_data$snow >= 0 & cleaned_data$snow <= 200))

  # Test pressure station range
  expect_true(all(cleaned_data$pressure_station >= 990 & cleaned_data$pressure_station <= 1030))

  # Test max temperature range
  expect_true(all(cleaned_data$max_temp >= -20 & cleaned_data$max_temp <= 35))

  # Test min temperature range
  expect_true(all(cleaned_data$min_temp >= -30 & cleaned_data$min_temp <= 20))

  # Test mean temperature range
  expect_true(all(cleaned_data$mean_temp >= -20 & cleaned_data$mean_temp <= 25))

  # Test total rain range
  expect_true(all(cleaned_data$total_rain >= 0 & cleaned_data$total_rain <= 400))

  # Test max gust speed
  expect_true(all(cleaned_data$gust_speed_km_h >= 0 & cleaned_data$gust_speed_km_h <= 300))
})
