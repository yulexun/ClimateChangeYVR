#### Preamble ####
# Purpose: Tests the structure and validity of the simulated data
# Author: Lexun Yu
# Date: 17 November 2024
# Contact: lx.yu@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
# Load required packages
library(testthat)
library(dplyr)
library(arrow)

simulated_data <- read_parquet("data/00-simulated_data/simulated_data.parquet")

# Define the tests

test_that("No NA values in the dataset", {
  # Check if any column contains NA values
  expect_false(any(is.na(simulated_data)), info = "Dataset contains NA values")
})

test_that("Simulated dataset has correct structure", {
  # Test the number of rows
  expect_equal(nrow(simulated_data), 132)

  # Test the number of columns
  expect_equal(ncol(simulated_data), 10)

  # Test the column names
  expected_colnames <- c(
    "date", "wind_speed", "total_precipitation", "snow", "pressure_station",
    "max_temp", "min_temp", "mean_temp", "total_rain", "total_snow"
  )
  expect_equal(colnames(simulated_data), expected_colnames)
})

test_that("Date column is correctly formatted", {
  # Test if the date column is of Date type
  expect_s3_class(simulated_data$date, "Date")

  # Test if the dates are in the expected range
  expect_true(all(simulated_data$date >= as.Date("1960-01-01") & simulated_data$date <= as.Date("1970-12-01")))
})

test_that("Numeric columns have expected ranges", {
  # Test wind speed range
  expect_true(all(simulated_data$wind_speed >= 10 & simulated_data$wind_speed <= 30))

  # Test total precipitation range
  expect_true(all(simulated_data$total_precipitation >= 50 & simulated_data$total_precipitation <= 250))

  # Test snow range
  expect_true(all(simulated_data$snow >= 0 & simulated_data$snow <= 200))

  # Test pressure station range
  expect_true(all(simulated_data$pressure_station >= 990 & simulated_data$pressure_station <= 1030))

  # Test max temperature range
  expect_true(all(simulated_data$max_temp >= -10 & simulated_data$max_temp <= 35))

  # Test min temperature range
  expect_true(all(simulated_data$min_temp >= -20 & simulated_data$min_temp <= 20))

  # Test mean temperature range
  expect_true(all(simulated_data$mean_temp >= -15 & simulated_data$mean_temp <= 25))

  # Test total rain range
  expect_true(all(simulated_data$total_rain >= 0 & simulated_data$total_rain <= 200))

  # Test total snow range
  expect_true(all(simulated_data$total_snow >= 0 & simulated_data$total_snow <= 150))
})
