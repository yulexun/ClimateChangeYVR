#### Preamble ####
# Purpose: Downloads and saves the data from Statistics Canada
# Author: Lexun Yu
# Date: 12 November 2024
# Contact: lx.yu@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)

#### Download data ####
url <- "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=5415&Year=2013&Month=1&Day=1&time=&timeframe=3&submit=Download+Data"

ahccd_url <- "https://api.weather.gc.ca/collections/ahccd-annual/items?datetime=1840/2020&station_id__id_station=7025250&sortby=province__province,identifier__identifiant&f=csv&limit=10000&offset=0"

#### Save data ####
download.file(url, "data/01-raw_data/climateyul.csv")
download.file(ahccd_url, "data/01-raw_data/ahccdyul.csv")
