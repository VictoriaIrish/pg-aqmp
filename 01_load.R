# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#Retrieve data from ENVAIR package
install.packages("remotes")
remotes::install_github("bcgov/envair")
library(envair)

#load pm25 data for PG
PG_pm25_data <- importBC_data_avg("pm25",
                                      2010:2023,
                                      averaging_type = "24-hr",
                                      data_threshold = 0.75) %>%
  filter(., station_name %in% "Prince George Plaza 400")

#load pm10 data for PG

#load trs data for PG


#data capture for pm2.5 in PG
pm25_PG_data_captures <- get_captures(param = c('pm25'),
                                   years = 2010:2023,
                                   merge_Stations = TRUE) %>%
  filter(., station_name %in% "Prince George Plaza 400")

#data capture for pm10 in PG - WEIRD THE DATA CAPTURE SEEMS TO WORK FOR
pm10_PG_data_captures <- get_captures(param = c('pm10'),
                                      years = 2010:2023,
                                      merge_Stations = TRUE) %>%
  filter(., station_name %in% "Prince George Plaza 400")

#data capture for trs in PG
trs_PG_data_captures <- get_captures(param = c('trs'),
                                     years = 2010:2023,
                                     merge_Stations = TRUE) %>%
  filter(., station_name %in% "Prince George Plaza 400")
