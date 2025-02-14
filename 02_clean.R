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

#------------------------------------------------------
# read and format original data
#------------------------------------------------------

library(dplyr)

# format column names and parameter column
data_1hr <- readRDS("data/data_1hr_original.rds")|>
  rename_with(tolower) |>
  rename(date_time_end = datetime, date_time_begin = date_pst) |>
  select(-station_name_full, -ems_id, -naps_id, -owner, -region) |>
  mutate(parameter = stringr::str_to_lower(parameter),
         parameter = case_when(
           parameter == "temp_mean" ~ "temp",
           parameter == "wdir_vect" ~ "wd",
           parameter == "wspd_sclr" ~ "ws",
           .default = as.character(parameter)
           ),
         instrument = stringr::str_to_lower(instrument)
         )

# remove PM2.5 teom data (collocated with SHARP) and keep one of PM10 teom or PM10 sharp - which ever is not NA
data_1hr <- data_1hr |>
  filter(instrument != "pm25_r&p_teom") |>
  group_by(parameter, date_time_end, date_time_begin, date, time, station_name) |>
  slice(which.max(!is.na(raw_value))) |>
  ungroup()


# check for duplicates
duplicates <- data_1hr |>
  select(parameter, date, time) |>
  group_by_all() |>
  filter(n() > 1) |>
  ungroup() |>
  distinct()

if(nrow(duplicates) == 0){
  rm(duplicates)
  saveRDS(data_1hr, file = "data/data_1hr.rds")

}

#------------------------------------------------------
# calculate and averaged data frames
#------------------------------------------------------

library(openair)

#data_1hr <- readRDS("data/data_1hr.rds")

data_24hr_meta <- data_1hr |>
  select(date, station_name, parameter, instrument, validation_status, flag_tfee, unit) |>
  distinct() |>
  rename(param = parameter)

data_1hr_wide <- data_1hr |>
  select(date_time_begin, parameter, raw_value) |>
  tidyr::pivot_wider(names_from = parameter, values_from = raw_value) |>
  rename(date = date_time_begin)

data_24hr_wide <- timeAverage(data_1hr_wide,
                         avg.time = "day",
                         data.thresh = 75,
                         statistic = "mean",
                         fill = TRUE)

### monthly, annual and seasonal averages
data_1m_wide <- timeAverage(data_24hr_wide,
                          avg.time = "month",
                          data.thresh = 75,
                          statistic = "mean",
                          fill = TRUE)

data_1y_wide <- timeAverage(data_24hr_wide,
                          avg.time = "year",
                          data.thresh = 75,
                          statistic = "mean",
                          fill = TRUE)

data_season_wide <- timeAverage(data_24hr_wide,
                           avg.time = "season",
                           statistic = "mean",
                           fill = TRUE)

#------------------------------------------------------
# format averaged data frames to long format, add meta data, and save to RDS
#------------------------------------------------------

data_24hr <- data_24hr_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         value = round(value, 1)) |>
  left_join(data_24hr_meta, by = c('date', 'param'))

save(data_24hr, file = "data/data_24hr.rds")


data_1y <- data_1y_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  left_join(data_24hr_meta |> select(-flag_tfee), by = c('date', 'param')) |>
  mutate(date = as.integer(format(date, "%Y")),
         value = round(value, 1))

save(data_1y, file = "data/data_1y.rds")

data_season <- data_season_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(value = round(value, 1))

save(data_season, file = "data/data_season.rds")

#------------------------------------------------------
# clean up: remove wide format dataframes
#------------------------------------------------------

rm(list = ls(pattern = "wide"))
rm(list = ls(pattern = "meta"))

#------------------------------------------------------
# data capture summary
#------------------------------------------------------







