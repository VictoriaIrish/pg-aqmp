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
  rename(param = parameter,
         date_time_end = datetime) |>
  select(param, date_time_end, station_name, instrument, raw_value, rounded_value, validation_status, unit) |>
  mutate(param = stringr::str_to_lower(param),
         param = case_when(
           param == "temp_mean" ~ "temp",
           param == "wdir_vect" ~ "wd",
           param == "wspd_sclr" ~ "ws",
           .default = as.character(param)),
         instrument = stringr::str_to_lower(instrument))

# remove PM2.5 teom data (collocated with SHARP) and keep one of PM10 teom or PM10 sharp - which ever is not NA
data_1hr <- data_1hr |>
  filter(instrument != "pm25_r&p_teom") |>
  group_by(param, date_time_end, station_name) |>
  slice(which.max(!is.na(raw_value))) |>
  ungroup()

### pad missing rows
library(lubridate)
library(tidyr)

# define start and end times
ts_start <- ymd_hm(paste0(min(year(data_1hr$date_time_end)), "-01-01 00:00"), tz = "Etc/GMT+8")
ts_end <- ymd_hm(paste0(max(year(data_1hr$date_time_end)), "-12-31 23:00"), tz = "Etc/GMT+8")

# generate hourly time series
hour_ts <- seq(from = ts_start, to = ts_end, by = "hour")

# pad missing datetime stamps, fill instrument, validation_status and unit (down)
data_1hr <- data_1hr |>
  complete(date_time_end = hour_ts, station_name, param) |>
  arrange(station_name, param, date_time_end) |>
  fill(instrument, validation_status, unit)

# add date_time_hour_begin, date, time and reorder columns
data_1hr <- data_1hr |>
  mutate(date_time_begin = date_time_end - hours(1),
         date = as.Date(date_time_begin, tz = "Etc/GMT+8"),
         time = paste0(hour(date_time_begin), ":00")) |>
  select(station_name,
         date_time_end,
         date_time_begin,
         date,
         time,
         param,
         raw_value,
         rounded_value,
         unit,
         instrument,
         validation_status)

# check for duplicates
duplicates <- data_1hr |>
  select(param, date, time) |>
  group_by_all() |>
  filter(n() > 1) |>
  ungroup() |>
  distinct()

cat("Number of duplicates:",nrow(duplicates))

if(nrow(duplicates) == 0){
  rm(duplicates)
  saveRDS(data_1hr, file = "data/data_1hr.rds")

}

#------------------------------------------------------
# calculate and averaged data frames
#------------------------------------------------------

library(openair)

#data_1hr <- readRDS("data/data_1hr.rds")

### daily average  #HERE - need to remove duplicate days - different intruement (keep 5014i)
data_24hr_meta <- data_1hr |>
  select(date, station_name, param, instrument, validation_status, unit) |>
  distinct()

data_1hr_wide <- data_1hr |>
  select(station_name, date_time_begin, param, raw_value) |>
  tidyr::pivot_wider(names_from = param, values_from = raw_value) |>
  rename(date = date_time_begin)

data_24hr_wide <- timeAverage(data_1hr_wide,
                         avg.time = "day",
                         data.thresh = 75,
                         statistic = "mean",
                         fill = TRUE,
                         type = "station_name")

### monthly, annual and seasonal averages
data_1m_wide <- timeAverage(data_24hr_wide,
                          avg.time = "month",
                          data.thresh = 75,
                          statistic = "mean",
                          fill = TRUE,
                          type = "station_name")

data_1y_wide <- timeAverage(data_24hr_wide,
                          avg.time = "year",
                          data.thresh = 75,
                          statistic = "mean",
                          fill = TRUE,
                          type = "station_name")

data_season_wide <- timeAverage(data_24hr_wide,
                           avg.time = "season",
                           statistic = "mean",
                           fill = TRUE,
                           type = "station_name")

#------------------------------------------------------
# data capture summary
#------------------------------------------------------

data_cap_1m <- timeAverage(data_24hr_wide,
                           avg.time = "month",
                           data.thresh = 75,
                           statistic = "data.cap",
                           fill = TRUE,
                           type = "station_name") |>
  mutate_if(is.numeric, round, digits = 1)

data_cap_1y <- timeAverage(data_24hr_wide,
                            avg.time = "year",
                            data.thresh = 75,
                            statistic = "data.cap",
                            fill = TRUE,
                            type = "station_name") |>
  mutate_if(is.numeric, round, digits = 1)

data_cap_season <- timeAverage(data_24hr_wide,
                                avg.time = "season",
                                statistic = "data.cap",
                                fill = TRUE,
                                type = "station_name")|>
  mutate_if(is.numeric, round, digits = 1)



#------------------------------------------------------
# format averaged data frames to long format, add meta data, and save to RDS
#------------------------------------------------------

data_24hr <- data_24hr_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         value = round(value, 1)) |>
  left_join(data_24hr_meta, by = c('station_name', 'date', 'param'))|>
  filter(date >= as.Date("2015-01-01"))

save(data_24hr, file = "data/data_24hr.rds")

data_1m <- data_1m_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         value = round(value, 1)) |>
  left_join(data_24hr_meta, by = c('date', 'param'))|>

save(data_1m, file = "data/data_1m.rds")


data_1y <- data_1y_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  left_join(data_24hr_meta |> select(-flag_tfee), by = c('date', 'param')) |>
  mutate(date = as.integer(format(date, "%Y")),
         value = round(value, 1)) |>
  filter(date <= 2024)

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
# add tf_ee for 2024 to data_1hr and data_24hr
#------------------------------------------------------

# read in tfee for prince george (2010:2024) and add to data_1hr and data_24hr.
# (tfee in envair starts in 2017, does not cover entire period of interest)

tfee <- read.csv(file = "data/tfee_log.csv") |>
  filter(station_name == "Prince George Plaza 400") |>
  select(date, station_name) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         flag_tfee = TRUE
         )

data_1hr <- data_1hr |>
  left_join(tfee,
            by = c('date', 'station_name', 'flag_tfee'),
            relationship = 'many-to-many')

saveRDS(data_1hr, file = "data/data_1hr.rds")

data_24hr <- data_24hr |>
  left_join(tfee,
            by = c('date', 'station_name', 'flag_tfee')
            )

save(data_24hr, file = "data/data_24hr.rds")

#------------------------------------------------------
# clean up: remove tfee
#------------------------------------------------------

rm(tfee)

#------------------------------------------------------
# data capture summary
#------------------------------------------------------




