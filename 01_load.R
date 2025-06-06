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
# retrieve data from ftp site
#------------------------------------------------------

library(envair)

params <- grep("pm|so2|trs|no2|o3|temp|wspd_sclr|wdir_vect", list_parameters(), value = TRUE)

data_1hr_original <- params |>
  purrr::map_dfr(~importBC_data(., 2015:2024, flag_TFEE = TRUE)) |>
  filter(grepl("Plaza", STATION_NAME))

# correct datetime stamps to PST ("Etc/GMT+8")
tz(data_1hr_original$DATETIME) <- "Etc/GMT+8"
tz(data_1hr_original$DATE_PST) <- "Etc/GMT+8"

saveRDS(data_1hr_original, file = "data/data_1hr_original.rds")


