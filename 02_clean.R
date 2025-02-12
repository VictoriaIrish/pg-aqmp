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

#Load packages
library(dplyr)

#Filter for PG plaza 400 and SHARP5030 instrument
clean_combined_pm25_data <- combined_pm25_data %>%
  filter(STATION_NAME == "Prince George Plaza 400",
         INSTRUMENT == "PM25 SHARP5030")

# Assuming 'combined_pm25_data' is the data frame containing your data
# Make sure 'DATE_PST' is in a proper datetime format
clean_combined_pm25_data$DATE_PST <- as.POSIXct(clean_combined_pm25_data$DATE_PST, format = "%Y-%m-%d %H:%M", tz = "Etc/GMT+8")

# Check for duplicates - find rows where the same 'DATE_PST' appears more than once
duplicate_datetimes <- clean_combined_pm25_data %>%
  group_by(DATE_PST) %>%
  filter(n() > 1)  # Keep rows where there are multiple entries for the same datetime

# View the duplicate datetime rows
head(duplicate_datetimes)


