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

#Analyse monthly data capture
# Extract the Year-Month from 'DATE_PST' for grouping
clean_combined_pm25_data$YearMonth <- format(clean_combined_pm25_data$DATE_PST, "%Y-%m")

# Count the total number of hours in each month (number of expected observations)
monthly_totals <- clean_combined_pm25_data %>%
  group_by(YearMonth) %>%
  summarise(total_expected = n())  # Number of rows (which corresponds to hourly data)

# Count the number of non-NA 'ROUNDED_VALUE' per month
monthly_data_capture <- clean_combined_pm25_data %>%
  filter(!is.na(ROUNDED_VALUE)) %>%
  group_by(YearMonth) %>%
  summarise(valid_data = n())  # Count non-NA values in 'ROUNDED_VALUE'

# Merge the total expected and valid data capture counts by YearMonth
monthly_capture_summary <- merge(monthly_totals, monthly_data_capture, by = "YearMonth")

# Calculate percent data capture
monthly_capture_summary <- monthly_capture_summary %>%
  mutate(percent_capture = (valid_data / total_expected) * 100)

# View the percent data capture for each month
print(monthly_capture_summary)
