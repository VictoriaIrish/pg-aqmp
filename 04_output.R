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
library(ggplot2)
# Load tidyr package if you haven't already
library(tidyr)

# Separate 'YearMonth' into 'Year' and 'Month'
monthly_capture_summary <- monthly_capture_summary %>%
  separate(YearMonth, into = c("Year", "Month"), sep = "-")

# Now create the plot and facet by Year
ggplot(monthly_capture_summary, aes(x = Month, y = percent_capture)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Data Capture Percentage",
    x = "Month",
    y = "Percent Data Capture (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  facet_wrap(~ Year, scales = "free_x")  # Facet by Year
