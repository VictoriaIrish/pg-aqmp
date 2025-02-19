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

# Packages
library(ggplot2)

#Plot PM25 1m data capture
data_cap_1m %>%
  filter(param == "pm25")%>%
  ggplot(aes(x = date, y = data_cap_percent)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_hline(yintercept = 60, col = "red", linetype = "dotted") +
  labs(x = "Month", y = "% data capture") +
  scale_x_datetime(breaks = "1 year", # Adjust frequency as needed
    labels = scales::date_format("%Y")) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 0),
    legend.position = "none")

#Plot PM25 season data capture
data_cap_season %>%
  filter(param == "pm25")%>%
  ggplot(aes(x = date, y = data_cap_percent)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_hline(yintercept = 60, col = "red", linetype = "dotted") +
  geom_text(aes(label = season, colour = "green"),  # Adding the season labels from your column
            position = position_dodge(width = 0.9),
            angle = 90) +  # Adjust label position above bars
  labs(x = "Season", y = "% data capture") +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 0),
    legend.position = "none")

#Plot PM25 1year data capture
data_cap_1y %>%
  filter(param == "pm25")%>%
  ggplot(aes(x = date, y = data_cap_percent)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_hline(yintercept = 60, col = "red", linetype = "dotted") +
  labs(x = "Year", y = "% data capture") +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 0),
    legend.position = "none")

#Plot 24hr PM2.5 average

data_24hr %>%
  filter(param == "pm25") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  labs(x = "Year", y = expression(paste("24-hr average ", PM[2.5]," (",mu,"g/",m^3,")"))) +
  scale_x_date(breaks = "1 year", labels = scales::date_format("%Y"))

#Plot 1m PM2.5 average

data_1m %>%
  filter(param == "pm25") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  labs(x = "Month", y = expression(paste("Monthly average ", PM[2.5]," (",mu,"g/",m^3,")")))

# Plot seasonal PM2.5 average

data_season %>%
  filter(param == "pm25") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  labs(x = "Year", y = expression(paste("Seasonal average ", PM[2.5]," (",mu,"g/",m^3,")")))

# PLot yearly PM2.5 average

data_1y %>%
  filter(param == "pm25") %>%
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  labs(x = "Year", y = expression(paste("Seasonal average ", PM[2.5]," (",mu,"g/",m^3,")"))) +
  scale_x_continuous(
    breaks = seq(2015, 2024, by = 1),  # This ensures ticks for every year
    labels = as.character(seq(2015, 2024, by = 1))  # Format tick labels as years
  )
