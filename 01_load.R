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


#LOAD ALL PM2.5 DATA FROM FTP SITE FOR 2013:2023
# Define the base FTP URL for the AnnualSummary folder
base_ftp_url <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/"

# Define the years you want to loop over
years <- 2013:2023

# Create an empty list to store the data for each year
pm25_data_list <- list()

# Loop over each year, download the data, and read the CSV
for (year in years) {

  # Build the full FTP URL for the current year
  ftp_url <- paste0(base_ftp_url, year, "/PM25.csv")

  # Define the local file path where you want to save the downloaded file
  destfile <- paste0(year, "_PM25.csv")

  # Download the file from the FTP server
  download.file(ftp_url, destfile, mode = "wb")

  # Read the CSV file into R
  annual_data <- read.csv(destfile)

  # Add the data to the list, using the year as the name
  pm25_data_list[[as.character(year)]] <- annual_data
}

# Check the data for the first year (for example, 2013)
head(pm25_data_list[["2013"]])

# Combine all the data into one data frame
combined_pm25_data <- do.call(rbind, pm25_data_list)

# View the combined data
head(combined_pm25_data)




# ####TEST WITH ENVAIR PACKAGE. WORKS FOR PM2.5 BUT NOT FOR PM10#####
# # #Retrieve data from ENVAIR package
# # install.packages("remotes")
# # remotes::install_github("bcgov/envair")
# # library(envair)
# #
# # #load pm25 data for PG
# # PG_pm25_data <- importBC_data_avg("pm25",
# #                                       2010:2023,
# #                                       averaging_type = "24-hr",
# #                                       data_threshold = 0.75) %>%
# #   filter(., station_name %in% "Prince George Plaza 400")
# #
# # #load pm10 data for PG
# #
# # #load trs data for PG
# #
# #
# # #data capture for pm2.5 in PG
# # pm25_PG_data_captures <- get_captures(param = c('pm25'),
# #                                    years = 2010:2023,
# #                                    merge_Stations = TRUE) %>%
# #   filter(., station_name %in% "Prince George Plaza 400")
# #
# # #data capture for pm10 in PG - WEIRD THE DATA CAPTURE SEEMS TO WORK FOR
# # pm10_PG_data_captures <- get_captures(param = c('pm10'),
# #                                       years = 2010:2023,
# #                                       merge_Stations = TRUE) %>%
# #   filter(., station_name %in% "Prince George Plaza 400")
# #
# # #data capture for trs in PG
# # trs_PG_data_captures <- get_captures(param = c('trs'),
# #                                      years = 2010:2023,
# #                                      merge_Stations = TRUE) %>%
# #   filter(., station_name %in% "Prince George Plaza 400")
# #
# # #example retrieves stat summaries - THIS ALSO DOES NOT WORK
# # stats_result <- get_stats(param = 'pm25',
# #                           years = 2010:2023,
# #                           add_TFEE = TRUE,
# #                           merge_Stations = TRUE)
#
# library(RCurl)
#
# # Set up the FTP URL
# pm25stat_ftp_url <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/STAT_SUMMARIES/PM25_Stats.xlsx"
#
# # Define the local file path where you want to save the downloaded file
# destfile <- "PM25_Stats.xlsx"
#
# # Download the file from the FTP server
# download.file(pm25stat_ftp_url, destfile, mode = "wb")
#
# # Install and load the 'readxl' package if you haven't already
# library(readxl)
#
# # Read the Excel file into R
# pm25statdata <- read_excel(destfile, col_names = FALSE)
#
# # Combine the first two rows into one (if they are character vectors)
# new_header <- paste(pm25statdata[1, ], pm25statdata[2, ], sep = "_")
#
# # Remove the first two rows since they've been turned into the header
# pm25statdata <- pm25statdata[-c(1, 2), ]
#
# # Set the new header as column names
# colnames(pm25statdata) <- new_header
#
# #load annual summary 2023 pm2.5
# # Set up the FTP URL
# annualpm25summary2023_ftp_url <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2023/PM25.csv"
#
# # Define the local file path where you want to save the downloaded file
# destfile <- "2023_AnnSum_PM25.csv"
#
# # Download the file from the FTP server
# download.file(annualpm25summary2023_ftp_url, destfile, mode = "wb")
#
# # Read the csv file into R
# annualpm25summary2023 <- read.csv(destfile)
#
