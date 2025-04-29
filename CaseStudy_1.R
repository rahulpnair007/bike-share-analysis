# Load the tidyverse package
library(tidyverse)

# Set your working directory where all your CSV files are stored
setwd("D:/Unni/Course_Materials/CaseStudy_1/Last_12_Months_Data/")  # <-- change this to your folder path

# List all CSV files in the folder
file_list <- list.files(pattern = "*.csv")

# Read and combine all CSV files into one dataframe
bike_data <- file_list %>%
  map_df(read_csv)

# View the first few rows to check
head(bike_data)

# Clean column names
bike_data <- clean_names(bike_data)

# Create ride_length in minutes
bike_data <- bike_data %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE)
  )

library(dplyr)
library(lubridate)

bike_data <- bike_data %>%
  mutate(
    started_at_parsed = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at_parsed, units = "mins")),
    day_of_week = wday(started_at_parsed, label = TRUE)
  )

# Find rows where parsing failed (started_at_parsed is NA)
failed_parse_starts <- bike_data %>%
  filter(is.na(started_at_parsed))

print(head(failed_parse_starts$started_at, n = 20)) # Show the first 20 problematic values

print(sample(bike_data$started_at, size = 50)) # Get a random sample of 50 values

bike_data <- bike_data %>%
  mutate(
    started_at_parsed = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at_parsed, units = "mins")),
    day_of_week = wday(started_at_parsed, label = TRUE)
  )

# Look at some rows where parsing failed
failed_parse_starts <- bike_data %>%
  filter(is.na(started_at_parsed))

print(head(failed_parse_starts$started_at, n = 20))

failed_parse_starts_after_trim <- bike_data %>%
  filter(is.na(started_at_parsed))

print(head(failed_parse_starts_after_trim$started_at_trimmed, n = 20))

library(stringr) # Make sure stringr is loaded

bike_data <- bike_data %>%
  mutate(
    started_at_trimmed = str_trim(started_at),
    started_at_parsed = ymd_hms(started_at_trimmed),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at_parsed, units = "mins")),
    day_of_week = wday(started_at_parsed, label = TRUE)
  )

# Find rows where parsing still failed (started_at_parsed is NA)
failed_parse_starts_after_trim <- bike_data %>%
  filter(is.na(started_at_parsed))

# Now print the head of the trimmed 'started_at' values for these failed rows
print(head(failed_parse_starts_after_trim$started_at_trimmed, n = 20))

library(dplyr)
library(lubridate)
library(stringr)

# Print a larger random sample of non-NA started_at values
non_na_started_at <- bike_data %>%
  filter(!is.na(started_at)) %>%
  pull(started_at) # Extract the started_at column as a vector

if (length(non_na_started_at) > 50) {
  print(sample(non_na_started_at, size = 50))
} else {
  print(non_na_started_at) # If there are fewer than 50, print all
}


bike_data <- bike_data %>%
  mutate(
    started_at_trimmed = str_trim(started_at),
    started_at_parsed = case_when(
      # Try YYYY-MM-DD HH:MM:SS first (and variations with "T" separator)
      !is.na(ymd_hms(started_at_trimmed)) ~ ymd_hms(started_at_trimmed),
      !is.na(parse_date_time(started_at_trimmed, "Ymd HMS")) ~ parse_date_time(started_at_trimmed, "Ymd HMS"),
      !is.na(parse_date_time(started_at_trimmed, "YmdTHMS")) ~ parse_date_time(started_at_trimmed, "YmdTHMS"),
      # Try MM/DD/YYYY HH:MM:SS (and variations)
      !is.na(mdy_hms(started_at_trimmed)) ~ mdy_hms(started_at_trimmed),
      !is.na(parse_date_time(started_at_trimmed, "mdy HMS")) ~ parse_date_time(started_at_trimmed, "mdy HMS"),
      TRUE ~ as.POSIXct(NA) # If none of the above work, set to NA
    ),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at_parsed, units = "mins")),
    day_of_week = wday(started_at_parsed, label = TRUE)
  )

print(sum(is.na(bike_data$started_at_parsed))) # See how many are still NA

bike_data <- bike_data %>%
  mutate(
    started_at_trimmed = str_trim(started_at),
    started_at_parsed = case_when(
      !is.na(ymd_hms(started_at_trimmed)) ~ ymd_hms(started_at_trimmed),
      !is.na(parse_date_time(started_at_trimmed, c("Ymd HMS", "YmdTHMS", "Ymd HM", "YmdT HM", "Ymd hms", "YmdThms"))) ~ parse_date_time(started_at_trimmed, c("Ymd HMS", "YmdTHMS", "Ymd HM", "YmdT HM", "Ymd hms", "YmdThms")),
      !is.na(mdy_hms(started_at_trimmed)) ~ mdy_hms(started_at_trimmed),
      !is.na(parse_date_time(started_at_trimmed, c("mdy HMS", "mdyHM", "mdy hms", "mdyhm"))) ~ parse_date_time(started_at_trimmed, c("mdy HMS", "mdyHM", "mdy hms", "mdyhm")),
      TRUE ~ as.POSIXct(NA)
    ),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at_parsed, units = "mins")),
    day_of_week = wday(started_at_parsed, label = TRUE)
  )
print(sum(is.na(bike_data$started_at_parsed)))

failed_parse_starts_again <- bike_data %>%
  filter(is.na(started_at_parsed))
print(head(failed_parse_starts_again$started_at_trimmed, n = 20))

# Remove rows with negative ride_length or NA
bike_data <- bike_data %>%
  filter(!is.na(ride_length) & ride_length > 0)

# Optional: remove rows with missing station names
bike_data <- bike_data %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name))

library(lubridate)

# Ensure start and end times are datetime
bike_data$started_at <- ymd_hms(bike_data$started_at)
bike_data$ended_at <- ymd_hms(bike_data$ended_at)

# Create ride_length column (in minutes)
bike_data$ride_length <- as.numeric(difftime(bike_data$ended_at, bike_data$started_at, units = "mins"))

# Summary of ride lengths
summary(bike_data$ride_length)

bike_type_summary <- bike_data %>%
  group_by(rideable_type) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides))

print(bike_type_summary)

# Create a day of the week column
bike_data$day_of_week <- weekdays(bike_data$started_at)

# Summarise rides by day
weekday_summary <- bike_data %>%
  group_by(day_of_week) %>%
  summarise(total_rides = n()) %>%
  arrange(match(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

print(weekday_summary)

# Extract hour
bike_data$start_hour <- hour(bike_data$started_at)

# Group by hour
hourly_usage <- bike_data %>%
  group_by(start_hour) %>%
  summarise(total_rides = n())

print(hourly_usage)

# Plot hourly usage
library(ggplot2)

ggplot(hourly_usage, aes(x = start_hour, y = total_rides)) +
  geom_line(color = "blue") +
  labs(title = "Ride Usage by Hour of Day", x = "Hour", y = "Number of Rides") +
  theme_minimal()

ggplot(bike_data, aes(x = ride_length)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  xlim(0, 100) +  # Limiting to remove extremely long rides
  labs(title = "Distribution of Ride Lengths",
       x = "Ride Length (minutes)",
       y = "Number of Rides") +
  theme_minimal()

bike_data %>%
  group_by(rideable_type) %>%
  summarise(total_rides = n()) %>%
  ggplot(aes(x = rideable_type, y = total_rides, fill = rideable_type)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Number of Rides by Bike Type",
       x = "Bike Type",
       y = "Total Rides") +
  theme_minimal()

bike_data %>%
  group_by(day_of_week) %>%
  summarise(total_rides = n()) %>%
  mutate(day_of_week = factor(day_of_week,
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  ggplot(aes(x = day_of_week, y = total_rides, fill = day_of_week)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Number of Rides by Weekday",
       x = "Day of the Week",
       y = "Total Rides") +
  theme_minimal()