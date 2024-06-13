# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)

# Define the full file path
full_file_path <- "G:/.shortcut-targets-by-id/1Hqecl4V9odrCM-fZun8l-1cDSCip00KD/ATELIER_Actividad_Tecnica/WP9/Info_IBERDROLA/FTP/atelier_digipen/Data from 2024 until May 16/combined_data_digipen.csv"

# Read the combined CSV file using read.csv
combined_data <- read.csv(full_file_path, stringsAsFactors = FALSE)

# Filter rows where 'Type' is 'AVG'
avg_data <- combined_data %>% filter(Type == "AVG")

# Calculate total power
avg_data <- avg_data %>%
  mutate(TotalPower = as.numeric(`kW.L1..kW.`) + as.numeric(`kW.L2..kW.`) + as.numeric(`kW.L3..kW.`))

# Convert Date and Hour columns to a POSIXct DateTime
avg_data <- avg_data %>%
  mutate(DateTime = as.POSIXct(paste(Date, Hour), format = "%d/%m/%Y %H:%M:%S"),
         DayOfWeek = factor(weekdays(DateTime), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         HourOfDay = hour(DateTime),
         WeekdayType = ifelse(DayOfWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# Reshape data into long format for individual phases
avg_data_long <- avg_data %>%
  select(Object.Name, `kW.L1..kW.`, `kW.L2..kW.`, `kW.L3..kW.`) %>%
  pivot_longer(cols = starts_with("kW"), names_to = "kW_type", values_to = "kW_value")

# Function to create box plots for each category
create_box_plot <- function(data, title) {
  ggplot(data, aes(x = kW_type, y = kW_value, fill = kW_type)) +
    geom_boxplot() +
    labs(title = str_wrap(title, width = 20),
         x = "Power Type",
         y = "Power (kW)") +
    scale_fill_manual(values = c("blue", "green", "red")) +
    theme_minimal()
}

# List of categories
categories <- c("GENERAL", "PLANTA BAJA", "PLANTA PRIMERA", "PLANTA SEGUNDA", "CUADRO CLIMA")

# Create and display box plots for each category
for (category in categories) {
  box_plot <- create_box_plot(filter(avg_data_long, Object.Name == category), paste(category, " Data"))
  print(box_plot)
}

# Function to process data for plotting by day of the week
process_and_summarize_by_day <- function(data, category_name) {
  data %>%
    filter(Object.Name == category_name) %>%
    group_by(DayOfWeek) %>%
    summarise(across(starts_with("kW.L"), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = starts_with("kW"), names_to = "Phase", values_to = "Power") %>%
    mutate(Category = category_name)
}

# Function to process data for plotting by hour of the day
process_and_summarize_by_hour <- function(data, category_name) {
  data %>%
    filter(Object.Name == category_name) %>%
    group_by(HourOfDay, WeekdayType) %>%
    summarise(across(starts_with("kW.L"), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = starts_with("kW"), names_to = "Phase", values_to = "Power") %>%
    mutate(Category = category_name)
}

# Process and summarize data for each category
summary_day <- bind_rows(lapply(categories, process_and_summarize_by_day, data = avg_data))
summary_hour <- bind_rows(lapply(categories, process_and_summarize_by_hour, data = avg_data))

# Function to create the plot for each category by day of the week
create_day_plot <- function(data, title) {
  ggplot(data, aes(x = DayOfWeek, y = Power, color = Phase, group = Phase)) +
    geom_line() +
    labs(title = str_wrap(title, width = 30),
         x = "Day of the Week",
         y = "Average Power (kW)") +
    scale_color_manual(values = c("blue", "green", "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to create the plot for each category by hour of the day
create_hourly_plot <- function(data, title) {
  ggplot(data, aes(x = HourOfDay, y = Power, color = WeekdayType, shape = Phase)) +
    geom_line() +
    geom_point() +
    labs(title = str_wrap(title, width = 30),
         x = "Hour of the Day",
         y = "Average Power (kW)",
         color = "Day Type",
         shape = "Phase") +
    scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create and display plots for each category by day of the week
for (category in categories) {
  print(create_day_plot(filter(summary_day, Category == category), paste("Average Power by Day of the Week\n(", category, " Data)", sep = "")))
}

# Create and display plots for each category by hour of the day
for (category in categories) {
  print(create_hourly_plot(filter(summary_hour, Category == category), paste("Average Power by Hour of the Day\n(", category, " Data)", sep = "")))
}

# Combined active power plot for all locations in the building by day of the week
combined_summary_day <- avg_data %>%
  group_by(DayOfWeek, Object.Name) %>%
  summarise(AvgTotalPower = mean(TotalPower, na.rm = TRUE), .groups = 'drop') %>%
  rename(Category = Object.Name)

combined_plot <- ggplot(combined_summary_day, aes(x = DayOfWeek, y = AvgTotalPower, color = Category, group = Category)) +
  geom_line() +
  labs(title = str_wrap("Average Total Power by Day of the Week", width = 30),
       x = "Day of the Week",
       y = "Average Total Power (kW)") +
  scale_color_manual(values = c("blue", "green", "red", "purple", "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the combined plot
print(combined_plot)
