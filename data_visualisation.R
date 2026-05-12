# Load packages -----------------------------------------------------

library(tidyverse)
library(lubridate)
library(stringr)

# Import data from Google Sheets -----------------------------------

logged_data <- read_csv(
  "https://docs.google.com/spreadsheets/d/1R288g5rjz6U5lAg9_T-lcYS7si9IddisI_SzBn7Zpsk/export?format=csv"
)

# Clean and rename variables ---------------------------------------

phone_data <- logged_data %>%
  rename(
    timestamp = `Timestamp`,
    screen_time = `Approximately how many minutes did you use your phone? Please round to the nearest 100 minutes.`,
    clear_purpose = `Did you have a clear purpose for using your phone?`,
    main_activity = `What was your main activity?`,
    time_of_day = `What time of day do you use your phone the most?`
  ) %>%
  
  # Create cleaner text variables and extract hour from timestamp
  mutate(
    main_activity = str_to_lower(main_activity),
    time_of_day = str_to_lower(time_of_day),
    clear_purpose = str_to_lower(clear_purpose),
    hour = hour(ymd_hms(timestamp))
  )

# Plot 1 ------------------------------------------------------------
# Average screen time based on whether users had a clear purpose

purpose_summary <- phone_data %>%
  group_by(clear_purpose) %>%
  summarise(
    mean_screen_time = mean(screen_time, na.rm = TRUE)
  ) %>%
  ungroup()

plot1 <- purpose_summary %>%
  ggplot() +
  
  # Create bar chart
  geom_col(
    aes(
      x = reorder(clear_purpose, -mean_screen_time),
      y = mean_screen_time,
      fill = clear_purpose
    )
  ) +
  
  # Add labels and title
  labs(
    title = "Average Screen Time by Purpose",
    x = "Clear purpose",
    y = "Average screen time"
  ) +
  
  # Remove legend
  guides(fill = "none") +
  
  # Apply minimal theme
  theme_minimal()

# Display plot
plot1

# Save plot
ggsave("plot1.png", plot = plot1)

# Plot 2 ------------------------------------------------------------
# Explore how screen time changes across the day

plot2 <- phone_data %>%
  ggplot(
    aes(
      x = hour,
      y = screen_time,
      colour = time_of_day
    )
  ) +
  
  # Add jittered points
  geom_jitter(height = 20, width = 0.2) +
  
  # Add labels
  labs(
    title = "Screen Time Across the Day",
    x = "Hour of day",
    y = "Screen time"
  ) +
  
  # Apply minimal theme
  theme_minimal()

# Display plot
plot2

# Save plot
ggsave("plot2.png", plot = plot2)

# Plot 3 ------------------------------------------------------------
# Compare screen time distributions across activities

activity_median <- phone_data %>%
  group_by(main_activity) %>%
  
  # Calculate median screen time
  mutate(
    median_screen_time = median(screen_time, na.rm = TRUE)
  ) %>%
  ungroup()

plot3 <- ggplot(
  data = activity_median,
  aes(
    x = screen_time,
    y = reorder(main_activity, -median_screen_time),
    colour = main_activity
  )
) +
  
  # Add raw observations
  geom_jitter(height = 0.2) +
  
  # Add boxplot layer
  geom_boxplot(fill = "transparent") +
  
  # Add labels
  labs(
    title = "Screen Time by Main Activity",
    x = "Screen time",
    y = "Main activity"
  ) +
  
  # Remove legend
  guides(colour = "none") +
  
  # Apply minimal theme
  theme_minimal()

# Display plot
plot3

# Save plot
ggsave("plot3.png", plot = plot3)

# Plot 4 ------------------------------------------------------------
# Count the number of responses for each time of day

time_counts <- phone_data %>%
  group_by(time_of_day) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup()

plot4 <- time_counts %>%
  ggplot() +
  
  # Create bar chart
  geom_col(
    aes(
      x = reorder(time_of_day, -n),
      y = n,
      fill = time_of_day
    )
  ) +
  
  # Add labels
  labs(
    title = "Phone Use by Time of Day",
    x = "Time of day",
    y = "Count"
  ) +
  
  # Remove legend
  guides(fill = "none") +
  
  # Apply minimal theme
  theme_minimal()

# Display plot
plot4

# Save plot
ggsave("plot4.png", plot = plot4)