
calculate_weather_averages <- function(immune_data, weather_data, weather_variable, time_intervals) {
  
  # Function to generate datetime columns for each interval
  generate_intervals <- function(df, intervals) {
    for (i in seq_along(intervals)) {
      df <- df %>% mutate(!!paste0("days", intervals[i]) := date - days(intervals[i]))
    }
    return(df)
  }
  
  # Generate datetime columns for the desired intervals
  immune_data <- generate_intervals(immune_data, time_intervals) %>%
    select(c(sample, date, starts_with("days"))) %>%
    pivot_longer(cols = starts_with("days"),
                 names_to = "time_window",
                 values_to = "window_date")
  
  # Explode the date range
  date_range <- immune_data %>%
    rowwise() %>%
    mutate(date_range = list(seq(window_date, date, by = "day"))) %>%
    ungroup()
  
  # Unnest the date range
  exploded_dates <- date_range %>%
    tidyr::unnest(date_range)
  
  # Merge with weather data
  merged_df <- exploded_dates %>%
    left_join(weather_data, by = "date_range", relationship = "many-to-many")
  
  # Calculate mean weather variable for each sample
  result_df <- merged_df %>%
    group_by(sample, time_window) %>%
    summarise(mean_weather_var = mean(get(weather_variable), na.rm = TRUE), .groups = 'drop')
  
  # Reshape to wide format
  weather_averages <- result_df %>%
    pivot_wider(names_from = time_window, values_from = mean_weather_var) %>%
    rename_with(~ paste0(weather_variable, "_", .), starts_with("days"))
  
  return(weather_averages)
  
}
