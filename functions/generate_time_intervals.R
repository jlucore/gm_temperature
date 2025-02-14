generate_intervals <- function(df, intervals) { 
  for (i in seq_along(intervals)) {
    col_name <- ifelse(intervals[i] < 0, 
                       paste0("pre_", abs(intervals[i]), "days"), 
                       paste0("post_", intervals[i], "days"))
    df <- df %>% mutate(!!col_name := date + days(intervals[i]))
  }
  
  # Generate datetime columns for the desired intervals
  df2 <- df %>%
    select(c(sample, ID, date, contains("days"))) %>%
    pivot_longer(cols = contains("days"),
                 names_to = "time_window",
                 values_to = "window_date")
  
  # Explode the date range
  date_range <- df2 %>%
    rowwise() %>%
    mutate(date_range = list(seq(window_date, date, by = ifelse(window_date < date, "day", "-1 day")))) %>%
    ungroup()
  
  # Unnest the date range
  exploded_dates <- date_range %>%
    tidyr::unnest(date_range)
  
  return(exploded_dates)
  
}



# df = data with sample dates that you want time intervals based of off
# interval arguments take negative and positive integers, whole integers are equal to days (i.e. 3 = 3 days)
# negative integers create a pre sample in interval (i.e. three days before sample collection) and positive integers create a post sample interval (i.e. 3 days after sample collection)
# remember that all intervals INCLUDE the date of sample collection
