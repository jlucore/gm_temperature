create_full_intervals <- function(df, pre_post_pattern, total_pattern) {
  df %>%
    filter(grepl(pre_post_pattern, time_window)) %>%
    group_by(sample, ID, date) %>%
    distinct(date_range) %>%
    mutate(time_window = total_pattern)
}


# this function gie you time windows before and after sample collection 
# df data after using the generate_intervals function to calculate pre and post time intervals 
# pre and post pattern is the naming scheme for the time intervals created by the generate_intervals function , if you generate intervals for pre and post 3 days then the input is "3days"
# total_pattern is the naming scheme for the full time interval, if you have 3 day intervals then its "total_3_day"
# need to run this fro every full interval you want to create and then join all the tables 