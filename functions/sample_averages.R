sample_averages <- function(sample_data, biomarker_data, biomarker_variable) {
  
    # Merge with biomarker data
  merged_df <- sample_data %>%
    left_join(biomarker_data, by = c("ID", "date_range"), relationship = "many-to-many")
    
    # Calculate mean biomarker variable for each sample
  result_df <- merged_df %>%
    group_by(sample, time_window) %>%
    summarise(mean_biomarker_var = mean(get(biomarker_variable), na.rm = TRUE), .groups = 'drop')
    
    # Reshape to wide format
  biomarker_averages <- result_df %>%
    pivot_wider(names_from = time_window, values_from = mean_biomarker_var) %>%
    rename_with(~ paste0(biomarker_variable, "_", .), contains("days"))
    
    return(biomarker_averages)
    
  }
    
