#### For height and bmi ####

library(readr)
library(dplyr)
library(fs)

# Function to calculate standardized difference for height and bmi
calculate_standardized_difference <- function(data, rank_col, top_percent) {
  bottom_percent <- 100 - top_percent
  
  # Calculate the top X% and bottom (100-X)%
  top_data <- data %>%
    arrange(get(rank_col)) %>%
    slice_head(prop = top_percent / 100)
  bottom_data <- data %>%
    arrange(get(rank_col)) %>%
    slice_tail(prop = bottom_percent / 100)
  
  # Calculate means
  mean_top <- mean(top_data$pheno, na.rm = TRUE)
  mean_bottom <- mean(bottom_data$pheno, na.rm = TRUE)
  
  # Calculate SE assuming you have the SD values
  SE_top <- sd(top_data$pheno) / sqrt(nrow(top_data))
  SE_bottom <- sd(bottom_data$pheno) / sqrt(nrow(bottom_data))
  
  # Calculate pooled SE and standardized difference
  pooled_SE <- sqrt((SE_top^2 + SE_bottom^2) / 2)
  standardized_difference <- (mean_top - mean_bottom) / pooled_SE
  
  return(data.frame(rank_method = rank_col,
                    group_percentile = paste0("Top ", top_percent, "%"),
                    mean_top_group = mean_top,
                    mean_bottom_group = mean_bottom,
                    standardized_difference = standardized_difference))
}

# Loop over each trait
traits <- c("height", "bmi")

for (trait in traits) {
  results_list <- list()
  
  for (iteration in 1:30) {
    for (group_id in 1:20) {
      # Construct the file name
      file_name <- sprintf("~/NORMAL/2.2_%s_test_subgroup_files/%s_subgroup_%d_iteration_%d_with_ranks.csv", trait, trait, group_id, iteration)
      
      # Check if the file exists
      if (file.exists(file_name)) {
        # Load the dataset
        data <- read_csv(file_name)
        
        # Identify all columns ending with "_rank"
        rank_columns <- names(data)[grepl("_rank$", names(data))]
        
        for (rank_col in rank_columns) {
          # Loop over different top percentage groups
          for (top_percent in c(5, 10, 20)) {
            results_list[[length(results_list) + 1]] <- calculate_standardized_difference(data, rank_col, top_percent)
          }
        }
      } else {
        warning(paste("File not found:", file_name))
      }
    }
  }
  
  # Combine all results into one dataframe
  combined_results <- bind_rows(results_list)
  
  # Calculate the mean standardized difference across all groups and iterations for each rank method and percentile
  summary_results <- combined_results %>%
    group_by(rank_method, group_percentile) %>%
    summarize(
      mean_standardized_difference = mean(standardized_difference, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Save the summary to a new CSV file
  output_file_name <- sprintf("NORMAL_%s_summary_standardized_difference_across_groups_and_iterations.csv", trait)
  write_csv(summary_results, output_file_name)
}

