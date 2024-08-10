library(readr)
library(dplyr)
library(fs)

# Function to calculate OR and RR for a given rank method and proportion
calculate_or_rr <- function(data, rank_col, prop) {
  # Filter top X% and bottom (100-X)%
  top_data <- data %>%
    arrange(get(rank_col)) %>%
    slice_head(prop = prop)
  bottom_data <- data %>%
    arrange(get(rank_col)) %>%
    slice_tail(prop = 1 - prop)
  
  # Calculate the number of cases (pheno == 1) and controls (pheno == 0) for top and bottom
  top_cases <- sum(top_data$pheno == 1, na.rm = TRUE)
  top_controls <- sum(top_data$pheno == 0, na.rm = TRUE)
  bottom_cases <- sum(bottom_data$pheno == 1, na.rm = TRUE)
  bottom_controls <- sum(bottom_data$pheno == 0, na.rm = TRUE)
  
  # Calculate odds ratio (OR) and relative risk (RR)
  odds_top <- top_cases / top_controls
  odds_bottom <- bottom_cases / bottom_controls
  or <- odds_top / odds_bottom
  
  risk_top <- top_cases / (top_cases + top_controls)
  risk_bottom <- bottom_cases / (bottom_cases + bottom_controls)
  rr <- risk_top / risk_bottom
  
  # Include the percentile in the results
  percentile <- paste0(round(prop * 100), "%")
  return(data.frame(rank_method = rank_col, percentile = percentile, odds_ratio = or, relative_risk = rr))
}

results_list <- list()

# Loop over each group file
for (iteration in 1:30) {
  for (group_id in 1:20) {
    file_name <- sprintf("~/NORMAL/2.2_brc_test_subgroup_files/brc_subgroup_%d_iteration_%d_with_ranks.csv", group_id, iteration)
    
    # Check if the file exists
    if (file.exists(file_name)) {
      # Load the dataset
      data <- read_csv(file_name)
      
      # Identify all columns ending with "_rank"
      rank_columns <- names(data)[grepl("_rank$", names(data))]
      
      # Calculate OR and RR for each rank method and each top percentile
      for (rank_col in rank_columns) {
        for (prop in c(0.05, 0.10, 0.20)) { # Top 5%, 10%, 20%
          results_list[[length(results_list) + 1]] <- calculate_or_rr(data, rank_col, prop)
        }
      }
    } else {
      warning(paste("File not found:", file_name))
    }
  }
}

# Combine all results into one dataframe
combined_results <- bind_rows(results_list)

# Calculate the mean OR and RR across all groups and iterations for each rank method and percentile
summary_results <- combined_results %>%
  group_by(rank_method, percentile) %>%
  summarize(
    mean_odds_ratio = mean(odds_ratio, na.rm = TRUE),
    mean_relative_risk = mean(relative_risk, na.rm = TRUE)
  ) %>%
  ungroup()

# Save the summary to a new CSV file
write_csv(summary_results, "NORMAL_brc_summary_odds_ratio_and_rr_across_groups_and_iterations.csv")
