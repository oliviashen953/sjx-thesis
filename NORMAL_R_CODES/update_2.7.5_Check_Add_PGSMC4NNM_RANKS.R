# Load necessary libraries
library(readr)
library(fs)

# Function to check the file size and columns
check_file <- function(file_path, expected_columns_count, expected_rows, log_file) {
  if (file_exists(file_path)) {
    data <- read_csv(file_path, show_col_types = FALSE)
    actual_rows <- nrow(data)
    actual_columns_count <- ncol(data)
    
    if (actual_rows != expected_rows) {
      write(sprintf("File %s: Unexpected number of rows (expected %d, got %d)\n", file_path, expected_rows, actual_rows), file = log_file, append = TRUE)
    } else {
      write(sprintf("File %s: Row count is correct (%d rows)\n", file_path, actual_rows), file = log_file, append = TRUE)
    }
    
    if (actual_columns_count != expected_columns_count) {
      write(sprintf("File %s: Unexpected number of columns (expected %d, got %d)\n", file_path, expected_columns_count, actual_columns_count), file = log_file, append = TRUE)
    } else {
      write(sprintf("File %s: Column count is correct (%d columns)\n", file_path, actual_columns_count), file = log_file, append = TRUE)
    }
  } else {
    write(sprintf("File %s does not exist\n", file_path), file = log_file, append = TRUE)
  }
}

# Expected columns and rows for each trait
height_expected_columns_count <- 162
height_expected_rows <- 3000
bmi_expected_columns_count <- 122
bmi_expected_rows <- 3000
brc_expected_columns_count <- 234
brc_expected_rows <- 3000

# Specify the log file
log_file <- "/n/home00/jiaxinshen/check_withPGSNNMMC4Ranks_file_log.txt"

# Loop through subgroups and iterations for each trait
for(subgroup in 1:20) {
  for(iteration in 1:30) {
    height_file <- sprintf("/n/home00/jiaxinshen/NORMAL/2.2_height_test_subgroup_files/height_subgroup_%d_iteration_%d_with_ranks.csv", subgroup, iteration)
    check_file(height_file, height_expected_columns_count, height_expected_rows, log_file)
    
    bmi_file <- sprintf("/n/home00/jiaxinshen/NORMAL/2.2_bmi_test_subgroup_files/bmi_subgroup_%d_iteration_%d_with_ranks.csv", subgroup, iteration)
    check_file(bmi_file, bmi_expected_columns_count, bmi_expected_rows, log_file)
    
    brc_file <- sprintf("/n/home00/jiaxinshen/NORMAL/2.2_brc_test_subgroup_files/brc_subgroup_%d_iteration_%d_with_ranks.csv", subgroup, iteration)
    check_file(brc_file, brc_expected_columns_count, brc_expected_rows, log_file)
  }
}

cat("File checks complete. See log file for details.\n")

