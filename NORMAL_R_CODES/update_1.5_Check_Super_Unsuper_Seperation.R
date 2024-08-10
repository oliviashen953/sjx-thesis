# Load necessary library
library(data.table)

# Function to check size and format of csv files
check_files <- function(directory, prefix, iterations, n_groups, expected_train_size, expected_test_size, expected_group_size, expected_train_cols, expected_test_cols, expected_group_cols, output_file) {
  
  # Initialize lists to store results
  train_check_results <- list()
  test_check_results <- list()
  group_check_results <- list()
  
  # Loop through iterations
  for (iter in 1:iterations) {
    
    # Check training data files
    train_file <- file.path(directory, prefix, paste0(prefix, "_train_data_iteration_", iter, ".csv"))
    if (file.exists(train_file)) {
      train_data <- fread(train_file)
      if (nrow(train_data) == expected_train_size && ncol(train_data) == expected_train_cols) {
        train_check_results[[iter]] <- paste0("Train file ", iter, ": Correct size")
      } else {
        train_check_results[[iter]] <- paste0("Train file ", iter, ": Incorrect size, found ", nrow(train_data), " rows and ", ncol(train_data), " columns")
      }
    } else {
      train_check_results[[iter]] <- paste0("Train file ", iter, ": Not found")
    }
    
    # Check testing data files
    test_file <- file.path(directory, prefix, paste0(prefix, "_test_data_iteration_", iter, ".csv"))
    if (file.exists(test_file)) {
      test_data <- fread(test_file)
      if (nrow(test_data) == expected_test_size && ncol(test_data) == expected_test_cols) {
        test_check_results[[iter]] <- paste0("Test file ", iter, ": Correct size")
      } else {
        test_check_results[[iter]] <- paste0("Test file ", iter, ": Incorrect size, found ", nrow(test_data), " rows and ", ncol(test_data), " columns")
      }
    } else {
      test_check_results[[iter]] <- paste0("Test file ", iter, ": Not found")
    }
    
    # Loop through groups to check subgroup files
    for (group in 1:n_groups) {
      group_file <- file.path(directory, prefix, paste0(prefix, "_subgroup_", group, "_iteration_", iter, ".csv"))
      if (file.exists(group_file)) {
        group_data <- fread(group_file)
        if (nrow(group_data) == expected_group_size && ncol(group_data) == expected_group_cols) {
          group_check_results[[paste0(iter, "_", group)]] <- paste0("Group ", group, " file ", iter, ": Correct size")
        } else {
          group_check_results[[paste0(iter, "_", group)]] <- paste0("Group ", group, " file ", iter, ": Incorrect size, found ", nrow(group_data), " rows and ", ncol(group_data), " columns")
        }
      } else {
        group_check_results[[paste0(iter, "_", group)]] <- paste0("Group ", group, " file ", iter, ": Not found")
      }
    }
  }
  
  # Write results to a file
  writeLines(c("Train Data Check Results:", unlist(train_check_results), 
               "\nTest Data Check Results:", unlist(test_check_results), 
               "\nGroup Data Check Results:", unlist(group_check_results)), 
             con = output_file)
}

# Directory and prefix settings
directory <- "~/"
prefix_height <- "height"
prefix_bmi <- "bmi"
prefix_brc <- "brc"

# Expected sizes and column numbers
expected_train_size <- 3000
expected_test_size_height <- 36899 - expected_train_size
expected_test_size_bmi <- 36461 - expected_train_size
expected_test_size_brc <- 60043 - expected_train_size
expected_group_size <- 3000

expected_train_cols_height <- 77
expected_train_cols_bmi <- 57
expected_train_cols_brc <- 113

expected_test_cols_height <- 77
expected_test_cols_bmi <- 57
expected_test_cols_brc <- 113

expected_group_cols_height <- 77
expected_group_cols_bmi <- 57
expected_group_cols_brc <- 113

# Check files for each dataset
cat("Checking HEIGHT files...\n")
check_files(directory, prefix_height, iterations = 30, n_groups = 20, expected_train_size, expected_test_size_height, expected_group_size, expected_train_cols_height, expected_test_cols_height, expected_group_cols_height, output_file = "height_check_results.txt")

cat("\nChecking BMI files...\n")
check_files(directory, prefix_bmi, iterations = 30, n_groups = 20, expected_train_size, expected_test_size_bmi, expected_group_size, expected_train_cols_bmi, expected_test_cols_bmi, expected_group_cols_bmi, output_file = "bmi_check_results.txt")

cat("\nChecking BRC files...\n")
check_files(directory, prefix_brc, iterations = 30, n_groups = 20, expected_train_size, expected_test_size_brc, expected_group_size, expected_train_cols_brc, expected_test_cols_brc, expected_group_cols_brc, output_file = "brc_check_results.txt")
