# Load necessary libraries
library(dplyr)

# Function to check OLS and Lasso regression output files
check_output_files <- function(directory, dataset_name, expected_num_coeffs, iterations, log_file) {
  # Initialize a character vector to store log messages
  log_messages <- character()
  
  # Check OLS files
  ols_dir <- file.path(directory, paste0(dataset_name, "_ols"))
  for (iter in 1:iterations) {
    ols_file <- file.path(ols_dir, paste0("coefficients_", dataset_name, "_iteration_", iter, ".rds"))
    if (file.exists(ols_file)) {
      coefficients <- readRDS(ols_file)
      num_coeffs <- length(coefficients)
      if (num_coeffs != expected_num_coeffs) {
        log_messages <- c(log_messages, paste("OLS file", ols_file, "has", num_coeffs, "coefficients instead of", expected_num_coeffs))
      } else {
        log_messages <- c(log_messages, paste("OLS file", ols_file, "has the correct number of coefficients:", num_coeffs))
      }
    } else {
      log_messages <- c(log_messages, paste("OLS file", ols_file, "not found"))
    }
  }
  
  # Check Lasso files
  lasso_dir <- file.path(directory, paste0(dataset_name, "_lasso"))
  for (iter in 1:iterations) {
    lasso_file <- file.path(lasso_dir, paste0("coefficients_", dataset_name, "_iteration_", iter, ".rds"))
    if (file.exists(lasso_file)) {
      coefficients <- readRDS(lasso_file)
      num_coeffs <- length(coefficients)
      if (num_coeffs != expected_num_coeffs) {
        log_messages <- c(log_messages, paste("Lasso file", lasso_file, "has", num_coeffs, "coefficients instead of", expected_num_coeffs))
      } else {
        log_messages <- c(log_messages, paste("Lasso file", lasso_file, "has the correct number of coefficients:", num_coeffs))
      }
    } else {
      log_messages <- c(log_messages, paste("Lasso file", lasso_file, "not found"))
    }
  }
  
  # Check Best PGS column selection file
  best_pgs_file <- file.path(directory, paste0(dataset_name, "_best_pgs"), paste0("best_pgs_columns_", dataset_name, ".csv"))
  if (file.exists(best_pgs_file)) {
    best_pgs_data <- read.csv(best_pgs_file)
    num_rows <- nrow(best_pgs_data)
    if (num_rows != iterations) {
      log_messages <- c(log_messages, paste("Best PGS file", best_pgs_file, "has", num_rows, "rows instead of", iterations))
    } else {
      log_messages <- c(log_messages, paste("Best PGS file", best_pgs_file, "has the correct number of rows:", num_rows))
    }
  } else {
    log_messages <- c(log_messages, paste("Best PGS file", best_pgs_file, "not found"))
  }
  
  # Write log messages to the log file
  writeLines(log_messages, con = log_file)
}

# Directory and prefix settings
directory <- "~/NORMAL"

# Expected number of coefficients for each dataset
expected_num_coeffs_height <- 72
expected_num_coeffs_bmi <- 52
expected_num_coeffs_brc <- 108

# Number of iterations
iterations <- 30

# Log file paths
log_file_height <- file.path(directory, "height_output_check.txt")
log_file_bmi <- file.path(directory, "bmi_output_check.txt")
log_file_brc <- file.path(directory, "brc_output_check.txt")

# Check output files for each dataset
cat("Checking HEIGHT files...\n")
check_output_files(directory, "height", expected_num_coeffs_height, iterations, log_file_height)

cat("Checking BMI files...\n")
check_output_files(directory, "bmi", expected_num_coeffs_bmi, iterations, log_file_bmi)

cat("Checking BRC files...\n")
check_output_files(directory, "brc", expected_num_coeffs_brc, iterations, log_file_brc)

cat("Output checks completed. See log files for details.\n")

