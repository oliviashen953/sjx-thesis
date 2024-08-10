# 
# To handle potential NA values in the OLS and LASSO coefficients, we can replace NA values with zeros before calculating the scores. 
# This ensures that any NA values do not affect the final OLS or LASSO scores and ranks.
# 
# Additionally, to ensure that the script overwrites already processed files, we will include an option to overwrite the files.
# 





library(readr)
library(dplyr)

# Function to add OLS and LASSO ranks
add_ols_lasso_ranks <- function(trait, subgroup, iteration, log_file) {
  # Define the file paths
  file_name <- sprintf("~/NORMAL/2.2_%s_test_subgroup_files/%s_subgroup_%d_iteration_%d_with_ranks.csv", trait, trait, subgroup, iteration)
  
  if (file.exists(file_name)) {
    data_group <- read_csv(file_name)
    
    # Identify the PGS columns (excluding rank columns)
    pgs_columns <- grep("^PGS\\d+$", names(data_group), value = TRUE)
    
    # Print the number of PGS columns for verification
    cat(sprintf("%s Subgroup %d, Iteration %d: Number of PGS columns: %d\n", trait, subgroup, iteration, length(pgs_columns)), file = log_file, append = TRUE)
    
    # Load OLS coefficients
    ols_file <- sprintf("~/NORMAL/%s_ols/coefficients_%s_iteration_%d.rds", trait, trait, iteration)
    if (file.exists(ols_file)) {
      ols_coefficients <- readRDS(ols_file)
      
      # Print the number of OLS coefficients for verification
      cat(sprintf("%s Subgroup %d, Iteration %d: Number of OLS coefficients: %d\n", trait, subgroup, iteration, length(ols_coefficients)), file = log_file, append = TRUE)
      
      # Replace NA values with 0 in OLS coefficients
      ols_coefficients[is.na(ols_coefficients)] <- 0
      
      # Ensure the coefficients match the PGS columns
      if (length(ols_coefficients) == length(pgs_columns)) {
        # Calculate OLS score using matrix multiplication
        ols_score <- as.matrix(data_group[, pgs_columns]) %*% as.numeric(ols_coefficients)
        data_group$ols_score <- as.vector(ols_score)
        data_group$ols_rank <- rank(-data_group$ols_score, ties.method = "average")
        
        # Print a few sample values for verification
        cat(sprintf("Sample OLS scores for %s subgroup %d, iteration %d: %s\n", trait, subgroup, iteration, toString(head(ols_score, 5))), file = log_file, append = TRUE)
      } else {
        cat("OLS coefficients do not match PGS columns\n", file = log_file, append = TRUE)
      }
    } else {
      cat("OLS coefficients file does not exist\n", file = log_file, append = TRUE)
    }
    
    # Load LASSO coefficients
    lasso_file <- sprintf("~/NORMAL/%s_lasso/coefficients_%s_iteration_%d.rds", trait, trait, iteration)
    if (file.exists(lasso_file)) {
      lasso_coefficients <- readRDS(lasso_file)
      
      # Print the number of LASSO coefficients for verification
      cat(sprintf("%s Subgroup %d, Iteration %d: Number of LASSO coefficients: %d\n", trait, subgroup, iteration, length(lasso_coefficients)), file = log_file, append = TRUE)
      
      # Replace NA values with 0 in LASSO coefficients
      lasso_coefficients[is.na(lasso_coefficients)] <- 0
      
      # Ensure the number of coefficients matches the number of PGS columns
      if (length(lasso_coefficients) == length(pgs_columns)) {
        # Calculate LASSO score using matrix multiplication
        lasso_score <- as.matrix(data_group[, pgs_columns]) %*% lasso_coefficients
        data_group$lasso_score <- as.vector(lasso_score)
        data_group$lasso_rank <- rank(-data_group$lasso_score, ties.method = "average")
        
        # Print a few sample values for verification
        cat(sprintf("Sample LASSO scores for %s subgroup %d, iteration %d: %s\n", trait, subgroup, iteration, toString(head(lasso_score, 5))), file = log_file, append = TRUE)
      } else {
        cat("LASSO coefficients do not match PGS columns in length\n", file = log_file, append = TRUE)
      }
    } else {
      cat("LASSO coefficients file does not exist\n", file = log_file, append = TRUE)
    }
    
    # Save the updated dataframe, allowing overwrite
    write_csv(data_group, file_name)
    cat(sprintf("Processed and updated ranks for %s subgroup %d, iteration %d\n", trait, subgroup, iteration), file = log_file, append = TRUE)
  }
}

# Specify the log file
log_file <- "~/NORMAL/correct_2.7.1_OLS_processing_log.txt"

# Traits to process
traits <- c("height", "bmi", "brc")

# Loop through all subgroups and iterations for each trait
for (iteration in 1:30) {
  for (subgroup in 1:20) {
    for (trait in traits) {
      add_ols_lasso_ranks(trait, subgroup, iteration, log_file)
    }
  }
}

cat("Processing complete. See log file for details.\n")

