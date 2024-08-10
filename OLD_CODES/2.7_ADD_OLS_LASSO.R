library(readr)
library(dplyr)

# Function to add OLS and LASSO ranks
add_ols_lasso_ranks <- function(subgroup, iteration) {
  # Load the existing with_rank file
  file_name <- sprintf("2.2_height_test_subgroup_files/height_subgroup_%d_iteration_%d_with_ranks.csv", subgroup, iteration)
  
  if (file.exists(file_name)) {
    height_group <- read_csv(file_name)
    
    # Identify the PGS columns (excluding rank columns)
    pgs_columns <- grep("^PGS\\d+$", names(height_group), value = TRUE)
    
    # Print the number of PGS columns for verification
    cat(sprintf("Subgroup %d, Iteration %d: Number of PGS columns: %d\n", subgroup, iteration, length(pgs_columns)))
    
    # Load OLS coefficients
    ols_file <- sprintf("~/2.1_ols_supervised_learning_coefficients/ols_coefficients_height_iteration_%d.rds", iteration)
    if (file.exists(ols_file)) {
      ols_coefficients <- readRDS(ols_file)
      
      # Print the number of OLS coefficients for verification
      cat(sprintf("Subgroup %d, Iteration %d: Number of OLS coefficients: %d\n", subgroup, iteration, length(ols_coefficients)))
      # Print the OLS coefficients for verification
      cat(sprintf("OLS coefficients for iteration %d:\n%s\n", iteration, toString(ols_coefficients)))
      
      # Ensure the coefficients match the PGS columns
      if (length(ols_coefficients) == length(pgs_columns)) {
        # Calculate OLS score using matrix multiplication
        ols_score <- as.matrix(height_group[, pgs_columns]) %*% as.numeric(ols_coefficients)
        height_group$ols_score <- as.vector(ols_score)
        height_group$ols_rank <- rank(-height_group$ols_score, ties.method = "average")
        
        # Print a few sample values for verification
        cat(sprintf("Sample OLS scores for subgroup %d, iteration %d: %s\n", subgroup, iteration, toString(head(ols_score, 5))))
      } else {
        cat("OLS coefficients do not match PGS columns\n")
      }
    } else {
      cat("OLS coefficients file does not exist\n")
    }
    
    # Load LASSO coefficients
    lasso_file <- sprintf("~/2.1_lasso_supervised_learning_coefficients/lasso_coefficients_height_iteration_%d.rds", iteration)
    if (file.exists(lasso_file)) {
      lasso_coefficients <- readRDS(lasso_file)
      
      # Print the number of LASSO coefficients for verification
      cat(sprintf("Subgroup %d, Iteration %d: Number of LASSO coefficients: %d\n", subgroup, iteration, length(lasso_coefficients)))
      # Print the LASSO coefficients for verification
      cat(sprintf("LASSO coefficients for iteration %d:\n%s\n", iteration, toString(lasso_coefficients)))
      
      # Ensure the number of coefficients matches the number of PGS columns
      if (length(lasso_coefficients) == length(pgs_columns)) {
        # Calculate LASSO score using matrix multiplication
        lasso_score <- as.matrix(height_group[, pgs_columns]) %*% lasso_coefficients
        height_group$lasso_score <- as.vector(lasso_score)
        height_group$lasso_rank <- rank(-height_group$lasso_score, ties.method = "average")
        
        # Print a few sample values for verification
        cat(sprintf("Sample LASSO scores for subgroup %d, iteration %d: %s\n", subgroup, iteration, toString(head(lasso_score, 5))))
      } else {
        cat("LASSO coefficients do not match PGS columns in length\n")
      }
    } else {
      cat("LASSO coefficients file does not exist\n")
    }
    
    # Save the updated dataframe
    write_csv(height_group, file_name)
    cat(sprintf("Processed and updated ranks for subgroup %d, iteration %d\n", subgroup, iteration))
  }
}

# Loop through all subgroups and iterations
for (iteration in 1:30) {
  for (subgroup in 1:20) {
    add_ols_lasso_ranks(subgroup, iteration)
  }
}

