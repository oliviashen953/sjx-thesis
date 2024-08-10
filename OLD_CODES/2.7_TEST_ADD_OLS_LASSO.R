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
    
    # Print PGS columns for debugging
    print("PGS Columns:")
    print(pgs_columns)
    
    # Load OLS coefficients
    ols_file <- sprintf("~/2.1_ols_supervised_learning_coefficients/ols_coefficients_height_iteration_%d.rds", iteration)
    if (file.exists(ols_file)) {
      ols_coefficients <- readRDS(ols_file)
      
      # Print OLS coefficients for debugging
      print("OLS Coefficients:")
      print(ols_coefficients)
      
      # Ensure the coefficients match the PGS columns
      if (length(ols_coefficients) == length(pgs_columns)) {
        # Calculate OLS score using matrix multiplication
        ols_score <- as.matrix(height_group[, pgs_columns]) %*% as.numeric(ols_coefficients)
        height_group$ols_score <- as.vector(ols_score)
        height_group$ols_rank <- rank(-height_group$ols_score, ties.method = "average")
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
      
      # Print LASSO coefficients for debugging
      print("LASSO Coefficients:")
      print(lasso_coefficients)
      
      # Ensure the number of coefficients matches the number of PGS columns
      if (length(lasso_coefficients) == length(pgs_columns)) {
        # Calculate LASSO score using matrix multiplication
        lasso_score <- as.matrix(height_group[, pgs_columns]) %*% lasso_coefficients
        height_group$lasso_score <- as.vector(lasso_score)
        height_group$lasso_rank <- rank(-height_group$lasso_score, ties.method = "average")
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

# Test the function on subgroup 1, iteration 1
add_ols_lasso_ranks(1, 1)


