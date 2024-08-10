####################
####################
# JuLY 22, 2024
####################
####################

# Load the updated dataset with "race" column
updated_eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_Height.txt")
View(updated_eMERGEIII_pheno_covar_Height)
updated_eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BMI.txt")
View(updated_eMERGEIII_pheno_covar_BMI)
updated_eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BrC.txt")
View(updated_eMERGEIII_pheno_covar_BrC)

# Load necessary libraries
library(dplyr)
library(fs)
library(glmnet)
library(Matrix)




 
# Function to perform OLS regression and save coefficients
perform_ols_regression <- function(dataset_path, dataset_name, iterations) {
  for (iter in 1:iterations) {
    # Load training data
    train_data <- read.csv(paste0(dataset_path, "/AGE65_", dataset_name, "_train_data_iteration_", iter, ".csv"))
    
    # Identify PGS columns (assuming they start with "PGS")
    pgs_columns <- grep("PGS", names(train_data), value = TRUE)
    
    # Prepare the formula for OLS regression
    formula <- paste("pheno ~", paste(pgs_columns, collapse = " + "))
    
    # Fit the linear model
    model <- lm(formula, data = train_data)
    
    # Extract coefficients (excluding intercept)
    coefficients <- model$coefficients[-1]
    
    # Save coefficients to an RDS file
    saveRDS(coefficients, paste0("AGE65_coefficients_", dataset_name, "_iteration_", iter, ".rds"))
  }
}

# Function to perform Lasso regression and save coefficients
perform_lasso_regression <- function(dataset_path, dataset_name, iterations, lambda_seq, n_folds = 10) {
  for (iter in 1:iterations) {
    # Load training data
    train_data <- read.csv(paste0(dataset_path, "/AGE65_", dataset_name, "_train_data_iteration_", iter, ".csv"))
    
    # Identify PGS columns (assuming they start with "PGS")
    pgs_columns <- grep("^PGS", names(train_data), value = TRUE)
    
    # Extract response variable (assuming 'pheno' is the response variable)
    response <- train_data$pheno
    
    # Extract predictor variables (only PGS columns)
    predictors <- as.matrix(train_data[, pgs_columns])
    
    # Perform Lasso regression using cross-validation
    cvfit <- cv.glmnet(predictors, response, alpha = 1, lambda = lambda_seq, nfolds = n_folds)
    
    # Select the lambda with minimum mean cross-validated error
    best_lambda <- cvfit$lambda.min
    
    # Fit final Lasso model with the selected lambda
    lasso_model <- glmnet(predictors, response, alpha = 1, lambda = best_lambda)
    
    # Extract coefficients (excluding intercept)
    coefficients <- coef(lasso_model)[-1]
    
    # Save coefficients to an RDS file
    saveRDS(coefficients, paste0("AGE65_lasso_coefficients_", dataset_name, "_iteration_", iter, ".rds"))
  }
}

# Function to find the best performing PGS column based on R-squared
perform_best_r_squared <- function(dataset_path, dataset_name, iterations) {
  for (iter in 1:iterations) {
    # Load training data
    train_data <- read.csv(paste0(dataset_path, "/AGE65_", dataset_name, "_train_data_iteration_", iter, ".csv"))
    
    # Identify PGS columns (assuming they start with "PGS")
    pgs_columns <- grep("PGS", names(train_data), value = TRUE)
    
    # Initialize variables to track the best R-squared and the corresponding column
    best_r_squared <- -Inf
    best_column <- NULL
    
    # Loop through each PGS column and fit a linear model
    for (pgs_col in pgs_columns) {
      formula <- paste("pheno ~", pgs_col)
      model <- lm(formula, data = train_data)
      r_squared <- summary(model)$r.squared
      
      # Update the best R-squared and column if this model is better
      if (r_squared > best_r_squared) {
        best_r_squared <- r_squared
        best_column <- pgs_col
      }
    }
    
    # Store the best PGS column for this iteration
    saveRDS(best_column, paste0("AGE65_best_pgs_column_", dataset_name, "_iteration_", iter, ".rds"))
  }
}

# Example usage with your datasets for OLS
perform_ols_regression("2.2_AGE65_height/training_files", "height", iterations = 37)
perform_ols_regression("2.2_AGE65_bmi/training_files", "bmi", iterations = 37)
perform_ols_regression("2.2_AGE65_brc/training_files", "brc", iterations = 37)

# perform_ols_regression("2.2_RACE_height/training_files", "height", iterations = 1)
# perform_ols_regression("2.2_RACE_bmi/training_files", "bmi", iterations = 37)
# perform_ols_regression("2.2_RACE_brc/training_files", "brc", iterations = 37)

# Example usage with your datasets for Lasso
perform_lasso_regression("2.2_AGE65_height/training_files", "height", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))
perform_lasso_regression("2.2_AGE65_bmi/training_files", "bmi", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))
perform_lasso_regression("2.2_AGE65_brc/training_files", "brc", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))

# perform_lasso_regression("2.2_RACE_height/training_files", "height", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))
# perform_lasso_regression("2.2_RACE_bmi/training_files", "bmi", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))
# perform_lasso_regression("2.2_RACE_brc/training_files", "brc", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))

# Example usage with your datasets for Best Performing PGS
perform_best_r_squared("2.2_AGE65_height/training_files", "height", iterations = 37)
perform_best_r_squared("2.2_AGE65_bmi/training_files", "bmi", iterations = 37)
perform_best_r_squared("2.2_AGE65_brc/training_files", "brc", iterations = 37)

# perform_best_r_squared("2.2_RACE_height/training_files", "height", iterations = 37)
# perform_best_r_squared("2.2_RACE_bmi/training_files", "bmi", iterations = 37)
# perform_best_r_squared("2.2_RACE_brc/training_files", "brc", iterations = 37)

#### MOVE OLS, LASSO, & BEST PGS COEFFICIENTS FILES ####
# Define the directories
old_dir <- "~/"
ols_new_dir <- "2.1_AGE65_ols_supervised_learning_coefficients"
lasso_new_dir <- "2.1_AGE65_lasso_supervised_learning_coefficients"
best_pgs_new_dir <- "2.1_AGE65_best_pgs_columns"

# Create new directories if they do not exist
dir_create(ols_new_dir)
dir_create(lasso_new_dir)
dir_create(best_pgs_new_dir)

# Rename and move OLS .rds files
ols_files <- list.files(path = old_dir, pattern = "^AGE65_coefficients_.*\\.rds$", full.names = TRUE)
for (file in ols_files) {
  # Extract the base name
  base_name <- basename(file)
  
  # Construct new name
  new_base_name <- gsub("coefficients_", "AGE65_ols_coefficients_", base_name)
  
  # Move and rename the file
  new_file <- file.path(ols_new_dir, new_base_name)
  file.rename(file, new_file)
}

# Rename and move Lasso .rds files
lasso_files <- list.files(path = old_dir, pattern = "^AGE65_lasso_coefficients_.*\\.rds$", full.names = TRUE)
for (file in lasso_files) {
  # Extract the base name
  base_name <- basename(file)
  
  # Move and rename the file
  new_file <- file.path(lasso_new_dir, base_name)
  file.rename(file, new_file)
}

# Rename and move best PGS .rds files
best_pgs_files <- list.files(path = old_dir, pattern = "^AGE65_best_pgs_column_.*\\.rds$", full.names = TRUE)
for (file in best_pgs_files) {
  # Extract the base name
  base_name <- basename(file)
  
  # Move and rename the file
  new_file <- file.path(best_pgs_new_dir, base_name)
  file.rename(file, new_file)
}

print("Files have been renamed and moved to the new directories.")



#### RENAME FILES UNDER 2,1_AGE65_ols_supervised_learning_coefficients ####

# Load necessary library
library(fs)

# Function to rename files to remove duplicate "AGE65_AGE65" prefix
correct_filenames <- function(directory) {
  # List all files in the directory
  files <- list.files(path = directory, pattern = "AGE65_AGE65", full.names = TRUE)
  
  # Loop through each file and rename it
  for (file in files) {
    # Extract the base name
    base_name <- basename(file)
    
    # Construct new name by removing the duplicate "AGE65_AGE65" prefix
    new_base_name <- gsub("AGE65_AGE65", "AGE65", base_name)
    
    # Rename the file
    new_file <- file.path(directory, new_base_name)
    file.rename(file, new_file)
  }
  
  print("Filenames have been corrected.")
}

# Apply the function to the directory with the duplicate filenames
correct_filenames("2.1_AGE65_ols_supervised_learning_coefficients")


