####################    
#################### 
# June 9, 2024
#################### 
#################### 


# load the updated dataset with "race" column
updated_eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_Height.txt")
View(updated_eMERGEIII_pheno_covar_Height)
updated_eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BMI.txt")
View(updated_eMERGEIII_pheno_covar_BMI)
updated_eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BrC.txt")
View(updated_eMERGEIII_pheno_covar_BrC)


# section 2.1, supervised learning on training datasets(size 3000 for each iteration 1:37)
# TRAINING FILES NAME: 
    # height_train_data_iteration_1.csv
    # height_train_data_iteration_37.csv
    # brc_train_data_iteration_1.csv
    # brc_train_data_iteration_37.csv
    # bmi_train_data_iteration_1.csv
    # bmi_train_data_iteration_37.csv


####################################################      
####################################################
##### 2. Individual Calculation               ######
####################################################
####################################################

    ##############################################################      
    ##############################################################
    ##### 2.1 Supervised OLS & Supervised Best Performing  #######
    ##############################################################
    ##############################################################

          
          ##############################################################
          ##### 2.1.A Supervised OLS REGRESSION #######
          ##############################################################
          # Ordinary Least Squares regression (OLS)
          # Fit a single linear regression with outcome being the pheno 
          # and each covariate being each PGS score column. 
          # Record all coefficients after this fitting.

              # Load necessary libraries (if not already loaded)
              library(dplyr)
              
              # Function to perform OLS regression and save coefficients
              perform_ols_regression <- function(dataset_name, iterations) {
                for (iter in 1:iterations) {
                  # Load training data
                  train_data <- read.csv(paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
                  
                  # Identify PGS columns (assuming they start with "PGS")
                  pgs_columns <- grep("PGS", names(train_data), value = TRUE)
                  
                  # Prepare the formula for OLS regression
                  formula <- paste("pheno ~", paste(pgs_columns, collapse = " + "))
                  
                  # Fit the linear model
                  model <- lm(formula, data = train_data)
                  
                  # Extract coefficients (excluding intercept)
                  coefficients <- model$coefficients[-1]
                  
                  # Save coefficients to an RDS file
                  saveRDS(coefficients, paste0("coefficients_", dataset_name, "_iteration_", iter, ".rds"))
                }
              }
            
              
              perform_ols_regression("height", iterations = 1)
              perform_ols_regression("bmi", iterations = 37)
              perform_ols_regression("brc", iterations = 37)
              
              ###############################
              ###### CHECK COEFFICIENTS #####
              ###############################
              
              # # Perform OLS regression for each dataset
              # perform_ols_regression("height", iterations = 1)
              # coefficients_height_iteration_1 <- readRDS("~/coefficients_height_iteration_1.rds")
              # coefficients_height_iteration_1
              # 
              # perform_ols_regression("bmi", iterations = 1)
              # coefficients_bmi_iteration_1 <- readRDS("~/coefficients_bmi_iteration_1.rds")
              # coefficients_bmi_iteration_1
              # 
              # perform_ols_regression("brc", iterations = 1)
              # coefficients_brc_iteration_1 <- readRDS("~/coefficients_brc_iteration_1.rds")
              # coefficients_brc_iteration_1






          ##############################################################
          ##### 2.1.B Supervised LASSO REGRESSSION  #######
          ##############################################################
          # Lasso regression, also called [[ L1 regularization ]], 
          # is a form of regularization for linear regression models.
              
              # " Ridge and LASSO regression are good enough to be applied as an alternative 
              # if our Ordinary Least Square (OLS) model has multicollinearity problems. 
              # Ridge and LASSO regression work by adding the bias parameter (λ) so that 
              # the estimator variance is reduced "
              
              # To perform Lasso regression instead of OLS regression and save the coefficients, 
              # we will use the glmnet package in R. Lasso regression includes a regularization 
              # parameter (lambda) that controls the strength of regularization applied to the coefficients.
              
              # 1. Install and Load Required Packages
              install.packages("glmnet")
              library(glmnet)
              library(Matrix)  # Necessary for glmnet
              
              
              
              # 2. Define Function for Lasso Regression
              # Create a function that performs Lasso regression on dataset and saves the coefficients:
              perform_lasso_regression <- function(dataset_name, iterations, lambda_seq, n_folds = 10) {
                
                # 10 Folds for Cross-Validation: This choice provides a good balance between 
                # computational efficiency and reliable estimation of model performance.
                
                for (iter in 1:iterations) {
                  # Load training data
                  train_data <- read.csv(paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
                  
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
                  saveRDS(coefficients, paste0("lasso_coefficients_", dataset_name, "_iteration_", iter, ".rds"))
                }
              }
              
            
              
              
              
              # 3. Example Usage with Your Datasets
              # Apply the perform_lasso_regression function to each dataset (height, bmi, brc), 
              # specifying a sequence of lambda values (lambda_seq) and the number of folds 
              # for cross-validation (n_folds).
              
              # Example usage for HEIGHT dataset
              perform_lasso_regression("height", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))
              lasso_coefficients_height_iteration_1 <- readRDS("~/lasso_coefficients_height_iteration_1.rds")
              lasso_coefficients_height_iteration_1
              
              # Example usage for BMI dataset
              perform_lasso_regression("bmi", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))
              
              # Example usage for BRC dataset
              perform_lasso_regression("brc", iterations = 37, lambda_seq = 10^seq(10, -2, length = 100))
              
              ############################################
              ##### comment on lasso implementation ##### 
              ############################################
              
              # 10 Folds for Cross-Validation: This choice provides a good balance between 
              # computational efficiency and reliable estimation of model performance.
                  # Reason for Choosing 10 Folds:
                  #   
                  #  Bias-Variance Tradeoff: Using 10-fold cross-validation strikes a balance between bias and variance. It provides a good estimation of model performance without too high computational cost.
                  # Common Practice: It is a widely adopted standard in the field. Research has shown that 10-fold cross-validation generally provides a reliable estimate of model performance.
                  # Computational Efficiency: It is more computationally efficient than leave-one-out cross-validation (LOOCV), which has very high computational cost, especially with large datasets.
                  
           
              # Lambda Sequence (10^seq(10, -2, length = 100)): 
              # This choice ensures a wide and fine-grained exploration of possible regularization strengths, 
              # allowing the model to find an optimal balance between bias and variance.
              
              # Wide Range of Values: The sequence 10^seq(10, -2, length = 100) covers a wide range of lambda values from 
              # 10^10 (a very high regularization) to 10^-2, (a very high regularization) to (a very low regularization). This ensures that the optimal lambda value can be found within this range.
              # 
              # Logarithmic Scale: Using a logarithmic scale ensures that the sequence includes both very large and very small values of lambda, as well as intermediate values. This is important because the impact of lambda on model performance is often not linear.
              
              # Fine Tuning: By using 100 values in the sequence, the tuning process is fine-grained enough to find an appropriate lambda value without being overly computationally expensive.


              
       
  #### MOVE OLS & LASSO COEFFICIENTS FILES ####
  # Define the directories
  old_dir <- "~/"
  ols_new_dir <- "2.1_ols_supervised_learning_coefficients"
  lasso_new_dir <- "2.1_lasso_supervised_learning_coefficients"
  
  # Create new directories if they do not exist
  dir.create(ols_new_dir, showWarnings = FALSE)
  dir.create(lasso_new_dir, showWarnings = FALSE)
  
  # Move OLS .rds files
  ols_files <- list.files(path = old_dir, pattern = "coefficients_.*\\.rds", full.names = TRUE)
  for (file in ols_files) {
    new_file <- file.path(ols_new_dir, basename(file))
    file.rename(file, new_file)
  }
  
  
  
  # Rename and move OLS .rds files
  # The pattern ^coefficients_.*\\.rds$ ensures that only files starting with 
  # "coefficients_" and ending with ".rds" are selected, 
  # thus excluding "lasso_coefficients_" files
  ols_files <- list.files(path = old_dir, pattern = "^coefficients_.*\\.rds$", full.names = TRUE)
  # 111 = 37*3
  for (file in ols_files) {
    # Extract the base name
    base_name <- basename(file)
    
    # Construct new name
    new_base_name <- gsub("coefficients_", "ols_coefficients_", base_name)
    
    # Move and rename the file
    new_file <- file.path(ols_new_dir, new_base_name)
    file.rename(file, new_file)
  }
  

  
  
  # Rename and move Lasso .rds files
  lasso_files <- list.files(path = old_dir, pattern = "^lasso_coefficients_.*\\.rds$", full.names = TRUE)
  lasso_files   
  # 111 = 37*3
  for (file in lasso_files) {
    # Extract the base name
    base_name <- basename(file)
    
    # Construct new name
    new_base_name <- gsub("lasso_coefficients_", "lasso_coefficients_", base_name)
    
    # Move and rename the file
    new_file <- file.path(lasso_new_dir, new_base_name)
    file.rename(file, new_file)
  }
  
  print("Files have been renamed and moved to the new directories.")
  

              
              
              
              
              
              
          ##############################################################
          ##### 2.1.C Supervised BEST PERFORMING  #######
          ##############################################################
          # Fit each single PGS score column with the outcome and 
          # record the R-squares for each single fitting. 
          # Record the PGS score column number with the highest R-squared.
  
          # Load necessary libraries
          library(dplyr)
          
          # Function to find the best performing PGS column based on R-squared
          perform_best_r_squared <- function(dataset_name, iterations) {
            for (iter in 1:iterations) {
              # Load training data
              train_data <- read.csv(paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
              
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
              saveRDS(best_column, paste0("best_pgs_column_", dataset_name, "_iteration_", iter, ".rds"))
            }
          }
          
          # Example usage with your datasets
          perform_best_r_squared("height", iterations = 37)
          perform_best_r_squared("bmi", iterations = 37)
          perform_best_r_squared("brc", iterations = 37)
          
          # best_pgs_column_height_iteration_1 <- readRDS("~/best_pgs_column_height_iteration_1.rds")
          # best_pgs_column_height_iteration_1
          
          
          
          ########################################################################
          # move all the existing .rds files to newly created directories,
          ########################################################################
          
          old_dir <- "~/"
          # Define the directories
          best_pgs_new_dir <- "2.1_best_pgs_columns"
          
          # Create new directories if they do not exist
          dir.create(best_pgs_new_dir, showWarnings = FALSE)
        
          
          # Move best PGS .rds files
          best_pgs_files <- list.files(path = old_dir, pattern = "^best_pgs_column_.*\\.rds$", full.names = TRUE)
          best_pgs_files
          for (file in best_pgs_files) {
            new_file <- file.path(best_pgs_new_dir, basename(file))
            file.rename(file, new_file)
          }
          
          print("Files have been moved to the new directories.")
          



########################
# SUPPLEMENT CODE
########################
            # load library
            library(readr)
        
            # # load each of the 30(or 37) groups of the training data for supervised learning
            # height_train_data_iteration_1 <- read_csv("height_train_data_iteration_1.csv")
            # # ...
            # height_train_data_iteration_37 <- read_csv("height_train_data_iteration_37.csv")
            # View(height_train_data_iteration_1)
            # View(height_train_data_iteration_37)
            # # 3000 rows/subjects * 77 columns
        
            #### 2.1.a : Supervised OLS ####
            # Supervised OLS: Fit a single linear regression with outcome being the pheno 
            # and each covariate being each PGS score column. Record all coefficients after this fitting.
        
            #### 2.1.b : Supervised Best Performing ####
          
        
          
          
          
          
          
            
# ##### FILES TO MOVE ALL RDS FILES TO NEWLY DEFINED DERIECTORY ####
# # Define the directories
# old_dir <- "~/path_to_existing_rds_files"
# ols_new_dir <- "2.1_ols_supervised_learning_coefficients"
# lasso_new_dir <- "2.1_lasso_supervised_learning_coefficients"
# best_pgs_new_dir <- "2.1_best_pgs_columns"
# 
# # Create new directories if they do not exist
# dir.create(ols_new_dir, showWarnings = FALSE)
# dir.create(lasso_new_dir, showWarnings = FALSE)
# dir.create(best_pgs_new_dir, showWarnings = FALSE)
# 
# # Move OLS .rds files
# ols_files <- list.files(path = old_dir, pattern = "^coefficients_.*\\.rds$", full.names = TRUE)
# for (file in ols_files) {
#   new_file <- file.path(ols_new_dir, basename(file))
#   file.rename(file, new_file)
# }
# 
# # Move Lasso .rds files
# lasso_files <- list.files(path = old_dir, pattern = "^lasso_coefficients_.*\\.rds$", full.names = TRUE)
# for (file in lasso_files) {
#   new_file <- file.path(lasso_new_dir, basename(file))
#   file.rename(file, new_file)
# }
# 
# # Move best PGS .rds files
# best_pgs_files <- list.files(path = old_dir, pattern = "^best_pgs_column_.*\\.rds$", full.names = TRUE)
# for (file in best_pgs_files) {
#   new_file <- file.path(best_pgs_new_dir, basename(file))
#   file.rename(file, new_file)
# }
# 
# print("Files have been moved to the new directories.")
# 

