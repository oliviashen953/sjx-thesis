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
# 
# "{~/height} {~/brc} {~/bmi} 
# height_train_data_iteration_30 <- read_csv("NORMAL/height/height_train_data_iteration_30.csv")
# bmi_train_data_iteration_30 <- read_csv(""NORMAL/bmi/bmi_train_data_iteration_30.csv"")
# brc_train_data_iteration_30 <- read_csv(""NORMAL/brc/brc_train_data_iteration_30.csv"")


# TRAINING FILES NAME: 
# height_train_data_iteration_1.csv
# height_train_data_iteration_30.csv
# brc_train_data_iteration_1.csv
# brc_train_data_iteration_30.csv
# bmi_train_data_iteration_1.csv
# bmi_train_data_iteration_30.csv


####################################################      
####################################################
##### 2. Individual Calculation               ######
####################################################
####################################################

##############################################################
##### 2.1.A Supervised OLS REGRESSION #######
##############################################################
# Ordinary Least Squares regression (OLS)
# Fit a single linear regression with outcome being the pheno 
# and each covariate being each PGS score column. 
# Record all coefficients after this fitting.

# Load necessary libraries
library(dplyr)

# Function to perform OLS regression and save coefficients
perform_ols_regression <- function(directory, dataset_name, iterations) {
  # Create new directory for saving OLS results
  new_dir <- file.path(directory, paste0(dataset_name, "_ols"))
  dir.create(new_dir, showWarnings = FALSE)
  
  for (iter in 1:iterations) {
    # Load training data
    train_file <- file.path(directory, dataset_name, paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
    train_data <- read.csv(train_file)
    
    # Identify PGS columns (assuming they start with "PGS")
    pgs_columns <- grep("PGS", names(train_data), value = TRUE)
    
    # Prepare the formula for OLS regression
    formula <- paste("pheno ~", paste(pgs_columns, collapse = " + "))
    
    # Fit the linear model
    model <- lm(formula, data = train_data)
    
    # Extract coefficients (excluding intercept)
    coefficients <- model$coefficients[-1]
    
    # Save coefficients to an RDS file
    saveRDS(coefficients, file.path(new_dir, paste0("coefficients_", dataset_name, "_iteration_", iter, ".rds")))
  }
}

# Directory and prefix settings
directory <- "~/NORMAL"

# Perform OLS regression for each dataset
perform_ols_regression(directory, "height", iterations = 30)
perform_ols_regression(directory, "bmi", iterations = 30)
perform_ols_regression(directory, "brc", iterations = 30)


###############################
###### CHECK COEFFICIENTS #####
###############################


##############################################################
##### 2.1.B Supervised LASSO REGRESSION #######
##############################################################
# Lasso regression, also called L1 regularization, 
# is a form of regularization for linear regression models.

# Load necessary libraries
#install.packages("glmnet")
library(glmnet)
library(Matrix)  # Necessary for glmnet

# Function to perform Lasso regression and save coefficients
perform_lasso_regression <- function(directory, dataset_name, iterations, lambda_seq, n_folds = 10) {
  # Create new directory for saving Lasso results
  new_dir <- file.path(directory, paste0(dataset_name, "_lasso"))
  dir.create(new_dir, showWarnings = FALSE)
  
  for (iter in 1:iterations) {
    # Load training data
    train_file <- file.path(directory, dataset_name, paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
    train_data <- read.csv(train_file)
    
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
    
    # Convert to named vector
    named_coefficients <- setNames(as.vector(coefficients), rownames(coefficients)[-1])
    
    # Save coefficients to an RDS file
    saveRDS(named_coefficients, file.path(new_dir, paste0("coefficients_", dataset_name, "_iteration_", iter, ".rds")))
  }
}

# Directory and prefix settings
directory <- "~/NORMAL"

# Perform Lasso regression for each dataset
perform_lasso_regression(directory, "height", iterations = 30, lambda_seq = 10^seq(10, -2, length = 100))
perform_lasso_regression(directory, "bmi", iterations = 30, lambda_seq = 10^seq(10, -2, length = 100))
perform_lasso_regression(directory, "brc", iterations = 30, lambda_seq = 10^seq(10, -2, length = 100))

# Example usage for HEIGHT dataset
lasso_coefficients_height_iteration_1 <- readRDS("~/NORMAL/height_lasso/coefficients_height_iteration_1.rds")
print(lasso_coefficients_height_iteration_1)




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






##############################################################
##### 2.1.C Supervised BEST PERFORMING  #######
##############################################################
# Fit each single PGS score column with the outcome and 
# record the R-squares for each single fitting. 
# Record the PGS score column number with the highest R-squared.

# Load necessary libraries
library(dplyr)

# Function to find the best performing PGS column based on R-squared
perform_best_r_squared <- function(directory, dataset_name, iterations) {
  # Create new directory for saving best PGS results
  new_dir <- file.path(directory, paste0(dataset_name, "_best_pgs"))
  dir.create(new_dir, showWarnings = FALSE)
  
  results <- data.frame(Iteration = integer(), Best_PGS_Column = character(), stringsAsFactors = FALSE)
  
  for (iter in 1:iterations) {
    # Load training data
    train_file <- file.path(directory, dataset_name, paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
    train_data <- read.csv(train_file)
    
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
    
    # Store the best PGS column for this iteration in the results data frame
    results <- rbind(results, data.frame(Iteration = iter, Best_PGS_Column = best_column, stringsAsFactors = FALSE))
  }
  
  # Save the results to a CSV file
  write.csv(results, file.path(new_dir, paste0("best_pgs_columns_", dataset_name, ".csv")), row.names = FALSE)
}

# Directory and prefix settings
directory <- "~/NORMAL"

# Perform best PGS column selection for each dataset
perform_best_r_squared(directory, "height", iterations = 30)
perform_best_r_squared(directory, "bmi", iterations = 30)
perform_best_r_squared(directory, "brc", iterations = 30)

# Example usage for HEIGHT dataset
best_pgs_columns_height <- read.csv("~/NORMAL/height_best_pgs/best_pgs_columns_height.csv")
print(best_pgs_columns_height)


########################
# SUPPLEMENT CODE
########################

