# Reproducible R Script for Supervised & Unsupervised Separation and Analysis
################################################################################
# Load necessary libraries
library(dplyr)
library(stringr)
library(data.table)
library(glmnet)
library(Matrix)

# Define paths and parameters (change these for new input files)
input_directory <- "/n/holyscratch01/duan_lab/jiaxin/new_emergeIII/newEMERGE_WithRace"
output_directory <- "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65"
iterations <- 30
train_size <- 3000
n_groups <- 20
group_size <- 3000

# Filenames for input data
height_file <- "updated_eMERGEIII_pheno_covar_Height.txt"
bmi_file <- "updated_eMERGEIII_pheno_covar_BMI.txt"
brc_file <- "updated_eMERGEIII_pheno_covar_BrC.txt"

# Expected number of columns for each dataset (change if input structure changes)
expected_train_cols_height <- 77
expected_train_cols_bmi <- 57
expected_train_cols_brc <- 113

# Load datasets
height_data <- read.csv(file.path(input_directory, height_file))
bmi_data <- read.csv(file.path(input_directory, bmi_file))
brc_data <- read.csv(file.path(input_directory, brc_file))






# 
# ######################
# # #### NORMAL OG ####
# #####################
# # Function to perform the sampling and save files in newly created folders
# perform_sampling <- function(data, prefix, n_train, n_groups, n_group_size, iterations, directory) {
#   # Create the main directory if it doesn't exist
#   if (!dir.exists(directory)) {
#     dir.create(directory, recursive = TRUE)
#   }
# 
#   # Create subdirectory for the specific dataset (e.g., height, bmi, brc)
#   sub_dir <- file.path(directory, prefix)
#   if (!dir.exists(sub_dir)) {
#     dir.create(sub_dir)
#   }
# 
#   for (iter in 1:iterations) {
#     train_indices <- sample(1:nrow(data), n_train)
#     train_data <- data[train_indices, ]
#     write.csv(train_data, file.path(sub_dir, paste0(prefix, "_train_data_iteration_", iter, ".csv")), row.names = FALSE)
# 
#     test_data <- data[-train_indices, ]
#     write.csv(test_data, file.path(sub_dir, paste0(prefix, "_test_data_iteration_", iter, ".csv")), row.names = FALSE)
# 
#     for (group in 1:n_groups) {
#       group_indices <- sample(1:nrow(test_data), n_group_size) # The sample() function in R by default samples without replacement
#       group_data <- test_data[group_indices, ] # Ensure unique rows within each group
#       write.csv(group_data, file.path(sub_dir, paste0(prefix, "_subgroup_", group, "_iteration_", iter, ".csv")), row.names = FALSE)
#     }
#   }
# }
# 
# 
# # Perform sampling for each dataset
# perform_sampling(height_data, "height", train_size, n_groups, group_size, iterations, output_directory)
# perform_sampling(bmi_data, "bmi", train_size, n_groups, group_size, iterations, output_directory)
# perform_sampling(brc_data, "brc", train_size, n_groups, group_size, iterations, output_directory)





###################################
# #### SUPER65 SAMPLING ####
##################################
# Function to perform the sampling with priority on individuals over 65 years of age
perform_sampling_AGE65 <- function(data, prefix, n_train, n_groups, n_group_size, iterations, directory) {
  # Create the main directory if it doesn't exist
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  # Create subdirectory for the specific dataset (e.g., height, bmi, brc)
  sub_dir <- file.path(directory, prefix)
  if (!dir.exists(sub_dir)) {
    dir.create(sub_dir)
  }
  
  for (iter in 1:iterations) {
    # Filter individuals who are older than 65
    eligible_older_individuals <- data %>%
      filter(age > 65)
    
    # Sample 3000 individuals randomly from the eligible older individuals if available
    if(nrow(eligible_older_individuals) >= n_train) {
      train_indices <- sample(1:nrow(eligible_older_individuals), n_train)
      train_data <- eligible_older_individuals[train_indices, ]
      cat("Sampled 3000 individuals over 65 years of age for supervised training in", prefix, "\n")
    } else {
      train_data <- eligible_older_individuals
      cat("Not enough individuals over 65 available in", prefix, ". Only", nrow(train_data), "available.\n")
    }
    
    # Save training data to CSV
    write.csv(train_data, file.path(sub_dir, paste0(prefix, "_train_data_iteration_", iter, ".csv")), row.names = FALSE)
    
    # Get the remaining data as testing data
    test_data <- data[!rownames(data) %in% rownames(train_data), ]
    write.csv(test_data, file.path(sub_dir, paste0(prefix, "_test_data_iteration_", iter, ".csv")), row.names = FALSE)
    
    for (group in 1:n_groups) {
      group_indices <- sample(1:nrow(test_data), n_group_size) # The sample() function in R by default samples without replacement
      group_data <- test_data[group_indices, ] # Ensure unique rows within each group
      
      # If any duplicates exist within the group, remove them
      group_data <- group_data[!duplicated(group_data), ]
      
      while (nrow(group_data) < n_group_size) {
        additional_indices <- sample(1:nrow(test_data), n_group_size - nrow(group_data))
        group_data <- rbind(group_data, test_data[additional_indices, ])
        group_data <- group_data[!duplicated(group_data), ]
      }
      
      write.csv(group_data, file.path(sub_dir, paste0(prefix, "_subgroup_", group, "_iteration_", iter, ".csv")), row.names = FALSE)
    }
  }
}

# Perform sampling for each dataset with priority for individuals over 65 years of age
perform_sampling_AGE65(height_data, "height", train_size, n_groups, group_size, iterations, output_directory)
perform_sampling_AGE65(bmi_data, "bmi", train_size, n_groups, group_size, iterations, output_directory)
perform_sampling_AGE65(brc_data, "brc", train_size, n_groups, group_size, iterations, output_directory)



# 
# ###################################
# # #### SUPER RACE==AFR SAMPLING ####
# ##################################
# # Function to perform the sampling with priority on individuals whose race is "afr"
# perform_sampling <- function(data, prefix, n_train, n_groups, n_group_size, iterations, directory) {
#   # Create the main directory if it doesn't exist
#   if (!dir.exists(directory)) {
#     dir.create(directory, recursive = TRUE)
#   }
#   
#   # Create subdirectory for the specific dataset (e.g., height, bmi, brc)
#   sub_dir <- file.path(directory, prefix)
#   if (!dir.exists(sub_dir)) {
#     dir.create(sub_dir)
#   }
#   
#   for (iter in 1:iterations) {
#     # Filter individuals whose race is "afr"
#     eligible_afr_individuals <- data %>%
#       filter(race == "afr")
#     
#     # Sample 3000 individuals randomly from the eligible "afr" individuals if available
#     if(nrow(eligible_afr_individuals) >= n_train) {
#       train_indices <- sample(1:nrow(eligible_afr_individuals), n_train)
#       train_data <- eligible_afr_individuals[train_indices, ]
#       cat("Sampled 3000 individuals with race 'afr' for supervised training in", prefix, "\n")
#     } else {
#       train_data <- eligible_afr_individuals
#       cat("Not enough individuals with race 'afr' available in", prefix, ". Only", nrow(train_data), "available.\n")
#     }
#     
#     # Save training data to CSV
#     write.csv(train_data, file.path(sub_dir, paste0(prefix, "_train_data_iteration_", iter, ".csv")), row.names = FALSE)
#     
#     # Get the remaining data as testing data
#     test_data <- data[!rownames(data) %in% rownames(train_data), ]
#     write.csv(test_data, file.path(sub_dir, paste0(prefix, "_test_data_iteration_", iter, ".csv")), row.names = FALSE)
#     
#     for (group in 1:n_groups) {
#       group_indices <- sample(1:nrow(test_data), n_group_size) # The sample() function in R by default samples without replacement
#       group_data <- test_data[group_indices, ] # Ensure unique rows within each group
#       
#       # If any duplicates exist within the group, remove them
#       group_data <- group_data[!duplicated(group_data), ]
#       
#       while (nrow(group_data) < n_group_size) {
#         additional_indices <- sample(1:nrow(test_data), n_group_size - nrow(group_data))
#         group_data <- rbind(group_data, test_data[additional_indices, ])
#         group_data <- group_data[!duplicated(group_data), ]
#       }
#       
#       write.csv(group_data, file.path(sub_dir, paste0(prefix, "_subgroup_", group, "_iteration_", iter, ".csv")), row.names = FALSE)
#     }
#   }
# }
# 
# # Perform sampling for each dataset with priority for individuals whose race is "afr"
# perform_sampling(height_data, "height", train_size, n_groups, group_size, iterations, output_directory)
# perform_sampling(bmi_data, "bmi", train_size, n_groups, group_size, iterations, output_directory)
# perform_sampling(brc_data, "brc", train_size, n_groups, group_size, iterations, output_directory)
# 










# Function to perform OLS regression and save coefficients
perform_ols_regression <- function(directory, dataset_name, iterations) {
  new_dir <- file.path(directory, paste0(dataset_name, "_ols"))
  dir.create(new_dir, showWarnings = FALSE)
  for (iter in 1:iterations) {
    train_file <- file.path(directory, dataset_name, paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
    train_data <- read.csv(train_file)
    pgs_columns <- grep("PGS", names(train_data), value = TRUE)
    formula <- paste("pheno ~", paste(pgs_columns, collapse = " + "))
    model <- lm(formula, data = train_data)
    coefficients <- model$coefficients[-1]
    saveRDS(coefficients, file.path(new_dir, paste0("coefficients_", dataset_name, "_iteration_", iter, ".rds")))
  }
}

# Perform OLS regression for each dataset
perform_ols_regression(output_directory, "height", iterations)
perform_ols_regression(output_directory, "bmi", iterations)
perform_ols_regression(output_directory, "brc", iterations)

# Function to perform Lasso regression and save coefficients
perform_lasso_regression <- function(directory, dataset_name, iterations, lambda_seq, n_folds = 10) {
  new_dir <- file.path(directory, paste0(dataset_name, "_lasso"))
  dir.create(new_dir, showWarnings = FALSE)
  for (iter in 1:iterations) {
    train_file <- file.path(directory, dataset_name, paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
    train_data <- read.csv(train_file)
    pgs_columns <- grep("^PGS", names(train_data), value = TRUE)
    response <- train_data$pheno
    predictors <- as.matrix(train_data[, pgs_columns])
    cvfit <- cv.glmnet(predictors, response, alpha = 1, lambda = lambda_seq, nfolds = n_folds)
    best_lambda <- cvfit$lambda.min
    lasso_model <- glmnet(predictors, response, alpha = 1, lambda = best_lambda)
    coefficients <- coef(lasso_model)[-1]
    named_coefficients <- setNames(as.vector(coefficients), rownames(coefficients)[-1])
    saveRDS(named_coefficients, file.path(new_dir, paste0("coefficients_", dataset_name, "_iteration_", iter, ".rds")))
  }
}

# Perform Lasso regression for each dataset
lambda_seq <- 10^seq(10, -2, length = 100)
perform_lasso_regression(output_directory, "height", iterations, lambda_seq)
perform_lasso_regression(output_directory, "bmi", iterations, lambda_seq)
perform_lasso_regression(output_directory, "brc", iterations, lambda_seq)

# Function to find the best performing PGS column based on R-squared
perform_best_r_squared <- function(directory, dataset_name, iterations) {
  new_dir <- file.path(directory, paste0(dataset_name, "_best_pgs"))
  dir.create(new_dir, showWarnings = FALSE)
  results <- data.frame(Iteration = integer(), Best_PGS_Column = character(), stringsAsFactors = FALSE)
  for (iter in 1:iterations) {
    train_file <- file.path(directory, dataset_name, paste0(dataset_name, "_train_data_iteration_", iter, ".csv"))
    train_data <- read.csv(train_file)
    pgs_columns <- grep("PGS", names(train_data), value = TRUE)
    best_r_squared <- -Inf
    best_column <- NULL
    for (pgs_col in pgs_columns) {
      formula <- paste("pheno ~", pgs_col)
      model <- lm(formula, data = train_data)
      r_squared <- summary(model)$r.squared
      if (r_squared > best_r_squared) {
        best_r_squared <- r_squared
        best_column <- pgs_col
      }
    }
    results <- rbind(results, data.frame(Iteration = iter, Best_PGS_Column = best_column, stringsAsFactors = FALSE))
  }
  write.csv(results, file.path(new_dir, paste0("best_pgs_columns_", dataset_name, ".csv")), row.names = FALSE)
}

# Perform best PGS column selection for each dataset
perform_best_r_squared(output_directory, "height", iterations)
perform_best_r_squared(output_directory, "bmi", iterations)
perform_best_r_squared(output_directory, "brc", iterations)




###############################################
#### ~/update_2.2_Individual_Calculation.R ####
###############################################

# Function to transpose the dataset and save it
transpose_and_save <- function(file, output_dir) {
  data <- read.csv(file)
  filtered_columns <- c('id', grep("^PGS00", names(data), value = TRUE))
  data_filtered <- data[, filtered_columns]
  transposed_data <- as.data.frame(t(data_filtered[,-1]))
  colnames(transposed_data) <- paste0("Subject_ID:", data_filtered$id)
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("transposed_", base_name))
  write.csv(transposed_data, output_file, row.names = FALSE, quote = FALSE)
  return(output_file)
}

# Function to rank each row of the dataframe
rank_per_row <- function(df) {
  t(apply(df, 1, function(row) rank(-row, ties.method = "average")))
}

# Function to rank transposed data and save it
rank_and_save <- function(file, output_dir) {
  transposed_data <- read.csv(file, row.names = NULL, header = TRUE)
  ranked_data <- rank_per_row(transposed_data)
  ranked_data_df <- as.data.frame(ranked_data)
  colnames(ranked_data_df) <- gsub("\\.", ":", colnames(transposed_data))
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("ranked_", base_name))
  write.csv(ranked_data_df, output_file, row.names = FALSE)
}

# Transpose and rank files for BMI, Height, and BRC
transpose_and_rank <- function(input_dir, transposed_dir, ranked_dir, file_prefix) {
  dir.create(transposed_dir, showWarnings = FALSE)
  dir.create(ranked_dir, showWarnings = FALSE)
  
  # Adjust pattern to match files with the correct prefix (e.g., bmi_subgroup_)
  pattern <- paste0("^", file_prefix, "_subgroup_\\d+_iteration_\\d+\\.csv$")
  files <- list.files(path = input_dir, pattern = pattern, full.names = TRUE)
  
  total_files <- length(files)
  
  for (i in seq_along(files)) {
    file <- files[i]
    
    # Generate the expected output filenames
    base_name <- basename(file)
    transposed_file <- file.path(transposed_dir, paste0("transposed_", base_name))
    ranked_file <- file.path(ranked_dir, paste0("ranked_", base_name))
    
    # Check if both output files already exist
    if (file.exists(transposed_file) && file.exists(ranked_file)) {
      cat(sprintf("Skipping file %s, output already exists.\n", base_name))
      next
    }
    
    # Process the file if outputs do not exist
    transposed_file <- transpose_and_save(file, transposed_dir)
    rank_and_save(transposed_file, ranked_dir)
    
    # Print progress
    percent_complete <- (i / total_files) * 100
    cat(sprintf("Completed: %d/%d files (%.2f%%)\n", i, total_files, percent_complete))
  }
}

# Directory settings for transposing and ranking
transpose_and_rank(file.path(output_directory, "bmi"), 
                   file.path(output_directory, "2.2_bmi_transposed_files"), 
                   file.path(output_directory, "2.2_bmi_ranked_transposed_files"), 
                   "bmi")

transpose_and_rank(file.path(output_directory, "height"), 
                   file.path(output_directory, "2.2_height_transposed_files"), 
                   file.path(output_directory, "2.2_height_ranked_transposed_files"), 
                   "height")

transpose_and_rank(file.path(output_directory, "brc"), 
                   file.path(output_directory, "2.2_brc_transposed_files"), 
                   file.path(output_directory, "2.2_brc_ranked_transposed_files"), 
                   "brc")

# 
# # Function to remove all files in a directory
# remove_files_in_directory <- function(dir) {
#   files <- list.files(path = dir, full.names = TRUE)
#   file.remove(files)
#   unlink(dir, recursive = TRUE)
# }
# 
# # Remove transposed files for BMI, Height, and BRC
# remove_files_in_directory(file.path(output_directory, "2.2_bmi_transposed_files"))
# remove_files_in_directory(file.path(output_directory, "2.2_height_transposed_files"))
# remove_files_in_directory(file.path(output_directory, "2.2_brc_transposed_files"))

#######################################################
#### ~/update_2.2.5_check_Individual_Calculation.R ####
#######################################################

# Function to check the size of files in a directory
check_ranked_files <- function(directory, expected_rows, expected_cols) {
  files <- list.files(path = directory, pattern = "^ranked_.*\\.csv$", full.names = TRUE)
  results <- data.frame(File = character(), Rows = integer(), Cols = integer(), Status = character(), stringsAsFactors = FALSE)
  for (file in files) {
    data <- read.csv(file)
    rows <- nrow(data)
    cols <- ncol(data)
    status <- ifelse(rows == expected_rows & cols == expected_cols, "Correct", "Incorrect")
    results <- rbind(results, data.frame(File = basename(file), Rows = rows, Cols = cols, Status = status))
  }
  return(results)
}

# Expected dimensions for each dataset
expected_rows_height <- 72  # Change this based on your actual data
expected_rows_bmi <- 52
expected_rows_brc <- 108
expected_cols <- 3000  # Change this if needed

# Check ranked files for each dataset
height_check <- check_ranked_files(file.path(output_directory, "2.2_height_ranked_transposed_files"), expected_rows_height, expected_cols)
bmi_check <- check_ranked_files(file.path(output_directory, "2.2_bmi_ranked_transposed_files"), expected_rows_bmi, expected_cols)
brc_check <- check_ranked_files(file.path(output_directory, "2.2_brc_ranked_transposed_files"), expected_rows_brc, expected_cols)

# Save the check results to CSV files
write.csv(height_check, file = file.path(output_directory, "height_ranked_files_check.csv"), row.names = FALSE)
write.csv(bmi_check, file = file.path(output_directory, "bmi_ranked_files_check.csv"), row.names = FALSE)
write.csv(brc_check, file = file.path(output_directory, "brc_ranked_files_check.csv"), row.names = FALSE)






################################################################################################
###############################################################################################
################################################################################################
###############################################################################################
#### SVD CONVERSION: NORMAL TRANSPOSED SCORE -> SVD TRANSPOSED SCORE --> SVD TRANSPOSED RANK ####
################################################################################################
###############################################################################################
################################################################################################
###############################################################################################

#### INPUT FILES: output_directory/2.2_bmi_transposed_files//.. ####

##########################################################
####  Step 2: ((Final Script)) to Process All Files: ####
##########################################################
library(readr)
library(irlba)

# Function to perform truncated SVD and save results for a single file
perform_svd <- function(file_path, output_folder, low_rank_threshold) {
  output_file_path <- file.path(output_folder, sprintf("adjusted_score_%s", basename(file_path)))
  
  # Check if the adjusted score file already exists
  if (file.exists(output_file_path)) {
    cat(sprintf("Skipping SVD for file %s because adjusted score file already exists.\n", basename(file_path)))
    return()
  }
  
  if (file.exists(file_path)) {
    # Read the CSV file
    transposed_data <- read_csv(file_path, col_types = cols())
    
    # Extract column names (subject IDs)
    subject_ids <- colnames(transposed_data)
    
    # Perform truncated SVD
    svd_result <- irlba(as.matrix(transposed_data), nv = low_rank_threshold)
    
    # Reconstruct the adjusted score matrix
    adjusted_score_matrix <- svd_result$u %*% diag(svd_result$d) %*% t(svd_result$v)
    
    # Retain the column names in the adjusted score matrix
    adjusted_score_matrix <- as.data.frame(adjusted_score_matrix)
    colnames(adjusted_score_matrix) <- subject_ids
    
    # Save the adjusted score matrix to CSV
    write_csv(adjusted_score_matrix, output_file_path)
    
    cat(sprintf("Processed and saved results for file %s\n", basename(file_path)))
    
    # Evaluate the SVD Performance
    evaluate_svd(transposed_data, adjusted_score_matrix, svd_result$d)
  } else {
    cat(sprintf("File not found: %s\n", file_path))
  }
}

# Function to evaluate SVD performance
evaluate_svd <- function(original_matrix, reconstructed_matrix, singular_values) {
  # Reconstruction Quality
  reconstruction_error <- sum((as.matrix(original_matrix) - as.matrix(reconstructed_matrix))^2)
  cat("Reconstruction Error (Sum of Squared Differences):\n", reconstruction_error, "\n")
  
  # Singular Values
  cat("Singular Values:\n")
  print(singular_values)
  
  # Variance Explained
  total_variance <- sum(singular_values^2)
  explained_variance <- cumsum(singular_values^2) / total_variance
  cat("Proportion of Variance Explained by each component:\n")
  print(explained_variance)
}

# Output folder for adjusted scores
adjusted_output_folder <- "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/SVD_adjusted_score_results"
if (!dir.exists(adjusted_output_folder)) {
  dir.create(adjusted_output_folder, recursive = TRUE)
}

# Low-rank threshold
low_rank_threshold <- 2

# Define the folders and file patterns for SVD
folders <- list(
  "height" = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_height_transposed_files",
  "bmi" = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_bmi_transposed_files",
  "brc" = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_brc_transposed_files"
)

# Perform SVD for each trait
for (trait in names(folders)) {
  base_folder <- folders[[trait]]
  transposed_files <- list.files(path = base_folder, pattern = "^transposed_.*\\.csv$", full.names = TRUE)
  
  for (file in transposed_files) {
    perform_svd(file, adjusted_output_folder, low_rank_threshold)
  }
}

#################################################################################################################
####  Step 3: "ADJUSTED" TRANSPOSED SCORE MATRIX: N*x M* --> "ADJUSTED" TRANSPOSED ((RANK)) MATRIX: N*x M*  #####
##################################################################################################################

# Load necessary library
library(dplyr)

# Define the directory containing the adjusted score matrices
adjusted_score_dir <- "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/SVD_adjusted_score_results"
ranked_score_dir <- "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/SVD_adjusted_ranked_score_results"

# Create the new directory if it does not exist
dir.create(ranked_score_dir, showWarnings = FALSE, recursive = TRUE)

# List all the adjusted score files in the directory
adjusted_score_files <- list.files(path = adjusted_score_dir, pattern = "^adjusted_score_.*\\.csv$", full.names = TRUE)

# Function to rank each row of the dataframe
rank_per_row <- function(df) {
  t(apply(df, 1, function(row) rank(-row, ties.method = "average"))) # Use -row to rank in descending order
}

# Function to rank adjusted score data and save it
rank_and_save <- function(file, output_dir) {
  # Load the adjusted score data
  adjusted_data <- read.csv(file, row.names = NULL, header = TRUE)
  
  # Apply ranking function to each row
  ranked_data <- as.data.frame(rank_per_row(adjusted_data))
  
  # Construct the output file name and path
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("ranked_", base_name))
  
  # Save the ranked data to a new CSV file without row names
  write.csv(ranked_data, output_file, row.names = FALSE, col.names = TRUE)
  
  cat(sprintf("Ranked and saved results for file %s\n", base_name))
}

# Process each adjusted score file: rank and save
for (file in adjusted_score_files) {
  rank_and_save(file, ranked_score_dir)
}

print("Adjusted score files have been ranked and saved to the new directory.")








############################################################################################################################################
# create three separate folders (for BMI, BRC, and Height) and 
# then copy the respective files from the existing SVD_adjusted_ranked_score_results directory into these new folders.
############################################################################################################################################

# Create directories for SVD-ranked transposed files by traits
bmi_svd_ranked_dir <- "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_bmi_SVD_ranked_transposed_files"
brc_svd_ranked_dir <- "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_brc_SVD_ranked_transposed_files"
height_svd_ranked_dir <- "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_height_SVD_ranked_transposed_files"

# Create the directories if they don't already exist
dir.create(bmi_svd_ranked_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(brc_svd_ranked_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(height_svd_ranked_dir, showWarnings = FALSE, recursive = TRUE)

# Define the directory containing the ranked adjusted score results
adjusted_score_dir <- "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/SVD_adjusted_ranked_score_results"

# Get all the ranked adjusted score files
adjusted_score_files <- list.files(path = adjusted_score_dir, pattern = "^ranked_adjusted_score_transposed_.*\\.csv$", full.names = TRUE)

# Copy files to their respective directories based on trait in the filename
for (file in adjusted_score_files) {
  # Determine the trait based on the filename
  if (grepl("bmi", file)) {
    file.copy(file, file.path(bmi_svd_ranked_dir, basename(file)))
    cat(sprintf("Copied %s to %s\n", basename(file), bmi_svd_ranked_dir))
  } else if (grepl("brc", file)) {
    file.copy(file, file.path(brc_svd_ranked_dir, basename(file)))
    cat(sprintf("Copied %s to %s\n", basename(file), brc_svd_ranked_dir))
  } else if (grepl("height", file)) {
    file.copy(file, file.path(height_svd_ranked_dir, basename(file)))
    cat(sprintf("Copied %s to %s\n", basename(file), height_svd_ranked_dir))
  }
}

print("All files have been successfully copied to their respective directories.")

