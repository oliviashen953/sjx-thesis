########################################################
####  DONE!!!!   Step 1: Transpose the M * N+5 --> N *M "SCORE TRANSPOSED"    ####
##########################################################

# ALREADY DID! the files are in 3 folders:
# NORMAL/2.2_height_transposed_files/transposed_height_subgroup_1_iteration_1.csv
# NORMAL/2.2_bmi_transposed_files/transposed_bmi_subgroup_1_iteration_1.csv
# NORMAL/2.2_brc_transposed_files/transposed_brc_subgroup_1_iteration_1.csv






#############################################################################################################
####  Step 2: Perform SVD on the N *M  Transposed Dataset --> "ADJUSTED" TRANSPOSED SCORE MATRIX: N*x M* ####
##############################################################################################################
# ALREADY DID! the files are ALL IN ONE folders:
# result files saved in the folder: "adjusted_score_results"
# W/ file names : ("ADJUSTED" TRANSPOSED SCORE MATRIX: N*x M*)

# "adjusted_score_transposed_brc_subgroup_1_iteration_1.csv
# "adjusted_score_transposed_brc_subgroup_20_iteration_37.csv
# "adjusted_score_transposed_bmi_subgroup_1_iteration_1.csv
# "adjusted_score_transposed_bmi_subgroup_20_iteration_37.csv
# "adjusted_score_transposed_height_subgroup_1_iteration_1.csv
# "adjusted_score_transposed_height_subgroup_20_iteration_37.csv



##########################################################
####  Step 2: ((Test)) in "2.2_FINAL_TEST_svd_nnm.r" #####
##########################################################


##########################################################
####  Step 2: ((Final Script)) to Process All Files: ####
##########################################################
library(readr)
library(irlba)

# Function to perform truncated SVD and save results for a single file
perform_svd <- function(file_path, output_folder, low_rank_threshold) {
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
    output_file_path <- file.path(output_folder, sprintf("adjusted_score_%s", basename(file_path)))
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
adjusted_output_folder <- "SVD_adjusted_score_results"
if (!dir.exists(adjusted_output_folder)) {
  dir.create(adjusted_output_folder)
}

# Low-rank threshold
low_rank_threshold <- 2

# Define the folders and file patterns for SVD
folders <- list(
  "height" = "~/NORMAL/2.2_height_transposed_files",
  #"bmi" = "~/NORMAL/2.2_bmi_transposed_files",
  "brc" = "~/NORMAL/2.2_brc_transposed_files"
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

#################################################################################################################
####  Step 3: "ADJUSTED" TRANSPOSED SCORE MATRIX: N*x M* --> "ADJUSTED" TRANSPOSED ((RANK)) MATRIX: N*x M*  #####
##################################################################################################################

# Load necessary library
library(dplyr)

# Define the directory containing the adjusted score matrices
adjusted_score_dir <- "SVD_adjusted_score_results"
ranked_score_dir <- "SVD_adjusted_ranked_score_results"

# Create the new directory if it does not exist
dir.create(ranked_score_dir, showWarnings = FALSE)

# List all the adjusted score files in the directory
adjusted_score_files <- list.files(path = adjusted_score_dir, pattern = "^adjusted_score_.*\\.csv$", full.names = TRUE)
adjusted_score_files
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
#### folder: adjusted_ranked_score_results ####






