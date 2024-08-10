####################    
#################### 
# July 12, 2024
#################### 
#################### 

library(readr)
library(irlba)
library(ggplot2)

# Function to perform truncated SVD and save results for a single file
perform_svd <- function(file_path, output_folder, low_rank_threshold) {
  if (file.exists(file_path)) {
    # Read the CSV file
    transposed_data <- read_csv(file_path, col_types = cols())
    
    # Print the transposed data frame
    cat("Transposed Data Frame:\n")
    print(head(transposed_data))
    
    # Extract column names (subject IDs)
    subject_ids <- colnames(transposed_data)
    
    # Perform truncated SVD
    svd_result <- irlba(as.matrix(transposed_data), nv = low_rank_threshold)
    
    # Reconstruct the adjusted score matrix
    adjusted_score_matrix <- svd_result$u %*% diag(svd_result$d) %*% t(svd_result$v)
    
    # Retain the column names in the adjusted score matrix
    adjusted_score_matrix <- as.data.frame(adjusted_score_matrix)
    colnames(adjusted_score_matrix) <- subject_ids
    
    # Print the adjusted score matrix
    cat("Adjusted Score Matrix:\n")
    print(head(adjusted_score_matrix))
    
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
  
  # Plot Singular Values
  singular_values_df <- data.frame(Index = 1:length(singular_values), SingularValues = singular_values)
  ggplot(singular_values_df, aes(x = Index, y = SingularValues)) +
    geom_line() +
    geom_point() +
    ggtitle("Singular Values") +
    xlab("Index") +
    ylab("Singular Values")
}

# Function to convert adjusted score matrix to rank matrix
convert_to_rank_matrix <- function(file_path, output_folder) {
  if (file.exists(file_path)) {
    # Read the adjusted score matrix
    adjusted_score_matrix <- read_csv(file_path, col_types = cols())
    
    # Extract column names (subject IDs)
    subject_ids <- colnames(adjusted_score_matrix)
    
    # Convert to adjusted rank matrix
    adjusted_rank_matrix <- apply(as.matrix(adjusted_score_matrix), 2, rank, ties.method = "min")
    
    # Retain the column names in the adjusted rank matrix
    adjusted_rank_matrix <- as.data.frame(adjusted_rank_matrix)
    colnames(adjusted_rank_matrix) <- subject_ids
    
    # Print the adjusted rank matrix
    cat("Adjusted Rank Matrix:\n")
    print(head(adjusted_rank_matrix))
    
    # Save the adjusted rank matrix to CSV
    output_file_path <- file.path(output_folder, sprintf("adjusted_rank_%s", basename(file_path)))
    write_csv(adjusted_rank_matrix, output_file_path)
    
    cat(sprintf("Processed and saved adjusted rank matrix for file %s\n", basename(file_path)))
  } else {
    cat(sprintf("File not found: %s\n", file_path))
  }
}

# Example file to process
transposed_file <- "2.2_brc_transposed_files/transposed_brc_subgroup_9_iteration_9.csv"

# Output folders for adjusted scores and rank matrices
adjusted_output_folder <- "adjusted_score_results_test"
rank_output_folder <- "adjusted_rank_results_test"
if (!dir.exists(adjusted_output_folder)) {
  dir.create(adjusted_output_folder)
}
if (!dir.exists(rank_output_folder)) {
  dir.create(rank_output_folder)
}

# Low-rank threshold
low_rank_threshold <- 10

# Perform SVD on the transposed file
perform_svd(transposed_file, adjusted_output_folder, low_rank_threshold)

# Convert the adjusted score matrix to rank matrix
adjusted_score_file <- file.path(adjusted_output_folder, sprintf("adjusted_score_transposed_brc_subgroup_9_iteration_9.csv"))
convert_to_rank_matrix(adjusted_score_file, rank_output_folder)

