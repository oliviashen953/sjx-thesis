# Load necessary library
library(dplyr)
library(readr)

# # Define the function to move files to a new directory
# move_files_to_directory <- function(old_dir, new_dir, pattern) {
#   dir.create(new_dir, showWarnings = FALSE)
#   files <- list.files(path = old_dir, pattern = pattern, full.names = TRUE)
#   for (file in files) {
#     new_file <- file.path(new_dir, basename(file))
#     file.rename(file, new_file)
#   }
#   cat("Files have been moved to the new directory:", new_dir, "\n")
# }

# Define the function to transpose the dataset and save it
transpose_and_save <- function(file, output_dir) {
  data <- read.csv(file)
  filtered_columns <- c('id', grep("^PGS00", names(data), value = TRUE))
  data_filtered <- data[, filtered_columns]
  transposed_data <- as.data.frame(t(data_filtered[,-1]))
  colnames(transposed_data) <- paste("Subject_id:", data_filtered$id)
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("transposed_", base_name))
  write.csv(transposed_data, output_file, row.names = FALSE, quote = FALSE)
  return(output_file)
}

# Define the function to rank each row of the dataframe
rank_per_row <- function(df) {
  t(apply(df, 1, function(row) rank(-row, ties.method = "average")))
}

# Define the function to rank transposed data and save it
rank_and_save <- function(file, output_dir) {
  transposed_data <- read.csv(file, row.names = NULL, header = TRUE)
  ranked_data <- rank_per_row(transposed_data)
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("ranked_", base_name))
  write.csv(ranked_data, output_file, row.names = FALSE, col.names = TRUE)
}

# Define the function to process each file: transpose, rank, and save
process_files <- function(input_dir, transposed_dir, ranked_dir, pattern) {
  dir.create(transposed_dir, showWarnings = FALSE)
  dir.create(ranked_dir, showWarnings = FALSE)
  files <- list.files(path = input_dir, pattern = pattern, full.names = TRUE)
  for (file in files) {
    transposed_file <- transpose_and_save(file, transposed_dir)
    rank_and_save(transposed_file, ranked_dir)
  }
  cat("Files have been transposed, ranked, and saved to the new directories:", transposed_dir, "and", ranked_dir, "\n")
}

# Define the function to count the number of files in a directory
count_files <- function(dir) {
  files <- list.files(path = dir, full.names = TRUE)
  return(length(files))
}

# Define the function to print the number of files in directories
print_file_counts <- function(transposed_dir, ranked_dir) {
  num_transposed_files <- count_files(transposed_dir)
  num_ranked_files <- count_files(ranked_dir)
  cat("Number of files in transposed directory (", transposed_dir, "): ", num_transposed_files, "\n")
  cat("Number of files in ranked directory (", ranked_dir, "): ", num_ranked_files, "\n")
}

# Process 2.2_RACE_bmi
#move_files_to_directory("~/", "2.2_RACE_bmi/test_subgroup_files", "^RACE_bmi_subgroup_\\d+_iteration_\\d+\\.csv$")
process_files("2.2_RACE_bmi/test_subgroup_files", "2.2_RACE_bmi/transposed_files", "2.2_RACE_bmi/ranked_transposed_files", "^RACE_bmi_subgroup_\\d+_iteration_\\d+\\.csv$")
print_file_counts("2.2_RACE_bmi/transposed_files", "2.2_RACE_bmi/ranked_transposed_files")

# Process 2.2_RACE_brc
#move_files_to_directory("~/", "2.2_RACE_brc/test_subgroup_files", "^RACE_brc_subgroup_\\d+_iteration_\\d+\\.csv$")
process_files("2.2_RACE_brc/test_subgroup_files", "2.2_RACE_brc/transposed_files", "2.2_RACE_brc/ranked_transposed_files", "^RACE_brc_subgroup_\\d+_iteration_\\d+\\.csv$")
print_file_counts("2.2_RACE_brc/transposed_files", "2.2_RACE_brc/ranked_transposed_files")

# Process 2.2_RACE_height
#move_files_to_directory("~/", "2.2_RACE_height/test_subgroup_files", "^RACE_height_subgroup_\\d+_iteration_\\d+\\.csv$")
process_files("2.2_RACE_height/test_subgroup_files", "2.2_RACE_height/transposed_files", "2.2_RACE_height/ranked_transposed_files", "^RACE_height_subgroup_\\d+_iteration_\\d+\\.csv$")
print_file_counts("2.2_RACE_height/transposed_files", "2.2_RACE_height/ranked_transposed_files")

