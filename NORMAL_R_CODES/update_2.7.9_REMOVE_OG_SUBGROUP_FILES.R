library(fs)

# Function to remove files without '_with_ranks.csv'
remove_unranked_files <- function(trait) {
  # Define the directory containing the files
  dir_path <- sprintf("~/NORMAL/2.2_%s_test_subgroup_files", trait)
  
  # Get a list of all files in the directory
  all_files <- dir_ls(dir_path, regexp = "subgroup_\\d+_iteration_\\d+\\.csv$")
  
  # Filter out the files with '_with_ranks.csv'
  files_to_remove <- all_files[!grepl("_with_ranks\\.csv$", all_files)]
  
  # Remove the unranked files
  file_delete(files_to_remove)
  
  # Print the names of the deleted files
  cat("Removed files for trait:", trait, "\n")
  print(files_to_remove)
}

# Traits to process
traits <- c("height", "bmi", "brc")

# Loop through each trait and remove unranked files
for (trait in traits) {
  remove_unranked_files(trait)
}

cat("Cleanup complete. Only '_with_ranks.csv' files are kept.\n")
