# Load necessary libraries
library(dplyr)

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

# Define expected dimensions for each dataset
expected_rows <- 72  # Number of PGS columns for height
expected_cols <- 3000  # Number of subjects
expected_rows_bmi <- 52  # Number of PGS columns for BMI
expected_rows_brc <- 108  # Number of PGS columns for BRC

# Define directories for ranked transposed files
ranked_dir_height <- "~/NORMAL/2.2_height_ranked_transposed_files"
ranked_dir_bmi <- "~/NORMAL/2.2_bmi_ranked_transposed_files"
ranked_dir_brc <- "~/NORMAL/2.2_brc_ranked_transposed_files"

# Check the ranked files for each dataset
height_check <- check_ranked_files(ranked_dir_height, expected_rows, expected_cols)
bmi_check <- check_ranked_files(ranked_dir_bmi, expected_rows_bmi, expected_cols)
brc_check <- check_ranked_files(ranked_dir_brc, expected_rows_brc, expected_cols)

# Print the results
print("Height Ranked Files Check:")
print(height_check)

print("BMI Ranked Files Check:")
print(bmi_check)

print("BRC Ranked Files Check:")
print(brc_check)

# Optionally, save the results to CSV files
write.csv(height_check, file = "~/NORMAL/height_ranked_files_check.csv", row.names = FALSE)
write.csv(bmi_check, file = "~/NORMAL/bmi_ranked_files_check.csv", row.names = FALSE)
write.csv(brc_check, file = "~/NORMAL/brc_ranked_files_check.csv", row.names = FALSE)
