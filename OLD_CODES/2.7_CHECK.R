library(dplyr)
library(readr)

# Function to process each file
process_file <- function(subgroup, iteration) {
  # File name
  file_name <- sprintf("2.2_height_test_subgroup_files/height_subgroup_%d_iteration_%d_with_ranks.csv", subgroup, iteration)
  
  # Read the file into a data frame
  if (file.exists(file_name)) {
    df <- read_csv(file_name)
    
    # Remove columns ending with _rank_rank
    df <- df %>% select(-ends_with("_rank_rank"))
    
    # Define the columns to keep
    keep_columns <- c("id", "pheno", "sex", "age", "race", "true_rank", "mc4_rank", "mct_rank", 
                      "ams_rank", "bcs_rank", "logs_rank", "sbs_rank", "mu_rank", 
                      "ols_rank", "lasso_rank", "avg_pgs_rank", "avg_ra_rank", "avg_pgs")
    
    # Identify PGS columns (assuming they start with "PGS00" and do not end with "_rank_rank")
    pgs_columns <- grep("^PGS00", names(df), value = TRUE)
    pgs_columns <- pgs_columns[!grepl("_rank_rank$", pgs_columns)]
    
    # Identify PGS rank columns
    pgs_rank_columns <- paste0(pgs_columns, "_rank")
    
    # Combine columns to keep
    keep_columns <- c(keep_columns, pgs_columns, pgs_rank_columns)
    
    # Filter the dataframe to keep only the specified columns
    df <- df %>% select(any_of(keep_columns))
    
    # Check for missing columns and report
    missing_columns <- setdiff(keep_columns, names(df))
    if (length(missing_columns) > 0) {
      cat("Missing columns in file", file_name, ":", paste(missing_columns, collapse = ", "), "\n")
    }
    
    # Report the number of columns in the current file
    cat("Total columns in file", file_name, ":", ncol(df), "\n")
    
    # Save the updated dataframe
    write_csv(df, file_name)
    cat("Processed and saved:", file_name, "\n")
  } else {
    cat("File does not exist:", file_name, "\n")
  }
}

# Process all files for all subgroups and iterations
for (iteration in 1:30) {
  for (subgroup in 1:20) {
    process_file(subgroup, iteration)
  }
}
