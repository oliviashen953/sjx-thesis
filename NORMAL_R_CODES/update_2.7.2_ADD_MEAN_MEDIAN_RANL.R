library(dplyr)
library(readr)

# This function calculates the mean ranks and adds them to the dataframe
add_rank_columns <- function(df, pgs_pattern, ra_columns) {
  # Identify PGS columns (assuming they start with "PGS00" and do not end with "_rank")
  pgs_columns <- grep(pgs_pattern, names(df), value = TRUE)
  pgs_columns <- pgs_columns[!grepl("_rank$", pgs_columns)]
  
  # Calculate the average for PGS columns
  df <- df %>% mutate(avg_pgs = rowMeans(select(., all_of(pgs_columns)), na.rm = TRUE),
                      avg_pgs_rank = rank(-avg_pgs, ties.method = "average"),
                      avg_ra_rank = rowMeans(select(., all_of(ra_columns)), na.rm = TRUE))
  
  df
}

# Function to process each file
process_file <- function(trait, subgroup, iteration) {
  # File name
  file_name <- sprintf("~/NORMAL/2.2_%s_test_subgroup_files/%s_subgroup_%d_iteration_%d_with_ranks.csv", trait, trait, subgroup, iteration)
  
  # Read the file into a data frame
  if (file.exists(file_name)) {
    df <- read_csv(file_name)
    
    # Define RA columns
    ra_columns <- c("ams_rank", "bcs_rank", "logs_rank", "sbs_rank", "mct_rank", "mc4_rank")
    
    # Add rank columns
    df <- add_rank_columns(df, "^PGS00", ra_columns)
    
    # Save the updated dataframe
    write_csv(df, file_name)
    cat("Processed and saved:", file_name, "\n")
  } else {
    cat("File does not exist:", file_name, "\n")
  }
}

# Traits to process
traits <- c("height", "bmi", "brc")

# Process all files for all subgroups and iterations
for (trait in traits) {
  for (iteration in 1:30) {
    for (subgroup in 1:20) {
      process_file(trait, subgroup, iteration)
    }
  }
}

