# Load necessary libraries
library(readr)
library(fs)

# Function to process each height subgroup file
process_height_subgroup <- function(subgroup, iteration) {
  # Define the output file path
  output_file <- sprintf("2.2_height_test_subgroup_files/height_subgroup_%d_iteration_%d_with_ranks.csv", subgroup, iteration)
  
  # Required rank columns
  required_columns <- c("true_rank", "mc4_rank", "mct_rank", "ams_rank", "bcs_rank", "logs_rank", "sbs_rank", "mu_rank")
  
  # Check if the output file already exists
  if (file_exists(output_file)) {
    height_group <- read_csv(output_file, show_col_types = FALSE)
    
    missing_columns <- setdiff(required_columns, names(height_group))
    
    if (length(missing_columns) == 0) {
      cat(sprintf("File already exists with all required columns: %s. Skipping.\n", output_file))
      return()
    } else {
      cat(sprintf("File exists but missing columns: %s. Adding missing columns.\n", paste(missing_columns, collapse = ", ")))
    }
  } else {
    # Load the main height subgroup file
    file_name <- sprintf("2.2_height_test_subgroup_files/height_subgroup_%d_iteration_%d.csv", subgroup, iteration)
    height_group <- read_csv(file_name, show_col_types = FALSE)
  }
  
  # Find PGS score columns and rank them
  score_columns <- grep("PGS", names(height_group), value = TRUE)
  for(col_name in score_columns) {
    rank_col_name <- paste0(col_name, "_rank")
    height_group[[rank_col_name]] <- rank(-height_group[[col_name]], ties.method = "average")
  }
  
  # Order by pheno and assign true rank
  ordered_indices <- order(height_group$pheno, decreasing = TRUE)
  height_group$true_rank <- order(ordered_indices)
  
  # Load mc4 and mct ranks if missing
  if (!"mc4_rank" %in% names(height_group)) {
    mc4_file <- sprintf("/n/home00/jiaxinshen/NORMAL_mc4_output_files/NORMAL_mc4_height_group%d_iteration_%d.csv", subgroup, iteration)
    mc4_data <- read_csv(mc4_file, show_col_types = FALSE)
    height_group$mc4_rank <- mc4_data$mc4_rank
  }
  
  if (!"mct_rank" %in% names(height_group)) {
    mct_file <- sprintf("/n/home00/jiaxinshen/NORMAL_mct_output_files/NORMAL_mct_height_group%d_iteration_%d.csv", subgroup, iteration)
    mct_data <- read_csv(mct_file, show_col_types = FALSE)
    height_group$mct_rank <- mct_data$mct_rank
  }
  
  # Load NUCLEAR NORM scores and merge them if missing
  norms <- c("ams", "bcs", "mu", "logs", "sbs")
  for(norm in norms) {
    rank_col_name <- paste0(norm, "_rank")
    if (!rank_col_name %in% names(height_group)) {
      norm_data <- read_csv(sprintf("/n/home00/jiaxinshen/skew-nuclear/matlab/new_%s_height_subgroup%d_iteration%d.csv", norm, subgroup, iteration), show_col_types = FALSE, col_names = FALSE)
      
      if (nrow(norm_data) == nrow(height_group)) {
        colnames(norm_data)[1] <- paste0(norm, "_score")
        height_group <- cbind(height_group, norm_data)
        
        # Rank the scores and adjust the ranks
        height_group[[rank_col_name]] <- 3001 - rank(-height_group[[paste0(norm, "_score")]], ties.method = "average")
      } else {
        cat(sprintf("Warning: Mismatch in row count for %s, subgroup %d, iteration %d\n", norm, subgroup, iteration))
      }
    }
  }
  
  # Save the updated dataframe
  write_csv(height_group, output_file)
  cat(sprintf("Processed and saved subgroup %d, iteration %d\n", subgroup, iteration))
}

# Loop through subgroups 1 to 20 and iterations 1 to 37
for(subgroup in 1:20) {
  for(iteration in 1:30) {
    process_height_subgroup(subgroup, iteration)
  }
}
