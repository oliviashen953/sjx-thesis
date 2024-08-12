#### OLD CODE: ~/2.4_Tie_Rank_Conversion.R ####

library(dplyr)
library(tidyr)

# Function to apply tie ranking to a vector
apply_tie_ranking <- function(ranks) {
  total_items <- length(ranks)
  
  ranked <- rank(ranks, ties.method = "average")
  
  ranked <- case_when(
    ranked <= 150 ~ ranked,
    ranked <= round(total_items * 0.10) ~ 151,
    ranked <= round(total_items * 0.15) ~ 152,
    ranked <= round(total_items * 0.20) ~ 153,
    ranked <= round(total_items * 0.25) ~ 154,
    ranked <= round(total_items * 0.30) ~ 155,
    ranked <= round(total_items * 0.35) ~ 156,
    ranked <= round(total_items * 0.40) ~ 157,
    ranked <= round(total_items * 0.45) ~ 158,
    ranked <= round(total_items * 0.50) ~ 159,
    ranked <= round(total_items * 0.55) ~ 160,
    ranked <= round(total_items * 0.60) ~ 161,
    ranked <= round(total_items * 0.65) ~ 162,
    ranked <= round(total_items * 0.70) ~ 163,
    ranked <= round(total_items * 0.75) ~ 164,
    ranked <= round(total_items * 0.80) ~ 165,
    ranked <= round(total_items * 0.85) ~ 166,
    ranked <= round(total_items * 0.90) ~ 167,
    ranked <= round(total_items * 0.95) ~ 168,
    TRUE ~ 169
  )
  
  return(ranked)
}


# Function to apply tie ranking to each row of a dataframe
apply_tie_ranking_to_df <- function(df) {
  df_tied <- df
  for (i in 1:nrow(df)) {
    df_tied[i, ] <- apply_tie_ranking(as.numeric(df[i, ]))
  }
  return(df_tied)
}

# Function to process all files in a folder and save the results in a new folder
process_files_in_folder <- function(input_folder, output_folder, pattern) {
  files <- list.files(input_folder, pattern = pattern, full.names = TRUE)
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  for (file in files) {
    df <- read.csv(file, header = TRUE)
    df_tied <- apply_tie_ranking_to_df(df)
    
    # Save the new file in the output folder
    output_file <- file.path(output_folder, sub("ranked_transposed", "tie_ranked", basename(file)))
    write.csv(df_tied, output_file, row.names = FALSE)
    cat("Processed and saved:", output_file, "\n")
  }
}

############################################################
##### NORMAL RANK TRASNPOSED -->  TIE RANK TRANSPOSED ####
#############################################################

# Define input folders and corresponding output folders
input_output_folders <- list(
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_height_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_height_TIE_ranked_transposed_files"),
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_bmi_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_bmi_TIE_ranked_transposed_files"),
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_brc_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/2.2_brc_TIE_ranked_transposed_files"),
  
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_height_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_height_TIE_ranked_transposed_files"),
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_bmi_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_bmi_TIE_ranked_transposed_files"),
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_brc_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/2.2_brc_TIE_ranked_transposed_files"),
  
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_height_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_height_TIE_ranked_transposed_files"),
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_bmi_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_bmi_TIE_ranked_transposed_files"),
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_brc_ranked_transposed_files", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/2.2_brc_TIE_ranked_transposed_files")
)

# Define file patterns
patterns <- c(
  "ranked_transposed_height_subgroup_\\d+_iteration_\\d+\\.csv",
  "ranked_transposed_bmi_subgroup_\\d+_iteration_\\d+\\.csv",
  "ranked_transposed_brc_subgroup_\\d+_iteration_\\d+\\.csv"
)

# Process files in each folder
for (folders in input_output_folders) {
  process_files_in_folder(folders$input, folders$output, patterns)
}

cat("All files processed and saved in their respective folders\n")


############################################################
##### SVD RANK TRASNPOSED --> SVD && TIE RANK TRANSPOSED ####
#############################################################

# Define input folders and corresponding output folders
# Define input folders and corresponding output folders
input_output_folders <- list(
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/SVD_adjusted_ranked_score_results", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/SVD&TIE_ranked_transposed_files"),
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/SVD_adjusted_ranked_score_results", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/SVD&TIE_ranked_transposed_files"),
  list(input = "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/SVD_adjusted_ranked_score_results", output = "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/SVD&TIE_ranked_transposed_files")
)

# Define the patterns for different traits
patterns <- c(
  "ranked_adjusted_score_transposed_height_subgroup_\\d+_iteration_\\d+\\.csv",
  "ranked_adjusted_score_transposed_bmi_subgroup_\\d+_iteration_\\d+\\.csv",
  "ranked_adjusted_score_transposed_brc_subgroup_\\d+_iteration_\\d+\\.csv"
)



# Function to process all files in a folder and save the results in a new folder
# Function to process all files in a folder and save the results in a new folder
process_files_in_folder <- function(input_folder, output_folder, pattern) {
  # List all files that match the given pattern in the input folder
  files <- list.files(input_folder, pattern = pattern, full.names = TRUE)
  
  # Create the output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Process each file
  for (file in files) {
    # Read the data from the file
    df <- read.csv(file, header = TRUE)
    
    # Apply tie ranking to the data
    df_tied <- apply_tie_ranking_to_df(df)
    
    # Create the output file path by replacing the input file prefix with "tie_ranked"
    output_file <- file.path(output_folder, sub("ranked_adjusted_score_transposed", "tie_ranked", basename(file)))
    
    # Save the processed (tie-ranked) data to the output folder
    write.csv(df_tied, output_file, row.names = FALSE)
    
    # Print a message indicating the file has been processed and saved
    cat("Processed and saved:", output_file, "\n")
  }
}





# Process files in each folder for each pattern
for (folders in input_output_folders) {
  for (pattern in patterns) {
    process_files_in_folder(folders$input, folders$output, pattern)
  }
}

cat("All files processed and saved in their respective folders\n")






####################################################################################
# the continuation of the R code that will create the necessary folders and 
# copy the processed SVD and TIE-ranked files into their respective directories 
# for BMI, BRC, and Height under each of the specified parent folders.
#####################################################################################


# Create directories for SVD&TIE-ranked transposed files by traits under each parent folder
parent_folders <- c("/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL", 
                    "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65", 
                    "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR")

trait_folders <- c("2.2_bmi_SVD&TIE_ranked_transposed_files", 
                   "2.2_brc_SVD&TIE_ranked_transposed_files", 
                   "2.2_height_SVD&TIE_ranked_transposed_files")

# Create the trait-specific directories under each parent folder
for (parent in parent_folders) {
  for (trait in trait_folders) {
    dir.create(file.path(parent, trait), showWarnings = FALSE, recursive = TRUE)
  }
}

# Define the original SVD&TIE rank results folder for copying
original_folders <- c(
  "/n/holyscratch01/duan_lab/jiaxin/NEW_NORMAL/SVD&TIE_ranked_transposed_files",
  "/n/holyscratch01/duan_lab/jiaxin/NEW_AGE65/SVD&TIE_ranked_transposed_files",
  "/n/holyscratch01/duan_lab/jiaxin/NEW_RACE_AFR/SVD&TIE_ranked_transposed_files"
)

# Define the patterns for different traits
patterns <- c(
  "tie_ranked_bmi_subgroup_\\d+_iteration_\\d+\\.csv",
  "tie_ranked_brc_subgroup_\\d+_iteration_\\d+\\.csv",
  "tie_ranked_height_subgroup_\\d+_iteration_\\d+\\.csv"
)

# Function to copy files based on the pattern to the respective trait folders
copy_files_to_trait_folders <- function(original_folder, parent_folder, trait_folder, pattern) {
  files <- list.files(original_folder, pattern = pattern, full.names = TRUE)
  for (file in files) {
    dest_folder <- file.path(parent_folder, trait_folder)
    file.copy(file, dest_folder)
    cat(sprintf("Copied %s to %s\n", basename(file), dest_folder))
  }
}

# Copy files to their respective trait-specific folders under each parent folder
for (i in seq_along(parent_folders)) {
  original_folder <- original_folders[i]
  parent_folder <- parent_folders[i]
  
  copy_files_to_trait_folders(original_folder, parent_folder, "2.2_bmi_SVD&TIE_ranked_transposed_files", patterns[1])
  copy_files_to_trait_folders(original_folder, parent_folder, "2.2_brc_SVD&TIE_ranked_transposed_files", patterns[2])
  copy_files_to_trait_folders(original_folder, parent_folder, "2.2_height_SVD&TIE_ranked_transposed_files", patterns[3])
}

print("All SVD&TIE-ranked files have been successfully copied to their respective directories.")



