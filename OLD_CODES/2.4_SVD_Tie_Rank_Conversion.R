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
    output_file <- file.path(output_folder, sub("ranked_adjusted_score_transposed", "tie_ranked_adjusted_score_transposed", basename(file)))
    write.csv(df_tied, output_file, row.names = FALSE)
    cat("Processed and saved:", output_file, "\n")
  }
}

# Define input folders and corresponding output folders
input_folder <- "adjusted_ranked_score_results"
output_folders <- list(
  "height" = file.path(Sys.getenv("HOME"), "2.4_height_SVD_TIE_ranked_transposed_files"),
  "bmi" = file.path(Sys.getenv("HOME"), "2.4_bmi_SVD_TIE_ranked_transposed_files"),
  "brc" = file.path(Sys.getenv("HOME"), "2.4_brc_SVD_TIE_ranked_transposed_files")
)

# Define file patterns
patterns <- c("ranked_adjusted_score_transposed_height_subgroup_\\d+_iteration_\\d+\\.csv",
              "ranked_adjusted_score_transposed_bmi_subgroup_\\d+_iteration_\\d+\\.csv",
              "ranked_adjusted_score_transposed_brc_subgroup_\\d+_iteration_\\d+\\.csv")

# Process files in each folder
for (i in seq_along(patterns)) {
  output_folder <- output_folders[[i]]
  process_files_in_folder(input_folder, output_folder, patterns[i])
}

cat("All files processed and saved in their respective folders\n")

