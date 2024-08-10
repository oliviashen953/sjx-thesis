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

# Define the input file and output folder
input_file <- "adjusted_ranked_score_results/ranked_adjusted_score_transposed_brc_subgroup_1_iteration_1.csv"
output_folder <- file.path(Sys.getenv("HOME"), "2.4_TEST_brc_SVD_TIE_ranked_transposed_files")

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Process the single file
df <- read.csv(input_file, header = TRUE)
df_tied <- apply_tie_ranking_to_df(df)

# Save the new file in the output folder
output_file <- file.path(output_folder, sub("ranked_adjusted_score_transposed", "tie_ranked_adjusted_score_transposed", basename(input_file)))
write.csv(df_tied, output_file, row.names = FALSE)
cat("Processed and saved:", output_file, "\n")

# Print the first few rows of the original and processed data for verification
cat("Original Data:\n")
print(head(df, 20))
cat("Tie-ranked Data:\n")
print(head(df_tied, 20))
