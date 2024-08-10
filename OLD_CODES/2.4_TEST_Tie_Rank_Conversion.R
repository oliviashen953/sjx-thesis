# Reads the test file.
# Applies the tie ranking to each row of the dataframe.
# Prints the first 20 rows of the original and processed data for verification.
# Optionally, saves the processed data to a new file.


library(dplyr)

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

# Test the function with a single file
test_file <- "2.2_brc_ranked_transposed_files/ranked_transposed_brc_subgroup_2_iteration_2.csv"
df <- read.csv(test_file, header = TRUE)
cat("File read successfully\n")
df_tied <- apply_tie_ranking_to_df(df)
cat("Tie ranking applied successfully\n")

# Print the first few rows of the original and processed data for verification
cat("Original Data:\n")
print(head(df, 20))
cat("Tie-ranked Data:\n")
print(head(df_tied, 20))

# Optionally, save the new file to check if the output file is correct
output_file <- sub("ranked_transposed", "tie_ranked", basename(test_file))
write.csv(df_tied, file.path(dirname(test_file), output_file), row.names = FALSE)
cat("Output file saved successfully\n")
