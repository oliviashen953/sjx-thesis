####### brc ####



####################    
#################### 
# July 22, 2024
#################### 
#################### 




####################################################      
####################################################
##### 2. Individual Calculation               ######
####################################################
####################################################

##############################################################      
##############################################################
##### 2.2 Unsupervised Rank Aggregation (MC4/ NNM)  #######
##############################################################
##############################################################






##################################################################  
##################################################################
##### brc: Unsupervised RA Methods Calcluation ################
##################################################################
##################################################################


################################################################################################
#### STEP (1) SUBGROUP DATASETS GATHERED TO A SINGLE FILE: "2.2_AGE65_brc/test_subgroup_files" ####
################################################################################################

# Define the directories
old_dir <- "~/"
new_dir <- "2.2_AGE65_brc/test_subgroup_files"
# 
# # Create the new directory if it does not exist
# dir.create(new_dir, showWarnings = FALSE)
# 
# # List all files matching the pattern in the current directory
# brc_files <- list.files(path = old_dir, pattern = "^brc_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
# 
# # Move each file to the new directory
# for (file in brc_files) {
#   new_file <- file.path(new_dir, basename(file))
#   file.rename(file, new_file)
# }
# 
# print("Files have been moved to the new directory.")





################################################################################################
#### STEP (2) TESTING // SUPPLEMENT: Convert the M × N dataset to the transposed version of N × M ####
################################################################################################
# For each of 30 times, the 20 groups of unsupervised size 3000:
#   (1) Convert the M × N dataset to the transposed version of N × M, 
#       with each of the N rows being the PGS scores and each of the M columns 
#       being the subjects with the column name being their Subject ID. 
#       {Raw SCORE matrix → Raw RANK matrix}






#########  #########  #########  #########  #########  ######### 
######### testing the tranposed codes with 1 single file ######
#########  #########  #########  #########  #########  ######### 
# 
# 
# # Load necessary library
# library(dplyr)
# 
# # Define the directory where the file is located and the new directory to save the transposed file
# input_dir <- "2.2_brc_test_subgroup_files"
# output_dir <- "2.2_brc_transposed_files_test"
# 
# # Create the new directory if it does not exist
# dir.create(output_dir, showWarnings = FALSE)
# 
# # Define the single file to be tested
# test_file <- file.path(input_dir, "brc_subgroup_1_iteration_1.csv")
# 
# # Function to transpose the dataset and save it
# transpose_and_save <- function(file, output_dir) {
#   # Load the data
#   data <- read.csv(file)
#   
#   # Filter columns: keep only 'id' and columns starting with 'PGS00'
#   filtered_columns <- c('id', grep("^PGS00", names(data), value = TRUE))
#   data_filtered <- data[, filtered_columns]
#   
#   # Transpose the data: PGS columns become rows, 'id' values become column names
#   transposed_data <- as.data.frame(t(data_filtered[,-1]))  # Remove 'id' column for transpose
#   
#   # Prepend "Subject_id:" to each ID value in the first row
#   colnames(transposed_data) <- paste("Subject_id:", data_filtered$id)
#   
#   # Construct the output file name and path
#   base_name <- basename(file)
#   output_file <- file.path(output_dir, paste0("transposed_", base_name))
#   
#   # Save the transposed data to a CSV file without row names
#   write.csv(transposed_data, output_file, row.names = FALSE, quote = FALSE)
# }
# 
# # Test the function with the single file
# transpose_and_save(test_file, output_dir)
# 
# print("The single file has been transposed and saved to the new directory.")
# 
# #### check correctiveness ####
# library(readr)
# brc_subgroup_1_iteration_1 <- read_csv("2.2_brc_test_subgroup_files/brc_subgroup_1_iteration_1.csv")
# View(brc_subgroup_1_iteration_1)
# 
# 
# library(readr)
# transposed_brc_subgroup_1_iteration_1 <- read_csv("2.2_brc_transposed_files_test/transposed_brc_subgroup_1_iteration_1.csv")
# View(transposed_brc_subgroup_1_iteration_1)
# 




################################################################################################
#### STEP (2)  Convert the M × N dataset to the transposed version of N × M ####
################################################################################################
# For each of 30 times, the 20 groups of unsupervised size 3000:
#   (1) Convert the M × N dataset to the transposed version of N × M, 
#       with each of the N rows being the PGS scores and each of the M columns 
#       being the subjects with the column name being their Subject ID. 
#       {Raw SCORE matrix → Raw RANK matrix}





############################################################################
####  raw score matrix --> transposed score --> transposed rank matrix ####
########################################################################### 

# Load necessary library
library(dplyr)


# Define the directories
input_dir <- "2.2_AGE65_brc/test_subgroup_files"
transposed_dir <- "2.2_AGE65_brc/2.2_AGE65_brc_transposed_files"
ranked_dir <- "2.2_AGE65_brc/2.2_AGE65_brc_ranked_transposed_files"

# Create the new directories if they do not exist
dir.create(transposed_dir, showWarnings = FALSE)
dir.create(ranked_dir, showWarnings = FALSE)

# List all the files matching the pattern in the input directory
brc_files <- list.files(path = input_dir, pattern = "^AGE65_brc_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
brc_files

# Function to transpose the dataset and save it
transpose_and_save <- function(file, output_dir) {
  # Load the data
  data <- read.csv(file)
  
  # Filter columns: keep only 'id' and columns starting with 'PGS00'
  filtered_columns <- c('id', grep("^PGS00", names(data), value = TRUE))
  data_filtered <- data[, filtered_columns]
  
  # Transpose the data: PGS columns become rows, 'id' values become column names
  transposed_data <- as.data.frame(t(data_filtered[,-1]))  # Remove 'id' column for transpose
  
  # Prepend "Subject_id:" to each ID value in the first row
  colnames(transposed_data) <- paste("Subject_id:", data_filtered$id)
  
  # Construct the output file name and path
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("transposed_", base_name))
  
  # Save the transposed data to a CSV file without row names
  write.csv(transposed_data, output_file, row.names = FALSE, quote = FALSE)
  
  return(output_file)
}

# Function to rank each row of the dataframe
rank_per_row <- function(df) {
  t(apply(df, 1, function(row) rank(-row, ties.method = "average"))) # Use -row to rank in descending order
}

# Function to rank transposed data and save it
rank_and_save <- function(file, output_dir) {
  # Load the transposed data
  transposed_data <- read.csv(file, row.names = NULL, header = TRUE)
  
  # Apply ranking function to each row
  ranked_data <- rank_per_row(transposed_data)
  
  # Construct the output file name and path
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("ranked_", base_name))
  
  # Save the ranked data to a new CSV file without row names
  write.csv(ranked_data, output_file, row.names = FALSE, col.names = TRUE)
}


# Process each file: transpose, rank, and save
for (file in brc_files) {
  transposed_file <- transpose_and_save(file, transposed_dir)
  rank_and_save(transposed_file, ranked_dir)
}

print("Files have been transposed, ranked, and saved to the new directories.")





####################################
#### check number in each files  ####
########################################



# Define the directories
transposed_dir <- "2.2_AGE65_brc/2.2_AGE65_brc_transposed_files"
ranked_dir <- "2.2_AGE65_brc/2.2_AGE65_brc_ranked_transposed_files"

# Function to count the number of files in a directory
count_files <- function(dir) {
  files <- list.files(path = dir, full.names = TRUE)
  return(length(files))
}

# Count the number of files in each directory
num_transposed_files <- count_files(transposed_dir)
num_ranked_files <- count_files(ranked_dir)

# Print the results
cat("Number of files in transposed directory (", transposed_dir, "): ", num_transposed_files, "\n")
cat("Number of files in ranked directory (", ranked_dir, "): ", num_ranked_files, "\n")






############################################################################
####  raw score matrix --> transposed score matrix  ####
########################################################################### 


# 
# # Load necessary library
# library(dplyr)
# 
# # Define the directory where the files are located and the new directory to save transposed files
# input_dir <- "2.2_brc_test_subgroup_files"
# output_dir <- "2.2_brc_transposed_files"
# 
# # Create the new directory if it does not exist
# dir.create(output_dir, showWarnings = FALSE)
# 
# # List all the files matching the pattern in the input directory
# brc_files <- list.files(path = input_dir, pattern = "^brc_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
# 
# # Function to transpose the dataset and save it
# transpose_and_save <- function(file, output_dir) {
#   # Load the data
#   data <- read.csv(file)
#   
#   # Filter columns: keep only 'id' and columns starting with 'PGS00'
#   filtered_columns <- c('id', grep("^PGS00", names(data), value = TRUE))
#   data_filtered <- data[, filtered_columns]
#   
#   # Transpose the data: PGS columns become rows, 'id' values become column names
#   transposed_data <- as.data.frame(t(data_filtered[,-1]))  # Remove 'id' column for transpose
#   
#   # Prepend "Subject_id:" to each ID value in the first row
#   colnames(transposed_data) <- paste("Subject_id:", data_filtered$id)
#   
#   # Construct the output file name and path
#   base_name <- basename(file)
#   output_file <- file.path(output_dir, paste0("transposed_", base_name))
#   
#   # Save the transposed data to a CSV file without row names
#   write.csv(transposed_data, output_file, row.names = FALSE, quote = FALSE)
# }
# 
# # Loop over each file, transpose it, and save it
# for (file in brc_files) {
#   transpose_and_save(file, output_dir)
# }
# 
# print("Files have been transposed and saved to the new directory.")
# 




