####### BMI #######

####################
####################
# July 5, 2024
####################
####################

####################################################
##### 2. Individual Calculation               ######
####################################################

##############################################################
##### 2.2 Unsupervised Rank Aggregation (MC4/ NNM)  #######
##############################################################

##################################################################
##### bmi: Unsupervised RA Methods Calculation ################
##################################################################

################################################################################################
#### STEP (1) SUBGROUP DATASETS GATHERED TO A SINGLE FILE: "2.2_bmi_test_subgroup_files" ####
################################################################################################

# Define the directories
old_dir <- "~/NORMAL/bmi"
new_dir <- "~/NORMAL/2.2_bmi_test_subgroup_files"

# Create the new directory if it does not exist
dir.create(new_dir, showWarnings = FALSE)

# List all files matching the pattern in the current directory
bmi_files <- list.files(path = old_dir, pattern = "^bmi_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
bmi_files
# Move each file to the new directory
for (file in bmi_files) {
  new_file <- file.path(new_dir, basename(file))
  file.rename(file, new_file)
}

print("Files have been moved to the new directory.")



# Define and create new directories for Height
old_dir_height <- "~/NORMAL/height"
new_dir_height <- "~/NORMAL/2.2_height_test_subgroup_files"
dir.create(new_dir_height, showWarnings = FALSE)

# List and move Height files
height_files <- list.files(path = old_dir_height, pattern = "^height_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
for (file in height_files) {
  new_file <- file.path(new_dir_height, basename(file))
  file.rename(file, new_file)
}
print("Height files have been moved to the new directory.")

# Define and create new directories for BRC
old_dir_brc <- "~/NORMAL/brc"
new_dir_brc <- "~/NORMAL/2.2_brc_test_subgroup_files"
dir.create(new_dir_brc, showWarnings = FALSE)

# List and move BRC files
brc_files <- list.files(path = old_dir_brc, pattern = "^brc_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
for (file in brc_files) {
  new_file <- file.path(new_dir_brc, basename(file))
  file.rename(file, new_file)
}
print("BRC files have been moved to the new directory.")


################################################################################################
#### STEP (2) TESTING // SUPPLEMENT: Convert the M × N dataset to the transposed version of N × M ####
################################################################################################
# For each of 30 times, the 20 groups of unsupervised size 3000:
#   (1) Convert the M × N dataset to the transposed version of N × M, 
#       with each of the N rows being the PGS scores and each of the M columns 
#       being the subjects with the column name being their Subject ID. 
#       {Raw SCORE matrix → Raw RANK matrix}

################################################################################################
####  raw score matrix --> transposed score --> transposed rank matrix ####
########################################################################### 

# Load necessary library
library(dplyr)

# Define the directories
input_dir <- "~/NORMAL/2.2_bmi_test_subgroup_files"
transposed_dir <- "~/NORMAL/2.2_bmi_transposed_files"
ranked_dir <- "~/NORMAL/2.2_bmi_ranked_transposed_files"

# Create the new directories if they do not exist
dir.create(transposed_dir, showWarnings = FALSE)
dir.create(ranked_dir, showWarnings = FALSE)

# List all the files matching the pattern in the input directory
bmi_files <- list.files(path = input_dir, pattern = "^bmi_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
bmi_files

# Function to transpose the dataset and save it
# Load necessary library
library(dplyr)

# Load necessary libraries
library(dplyr)

# Function to transpose the dataset and save it
transpose_and_save <- function(file, output_dir) {
  # Load the data
  data <- read.csv(file)
  
  # Filter columns: keep only 'id' and columns starting with 'PGS00'
  filtered_columns <- c('id', grep("^PGS00", names(data), value = TRUE))
  data_filtered <- data[, filtered_columns]
  
  # Transpose the data: PGS columns become rows, 'id' values become column names
  transposed_data <- as.data.frame(t(data_filtered[,-1]))  # Remove 'id' column for transpose
  
  # Prepend "Subject_ID:" to each ID value in the first row
  colnames(transposed_data) <- paste0("Subject_ID:", data_filtered$id)
  
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
  
  # Convert the matrix back to data frame and retain original column names
  ranked_data_df <- as.data.frame(ranked_data)
  
  # Restore the column names to the original format
  colnames(ranked_data_df) <- gsub("\\.", ":", colnames(transposed_data))  # Change back '.' to ':'
  
  # Construct the output file name and path
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("ranked_", base_name))
  
  # Save the ranked data to a new CSV file without row names
  write.csv(ranked_data_df, output_file, row.names = FALSE)
}

# Test the transpose and rank functions on a single file
test_file <- "~/NORMAL/2.2_bmi_test_subgroup_files/bmi_subgroup_1_iteration_1.csv"
transposed_file <- transpose_and_save(test_file, "~/NORMAL/2.2_bmi_transposed_files")
rank_and_save(transposed_file, "~/NORMAL/2.2_bmi_ranked_transposed_files")

# Check the transposed and ranked file
transposed_bmi_subgroup_1_iteration_1 <- read.csv("~/NORMAL/2.2_bmi_transposed_files/transposed_bmi_subgroup_1_iteration_1.csv")
ranked_transposed_bmi_subgroup_1_iteration_1 <- read.csv("~/NORMAL/2.2_bmi_ranked_transposed_files/ranked_transposed_bmi_subgroup_1_iteration_1.csv")

# View the data to ensure correctness
print(colnames(transposed_bmi_subgroup_1_iteration_1))
print(colnames(ranked_transposed_bmi_subgroup_1_iteration_1))





#######################################
#######################################
##### PROCESS ALL FILES ###############
#######################################
#######################################
#############################################
############## NOW PROCESS ALL FILES #########
#############################################

# Load necessary libraries
library(dplyr)

# Function to transpose the dataset and save it
transpose_and_save <- function(file, output_dir) {
  # Load the data
  data <- read.csv(file)
  
  # Filter columns: keep only 'id' and columns starting with 'PGS00'
  filtered_columns <- c('id', grep("^PGS00", names(data), value = TRUE))
  data_filtered <- data[, filtered_columns]
  
  # Transpose the data: PGS columns become rows, 'id' values become column names
  transposed_data <- as.data.frame(t(data_filtered[,-1]))  # Remove 'id' column for transpose
  
  # Prepend "Subject_ID:" to each ID value in the first row
  colnames(transposed_data) <- paste0("Subject_ID:", data_filtered$id)
  
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
  
  # Convert the matrix back to data frame and retain original column names
  ranked_data_df <- as.data.frame(ranked_data)
  
  # Restore the column names to the original format
  colnames(ranked_data_df) <- gsub("\\.", ":", colnames(transposed_data))  # Change back '.' to ':'
  
  # Construct the output file name and path
  base_name <- basename(file)
  output_file <- file.path(output_dir, paste0("ranked_", base_name))
  
  # Save the ranked data to a new CSV file without row names
  write.csv(ranked_data_df, output_file, row.names = FALSE)
}










# Define directories for BMI
bmi_input_dir <- "~/NORMAL/2.2_bmi_test_subgroup_files"
bmi_transposed_dir <- "~/NORMAL/2.2_bmi_transposed_files"
bmi_ranked_dir <- "~/NORMAL/2.2_bmi_ranked_transposed_files"

# Create the new directories if they do not exist
dir.create(bmi_transposed_dir, showWarnings = FALSE)
dir.create(bmi_ranked_dir, showWarnings = FALSE)

# List all the files matching the pattern in the input directory
bmi_files <- list.files(path = bmi_input_dir, pattern = "^bmi_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)

# Process each file: transpose, rank, and save
for (file in bmi_files) {
  transposed_file <- transpose_and_save(file, bmi_transposed_dir)
  rank_and_save(transposed_file, bmi_ranked_dir)
}

print("BMI files have been transposed, ranked, and saved to the new directories.")















# Define directories for Height
height_input_dir <- "~/NORMAL/2.2_height_test_subgroup_files"
height_transposed_dir <- "~/NORMAL/2.2_height_transposed_files"
height_ranked_dir <- "~/NORMAL/2.2_height_ranked_transposed_files"

# Create the new directories if they do not exist
dir.create(height_transposed_dir, showWarnings = FALSE)
dir.create(height_ranked_dir, showWarnings = FALSE)

# List all the files matching the pattern in the input directory
height_files <- list.files(path = height_input_dir, pattern = "^height_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
height_files
# Process each file: transpose, rank, and save
for (file in height_files) {
  transposed_file <- transpose_and_save(file, height_transposed_dir)
  #rank_and_save(transposed_file, height_ranked_dir)
}

print("Height files have been transposed, ranked, and saved to the new directories.")













# Define directories for BRC
brc_input_dir <- "~/NORMAL/2.2_brc_test_subgroup_files"
brc_transposed_dir <- "~/NORMAL/2.2_brc_transposed_files"
brc_ranked_dir <- "~/NORMAL/2.2_brc_ranked_transposed_files"

# Create the new directories if they do not exist
dir.create(brc_transposed_dir, showWarnings = FALSE)
dir.create(brc_ranked_dir, showWarnings = FALSE)

# List all the files matching the pattern in the input directory
brc_files <- list.files(path = brc_input_dir, pattern = "^brc_subgroup_\\d+_iteration_\\d+\\.csv$", full.names = TRUE)
brc_files
# Process each file: transpose, rank, and save
for (file in brc_files) {
  transposed_file <- transpose_and_save(file, brc_transposed_dir)
  #rank_and_save(transposed_file, brc_ranked_dir)
}

print("BRC files have been transposed, ranked, and saved to the new directories.")




################################################
############# remove score-transposed files #####
################################################

#############
# Define the directories for BMI, Height, and BRC transposed files
transposed_dir_bmi <- "~/NORMAL/2.2_bmi_transposed_files"
transposed_dir_height <- "~/NORMAL/2.2_height_transposed_files"
transposed_dir_brc <- "~/NORMAL/2.2_brc_transposed_files"

# Function to remove all files in a directory
remove_files_in_directory <- function(dir) {
  files <- list.files(path = dir, full.names = TRUE)
  file.remove(files)
  print(paste("All files in", dir, "have been removed."))
}

# Remove transposed files for BMI, Height, and BRC
remove_files_in_directory(transposed_dir_bmi)
remove_files_in_directory(transposed_dir_height)
remove_files_in_directory(transposed_dir_brc)

# Optional: Remove the empty directories as well
unlink(transposed_dir_bmi, recursive = TRUE)
unlink(transposed_dir_height, recursive = TRUE)
unlink(transposed_dir_brc, recursive = TRUE)

print("Transposed directories have been removed.")


