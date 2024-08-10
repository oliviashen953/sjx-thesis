####################    
#################### 
# June 6, 2024
#################### 
#################### 

####################################################      
####################################################
##### 1. Supervised & Unsupervised Separation ######
####################################################
####################################################

# load the updated dataset with "race" column
updated_eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_Height.txt")
View(updated_eMERGEIII_pheno_covar_Height)
updated_eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BMI.txt")
View(updated_eMERGEIII_pheno_covar_BMI)
updated_eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BrC.txt")
View(updated_eMERGEIII_pheno_covar_BrC)


########################################################################################################### 
# For the data updated_eMERGEIII_pheno_covar_Height
# Select random 3000 rows(subjects) repetitively as training data (save as csv file) 
# and leave rest as testing data (also save as csv file).

# Within the rest testing data, sample 20 groups of 3000 people as the subgroup in testing data
# where the 3000 people within each of the 20 groups cannot be repeated,
# but the people selected can be repeated between 20 groups.


# Repeat the above resampling for multiple times, meaning select a random 3000 as training
# repeatedly (in the outer loop), and within each selection of 3000 (within the loop), perform
# the sampling of 20 groups (at least, the more the better) of 3000 people (at least, the more the better).
###########################################################################################################

# RANDOM NO REP WITHIN BUT CAN REPLICATE BETEEN GROUPS 



# Load necessary library
library(dplyr)

# Function to perform the sampling
# perform_sampling <- function(data, n_train = 3000, n_groups = 20, n_group_size = 3000, iterations = 1) {
#   
#   # Loop for multiple iterations
#   for (iter in 1:iterations) {
#     
#     # Sample 3000 rows for training
#     # 'train_indices <- sample(1:nrow(data), n_train) ensures that 3000 unique rows are sampled without replacement for the training data.
#     train_indices <- sample(1:nrow(data), n_train) # The sample() function in R by default samples without replacement
#     
#     # train_data <- data[train_indices, ] assigns these unique rows to train_data.
#     train_data <- data[train_indices, ]
#     
#     # Save training data to csv
#     write.csv(train_data, paste0("train_data_iteration_", iter, ".csv"), row.names = FALSE)
#     
#     # Get the rest as testing data
#     # 'test_data <- data[-train_indices, ] ensures that the testing data consists of the remaining rows not included in the training data.
#     test_data <- data[-train_indices, ]
#     
#     # Save testing data to csv
#     write.csv(test_data, paste0("test_data_iteration_", iter, ".csv"), row.names = FALSE)
#     
#     # Sample 20 groups of 3000 from testing data
#     for (group in 1:n_groups) {
#       #group_indices <- sample(1:nrow(test_data), n_group_size) ensures that 3000 unique rows are sampled without replacement for each subgroup.
#       group_indices <- sample(1:nrow(test_data), n_group_size) # The sample() function in R by default samples without replacement
#       
#       #group_data <- test_data[unique(group_indices), ] ensures the selected rows are unique within each subgroup.
#       group_data <- test_data[unique(group_indices), ]  # Ensure unique rows within each group
#       
#       # Check if the group_data has enough unique rows
#       while (nrow(group_data) < n_group_size) {
#         additional_indices <- sample(1:nrow(test_data), n_group_size - nrow(group_data))
#         group_data <- rbind(group_data, test_data[unique(additional_indices), ])
#         group_data <- group_data[!duplicated(group_data), ]  # Remove duplicates if any
#       }
#       
#       # Save each subgroup to csv
#       write.csv(group_data, paste0("subgroup_", group, "_iteration_", iter, ".csv"), row.names = FALSE)
#     }
#   }
# }


perform_sampling <- function(data, n_train = 3000, n_groups = 20, n_group_size = 3000, iterations = 1) {
  
  # Loop for multiple iterations
  for (iter in 1:iterations) {
    
    # Sample 3000 rows for training
    train_indices <- sample(1:nrow(data), n_train) # The sample() function in R by default samples without replacement
    train_data <- data[train_indices, ]
    
    # Save training data to csv
    write.csv(train_data, paste0("train_data_iteration_", iter, ".csv"), row.names = FALSE)
    
    # Get the rest as testing data
    test_data <- data[-train_indices, ]
    
    # Save testing data to csv
    write.csv(test_data, paste0("test_data_iteration_", iter, ".csv"), row.names = FALSE)
    
    # Sample 20 groups of 3000 from testing data
    for (group in 1:n_groups) {
      group_indices <- sample(1:nrow(test_data), n_group_size) # The sample() function in R by default samples without replacement
      group_data <- test_data[group_indices, ]  # Ensure unique rows within each group
      
      # Check if the group_data has enough unique rows
      while (nrow(group_data) < n_group_size) {
        additional_indices <- sample(1:nrow(test_data), n_group_size - nrow(group_data))
        group_data <- rbind(group_data, test_data[additional_indices, ])
        group_data <- unique(group_data)  # Ensure unique rows
      }
      
      # Save each subgroup to csv
      write.csv(group_data, paste0("subgroup_", group, "_iteration_", iter, ".csv"), row.names = FALSE)
    }
  }
}


# Load your data (CHANGE TO CORRECT DATASET WE WANT, EITHER : HEIGHT/ BMI/ BRC)
data <- updated_eMERGEIII_pheno_covar_BrC

# Perform the sampling
perform_sampling(data, iterations = 37)

    # Output files
    # train_data_iteration_37.csv 
    # test_data_iteration_37.csv
      # subgroup_1_iteration_37.csv
      # ....
      # subgroup_20_iteration_37.csv

    # Example Usage: train_data_iteration_37 <- read_csv("train_data_iteration_37.csv")



#############################################################################
##### Rename them by adding "height_" to the beginning of each filename #####
#############################################################################

# Load necessary library
library(stringr)

# Define function to rename files
rename_files <- function(directory, iterations, n_groups) {
  
  # Loop through iterations
  for (iter in 1:iterations) {
    # Rename training data files
    old_train_file <- paste0("train_data_iteration_", iter, ".csv")
    new_train_file <- paste0("brc_train_data_iteration_", iter, ".csv") # CHANGE HEIGHT/ BMI/ BRC
    
    if (file.exists(file.path(directory, old_train_file))) {
      file.rename(file.path(directory, old_train_file), file.path(directory, new_train_file))
      cat("Renamed", old_train_file, "to", new_train_file, "\n")
    }
    
    # Rename testing data files
    old_test_file <- paste0("test_data_iteration_", iter, ".csv")
    new_test_file <- paste0("brc_test_data_iteration_", iter, ".csv") # CHANGE HEIGHT/ BMI/ BRC
    
    if (file.exists(file.path(directory, old_test_file))) {
      file.rename(file.path(directory, old_test_file), file.path(directory, new_test_file))
      cat("Renamed", old_test_file, "to", new_test_file, "\n")
    }
    
    # Loop through groups to rename subgroup files
    for (group in 1:n_groups) {
      old_group_file <- paste0("subgroup_", group, "_iteration_", iter, ".csv")
      new_group_file <- paste0("brc_subgroup_", group, "_iteration_", iter, ".csv") # CHANGE HEIGHT/ BMI/ BRC
      
      if (file.exists(file.path(directory, old_group_file))) {
        file.rename(file.path(directory, old_group_file), file.path(directory, new_group_file))
        cat("Renamed", old_group_file, "to", new_group_file, "\n")
      }
    }
  }
}

# Specify directory where your files are located
directory <- "~/"  # Change this to your directory

# Perform renaming
rename_files(directory, iterations = 37, n_groups = 20)

  # Renamed test_data_iteration_37.csv to "height_test_data_iteration_37.csv"
  # Renamed train_data_iteration_37.csv to "height_train_data_iteration_37.csv" 
    # Renamed subgroup_1_iteration_37.csv to "height_subgroup_1_iteration_37.csv" 
    # ... 
    # Renamed subgroup_20_iteration_37.csv to "height_subgroup_20_iteration_37.csv"
  

    # Output files
    # height_train_data_iteration_37.csv 
    # height_test_data_iteration_37.csv
      # height_subgroup_1_iteration_37.csv
      # ....
      # height_subgroup_20_iteration_37.csv
    
    # Example Usage: height_train_data_iteration_37 <- read_csv("height_train_data_iteration_37.csv")






#### Example Usage Adjustment ####


# Example Usage for HEIGHT dataset
height_train_data_iteration_37 <- read.csv("height_train_data_iteration_37.csv")
height_test_data_iteration_37 <- read.csv("height_test_data_iteration_37.csv")
height_subgroup_1_iteration_37 <- read.csv("height_subgroup_1_iteration_37.csv")
# ... (similarly for other subgroups)

# Example Usage for BMI dataset
bmi_train_data_iteration_37 <- read.csv("bmi_train_data_iteration_37.csv")
bmi_test_data_iteration_37 <- read.csv("bmi_test_data_iteration_37.csv")
bmi_subgroup_1_iteration_37 <- read.csv("bmi_subgroup_1_iteration_37.csv")
# ... (similarly for other subgroups)

# Example Usage for BRC dataset
brc_train_data_iteration_37 <- read.csv("brc_train_data_iteration_37.csv")
brc_test_data_iteration_37 <- read.csv("brc_test_data_iteration_37.csv")
brc_subgroup_1_iteration_37 <- read.csv("brc_subgroup_1_iteration_37.csv")
# ... (similarly for other subgroups)










############################################################################################## 
#### Supplement codes ####
##############################################################################################


# # Add progress printing to the above recurring loop

# # Load necessary library
# library(dplyr)
# 
# # Function to perform the sampling
# perform_sampling <- function(data, n_train = 3000, n_groups = 20, n_group_size = 3000, iterations = 1) {
#   
#   # Loop for multiple iterations
#   for (iter in 1:iterations) {
#     cat("Iteration", iter, "of", iterations, "starting...\n")
#     
#     # Sample 3000 rows for training
#     train_indices <- sample(1:nrow(data), n_train)
#     train_data <- data[train_indices, ]
#     
#     # Save training data to csv
#     write.csv(train_data, paste0("train_data_iteration_", iter, ".csv"), row.names = FALSE)
#     
#     # Get the rest as testing data
#     test_data <- data[-train_indices, ]
#     
#     # Save testing data to csv
#     write.csv(test_data, paste0("test_data_iteration_", iter, ".csv"), row.names = FALSE)
#     
#     # Sample 20 groups of 3000 from testing data
#     for (group in 1:n_groups) {
#       group_indices <- sample(1:nrow(test_data), n_group_size)
#       group_data <- test_data[unique(group_indices), ]  # Ensure unique rows within each group
#       
#       # Check if the group_data has enough unique rows
#       while (nrow(group_data) < n_group_size) {
#         additional_indices <- sample(1:nrow(test_data), n_group_size - nrow(group_data))
#         group_data <- rbind(group_data, test_data[unique(additional_indices), ])
#         group_data <- group_data[!duplicated(group_data), ]  # Remove duplicates if any
#       }
#       
#       # Save each subgroup to csv
#       write.csv(group_data, paste0("subgroup_", group, "_iteration_", iter, ".csv"), row.names = FALSE)
#       
#       # Print progress for subgroups
#       cat("Iteration", iter, "-", round((group / n_groups) * 100), "% of subgroups complete\n")
#     }
#     
#     # Print progress for iterations
#     cat("Iteration", iter, "complete\n")
#   }
# }
# 
# # Load your data
# data <- read.csv("updated_eMERGEIII_pheno_covar_Height.csv")
# 
# # Perform the sampling
# perform_sampling(data, iterations = 200)
# 
