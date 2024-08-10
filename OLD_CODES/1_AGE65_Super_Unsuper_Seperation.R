####################
####################
# July 22, 2024
####################
####################

####################################################
####################################################
##### 1. Supervised & Unsupervised Separation ######
####################################################
####################################################

# Load the updated dataset with "race" column
updated_eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_Height.txt")
updated_eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BMI.txt")
updated_eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BrC.txt")

# Load necessary library
library(dplyr)
library(fs)

# Create directories
create_directories <- function(trait_name) {
  dir_create(paste0("2.2_AGE65_", trait_name, "/training_files"))
  dir_create(paste0("2.2_AGE65_", trait_name, "/testing_files"))
  dir_create(paste0("2.2_AGE65_", trait_name, "/test_subgroup_files"))
}

# Function to perform the sampling
perform_sampling <- function(data, trait_name, n_train = 3000, n_groups = 20, n_group_size = 3000, iterations = 37) {
  create_directories(trait_name)
  
  for (iter in 1:iterations) {
    cat("Iteration", iter, "of", iterations, "\n")
    
    # Filter individuals who are older than 65
    eligible_older_individuals <- data %>%
      filter(age > 65)
    
    # Sample 3000 individuals randomly from the eligible older individuals if available
    if(nrow(eligible_older_individuals) >= n_train) {
      train_indices <- sample(1:nrow(eligible_older_individuals), n_train)
      train_data <- eligible_older_individuals[train_indices, ]
      cat("Sampled 3000 individuals over 65 years of age for supervised training in", trait_name, "\n")
    } else {
      train_data <- eligible_older_individuals
      cat("Not enough individuals over 65 available in", trait_name, ". Only", nrow(train_data), "available.\n")
    }
    
    # Save training data to csv
    write.csv(train_data, paste0("2.2_AGE65_", trait_name, "/training_files/", trait_name, "_train_data_iteration_", iter, ".csv"), row.names = FALSE)
    
    # Get the rest as testing data
    test_data <- data[!rownames(data) %in% rownames(train_data), ]
    write.csv(test_data, paste0("2.2_AGE65_", trait_name, "/testing_files/", trait_name, "_test_data_iteration_", iter, ".csv"), row.names = FALSE)
    
    # Sample 20 groups of 3000 from testing data without replication within each group
    for (group in 1:n_groups) {
      group_indices <- sample(1:nrow(test_data), n_group_size)
      group_data <- test_data[group_indices, ]
      
      # Ensure no replication within each group
      group_data <- group_data[!duplicated(group_data), ]
      
      while (nrow(group_data) < n_group_size) {
        additional_indices <- sample(1:nrow(test_data), n_group_size - nrow(group_data))
        group_data <- rbind(group_data, test_data[additional_indices, ])
        group_data <- group_data[!duplicated(group_data), ]
      }
      
      write.csv(group_data, paste0("2.2_AGE65_", trait_name, "/test_subgroup_files/", trait_name, "_subgroup_", group, "_iteration_", iter, ".csv"), row.names = FALSE)
    }
  }
}

# Perform the sampling for each dataset
perform_sampling(updated_eMERGEIII_pheno_covar_Height, "height", iterations = 37)
perform_sampling(updated_eMERGEIII_pheno_covar_BMI, "bmi", iterations = 37)
perform_sampling(updated_eMERGEIII_pheno_covar_BrC, "brc", iterations = 37)







# Example Usage Adjustment
height_train_data_iteration_37 <- read.csv("2.2_AGE65_height/training_files/height_train_data_iteration_37.csv")
height_test_data_iteration_37 <- read.csv("2.2_AGE65_height/testing_files/height_test_data_iteration_37.csv")
height_subgroup_1_iteration_37 <- read.csv("2.2_AGE65_height/test_subgroup_files/height_subgroup_1_iteration_37.csv")

bmi_train_data_iteration_37 <- read.csv("2.2_AGE65_bmi/training_files/bmi_train_data_iteration_37.csv")
bmi_test_data_iteration_37 <- read.csv("2.2_AGE65_bmi/testing_files/bmi_test_data_iteration_37.csv")
bmi_subgroup_1_iteration_37 <- read.csv("2.2_AGE65_bmi/test_subgroup_files/bmi_subgroup_1_iteration_37.csv")

brc_train_data_iteration_37 <- read.csv("2.2_AGE65_brc/training_files/brc_train_data_iteration_37.csv")
brc_test_data_iteration_37 <- read.csv("2.2_AGE65_brc/testing_files/brc_test_data_iteration_37.csv")
brc_subgroup_1_iteration_37 <- read.csv("2.2_AGE65_brc/test_subgroup_files/brc_subgroup_1_iteration_37.csv")




##### RENAME SINGLE FILES  UNDER EACH FOLDER ####
# 
# ####################
# ####################
# # July 22, 2024
# ####################
# ####################
# 
# # Load necessary library
# library(fs)
# 
# # Function to rename files
# rename_files <- function(directory, trait_name, prefix, iterations, n_groups) {
#   for (iter in 1:iterations) {
#     # Rename training data files
#     old_train_file <- paste0(directory, "/", trait_name, "_train_data_iteration_", iter, ".csv")
#     new_train_file <- paste0(directory, "/", prefix, "_", trait_name, "_train_data_iteration_", iter, ".csv")
#     
#     if (file_exists(old_train_file)) {
#       file_move(old_train_file, new_train_file)
#     }
#     
#     # Rename testing data files
#     old_test_file <- paste0(directory, "/", trait_name, "_test_data_iteration_", iter, ".csv")
#     new_test_file <- paste0(directory, "/", prefix, "_", trait_name, "_test_data_iteration_", iter, ".csv")
#     
#     if (file_exists(old_test_file)) {
#       file_move(old_test_file, new_test_file)
#     }
#     
#     # Rename subgroup files
#     for (group in 1:n_groups) {
#       old_group_file <- paste0(directory, "/", trait_name, "_subgroup_", group, "_iteration_", iter, ".csv")
#       new_group_file <- paste0(directory, "/", prefix, "_", trait_name, "_subgroup_", group, "_iteration_", iter, ".csv")
#       
#       if (file_exists(old_group_file)) {
#         file_move(old_group_file, new_group_file)
#       }
#     }
#   }
# }
# 
# # Directories for AGE65 and RACE for each trait
# directories <- list(
#   list(directory = "2.2_AGE65_height/training_files", trait_name = "height", prefix = "AGE65"),
#   list(directory = "2.2_AGE65_height/testing_files", trait_name = "height", prefix = "AGE65"),
#   list(directory = "2.2_AGE65_height/test_subgroup_files", trait_name = "height", prefix = "AGE65"),
#   
#   list(directory = "2.2_AGE65_bmi/training_files", trait_name = "bmi", prefix = "AGE65"),
#   list(directory = "2.2_AGE65_bmi/testing_files", trait_name = "bmi", prefix = "AGE65"),
#   list(directory = "2.2_AGE65_bmi/test_subgroup_files", trait_name = "bmi", prefix = "AGE65"),
#   
#   list(directory = "2.2_AGE65_brc/training_files", trait_name = "brc", prefix = "AGE65"),
#   list(directory = "2.2_AGE65_brc/testing_files", trait_name = "brc", prefix = "AGE65"),
#   list(directory = "2.2_AGE65_brc/test_subgroup_files", trait_name = "brc", prefix = "AGE65"),
#   
#   list(directory = "2.2_RACE_height/training_files", trait_name = "height", prefix = "RACE"),
#   list(directory = "2.2_RACE_height/testing_files", trait_name = "height", prefix = "RACE"),
#   list(directory = "2.2_RACE_height/test_subgroup_files", trait_name = "height", prefix = "RACE"),
#   
#   list(directory = "2.2_RACE_bmi/training_files", trait_name = "bmi", prefix = "RACE"),
#   list(directory = "2.2_RACE_bmi/testing_files", trait_name = "bmi", prefix = "RACE"),
#   list(directory = "2.2_RACE_bmi/test_subgroup_files", trait_name = "bmi", prefix = "RACE"),
#   
#   list(directory = "2.2_RACE_brc/training_files", trait_name = "brc", prefix = "RACE"),
#   list(directory = "2.2_RACE_brc/testing_files", trait_name = "brc", prefix = "RACE"),
#   list(directory = "2.2_RACE_brc/test_subgroup_files", trait_name = "brc", prefix = "RACE")
# )
# 
# # Perform renaming for each directory and trait
# for (dir_info in directories) {
#   rename_files(dir_info$directory, dir_info$trait_name, dir_info$prefix, iterations = 37, n_groups = 20)
# }
