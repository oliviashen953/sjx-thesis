################################################################################        
################################################################################
#########1. Supervised & Unsupervised Separation  #############
################################################################################
################################################################################


############################################################################################## 
# Still use the eMERGE III dataset, ignore trait of T2D, focus solely on Height, BMI, and BrC.
##############################################################################################

################################################################################################ 
# Add the new "race" column. The new 5 covariate columns are: subject ID, pheno, sex, age, race
################################################################################################

# Or directly load the existing dataset with "race" column
updated_eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_Height.txt")
View(updated_eMERGEIII_pheno_covar_Height)
updated_eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BMI.txt")
View(updated_eMERGEIII_pheno_covar_BMI)
updated_eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/newEMERGE_WithRace/updated_eMERGEIII_pheno_covar_BrC.txt")
View(updated_eMERGEIII_pheno_covar_BrC)


# Load necessary libraries
library(dplyr)
library(stringr)

# Function to perform the sampling and save files in newly created folders
perform_sampling <- function(data, prefix, n_train = 3000, n_groups = 20, n_group_size = 3000, iterations = 1, directory = "~/") {
  
  # Create directory for saving files if it doesn't exist
  dir.create(file.path(directory, prefix), showWarnings = FALSE)
  
  # Loop for multiple iterations
  for (iter in 1:iterations) {
    
    # Sample 3000 rows for training
    train_indices <- sample(1:nrow(data), n_train) # The sample() function in R by default samples without replacement
    train_data <- data[train_indices, ]
    
    # Save training data to csv
    write.csv(train_data, file.path(directory, prefix, paste0(prefix, "_train_data_iteration_", iter, ".csv")), row.names = FALSE)
    
    # Get the rest as testing data
    test_data <- data[-train_indices, ]
    
    # Save testing data to csv
    write.csv(test_data, file.path(directory, prefix, paste0(prefix, "_test_data_iteration_", iter, ".csv")), row.names = FALSE)
    
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
      write.csv(group_data, file.path(directory, prefix, paste0(prefix, "_subgroup_", group, "_iteration_", iter, ".csv")), row.names = FALSE)
    }
  }
}

# Load your data and specify prefixes
height_data <- updated_eMERGEIII_pheno_covar_Height
bmi_data <- updated_eMERGEIII_pheno_covar_BMI
brc_data <- updated_eMERGEIII_pheno_covar_BrC

# Perform the sampling and save files in separate folders
perform_sampling(height_data, prefix = "height", iterations = 30, directory = "~/")
perform_sampling(bmi_data, prefix = "bmi", iterations = 30, directory = "~/")
perform_sampling(brc_data, prefix = "brc", iterations = 30, directory = "~/")

# Example Usage:
# height_train_data_iteration_37 <- read.csv("~/new_emergeIII/results/height/height_train_data_iteration_37.csv")
# bmi_train_data_iteration_37 <- read.csv("~/new_emergeIII/results/bmi/bmi_train_data_iteration_37.csv")
# brc_train_data_iteration_37 <- read.csv("~/new_emergeIII/results/brc/brc_train_data_iteration_37.csv")
