####################    
#################### 
# June 6, 2024
#################### 
#################### 

########################################        
########################################
######### 0. Initial Setup #############
########################################
########################################

  
############################################################################################## 
# Still use the eMERGE III dataset, ignore trait of T2D, focus solely on Height, BMI, and BrC.
##############################################################################################

# Load new eMERGE III dataset for Height/ BMI/ BRC
eMERGEIII_pheno_covar_Height <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_Height.txt", sep="")
View(eMERGEIII_pheno_covar_Height)
eMERGEIII_pheno_covar_BMI <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BMI.txt", sep="")
View(eMERGEIII_pheno_covar_BMI)
eMERGEIII_pheno_covar_BrC <- read.csv("~/new_emergeIII/eMERGEIII_pheno_covar_BrC.txt", sep="")
View(eMERGEIII_pheno_covar_BrC)


  # Step: Import Dataset -> From Text (Base) -> new_emergeIII -> eMERGEIII_pheno_covar_XXX.txt
  # Datasets: 
  # Height: 36899 subjects/rows * 76 cols (id, pheno, age, sex, and 72 PGS00XXXX)
  # BMI: 36461 subjects/rows * 56 cols (id, pheno, age, sex, and 52 PGS00XXXX)
  # BrC: 60043 subjects/rows * 112 cols (id, pheno, age, sex, and 108 PGS00XXXX)







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

  # Step: Import Dataset -> From Text (Base) -> new_emergeIII -> newEMERGE_WithRace -> updated_eMERGEIII_pheno_covar_XXX.txt
  # Datasets: 
  # Height: 36899 subjects/rows * 77 cols ("race", id, pheno, age, sex, and 73 PGS00XXXX)
  # BMI: 36461 subjects/rows * 57 cols ("race", id, pheno, age, sex, and 53 PGS00XXXX)
  # BrC: 60043 subjects/rows * 113 cols ("race", id, pheno, age, sex, and 109 PGS00XXXX)








