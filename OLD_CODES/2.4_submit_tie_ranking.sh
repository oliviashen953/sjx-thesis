#!/bin/bash
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 0-02:00:00
#SBATCH -p shared
#SBATCH --mem=8G
#SBATCH -c 1
#SBATCH -J process_all_files
#SBATCH -o process_all_files.out
#SBATCH -e process_all_files.err
#SBATCH --mail-user=your_email@domain.com

# Set R packages and Singularity image locations
my_packages=${HOME}/R/ifxrstudio/RELEASE_3_16
rstudio_singularity_image="/n/singularity_images/informatics/ifxrstudio/ifxrstudio:RELEASE_3_16.sif"

# Run the R script using the RStudio Server Singularity image
singularity exec --cleanenv --env R_LIBS_USER=${my_packages} ${rstudio_singularity_image} Rscript /n/home00/jiaxinshen/2.4_Tie_Rank_Conversion.R
